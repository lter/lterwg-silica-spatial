suppressPackageStartupMessages({
  library(dplyr)
  library(data.table)
})

# Shared cleanup from combine_qaqc
source(file.path("04_combine_qaqc", "00_qaqc_functions.R"))

# Stream name cleanup used across all harmonization joins
harmonize_stream_name <- function(x) {
  x <- norm_chr(x)
  x <- normalize_stream_key(x)
  x <- ifelse(x == "amazon river at obidos", "obidos", x)
  x
}

# Base table

# Read the vetted combined table and attach Stream_ID
read_harmonized_base_table <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  df <- prepare_combined_table(df, sanitize_new = FALSE)
  df
}

# Q summaries

# Read annual WRTDS outputs and keep Year + Q for each Stream_ID
read_wrtds_annual_q <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

  if (all(c("LTER.x", "Stream_Name", "DecYear", "Q") %in% names(df))) {
    lter <- df[["LTER.x"]]
    stream_name <- df[["Stream_Name"]]
    year <- floor(suppressWarnings(as.numeric(df[["DecYear"]])))
    q <- suppressWarnings(as.numeric(df[["Q"]]))
  } else if (all(c("LTER", "Stream_Name", "Year", "Discharge_cms") %in% names(df))) {
    lter <- df[["LTER"]]
    stream_name <- df[["Stream_Name"]]
    year <- suppressWarnings(as.integer(df[["Year"]]))
    q <- suppressWarnings(as.numeric(df[["Discharge_cms"]]))
  } else {
    stop(
      "WRTDS annual file needs either LTER.x + Stream_Name + DecYear + Q or LTER + Stream_Name + Year + Discharge_cms.",
      call. = FALSE
    )
  }

  data.frame(
    LTER = lter,
    Stream_Name = stream_name,
    Year = year,
    Q = q,
    stringsAsFactors = FALSE
  ) %>%
    transmute(
      LTER = norm_lter(LTER),
      Stream_Name = harmonize_stream_name(Stream_Name),
      Stream_ID = build_stream_id(data.frame(
        LTER = norm_lter(LTER),
        Stream_Name = harmonize_stream_name(Stream_Name),
        stringsAsFactors = FALSE
      )),
      Year = Year,
      Q = Q
    ) %>%
    filter(!is.na(Stream_ID), Stream_ID != "", !is.na(Year)) %>%
    group_by(Stream_ID, Year) %>%
    summarise(
      Q = dplyr::first(Q),
      .groups = "drop"
    )
}

# Summarize site-level Q variables
summarize_wrtds_q <- function(wrtds_q) {
  wrtds_q %>%
    group_by(Stream_ID) %>%
    summarise(
      n_q_years = sum(!is.na(Q)),
      q_start_year = suppressWarnings(min(Year[!is.na(Q)])),
      q_end_year = suppressWarnings(max(Year[!is.na(Q)])),
      mean_q = mean(Q, na.rm = TRUE),
      med_q = median(Q, na.rm = TRUE),
      sd_q = sd(Q, na.rm = TRUE),
      min_Q = min(Q, na.rm = TRUE),
      max_Q = max(Q, na.rm = TRUE),
      q_95 = as.numeric(stats::quantile(Q, 0.95, na.rm = TRUE, names = FALSE)),
      q_5 = as.numeric(stats::quantile(Q, 0.05, na.rm = TRUE, names = FALSE)),
      .groups = "drop"
    ) %>%
    mutate(
      CV_Q = dplyr::if_else(is.na(mean_q) | mean_q == 0, NA_real_, sd_q / mean_q)
    )
}

# Join site-level Q summary variables
add_wrtds_q_summary <- function(df, q_summary) {
  df %>% left_join(q_summary, by = "Stream_ID")
}

# Output names

# Build dated output names
harmonization_file_name <- function(stem, date_tag, ext = "csv") {
  paste0(stem, "_", date_tag, ".", ext)
}

# RBI and recession slope

# Read the daily discharge file in either old or new format
read_daily_discharge_input <- function(path) {
  df <- fread(path, data.table = FALSE)

  if (all(c("Stream_ID", "Date", "Q") %in% names(df))) {
    df <- df %>%
      mutate(
        Stream_ID = trimws(as.character(Stream_ID)),
        Date = as.Date(Date),
        Q = suppressWarnings(as.numeric(Q))
      )
  } else if (all(c("LTER", "Stream_Name", "Date", "Qcms") %in% names(df))) {
    df <- df %>%
      transmute(
        Stream_ID = build_stream_id(data.frame(
          LTER = norm_lter(LTER),
          Stream_Name = harmonize_stream_name(Stream_Name),
          stringsAsFactors = FALSE
        )),
        Date = as.Date(Date),
        Q = suppressWarnings(as.numeric(Qcms))
      )
  } else {
    stop(
      "Daily discharge file needs Stream_ID + Date + Q or LTER + Stream_Name + Date + Qcms",
      call. = FALSE
    )
  }

  df %>%
    filter(!is.na(Stream_ID), Stream_ID != "", !is.na(Date), !is.na(Q))
}

# Compute site-level discharge metrics from daily Q
# RBI uses absolute day-to-day change
# recession_slope uses a log-log fit on recession days only
compute_discharge_metrics <- function(daily_q, min_recession_days = 50L) {
  q_diff <- daily_q %>%
    arrange(Stream_ID, Date) %>%
    group_by(Stream_ID) %>%
    mutate(
      dQ = Q - lag(Q),
      change_dQ = Q / lag(Q),
      dQ_dt = dQ / as.numeric(Date - lag(Date))
    ) %>%
    filter(!is.na(dQ_dt)) %>%
    filter(!(change_dQ < 0.7))

  recession_slopes <- q_diff %>%
    filter(dQ < 0) %>%
    mutate(recession_slope_day = -dQ_dt) %>%
    group_by(Stream_ID) %>%
    filter(!if_any(where(is.numeric), ~ . == Inf | . == -Inf)) %>%
    summarise(
      n_recession_days = n(),
      recession_slope = if (n_recession_days >= min_recession_days) {
        fit <- lm(log(recession_slope_day) ~ log(Q), data = pick(everything()))
        unname(coef(fit)[2])
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    filter(!is.na(recession_slope), recession_slope >= 0)

  flashiness <- daily_q %>%
    group_by(Stream_ID) %>%
    arrange(Date) %>%
    mutate(
      dQ = Q - lag(Q),
      abs_dQ = abs(dQ)
    ) %>%
    filter(!is.na(abs_dQ)) %>%
    summarise(
      total_discharge = sum(Q, na.rm = TRUE),
      total_change = sum(abs_dQ, na.rm = TRUE),
      RBI = total_change / total_discharge,
      .groups = "drop"
    )

  full_join(recession_slopes, flashiness, by = "Stream_ID")
}

# KG class

# Join Köppen-Geiger classes by Stream_ID
add_kg_table <- function(df, kg_path) {
  kg <- read.csv(kg_path, stringsAsFactors = FALSE, check.names = FALSE)

  if (!"Stream_ID" %in% names(kg)) {
    if (all(c("LTER", "Stream_Name") %in% names(kg))) {
      kg$LTER <- norm_lter(kg$LTER)
      kg$Stream_Name <- harmonize_stream_name(kg$Stream_Name)
      kg$Stream_ID <- build_stream_id(kg)
    } else {
      stop("KG file needs Stream_ID or LTER + Stream_Name.", call. = FALSE)
    }
  }

  keep_cols <- intersect(c("Stream_ID", "ClimateZ", "Name"), names(kg))
  kg <- kg[, keep_cols, drop = FALSE]
  names(kg)[names(kg) == "ClimateZ"] <- "KG_class"
  names(kg)[names(kg) == "Name"] <- "KG_name"
  kg <- kg %>% distinct(Stream_ID, .keep_all = TRUE)

  df %>% left_join(kg, by = "Stream_ID")
}

# Lookup helpers

# Pull the first matching column from a messy data frame
first_named_column <- function(df, name) {
  idx <- which(names(df) == name)[1]
  if (is.na(idx)) {
    return(NULL)
  }
  df[[idx]]
}

# Build a Stream_ID to latitude lookup for daylength
read_site_latitude_lookup <- function(reference_path, kg_path = "") {
  lookup_list <- list()

  if (nzchar(reference_path) && file.exists(reference_path)) {
    ref <- read.csv(reference_path, stringsAsFactors = FALSE, check.names = FALSE)
    ref_lter <- first_named_column(ref, "LTER")
    ref_stream <- first_named_column(ref, "Stream_Name")
    ref_lat <- first_named_column(ref, "Latitude")
    if (!is.null(ref_lter) && !is.null(ref_stream) && !is.null(ref_lat)) {
      ref <- data.frame(
        Stream_ID = build_stream_id(data.frame(
          LTER = norm_lter(ref_lter),
          Stream_Name = harmonize_stream_name(ref_stream),
          stringsAsFactors = FALSE
        )),
        Latitude = suppressWarnings(as.numeric(ref_lat)),
        stringsAsFactors = FALSE
      )
      ref <- ref[!is.na(ref$Stream_ID) & ref$Stream_ID != "" & !is.na(ref$Latitude), , drop = FALSE]
      ref <- ref[!duplicated(ref$Stream_ID), , drop = FALSE]
      lookup_list[[length(lookup_list) + 1L]] <- ref
    }
  }

  if (nzchar(kg_path) && file.exists(kg_path)) {
    kg <- read.csv(kg_path, stringsAsFactors = FALSE, check.names = FALSE)
    kg_lter <- first_named_column(kg, "LTER")
    kg_stream <- first_named_column(kg, "Stream_Name")
    kg_lat <- first_named_column(kg, "Latitude")
    if (!is.null(kg_lter) && !is.null(kg_stream) && !is.null(kg_lat)) {
      kg <- data.frame(
        Stream_ID = build_stream_id(data.frame(
          LTER = norm_lter(kg_lter),
          Stream_Name = harmonize_stream_name(kg_stream),
          stringsAsFactors = FALSE
        )),
        Latitude = suppressWarnings(as.numeric(kg_lat)),
        stringsAsFactors = FALSE
      )
      kg <- kg[!is.na(kg$Stream_ID) & kg$Stream_ID != "" & !is.na(kg$Latitude), , drop = FALSE]
      kg <- kg[!duplicated(kg$Stream_ID), , drop = FALSE]
      lookup_list[[length(lookup_list) + 1L]] <- kg
    }
  }

  if (!length(lookup_list)) {
    return(data.frame(Stream_ID = character(), Latitude = numeric(), stringsAsFactors = FALSE))
  }

  bind_rows(lookup_list) %>%
    distinct(Stream_ID, .keep_all = TRUE)
}

# Max daylength

# Compute the maximum monthly mean daylength from latitude
compute_max_daylength <- function(latitude_deg) {
  doy <- 1:365
  latitude_rad <- latitude_deg * pi / 180
  declination <- 0.409 * sin((2 * pi / 365) * doy - 1.39)
  sunset_hour_angle <- acos(pmin(pmax(-tan(latitude_rad) * tan(declination), -1), 1))
  daylength_hours <- 24 / pi * sunset_hour_angle

  month_index <- format(as.Date(doy - 1, origin = "2015-01-01"), "%m")
  monthly_means <- tapply(daylength_hours, month_index, mean, na.rm = TRUE)
  max(as.numeric(monthly_means), na.rm = TRUE)
}

# Add site-level max daylength
add_max_daylength_values <- function(df, reference_path, kg_path = "") {
  lat_lookup <- read_site_latitude_lookup(reference_path, kg_path)
  if (!nrow(lat_lookup)) {
    df$max_daylength <- NA_real_
    return(df)
  }

  lat_lookup$max_daylength <- vapply(lat_lookup$Latitude, compute_max_daylength, numeric(1))

  df %>%
    left_join(lat_lookup[, c("Stream_ID", "max_daylength")], by = "Stream_ID")
}

# Basin slope fill

# Fill missing basin slope values from the US and Krycklan fill tables
gap_fill_basin_slope_values <- function(df, us_slope_path, krycklan_slope_path, stream_id_key_path) {
  if (!"basin_slope_mean_degree" %in% names(df)) {
    df$basin_slope_mean_degree <- NA_real_
  }

  if (!all(file.exists(c(us_slope_path, krycklan_slope_path, stream_id_key_path)))) {
    return(df)
  }

  stream_key <- read.csv(stream_id_key_path, stringsAsFactors = FALSE, check.names = FALSE)

  krycklan_slopes <- read.csv(krycklan_slope_path, stringsAsFactors = FALSE, check.names = FALSE) %>%
    transform(basin_slope_mean_degree = atan(gradient_pct / 100) * (180 / pi)) %>%
    left_join(stream_key, by = "Stream_Name") %>%
    filter(!is.na(Stream_ID), !is.na(basin_slope_mean_degree)) %>%
    select(Stream_ID, basin_slope_mean_degree)

  us_slopes <- read.csv(us_slope_path, header = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
  colnames(us_slopes) <- us_slopes[1, ]
  us_slopes <- us_slopes[-1, ] %>%
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "Stream_Name",
      values_to = "basin_slope_mean_degree"
    ) %>%
    mutate(basin_slope_mean_degree = as.numeric(basin_slope_mean_degree)) %>%
    left_join(stream_key, by = "Stream_Name") %>%
    filter(!is.na(Stream_ID), !is.na(basin_slope_mean_degree)) %>%
    select(Stream_ID, basin_slope_mean_degree)

  fill_df <- df %>%
    filter(is.na(basin_slope_mean_degree)) %>%
    distinct(Stream_ID) %>%
    left_join(us_slopes, by = "Stream_ID") %>%
    left_join(krycklan_slopes, by = "Stream_ID", suffix = c("_US", "_KR")) %>%
    mutate(
      basin_slope_mean_degree = dplyr::coalesce(
        basin_slope_mean_degree_US,
        basin_slope_mean_degree_KR
      )
    ) %>%
    select(Stream_ID, basin_slope_mean_degree)

  df %>%
    left_join(fill_df, by = "Stream_ID", suffix = c("", "_fill")) %>%
    mutate(
      basin_slope_mean_degree = dplyr::coalesce(
        basin_slope_mean_degree,
        basin_slope_mean_degree_fill
      )
    ) %>%
    select(-basin_slope_mean_degree_fill)
}

# Output writing

# Write the harmonized table and a simple non-missing summary
write_harmonization_outputs <- function(df, output_dir, date_tag) {
  out_file <- file.path(
    output_dir,
    harmonization_file_name("harmonized-spatial-drivers", date_tag)
  )

  summary_file <- file.path(
    output_dir,
    harmonization_file_name("harmonized-spatial-drivers-summary", date_tag)
  )

  variable_summary <- data.frame(
    variable = names(df),
    n_non_missing = vapply(df, function(x) sum(!is.na(x) & as.character(x) != ""), numeric(1)),
    stringsAsFactors = FALSE
  )

  write.csv(df, out_file, row.names = FALSE, na = "")
  write.csv(variable_summary, summary_file, row.names = FALSE)

  list(out_file = out_file, summary_file = summary_file)
}
