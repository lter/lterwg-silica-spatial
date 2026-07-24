librarian::shelf(dplyr, data.table, tidyr, stringr)

source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

norm_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

norm_lter <- function(x) {
  canonical_lter_label(norm_chr(x))
}

build_stream_id <- function(df) {
  paste(
    norm_lter(df$LTER),
    normalize_stream_key(df$Stream_Name),
    sep = "__"
  )
}

build_harmonization_key <- function(df) {
  paste(
    tolower(norm_lter(df$LTER)),
    normalize_stream_key(df$Stream_Name),
    tolower(norm_chr(df$Discharge_File_Name)),
    tolower(norm_chr(df$Shapefile_Name)),
    sep = "||"
  )
}

prepare_combined_table <- function(df) {
  for (key in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) {
    if (!key %in% names(df)) {
      df[[key]] <- NA_character_
    }
  }

  df$LTER <- norm_lter(df$LTER)
  df$Stream_Name <- norm_chr(df$Stream_Name)
  df$Discharge_File_Name <- norm_chr(df$Discharge_File_Name)
  df$Shapefile_Name <- norm_chr(df$Shapefile_Name)
  df$Stream_ID <- build_stream_id(df)
  df$key <- build_harmonization_key(df)
  df %>% dplyr::distinct(key, .keep_all = TRUE)
}

# Stream name cleanup used across all harmonization joins
harmonize_stream_name <- function(x) {
  x <- norm_chr(x)
  normalize_stream_key(x)
}

# Base table

# Read the vetted combined table and attach Stream_ID
read_harmonized_base_table <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  df <- prepare_combined_table(df)
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

# Compute discharge metrics over a grouping window
# RBI uses absolute day-to-day change divided by total discharge
# RCS is the log-log recession slope from recession days only
compute_discharge_metrics_by_group <- function(daily_q, group_cols, min_recession_days = 50L) {
  q_diff <- daily_q %>%
    arrange(across(all_of(c(group_cols, "Date")))) %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      dQ = Q - lag(Q),
      change_dQ = Q / lag(Q),
      day_gap = as.numeric(Date - lag(Date)),
      dQ_dt = dQ / day_gap
    ) %>%
    ungroup() %>%
    filter(!is.na(dQ_dt), !is.na(change_dQ), day_gap > 0) %>%
    filter(change_dQ >= 0.7)

  recession_slopes <- q_diff %>%
    filter(dQ < 0, Q > 0) %>%
    mutate(recession_slope_day = -dQ_dt) %>%
    filter(is.finite(recession_slope_day), recession_slope_day > 0) %>%
    group_by(across(all_of(group_cols))) %>%
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
    mutate(
      recession_slope = if_else(
        is.finite(recession_slope) & recession_slope >= 0,
        recession_slope,
        NA_real_
      ),
      RCS = recession_slope
    )

  flashiness <- daily_q %>%
    arrange(across(all_of(c(group_cols, "Date")))) %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      dQ = Q - lag(Q),
      abs_dQ = abs(dQ)
    ) %>%
    ungroup() %>%
    filter(!is.na(abs_dQ)) %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      total_discharge = sum(Q, na.rm = TRUE),
      total_change = sum(abs_dQ, na.rm = TRUE),
      RBI = if_else(total_discharge > 0, total_change / total_discharge, NA_real_),
      .groups = "drop"
    )

  full_join(recession_slopes, flashiness, by = group_cols)
}

# Compute site-level discharge metrics from daily Q
compute_discharge_metrics <- function(daily_q, min_recession_days = 50L) {
  compute_discharge_metrics_by_group(
    daily_q = daily_q,
    group_cols = "Stream_ID",
    min_recession_days = min_recession_days
  )
}

# Compute annual discharge metrics from daily Q
compute_annual_discharge_metrics <- function(daily_q, min_recession_days = 20L) {
  daily_q %>%
    mutate(Year = as.integer(format(Date, "%Y"))) %>%
    filter(!is.na(Year)) %>%
    compute_discharge_metrics_by_group(
      group_cols = c("Stream_ID", "Year"),
      min_recession_days = min_recession_days
    )
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

# GEE/GLC land cover

clean_gee_glc_class_name <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

first_available_character_col <- function(df, candidates) {
  found <- intersect(candidates, names(df))
  if (!length(found)) {
    return(rep(NA_character_, nrow(df)))
  }
  as.character(df[[found[[1]]]])
}

# Add wide GEE/GLC simple-class land-cover columns by stream name
add_gee_glc_land_cover <- function(df, lulc_path) {
  if (!file.exists(lulc_path)) {
    stop("Missing GEE/GLC LULC file: ", lulc_path, call. = FALSE)
  }

  gee <- read.csv(lulc_path, stringsAsFactors = FALSE, check.names = FALSE)
  unnamed_index_cols <- is.na(names(gee)) | !nzchar(trimws(names(gee)))
  if (any(unnamed_index_cols)) {
    gee <- gee[, !unnamed_index_cols, drop = FALSE]
  }
  required <- c("Stream_Name", "Year", "Simple_Class", "LandClass_sum")
  missing_required <- setdiff(required, names(gee))
  if (length(missing_required)) {
    stop(
      "GEE/GLC LULC file is missing required columns: ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  old_land_cols <- grep(
    "^(gee_glc_|gee_lulc_|glc_|gee_glc_match$|gee_glc_match_method$)",
    names(df),
    value = TRUE
  )
  df_base <- df[, setdiff(names(df), old_land_cols), drop = FALSE]

  gee_long <- gee %>%
    transmute(
      gee_stream_raw = as.character(Stream_Name),
      .gee_stream_key = normalize_stream_key(Stream_Name),
      Year = suppressWarnings(as.integer(Year)),
      Simple_Class = clean_gee_glc_class_name(Simple_Class),
      LandClass_sum = suppressWarnings(as.numeric(LandClass_sum))
    ) %>%
    filter(
      !is.na(.gee_stream_key),
      nzchar(.gee_stream_key),
      !is.na(Year),
      nzchar(Simple_Class)
    )

  gee_stream_variants <- gee_long %>%
    distinct(.gee_stream_key, gee_stream_raw)

  duplicate_gee_keys <- gee_stream_variants %>%
    count(.gee_stream_key, name = "n_stream_name_variants") %>%
    filter(n_stream_name_variants > 1)

  unique_gee_keys <- setdiff(
    unique(gee_stream_variants$.gee_stream_key),
    duplicate_gee_keys$.gee_stream_key
  )

  # Exact matches preserve stream variants that normalize to the same key.
  gee_exact <- gee_long %>%
    mutate(.gee_match_id = paste0("exact:", gee_stream_raw))

  gee_keyed <- gee_long %>%
    filter(.gee_stream_key %in% unique_gee_keys) %>%
    mutate(.gee_match_id = paste0("key:", .gee_stream_key))

  gee_wide <- bind_rows(gee_exact, gee_keyed) %>%
    group_by(.gee_match_id, Year, Simple_Class) %>%
    summarise(
      LandClass_sum = if (all(is.na(LandClass_sum))) NA_real_ else mean(LandClass_sum, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(.gee_col = paste("gee_glc", Year, Simple_Class, sep = "_")) %>%
    select(.gee_match_id, .gee_col, LandClass_sum) %>%
    pivot_wider(names_from = .gee_col, values_from = LandClass_sum)

  stream_raw <- first_available_character_col(df_base, c("Stream_Name"))
  df_keyed <- df_base %>%
    mutate(
      .gee_stream_raw = stream_raw,
      .gee_stream_key = normalize_stream_key(.gee_stream_raw),
      gee_glc_match_method = case_when(
        .gee_stream_raw %in% gee_long$gee_stream_raw ~ "exact stream name",
        .gee_stream_key %in% unique_gee_keys ~ "normalized stream name",
        TRUE ~ "no GEE/GLC match"
      ),
      .gee_match_id = case_when(
        gee_glc_match_method == "exact stream name" ~ paste0("exact:", .gee_stream_raw),
        gee_glc_match_method == "normalized stream name" ~ paste0("key:", .gee_stream_key),
        TRUE ~ NA_character_
      )
    )

  out <- left_join(df_keyed, gee_wide, by = ".gee_match_id", na_matches = "never") %>%
    mutate(gee_glc_match = gee_glc_match_method != "no GEE/GLC match") %>%
    select(-.gee_stream_raw, -.gee_stream_key, -.gee_match_id)

  gee_value_cols <- grep("^gee_glc_[0-9]{4}_", names(out), value = TRUE)
  after_candidates <- intersect(c("Shapefile_Name", "Discharge_File_Name", "Stream_Name", "Stream_ID"), names(out))
  if (length(after_candidates)) {
    out <- out %>%
      relocate(
        all_of(c("gee_glc_match_method", "gee_glc_match", gee_value_cols)),
        .after = all_of(tail(after_candidates, 1))
      )
  }

  out
}

# Basin slope fill

# Read one configured slope-fill source and return Stream_ID/value pairs.
read_basin_slope_fill_source <- function(
    rule,
    master_dir,
    stream_key,
    priority) {
  configured_path <- trimws(Sys.getenv(rule$File_Env, unset = ""))
  source_path <- if (nzchar(configured_path)) {
    configured_path
  } else {
    file.path(master_dir, rule$Default_File)
  }
  if (!file.exists(source_path)) {
    stop(
      "Missing basin-slope fill source ",
      rule$Source_ID,
      ": ",
      source_path,
      ". Set ",
      rule$File_Env,
      " to override it.",
      call. = FALSE
    )
  }

  source <- if (rule$Layout == "wide_stream_columns") {
    wide <- read.csv(
      source_path,
      header = FALSE,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    names(wide) <- as.character(unlist(wide[1, ], use.names = FALSE))
    wide <- wide[-1, , drop = FALSE]
    tidyr::pivot_longer(
      wide,
      cols = dplyr::everything(),
      names_to = "Stream_Name",
      values_to = "source_value"
    )
  } else if (rule$Layout == "long") {
    required <- c(rule$Input_Key, rule$Input_Value)
    long <- read.csv(
      source_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    missing <- setdiff(required, names(long))
    if (length(missing)) {
      stop(
        "Basin-slope source ",
        rule$Source_ID,
        " is missing columns: ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    data.frame(
      Stream_Name = long[[rule$Input_Key]],
      source_value = long[[rule$Input_Value]],
      stringsAsFactors = FALSE
    )
  } else {
    stop(
      "Unsupported basin-slope layout for ",
      rule$Source_ID,
      ": ",
      rule$Layout,
      call. = FALSE
    )
  }

  value <- suppressWarnings(as.numeric(source$source_value))
  if (rule$Value_Transform == "percent_gradient_to_degrees") {
    value <- atan(value / 100) * (180 / pi)
  } else if (rule$Value_Transform != "identity") {
    stop(
      "Unsupported basin-slope transform for ",
      rule$Source_ID,
      ": ",
      rule$Value_Transform,
      call. = FALSE
    )
  }

  source %>%
    transmute(
      Stream_Name = as.character(Stream_Name),
      basin_slope_mean_degree = value
    ) %>%
    left_join(stream_key, by = "Stream_Name") %>%
    filter(!is.na(Stream_ID), !is.na(basin_slope_mean_degree)) %>%
    transmute(
      Stream_ID,
      basin_slope_mean_degree,
      .source_priority = priority
    )
}

# Fill missing basin slopes using the ordered, editable source manifest.
gap_fill_basin_slope_values <- function(
    df,
    manifest_path,
    master_dir,
    stream_id_key_path) {
  if (!"basin_slope_mean_degree" %in% names(df)) {
    df$basin_slope_mean_degree <- NA_real_
  }

  manifest <- read.delim(
    manifest_path,
    sep = "\t",
    quote = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  required_manifest_columns <- c(
    "Source_ID", "File_Env", "Default_File", "Layout", "Input_Key",
    "Input_Value", "Value_Transform"
  )
  missing_manifest_columns <- setdiff(
    required_manifest_columns,
    names(manifest)
  )
  if (length(missing_manifest_columns)) {
    stop(
      "Basin-slope manifest is missing columns: ",
      paste(missing_manifest_columns, collapse = ", "),
      call. = FALSE
    )
  }
  if (!nrow(manifest) || anyDuplicated(manifest$Source_ID)) {
    stop(
      "Basin-slope manifest must contain unique source rows.",
      call. = FALSE
    )
  }

  stream_key <- read.csv(
    stream_id_key_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  required_key_columns <- c("Stream_Name", "Stream_ID")
  missing_key_columns <- setdiff(required_key_columns, names(stream_key))
  if (length(missing_key_columns)) {
    stop(
      "Stream ID key is missing columns: ",
      paste(missing_key_columns, collapse = ", "),
      call. = FALSE
    )
  }

  fill_lookup <- dplyr::bind_rows(lapply(seq_len(nrow(manifest)), function(i) {
    read_basin_slope_fill_source(
      manifest[i, , drop = FALSE],
      master_dir = master_dir,
      stream_key = stream_key,
      priority = i
    )
  })) %>%
    arrange(.source_priority) %>%
    distinct(Stream_ID, .keep_all = TRUE) %>%
    select(Stream_ID, basin_slope_mean_degree)

  df %>%
    left_join(fill_lookup, by = "Stream_ID", suffix = c("", "_fill")) %>%
    mutate(
      basin_slope_mean_degree = dplyr::coalesce(
        basin_slope_mean_degree,
        basin_slope_mean_degree_fill
      )
    ) %>%
    select(-basin_slope_mean_degree_fill)
}

# Output writing

# Annual and site-average spatial driver tables

annual_driver_specs <- tibble::tribble(
  ~value_col, ~regex, ~type,
  "temp_degC", "^temp_([0-9]{4})_degC$", "numeric",
  "precip_mm_per_day", "^precip_([0-9]{4})_mm_per_day$", "numeric",
  "evapotrans_kg_m2", "^evapotrans_([0-9]{4})_kg_m2$", "numeric",
  "npp_kgC_m2_year", "^npp_([0-9]{4})_kgC_m2_year$", "numeric",
  "greenup_cycle0_date", "^greenup_cycle0_([0-9]{4})MMDD$", "date",
  "greenup_cycle1_date", "^greenup_cycle1_([0-9]{4})MMDD$", "date",
  "snow_num_days", "^snow_([0-9]{4})_num_days$", "numeric",
  "snow_max_prop_area", "^snow_([0-9]{4})_max_prop_area$", "numeric"
)

annual_driver_columns <- function(df) {
  unique(unlist(lapply(annual_driver_specs$regex, function(rx) grep(rx, names(df), value = TRUE))))
}

extract_one_annual_driver <- function(df, spec) {
  cols <- grep(spec$regex, names(df), value = TRUE)
  if (!length(cols)) {
    return(NULL)
  }

  base_cols <- intersect(
    c("Stream_ID", "LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name", "key"),
    names(df)
  )

  out <- df[, c(base_cols, cols), drop = FALSE] %>%
    pivot_longer(
      cols = all_of(cols),
      names_to = "source_column",
      values_to = spec$value_col
    ) %>%
    mutate(
      Year = as.integer(str_match(source_column, spec$regex)[, 2])
    ) %>%
    select(-source_column)

  if (spec$type == "numeric") {
    out[[spec$value_col]] <- suppressWarnings(as.numeric(out[[spec$value_col]]))
  }

  if (spec$type == "date") {
    date_raw <- trimws(as.character(out[[spec$value_col]]))
    date_raw[!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", date_raw)] <- NA_character_
    date_value <- suppressWarnings(as.Date(date_raw))
    out[[spec$value_col]] <- as.character(date_value)
    doy_col <- sub("_date$", "_doy", spec$value_col)
    out[[doy_col]] <- as.integer(format(date_value, "%j"))
  }

  out
}

build_annual_driver_table <- function(df, annual_discharge = NULL, wrtds_q = NULL) {
  pieces <- lapply(seq_len(nrow(annual_driver_specs)), function(i) {
    extract_one_annual_driver(df, annual_driver_specs[i, ])
  })
  pieces <- pieces[!vapply(pieces, is.null, logical(1))]

  if (!length(pieces)) {
    return(data.frame())
  }

  join_cols <- intersect(
    c("Stream_ID", "LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name", "key", "Year"),
    names(pieces[[1]])
  )

  annual <- Reduce(function(x, y) {
    full_join(x, y, by = intersect(join_cols, names(y)))
  }, pieces) %>%
    arrange(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, Year)

  if (!is.null(wrtds_q) && nrow(wrtds_q)) {
    annual <- annual %>%
      left_join(
        wrtds_q %>% select(Stream_ID, Year, Q),
        by = c("Stream_ID", "Year")
      )
  }

  if (!is.null(annual_discharge) && nrow(annual_discharge)) {
    annual <- annual %>%
      left_join(annual_discharge, by = c("Stream_ID", "Year"))
  }

  annual
}

build_site_average_driver_table <- function(df, annual_table) {
  drop_annual_cols <- annual_driver_columns(df)
  site_base <- df[, setdiff(names(df), drop_annual_cols), drop = FALSE]

  if (!nrow(annual_table)) {
    site_base$n_annual_driver_years <- NA_integer_
    return(site_base)
  }

  mean_cols <- intersect(
    c(
      "temp_degC",
      "precip_mm_per_day",
      "evapotrans_kg_m2",
      "npp_kgC_m2_year",
      "greenup_cycle0_doy",
      "greenup_cycle1_doy",
      "snow_num_days",
      "snow_max_prop_area",
      "Q",
      "RBI",
      "recession_slope",
      "RCS"
    ),
    names(annual_table)
  )

  annual_means <- annual_table %>%
    mutate(.has_annual_driver_value = rowSums(!is.na(pick(all_of(mean_cols)))) > 0) %>%
    group_by(Stream_ID) %>%
    summarise(
      n_annual_driver_years = n_distinct(Year[.has_annual_driver_value]),
      across(
        all_of(mean_cols),
        list(
          mean = ~ if (all(is.na(.))) NA_real_ else mean(., na.rm = TRUE),
          n = ~ sum(!is.na(.))
        ),
        .names = "annual_{.fn}_{.col}"
      ),
      .groups = "drop"
    )

  site_base %>%
    left_join(annual_means, by = "Stream_ID")
}

# Write the harmonized tables and a simple non-missing summary
write_harmonization_outputs <- function(
  df,
  output_dir,
  date_tag,
  annual_table = NULL,
  site_average_table = NULL
) {
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

  out <- list(out_file = out_file, summary_file = summary_file)

  if (!is.null(annual_table) && nrow(annual_table)) {
    annual_file <- file.path(
      output_dir,
      harmonization_file_name("harmonized-spatial-drivers-annual", date_tag)
    )
    write.csv(annual_table, annual_file, row.names = FALSE, na = "")
    out$annual_file <- annual_file
  }

  if (!is.null(site_average_table) && nrow(site_average_table)) {
    site_average_file <- file.path(
      output_dir,
      harmonization_file_name("harmonized-spatial-drivers-site-averages", date_tag)
    )
    write.csv(site_average_table, site_average_file, row.names = FALSE, na = "")
    out$site_average_file <- site_average_file
  }

  out
}
