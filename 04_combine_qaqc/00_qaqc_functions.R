# QA/QC helper functions for combining old and new extraction outputs

librarian::shelf(stringr)

# Shared path helpers used across the broader extraction workflow
source(file.path(getwd(), "tools", "workflow_paths.R"))
source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

# Build review output file names that carry mode, label, and date tags
review_file_name <- function(stem, date_tag, ext = "csv") {
  mode_tag <- trimws(Sys.getenv("SILICA_RUN_MODE", unset = "manual"))
  label_tag <- trimws(Sys.getenv("SILICA_RUN_LABEL", unset = ""))

  clean <- function(x) {
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "-", x)
    gsub("(^-+|-+$)", "", x)
  }

  mode_tag <- clean(mode_tag)
  label_tag <- clean(label_tag)

  parts <- c(stem, mode_tag)
  if (nzchar(label_tag)) {
    parts <- c(parts, label_tag)
  }
  parts <- c(parts, date_tag)
  paste0(paste(parts, collapse = "_"), ".", ext)
}

glimpse_checkpoint <- function(df, label, out_dir = NULL, date_tag = format(Sys.Date(), "%Y%m%d")) {
  stamp <- gsub("[^a-z0-9]+", "-", tolower(label))
  stamp <- gsub("(^-+|-+$)", "", stamp)

  header <- sprintf("=== %s ===", label)
  dims <- sprintf("rows=%s cols=%s", nrow(df), ncol(df))
  body <- utils::capture.output(dplyr::glimpse(df, width = 80))
  lines <- c(header, dims, body, "")

  cat(paste(lines, collapse = "\n"))

  if (!is.null(out_dir) && nzchar(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(out_dir, review_file_name(paste0("glimpse-", stamp), date_tag, "txt"))
    tryCatch(
      writeLines(lines, out_file),
      error = function(e) {
        warning("Could not write glimpse checkpoint file: ", out_file, " (", conditionMessage(e), ")", call. = FALSE)
      }
    )
  }

  invisible(df)
}

# Standardize site key fields before any joins or comparisons
norm_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

# Normalize LTER labels that appear under multiple spellings or aliases
norm_lter <- function(x) {
  x <- clean_lter_label(norm_chr(x))
  dplyr::recode(
    x,
    "Swedish Goverment" = "Sweden",
    "Swedish Government" = "Sweden",
    "Cameroon" = "Congo Basin",
    "Cameroon Site" = "Congo Basin",
    "Cameroon Sites" = "Congo Basin",
    "congo-basin" = "Congo Basin",
    "Congo-Basin" = "Congo Basin",
    "Congo" = "Congo Basin",
    .default = x
  )
}

build_key <- function(df) {
  paste(
    tolower(norm_lter(df$LTER)),
    normalize_stream_key(df$Stream_Name),
    tolower(norm_chr(df$Discharge_File_Name)),
    tolower(norm_chr(df$Shapefile_Name)),
    sep = "||"
  )
}

# Build the readable site identifier used in review outputs
build_stream_id <- function(df) {
  paste(
    norm_lter(df$LTER),
    normalize_stream_key(df$Stream_Name),
    sep = "__"
  )
}

# Pull min and max years from annual driver column names
max_year_from_names <- function(nms, rx) {
  y <- as.integer(stringr::str_match(nms, rx)[, 2])
  y <- y[!is.na(y)]
  if (!length(y)) {
    return(NA_integer_)
  }
  max(y)
}

min_year_from_names <- function(nms, rx) {
  y <- as.integer(stringr::str_match(nms, rx)[, 2])
  y <- y[!is.na(y)]
  if (!length(y)) {
    return(NA_integer_)
  }
  min(y)
}

# Count non-missing values across a selected set of columns
count_non_na <- function(df, cols) {
  if (!length(cols)) {
    return(rep(0L, nrow(df)))
  }
  vals <- df[, cols, drop = FALSE]
  rowSums(!is.na(vals) & vals != "")
}

# Parse common yes/no note fields into logical values
parse_yes_no <- function(x, default = TRUE) {
  out <- rep(default, length(x))
  clean <- tolower(trimws(as.character(x)))
  out[clean %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[clean %in% c("false", "f", "0", "no", "n")] <- FALSE
  out
}

# Fill one clean field from several possible alias columns
coalesce_nonempty_chr <- function(df, cols) {
  if (!length(cols)) {
    return(rep(NA_character_, nrow(df)))
  }

  out <- rep(NA_character_, nrow(df))
  for (nm in cols) {
    vals <- trimws(as.character(df[[nm]]))
    vals[vals == ""] <- NA_character_
    fill <- is.na(out) & !is.na(vals)
    out[fill] <- vals[fill]
  }
  out
}

# Standardize key column names in the incoming new table
normalize_harmonization_key_columns <- function(df) {
  alias_patterns <- list(
    LTER = "^LTER$",
    Stream_Name = "^(Stream_Name|Stream_Nm|Stream_Nam|Strem_Nm|Strm_Nm)(\\..*)?$",
    Discharge_File_Name = "^(Discharge_File_Name|Discharge_Site_Name|Dsch_F_N|Dschr_F_N|Dschrg_F_N|Dsc_F_N)(\\..*)?$",
    Shapefile_Name = "^(Shapefile_Name|Shapefile_Nm|Shapfl_Nm|Shpfl_Nm|Shpfl_N)(\\..*)?$"
  )

  for (target in names(alias_patterns)) {
    hits <- grep(alias_patterns[[target]], names(df), value = TRUE)
    if (!length(hits)) {
      df[[target]] <- NA_character_
      next
    }
    df[[target]] <- coalesce_nonempty_chr(df, hits)
  }

  df
}

# Drop stale join artifact columns that should not drive QA or matching
drop_harmonization_join_artifacts <- function(df) {
  artifact_patterns <- c(
    "^geom(\\..*)?$",
    "^Shpfl_N(\\..*)?$",
    "^Shapfl_Nm(\\..*)?$",
    "^Shpfl_Nm(\\..*)?$",
    "^Stream_Nm(\\..*)?$",
    "^Stream_Nam(\\..*)?$",
    "^Strem_Nm(\\..*)?$",
    "^Strm_Nm(\\..*)?$",
    "^Discharge_Site_Name(\\..*)?$",
    "^Dsch_F_N(\\..*)?$",
    "^Dschr_F_N(\\..*)?$",
    "^Dschrg_F_N(\\..*)?$",
    "^Dsc_F_N(\\..*)?$",
    "^hydroshds(\\..*)?$",
    "^hydrosheds(\\..*)?$",
    "^real_are(\\..*)?$"
  )

  drop_cols <- unique(unlist(lapply(
    artifact_patterns,
    function(pat) grep(pat, names(df), value = TRUE)
  )))
  drop_cols <- setdiff(
    drop_cols,
    c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")
  )

  if (!length(drop_cols)) {
    return(df)
  }

  df[, setdiff(names(df), drop_cols), drop = FALSE]
}

# Apply the new-table cleanup steps before comparison to old data
sanitize_new_harmonization_table <- function(df) {
  df <- normalize_harmonization_key_columns(df)
  df <- drop_harmonization_join_artifacts(df)
  df
}

# Normalize key fields and attach both Stream_ID and the stricter merge key
prepare_combined_table <- function(df, sanitize_new = FALSE) {
  if (sanitize_new) {
    df <- sanitize_new_harmonization_table(df)
  }

  for (k in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) {
    if (!k %in% names(df)) {
      df[[k]] <- NA_character_
    }
  }

  df$LTER <- norm_lter(df$LTER)
  df$Stream_Name <- norm_chr(df$Stream_Name)
  df$Discharge_File_Name <- norm_chr(df$Discharge_File_Name)
  df$Shapefile_Name <- norm_chr(df$Shapefile_Name)
  df$Stream_ID <- build_stream_id(df)
  df$key <- build_key(df)
  df %>% dplyr::distinct(key, .keep_all = TRUE)
}

# Read one combined extraction table and standardize it for QA
read_combined_table <- function(path, sanitize_new = FALSE) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  prepare_combined_table(df, sanitize_new = sanitize_new)
}

# List the distinct sites present in a table after standardization
summarize_site_inventory <- function(df) {
  df %>%
    dplyr::distinct(
      Stream_ID,
      LTER,
      Stream_Name,
      Discharge_File_Name,
      Shapefile_Name,
      key
    ) %>%
    dplyr::arrange(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name)
}

# Summarize which driver-year columns exist in a table
summarize_driver_year_extent <- function(df, driver_specs) {
  dplyr::bind_rows(lapply(names(driver_specs), function(driver) {
    rx <- driver_specs[[driver]]
    cols <- grep(rx, names(df), value = TRUE)
    data.frame(
      driver = driver,
      n_columns = length(cols),
      min_year = min_year_from_names(names(df), rx),
      max_year = max_year_from_names(names(df), rx),
      stringsAsFactors = FALSE
    )
  }))
}

# Compare the year coverage in old and new tables by driver
summarize_overlap_coverage <- function(old, new, driver_specs) {
  dplyr::bind_rows(lapply(names(driver_specs), function(driver) {
    rx <- driver_specs[[driver]]
    data.frame(
      driver = driver,
      old_min = min_year_from_names(names(old), rx),
      old_max = max_year_from_names(names(old), rx),
      new_min = min_year_from_names(names(new), rx),
      new_max = max_year_from_names(names(new), rx),
      stringsAsFactors = FALSE
    )
  }))
}

# Count how much old and new data exist for each shared site and driver
summarize_shared_site_counts <- function(old, new, driver_specs) {
  shared_keys <- intersect(old$key, new$key)

  dplyr::bind_rows(lapply(names(driver_specs), function(driver) {
    rx <- driver_specs[[driver]]
    old_cols <- grep(rx, names(old), value = TRUE)
    new_cols <- grep(rx, names(new), value = TRUE)

    old_shared <- old[
      match(shared_keys, old$key),
      c("key", old_cols),
      drop = FALSE
    ]
    new_shared <- new[
      match(shared_keys, new$key),
      c("key", new_cols),
      drop = FALSE
    ]

    data.frame(
      key = shared_keys,
      driver = driver,
      old_non_na = count_non_na(old_shared, old_cols),
      new_non_na = count_non_na(new_shared, new_cols),
      stringsAsFactors = FALSE
    ) %>%
      dplyr::mutate(has_more_new_years = new_non_na > old_non_na)
  }))
}

# Reshape annual driver columns into one site-driver-year row per value
extract_driver_long <- function(df, driver_specs, value_label = "value") {
  out <- list()
  base_cols <- c(
    "key",
    "LTER",
    "Stream_Name",
    "Discharge_File_Name",
    "Shapefile_Name"
  )

  for (i in seq_len(nrow(driver_specs))) {
    cols <- grep(driver_specs$regex[i], names(df), value = TRUE)
    if (!length(cols)) {
      next
    }

    years <- as.integer(stringr::str_match(cols, driver_specs$regex[i])[, 2])
    tmp <- df[, c(base_cols, cols), drop = FALSE]
    for (nm in cols) {
      tmp[[nm]] <- suppressWarnings(as.numeric(tmp[[nm]]))
    }

    metric_names <- paste0("COL__", cols)
    names(tmp)[seq.int(
      from = length(base_cols) + 1,
      length.out = length(cols)
    )] <- metric_names

    tmp <- tmp %>%
      tidyr::pivot_longer(
        starts_with("COL__"),
        names_to = "metric_col",
        values_to = value_label
      ) %>%
      dplyr::mutate(
        driver = driver_specs$driver[i],
        metric_col = sub("^COL__", "", metric_col),
        year = years[match(metric_col, cols)]
      )

    out[[length(out) + 1]] <- tmp
  }

  if (!length(out)) {
    empty <- data.frame(
      key = character(0),
      LTER = character(0),
      Stream_Name = character(0),
      Discharge_File_Name = character(0),
      Shapefile_Name = character(0),
      metric_col = character(0),
      driver = character(0),
      year = integer(0),
      stringsAsFactors = FALSE
    )
    empty[[value_label]] <- numeric(0)
    return(empty)
  }

  dplyr::bind_rows(out)
}
