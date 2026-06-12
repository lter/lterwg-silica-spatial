#!/usr/bin/env Rscript

source("tools/subset_and_output_helpers.R")

arg_or_default <- function(args, i, default) {
  if (length(args) >= i && nzchar(args[[i]])) args[[i]] else default
}

read_csv_clean <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- make.unique(ifelse(is.na(names(x)) | names(x) == "", "blank_col", names(x)))
  x
}

norm_blank <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x %in% c("", "NA", "NaN", "nan", "NULL", "null")] <- NA_character_
  x
}

norm_key_part <- function(x) {
  x <- norm_blank(x)
  x[is.na(x)] <- ""
  tolower(x)
}

site_key <- function(df) {
  paste(normalize_lter_key(df$LTER), normalize_stream_key(df$Stream_Name), sep = "||")
}

row_key <- function(df) {
  paste(
    normalize_lter_key(df$LTER),
    normalize_stream_key(df$Stream_Name),
    norm_key_part(df$Discharge_File_Name),
    norm_key_part(df$Shapefile_Name),
    sep = "||"
  )
}

driver_from_column <- function(col) {
  if (grepl("^evapotrans_[0-9]{4}_kg_m2$", col)) return("evapo")
  if (grepl("^greenup_cycle[0-9]+_[0-9]{4}MMDD$", col)) return("greenup")
  if (grepl("^npp_[0-9]{4}_kgC_m2_year$", col)) return("npp")
  if (grepl("^snow_[0-9]{4}_num_days$", col)) return("snow_num_days")
  if (grepl("^snow_[0-9]{4}_max_prop_area$", col)) return("snow_max_prop_area")
  if (grepl("^temp_[0-9]{4}_degC$", col)) return("airtemp")
  if (grepl("^precip_[0-9]{4}_mm_per_day$", col)) return("precip")
  "other"
}

year_from_column <- function(col) {
  match <- regmatches(col, regexpr("[0-9]{4}", col))
  if (!length(match) || match == "") NA_integer_ else as.integer(match)
}

values_equal <- function(old_value, new_value, tol = 1e-8) {
  old_chr <- norm_blank(old_value)
  new_chr <- norm_blank(new_value)

  if (is.na(old_chr) && is.na(new_chr)) return(TRUE)
  if (is.na(old_chr) || is.na(new_chr)) return(FALSE)

  old_num <- suppressWarnings(as.numeric(old_chr))
  new_num <- suppressWarnings(as.numeric(new_chr))
  if (!is.na(old_num) && !is.na(new_num)) {
    return(abs(old_num - new_num) <= tol * max(1, abs(old_num), abs(new_num)))
  }

  old_chr == new_chr
}

compare_column <- function(old_df, new_df, col) {
  old_values <- norm_blank(old_df[[col]])
  new_values <- norm_blank(new_df[[col]])

  old_missing <- is.na(old_values)
  new_missing <- is.na(new_values)

  equal <- mapply(values_equal, old_values, new_values, USE.NAMES = FALSE)
  status <- rep("both_filled_different", length(old_values))
  status[old_missing & new_missing] <- "both_missing"
  status[old_missing & !new_missing] <- "old_missing_new_filled"
  status[!old_missing & new_missing] <- "old_filled_new_missing"
  status[!old_missing & !new_missing & equal] <- "both_filled_same"

  data.frame(
    row_key = old_df$.row_key,
    LTER = new_df$LTER,
    Stream_Name = new_df$Stream_Name,
    Discharge_File_Name = new_df$Discharge_File_Name,
    Shapefile_Name = new_df$Shapefile_Name,
    column = col,
    driver = driver_from_column(col),
    year = year_from_column(col),
    old_value = old_values,
    new_value = new_values,
    status = status,
    stringsAsFactors = FALSE
  )
}

range_violations_for_column <- function(df, col) {
  driver <- driver_from_column(col)
  if (driver == "other") return(NULL)

  values <- norm_blank(df[[col]])
  present <- !is.na(values)
  if (!any(present)) return(NULL)

  bad <- rep(FALSE, length(values))
  reason <- rep("", length(values))

  if (driver == "greenup") {
    parsed <- suppressWarnings(as.Date(values))
    expected_year <- year_from_column(col)
    bad[present & is.na(parsed)] <- TRUE
    reason[present & is.na(parsed)] <- "invalid_date"
    # Greenup cycle dates can legitimately fall late in the previous calendar
    # year for southern hemisphere and tropical growing-season windows.
    parsed_year <- as.integer(format(parsed, "%Y"))
    wrong_year <- present & !is.na(parsed) & !(parsed_year %in% c(expected_year - 1L, expected_year))
    bad[wrong_year] <- TRUE
    reason[wrong_year] <- "date_year_does_not_match_column"
  } else {
    numeric_values <- suppressWarnings(as.numeric(values))
    nonnumeric <- present & is.na(numeric_values)
    bad[nonnumeric] <- TRUE
    reason[nonnumeric] <- "non_numeric"

    check_range <- function(lo, hi, label) {
      out <- present & !is.na(numeric_values) & (numeric_values < lo | numeric_values > hi)
      bad[out] <<- TRUE
      reason[out] <<- label
    }

    if (driver == "evapo") check_range(0, 5000, "outside_0_5000_kg_m2")
    if (driver == "npp") check_range(0, 5, "outside_0_5_kgC_m2_year")
    if (driver == "snow_num_days") check_range(0, 366, "outside_0_366_days")
    if (driver == "snow_max_prop_area") check_range(0, 1, "outside_0_1_proportion")
    if (driver == "airtemp") check_range(-90, 70, "outside_minus90_70_degC")
    if (driver == "precip") check_range(0, 1000, "outside_0_1000_mm_per_day")
  }

  if (!any(bad)) return(NULL)

  data.frame(
    LTER = df$LTER[bad],
    Stream_Name = df$Stream_Name[bad],
    Discharge_File_Name = df$Discharge_File_Name[bad],
    Shapefile_Name = df$Shapefile_Name[bad],
    column = col,
    driver = driver,
    year = year_from_column(col),
    value = values[bad],
    reason = reason[bad],
    stringsAsFactors = FALSE
  )
}

history_columns_for <- function(col, available_cols) {
  patterns <- c(
    "^evapotrans_[0-9]{4}_kg_m2$",
    "^npp_[0-9]{4}_kgC_m2_year$",
    "^snow_[0-9]{4}_num_days$",
    "^snow_[0-9]{4}_max_prop_area$",
    "^temp_[0-9]{4}_degC$",
    "^precip_[0-9]{4}_mm_per_day$",
    "^greenup_cycle0_[0-9]{4}MMDD$",
    "^greenup_cycle1_[0-9]{4}MMDD$"
  )

  matched_pattern <- patterns[vapply(patterns, function(pattern) grepl(pattern, col), logical(1))]
  if (!length(matched_pattern)) return(character())
  grep(matched_pattern[[1]], available_cols, value = TRUE)
}

history_numeric_values <- function(values, driver) {
  values <- norm_blank(values)
  if (driver == "greenup") {
    parsed <- suppressWarnings(as.Date(values))
    return(as.numeric(format(parsed, "%j")))
  }
  suppressWarnings(as.numeric(values))
}

audit_new_values_against_site_history <- function(candidates, old_df, new_df) {
  if (!nrow(candidates)) {
    return(data.frame())
  }

  pieces <- lapply(seq_len(nrow(candidates)), function(i) {
    candidate <- candidates[i, ]
    old_row_index <- match(candidate$row_key, old_df$.row_key)
    new_row_index <- match(candidate$row_key, new_df$.row_key)
    if (is.na(old_row_index) || is.na(new_row_index)) return(NULL)

    history_cols <- history_columns_for(candidate$column, names(old_df))
    history_cols <- setdiff(history_cols, candidate$column)
    if (!length(history_cols)) return(NULL)

    driver <- driver_from_column(candidate$column)
    history_values <- history_numeric_values(unlist(old_df[old_row_index, history_cols, drop = FALSE]), driver)
    history_values <- history_values[!is.na(history_values)]
    new_value <- history_numeric_values(new_df[[candidate$column]][new_row_index], driver)
    if (is.na(new_value)) return(NULL)

    old_min <- if (length(history_values)) min(history_values) else NA_real_
    old_max <- if (length(history_values)) max(history_values) else NA_real_
    old_median <- if (length(history_values)) median(history_values) else NA_real_
    old_iqr <- if (length(history_values) >= 2) IQR(history_values) else NA_real_
    outside_minmax <- length(history_values) >= 3 && (new_value < old_min || new_value > old_max)
    outside_iqr_fence <- length(history_values) >= 4 &&
      !is.na(old_iqr) &&
      old_iqr > 0 &&
      (new_value < old_median - 4 * old_iqr || new_value > old_median + 4 * old_iqr)

    data.frame(
      row_key = candidate$row_key,
      LTER = new_df$LTER[new_row_index],
      Stream_Name = new_df$Stream_Name[new_row_index],
      Discharge_File_Name = new_df$Discharge_File_Name[new_row_index],
      Shapefile_Name = new_df$Shapefile_Name[new_row_index],
      column = candidate$column,
      driver = driver,
      year = year_from_column(candidate$column),
      value = new_df[[candidate$column]][new_row_index],
      comparable_value = new_value,
      history_n = length(history_values),
      history_min = old_min,
      history_median = old_median,
      history_max = old_max,
      history_iqr = old_iqr,
      outside_march_site_minmax = outside_minmax,
      outside_march_site_iqr_fence = outside_iqr_fence,
      candidate_type = candidate$candidate_type,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, pieces[!vapply(pieces, is.null, logical(1))])
  if (is.null(out)) {
    out <- data.frame()
  }
  out
}

args <- commandArgs(trailingOnly = TRUE)
date_tag <- Sys.getenv("SILICA_AUDIT_DATE", unset = "20260606")
box_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions"
old_file <- arg_or_default(
  args,
  1,
  file.path(box_root, "final-data", "full-dataset", "all-data_si-extract_2_20250325.csv")
)
new_file <- arg_or_default(
  args,
  2,
  file.path(box_root, "final-data", "full-dataset", "all-data_si-extract_4_20260526_final-extract-merge-v4-remaining-dynamic-domain-spatial-data-extractions.csv")
)
out_dir <- arg_or_default(
  args,
  3,
  file.path(getwd(), "generated_outputs", "review", "harmonization", paste0("final-", date_tag))
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

old <- read_csv_clean(old_file)
new <- read_csv_clean(new_file)

old$.site_key <- site_key(old)
new$.site_key <- site_key(new)
old$.row_key <- row_key(old)
new$.row_key <- row_key(new)

old_unique <- old[!duplicated(old$.row_key), ]
new_unique <- new[!duplicated(new$.row_key), ]

matched_keys <- intersect(old_unique$.row_key, new_unique$.row_key)
old_matched <- old_unique[match(matched_keys, old_unique$.row_key), ]
new_matched <- new_unique[match(matched_keys, new_unique$.row_key), ]

metadata_cols <- c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name", ".site_key", ".row_key")
common_cols <- intersect(names(old), names(new))
compare_cols <- setdiff(common_cols, metadata_cols)

comparison <- do.call(rbind, lapply(compare_cols, function(col) {
  compare_column(old_matched, new_matched, col)
}))

column_summary <- do.call(rbind, lapply(split(comparison, comparison$column), function(x) {
  counts <- table(factor(
    x$status,
    levels = c(
      "both_filled_same",
      "both_filled_different",
      "old_missing_new_filled",
      "old_filled_new_missing",
      "both_missing"
    )
  ))
  data.frame(
    column = x$column[1],
    driver = x$driver[1],
    year = x$year[1],
    n_compared_rows = nrow(x),
    both_filled_same = as.integer(counts["both_filled_same"]),
    both_filled_different = as.integer(counts["both_filled_different"]),
    old_missing_new_filled = as.integer(counts["old_missing_new_filled"]),
    old_filled_new_missing = as.integer(counts["old_filled_new_missing"]),
    both_missing = as.integer(counts["both_missing"]),
    stringsAsFactors = FALSE
  )
}))
column_summary <- column_summary[order(column_summary$driver, column_summary$year, column_summary$column), ]

status_counts <- table(factor(
  comparison$status,
  levels = c(
    "both_filled_same",
    "both_filled_different",
    "old_missing_new_filled",
    "old_filled_new_missing",
    "both_missing"
  )
))

row_key_removed <- old_unique[!(old_unique$.row_key %in% new_unique$.row_key), c(
  "LTER",
  "Stream_Name",
  "Discharge_File_Name",
  "Shapefile_Name",
  ".site_key",
  ".row_key"
)]
new_row_keys <- new_unique[!(new_unique$.row_key %in% old_unique$.row_key), c(
  "LTER",
  "Stream_Name",
  "Discharge_File_Name",
  "Shapefile_Name",
  ".site_key",
  ".row_key"
)]

new_columns <- data.frame(
  column = setdiff(names(new), names(old)),
  driver = vapply(setdiff(names(new), names(old)), driver_from_column, character(1)),
  year = vapply(setdiff(names(new), names(old)), year_from_column, integer(1)),
  stringsAsFactors = FALSE
)

range_violations <- do.call(rbind, lapply(setdiff(names(new), metadata_cols), function(col) {
  range_violations_for_column(new, col)
}))
if (is.null(range_violations)) {
  range_violations <- data.frame(
    LTER = character(),
    Stream_Name = character(),
    Discharge_File_Name = character(),
    Shapefile_Name = character(),
    column = character(),
    driver = character(),
    year = integer(),
    value = character(),
    reason = character(),
    stringsAsFactors = FALSE
  )
}

old_missing_new_filled <- comparison[comparison$status == "old_missing_new_filled", ]
new_history_cols <- new_columns$column[new_columns$driver != "other"]
new_column_candidates <- do.call(rbind, lapply(new_history_cols, function(col) {
  values <- norm_blank(new_matched[[col]])
  present <- !is.na(values)
  if (!any(present)) return(NULL)
  data.frame(
    row_key = new_matched$.row_key[present],
    column = col,
    candidate_type = "new_column_not_in_march",
    stringsAsFactors = FALSE
  )
}))
if (is.null(new_column_candidates)) {
  new_column_candidates <- data.frame(row_key = character(), column = character(), candidate_type = character())
}

history_candidates <- rbind(
  data.frame(
    row_key = old_missing_new_filled$row_key,
    column = old_missing_new_filled$column,
    candidate_type = "old_missing_new_filled",
    stringsAsFactors = FALSE
  ),
  new_column_candidates
)
history_audit <- audit_new_values_against_site_history(history_candidates, old_matched, new_matched)
if (!nrow(history_audit)) {
  history_audit <- data.frame(
    row_key = character(),
    LTER = character(),
    Stream_Name = character(),
    Discharge_File_Name = character(),
    Shapefile_Name = character(),
    column = character(),
    driver = character(),
    year = integer(),
    value = character(),
    comparable_value = numeric(),
    history_n = integer(),
    history_min = numeric(),
    history_median = numeric(),
    history_max = numeric(),
    history_iqr = numeric(),
    outside_march_site_minmax = logical(),
    outside_march_site_iqr_fence = logical(),
    candidate_type = character(),
    stringsAsFactors = FALSE
  )
}
history_flags <- history_audit[
  history_audit$history_n >= 3 &
    (history_audit$outside_march_site_minmax | history_audit$outside_march_site_iqr_fence),
]

summary <- data.frame(
  old_file = old_file,
  new_file = new_file,
  old_rows = nrow(old),
  new_rows = nrow(new),
  old_unique_site_keys = length(unique(old$.site_key)),
  new_unique_site_keys = length(unique(new$.site_key)),
  old_site_keys_in_new = sum(unique(old$.site_key) %in% unique(new$.site_key)),
  new_site_keys_not_old = sum(!(unique(new$.site_key) %in% unique(old$.site_key))),
  old_unique_row_keys = nrow(old_unique),
  new_unique_row_keys = nrow(new_unique),
  old_row_keys_in_new = length(matched_keys),
  old_row_keys_not_in_new = nrow(row_key_removed),
  new_row_keys_not_old = nrow(new_row_keys),
  common_data_columns_compared = length(compare_cols),
  new_columns_not_in_march = nrow(new_columns),
  both_filled_same = as.integer(status_counts["both_filled_same"]),
  both_filled_different = as.integer(status_counts["both_filled_different"]),
  old_missing_new_filled = as.integer(status_counts["old_missing_new_filled"]),
  old_filled_new_missing = as.integer(status_counts["old_filled_new_missing"]),
  both_missing = as.integer(status_counts["both_missing"]),
  range_or_date_violations = nrow(range_violations),
  new_values_history_checked = nrow(history_audit),
  new_values_outside_march_site_history = nrow(history_flags),
  stringsAsFactors = FALSE
)

changed_nonmissing <- comparison[comparison$status == "both_filled_different", ]
old_filled_new_missing <- comparison[comparison$status == "old_filled_new_missing", ]

paths <- c(
  summary = file.path(out_dir, paste0("march2025_vs_final20260606_value_audit_summary_", date_tag, ".csv")),
  column_summary = file.path(out_dir, paste0("march2025_vs_final20260606_column_value_audit_", date_tag, ".csv")),
  changed_nonmissing = file.path(out_dir, paste0("march2025_vs_final20260606_changed_nonmissing_values_", date_tag, ".csv")),
  old_missing_new_filled = file.path(out_dir, paste0("march2025_vs_final20260606_old_missing_new_filled_values_", date_tag, ".csv")),
  old_filled_new_missing = file.path(out_dir, paste0("march2025_vs_final20260606_old_filled_new_missing_values_", date_tag, ".csv")),
  range_violations = file.path(out_dir, paste0("march2025_vs_final20260606_range_date_violations_", date_tag, ".csv")),
  row_key_removed = file.path(out_dir, paste0("march2025_vs_final20260606_old_row_keys_not_in_new_", date_tag, ".csv")),
  new_row_keys = file.path(out_dir, paste0("march2025_vs_final20260606_new_row_keys_not_old_", date_tag, ".csv")),
  new_columns = file.path(out_dir, paste0("march2025_vs_final20260606_new_columns_not_in_march_", date_tag, ".csv")),
  history_audit = file.path(out_dir, paste0("march2025_vs_final20260606_new_values_vs_march_site_history_", date_tag, ".csv")),
  history_flags = file.path(out_dir, paste0("march2025_vs_final20260606_new_values_outside_march_site_history_", date_tag, ".csv"))
)

write.csv(summary, paths["summary"], row.names = FALSE, na = "")
write.csv(column_summary, paths["column_summary"], row.names = FALSE, na = "")
write.csv(changed_nonmissing, paths["changed_nonmissing"], row.names = FALSE, na = "")
write.csv(old_missing_new_filled, paths["old_missing_new_filled"], row.names = FALSE, na = "")
write.csv(old_filled_new_missing, paths["old_filled_new_missing"], row.names = FALSE, na = "")
write.csv(range_violations, paths["range_violations"], row.names = FALSE, na = "")
write.csv(row_key_removed, paths["row_key_removed"], row.names = FALSE, na = "")
write.csv(new_row_keys, paths["new_row_keys"], row.names = FALSE, na = "")
write.csv(new_columns, paths["new_columns"], row.names = FALSE, na = "")
write.csv(history_audit, paths["history_audit"], row.names = FALSE, na = "")
write.csv(history_flags, paths["history_flags"], row.names = FALSE, na = "")

cat("WROTE:\n")
cat(paste0(names(paths), "=", paths, collapse = "\n"), "\n")
print(summary, row.names = FALSE)
