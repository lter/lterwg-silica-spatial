audit_date <- Sys.getenv("SILICA_GEE_GLC_AUDIT_DATE", unset = format(Sys.Date(), "%Y%m%d"))
date_tag <- Sys.getenv("SILICA_FINAL_HARMONIZED_DATE", unset = "20260629")

box_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn"
data_root <- file.path(box_root, "spatial-data-extractions")
audit_dir <- file.path(data_root, "audit-summaries")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

source_file <- file.path(
  data_root,
  "gee-glc-lulc-outputs",
  "merged-master-checkpoints",
  "DSi_LULC_filled_interpolated_Simple_06252026_V2.csv"
)

paths <- c(
  full = file.path(data_root, paste0("final_annual_dataset_", date_tag, ".csv"))
)

value_or_missing <- function(x) {
  if (length(x) && !is.na(x) && trimws(as.character(x)) != "") {
    return(as.character(x))
  }
  NA_character_
}

stats <- list()
missing <- list()
row_sum_flags <- list()

for (dataset in names(paths)) {
  df <- read.csv(paths[[dataset]], stringsAsFactors = FALSE, check.names = FALSE)
  land_cols <- grep("^land_", names(df), value = TRUE)
  land_values <- as.data.frame(lapply(df[land_cols], function(x) suppressWarnings(as.numeric(x))))

  land_sum <- rowSums(land_values, na.rm = TRUE)
  has_land <- rowSums(!is.na(land_values)) > 0
  values <- suppressWarnings(as.numeric(unlist(land_values, use.names = FALSE)))
  values <- values[is.finite(values)]

  stats[[dataset]] <- data.frame(
    dataset = dataset,
    source_file = source_file,
    checked_file = paths[[dataset]],
    rows = nrow(df),
    cols = ncol(df),
    land_columns = length(land_cols),
    rows_with_land = sum(has_land),
    rows_missing_land = sum(!has_land),
    value_min = if (length(values)) min(values) else NA_real_,
    value_max = if (length(values)) max(values) else NA_real_,
    min_land_sum = if (any(has_land)) min(land_sum[has_land]) else NA_real_,
    max_land_sum = if (any(has_land)) max(land_sum[has_land]) else NA_real_,
    row_sum_flag_note = "Rows flagged when land-class percentages sum outside 99.9-100.1.",
    stringsAsFactors = FALSE
  )

  missing_rows <- which(!has_land)
  missing_detail <- data.frame(
    dataset = dataset,
    Stream_ID = if ("Stream_ID" %in% names(df)) as.character(df$Stream_ID[missing_rows]) else NA_character_,
    Year = if ("Year" %in% names(df)) suppressWarnings(as.integer(df$Year[missing_rows])) else NA_integer_,
    stringsAsFactors = FALSE
  )
  if (nrow(missing_detail)) {
    split_key <- interaction(
      missing_detail$dataset,
      missing_detail$Stream_ID,
      drop = TRUE,
      sep = "\r"
    )
    missing[[dataset]] <- do.call(rbind, lapply(split(missing_detail, split_key), function(x) {
      data.frame(
        dataset = value_or_missing(x$dataset[[1]]),
        Stream_ID = value_or_missing(x$Stream_ID[[1]]),
        missing_years = nrow(x),
        first_year = min(x$Year, na.rm = TRUE),
        last_year = max(x$Year, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    missing[[dataset]] <- data.frame(
      dataset = character(),
      Stream_ID = character(),
      missing_years = integer(),
      first_year = integer(),
      last_year = integer(),
      stringsAsFactors = FALSE
    )
  }

  bad_rows <- which(has_land & (land_sum < 99.9 | land_sum > 100.1))
  if (length(bad_rows)) {
    flagged <- data.frame(
      dataset = dataset,
      row = bad_rows,
      Stream_ID = if ("Stream_ID" %in% names(df)) as.character(df$Stream_ID[bad_rows]) else NA_character_,
      Year = if ("Year" %in% names(df)) suppressWarnings(as.integer(df$Year[bad_rows])) else NA_integer_,
      row_land_sum = land_sum[bad_rows],
      stringsAsFactors = FALSE
    )

    split_key <- interaction(
      flagged$dataset,
      flagged$Stream_ID,
      drop = TRUE,
      sep = "\r"
    )
    row_sum_flags[[dataset]] <- do.call(rbind, lapply(split(flagged, split_key), function(x) {
      data.frame(
        dataset = value_or_missing(x$dataset[[1]]),
        Stream_ID = value_or_missing(x$Stream_ID[[1]]),
        min_land_sum = min(x$row_land_sum),
        max_land_sum = max(x$row_land_sum),
        n_flagged_years = nrow(x),
        first_year = min(x$Year, na.rm = TRUE),
        last_year = max(x$Year, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    }))
  }
}

stats_out <- do.call(rbind, stats)
missing_out <- do.call(rbind, missing)
flags_out <- if (length(row_sum_flags)) {
  do.call(rbind, row_sum_flags)
} else {
  data.frame(
    dataset = character(),
    Stream_ID = character(),
    min_land_sum = numeric(),
    max_land_sum = numeric(),
    n_flagged_years = integer(),
    first_year = integer(),
    last_year = integer(),
    stringsAsFactors = FALSE
  )
}

summary_file <- file.path(audit_dir, paste0("gee_glc_qaqc_", audit_date, ".csv"))
missing_file <- file.path(audit_dir, paste0("gee_glc_missing_sites_", audit_date, ".csv"))
flags_file <- file.path(audit_dir, paste0("gee_glc_rowsum_flags_", audit_date, ".csv"))

write.csv(stats_out, summary_file, row.names = FALSE, na = "")
write.csv(missing_out, missing_file, row.names = FALSE, na = "")
write.csv(flags_out, flags_file, row.names = FALSE, na = "")

cat("WROTE:", summary_file, "\n", sep = "")
cat("WROTE:", missing_file, "\n", sep = "")
cat("WROTE:", flags_file, "\n", sep = "")
