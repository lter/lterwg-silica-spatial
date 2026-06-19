audit_date <- Sys.getenv("SILICA_GEE_GLC_AUDIT_DATE", unset = format(Sys.Date(), "%Y%m%d"))

box_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn"
data_root <- file.path(box_root, "spatial-data-extractions")
audit_dir <- file.path(data_root, "final-data", "audit-summaries")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

source_file <- file.path(
  data_root,
  "spatial_data_harmonization",
  "master_datasets",
  "DSi_LULC_filled_interpolated_Simple_20260524_nor27.csv"
)

paths <- c(
  full = file.path(data_root, "final-data", "full-dataset", "final_full_dataset_20260607.csv"),
  esom = file.path(data_root, "final-data", "esom", "ESOM_final_combined_spatial_data_20260607.csv")
)

first_existing_value <- function(df, candidates, rows = seq_len(nrow(df))) {
  found <- intersect(candidates, names(df))
  if (!length(found)) {
    return(rep(NA_character_, length(rows)))
  }
  as.character(df[[found[[1]]]][rows])
}

count_method <- function(x, value) {
  tab <- table(as.character(x), useNA = "no")
  if (value %in% names(tab)) {
    return(unname(tab[[value]]))
  }
  0L
}

is_true <- function(x) {
  x %in% TRUE | x == "TRUE"
}

stats <- list()
missing <- list()
row_sum_flags <- list()

for (dataset in names(paths)) {
  df <- read.csv(paths[[dataset]], stringsAsFactors = FALSE, check.names = FALSE)

  gee_cols <- grep("^gee_glc_[0-9]{4}_", names(df), value = TRUE)
  years <- sort(unique(substr(gee_cols, 9, 12)))
  classes <- sort(unique(sub("^gee_glc_[0-9]{4}_", "", gee_cols)))
  values <- suppressWarnings(as.numeric(unlist(df[gee_cols], use.names = FALSE)))
  values <- values[is.finite(values)]
  matched <- if ("gee_glc_match" %in% names(df)) is_true(df$gee_glc_match) else rep(FALSE, nrow(df))

  stats[[dataset]] <- data.frame(
    dataset = dataset,
    source_file = source_file,
    rows = nrow(df),
    cols = ncol(df),
    gee_value_cols = length(gee_cols),
    years = paste0(min(years), "-", max(years)),
    n_years = length(years),
    n_simple_classes = length(classes),
    gee_matched = sum(matched),
    gee_missing = sum(!matched),
    exact_stream_name = count_method(df$gee_glc_match_method, "exact stream name"),
    normalized_stream_name = count_method(df$gee_glc_match_method, "normalized stream name"),
    no_gee_glc_match = count_method(df$gee_glc_match_method, "no GEE/GLC match"),
    value_min = min(values),
    value_max = max(values),
    row_sum_flag_note = "Rows flagged when matched site-year simple-class sums are outside 0.999-1.001; values are retained from source and not normalized.",
    stringsAsFactors = FALSE
  )

  missing_rows <- which(!matched)
  missing[[dataset]] <- data.frame(
    dataset = dataset,
    LTER = first_existing_value(df, c("ESOM_LTER", "LTER"), missing_rows),
    Stream_Name = first_existing_value(df, c("ESOM_Stream_Name", "Stream_Name"), missing_rows),
    Shapefile_Name = first_existing_value(df, c("Shapefile_Name", "shp_nm"), missing_rows),
    gee_glc_match_method = if ("gee_glc_match_method" %in% names(df)) {
      as.character(df$gee_glc_match_method[missing_rows])
    } else {
      rep(NA_character_, length(missing_rows))
    },
    stringsAsFactors = FALSE
  )

  flagged_years <- list()
  for (year in years) {
    year_cols <- grep(paste0("^gee_glc_", year, "_"), names(df), value = TRUE)
    sums <- rowSums(df[matched, year_cols, drop = FALSE], na.rm = TRUE)
    bad <- which(sums < 0.999 | sums > 1.001)
    if (length(bad)) {
      flagged_years[[year]] <- data.frame(
        row = which(matched)[bad],
        year = as.integer(year),
        row_year_sum = sums[bad],
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(flagged_years)) {
    flagged <- do.call(rbind, flagged_years)
    flagged$dataset <- dataset
    flagged$LTER <- first_existing_value(df, c("ESOM_LTER", "LTER"), flagged$row)
    flagged$Stream_Name <- first_existing_value(df, c("ESOM_Stream_Name", "Stream_Name"), flagged$row)
    flagged$Shapefile_Name <- first_existing_value(df, c("Shapefile_Name", "shp_nm"), flagged$row)

    split_key <- interaction(
      flagged$dataset,
      flagged$LTER,
      flagged$Stream_Name,
      flagged$Shapefile_Name,
      drop = TRUE,
      sep = "\r"
    )
    row_sum_flags[[dataset]] <- do.call(rbind, lapply(split(flagged, split_key), function(x) {
      data.frame(
        dataset = x$dataset[[1]],
        LTER = x$LTER[[1]],
        Stream_Name = x$Stream_Name[[1]],
        Shapefile_Name = x$Shapefile_Name[[1]],
        min_year_sum = min(x$row_year_sum),
        max_year_sum = max(x$row_year_sum),
        n_flagged_years = nrow(x),
        first_year = min(x$year),
        last_year = max(x$year),
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
    LTER = character(),
    Stream_Name = character(),
    Shapefile_Name = character(),
    min_year_sum = numeric(),
    max_year_sum = numeric(),
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
