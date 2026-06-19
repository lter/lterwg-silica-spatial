date_tag <- Sys.getenv("SILICA_MONTHLY_REAVERAGE_DATE", unset = "20260608")

box_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn"
data_root <- file.path(box_root, "spatial-data-extractions")

final_file <- file.path(data_root, "final-data", "full-dataset", "final_full_dataset_20260607.csv")
base_file <- file.path(
  data_root,
  "final-data",
  "full-dataset",
  "archive-superseded-20260608",
  "all-data_si-extract_4_20260526_final-extract-merge-v4-remaining-dynamic-domain-spatial-data-extractions.csv"
)
evapo_patch_file <- "/private/tmp/final_consistency_gap_dynamic_20260607/extracted-data/si-extract_evapo_v061_20260607_final-consistency-gap-dynamic-20260607.csv"
snow_patch_file <- "/private/tmp/final_consistency_gap_dynamic_20260607/extracted-data/si-extract_snow_v061_20260607_final-consistency-gap-dynamic-20260607.csv"
audit_dir <- file.path(data_root, "final-data", "audit-summaries")

dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

required_files <- c(final_file, base_file, evapo_patch_file, snow_patch_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) {
  stop("Missing required files:\n- ", paste(missing_files, collapse = "\n- "), call. = FALSE)
}

month_names <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

read_csv <- function(path) {
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

site_key <- function(df) {
  paste(toupper(trimws(as.character(df$LTER))), toupper(trimws(as.character(df$Shapefile_Name))), sep = "||")
}

finite_number <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  is.finite(x)
}

weighted_update <- function(base_value, patch_value, n_old, n_new) {
  base_value <- suppressWarnings(as.numeric(base_value))
  patch_value <- suppressWarnings(as.numeric(patch_value))

  if (is.finite(base_value) && is.finite(patch_value) && n_old > 0 && n_new > 0) {
    return(((base_value * n_old) + (patch_value * n_new)) / (n_old + n_new))
  }
  if (is.finite(patch_value)) {
    return(patch_value)
  }
  if (is.finite(base_value)) {
    return(base_value)
  }
  NA_real_
}

apply_monthly_reaverage <- function(final_df, base_df, patch_df, driver, monthly_cols, annual_cols) {
  patch_df$.site_key <- site_key(patch_df)
  final_df$.site_key <- site_key(final_df)
  base_df$.site_key <- site_key(base_df)

  audit <- list()

  for (patch_row in seq_len(nrow(patch_df))) {
    key <- patch_df$.site_key[[patch_row]]
    final_row <- match(key, final_df$.site_key)
    base_row <- match(key, base_df$.site_key)

    if (is.na(final_row) || is.na(base_row)) {
      next
    }

    n_old <- sum(!is.na(base_df[base_row, annual_cols, drop = TRUE]))
    n_new <- sum(!is.na(patch_df[patch_row, intersect(annual_cols, names(patch_df)), drop = TRUE]))
    if (n_new == 0) {
      n_new <- 1L
    }

    for (col in monthly_cols) {
      if (!all(c(col) %in% names(final_df)) || !col %in% names(base_df) || !col %in% names(patch_df)) {
        next
      }

      current_value <- suppressWarnings(as.numeric(final_df[[col]][[final_row]]))
      base_value <- suppressWarnings(as.numeric(base_df[[col]][[base_row]]))
      patch_value <- suppressWarnings(as.numeric(patch_df[[col]][[patch_row]]))
      corrected_value <- weighted_update(base_value, patch_value, n_old, n_new)

      if (
        (is.na(current_value) && is.na(corrected_value)) ||
          (is.finite(current_value) && is.finite(corrected_value) && abs(current_value - corrected_value) < 1e-12)
      ) {
        next
      }

      final_df[[col]][[final_row]] <- corrected_value
      audit[[length(audit) + 1L]] <- data.frame(
        driver = driver,
        LTER = final_df$LTER[[final_row]],
        Stream_Name = final_df$Stream_Name[[final_row]],
        Shapefile_Name = final_df$Shapefile_Name[[final_row]],
        variable = col,
        n_old_years = n_old,
        n_new_years = n_new,
        old_current_value = current_value,
        base_full_record_value = base_value,
        patch_single_year_value = patch_value,
        corrected_full_record_value = corrected_value,
        stringsAsFactors = FALSE
      )
    }
  }

  final_df$.site_key <- NULL
  base_df$.site_key <- NULL
  patch_df$.site_key <- NULL

  list(
    data = final_df,
    audit = if (length(audit)) do.call(rbind, audit) else data.frame()
  )
}

final_df <- read_csv(final_file)
base_df <- read_csv(base_file)

evapo_patch <- read_csv(evapo_patch_file)
evapo_monthly_cols <- paste0("evapotrans_", month_names, "_kg_m2")
evapo_annual_cols <- grep("^evapotrans_[0-9]{4}_kg_m2$", names(final_df), value = TRUE)
evapo_result <- apply_monthly_reaverage(
  final_df = final_df,
  base_df = base_df,
  patch_df = evapo_patch,
  driver = "evapo",
  monthly_cols = evapo_monthly_cols,
  annual_cols = evapo_annual_cols
)

snow_patch <- read_csv(snow_patch_file)
snow_monthly_cols <- as.vector(rbind(
  paste0("snow_", month_names, "_avg_prop_area"),
  paste0("snow_", month_names, "_num_days")
))
snow_annual_cols <- grep("^snow_[0-9]{4}_num_days$", names(final_df), value = TRUE)
snow_result <- apply_monthly_reaverage(
  final_df = evapo_result$data,
  base_df = base_df,
  patch_df = snow_patch,
  driver = "snow",
  monthly_cols = snow_monthly_cols,
  annual_cols = snow_annual_cols
)

audit <- rbind(evapo_result$audit, snow_result$audit)

write.csv(snow_result$data, final_file, row.names = FALSE, na = "")

audit_file <- file.path(audit_dir, paste0("monthly_full_record_reaverage_", date_tag, ".csv"))
write.csv(audit, audit_file, row.names = FALSE, na = "")

cat("WROTE:", final_file, "\n", sep = "")
cat("WROTE:", audit_file, "\n", sep = "")
cat("corrected_cells=", nrow(audit), "\n", sep = "")
