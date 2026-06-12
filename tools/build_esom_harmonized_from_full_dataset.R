#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
})

source("tools/subset_and_output_helpers.R")

arg_or_default <- function(args, i, default) {
  if (length(args) >= i && nzchar(args[[i]])) args[[i]] else default
}

read_csv_clean <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  blank_names <- is.na(names(x)) | names(x) == ""
  names(x)[blank_names] <- paste0("source_col_", seq_len(sum(blank_names)))
  names(x) <- make.unique(names(x))
  x
}

site_key <- function(lter, stream_name) {
  paste(normalize_lter_key(lter), normalize_stream_key(stream_name), sep = "||")
}

box_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn"
data_root <- file.path(box_root, "spatial-data-extractions")
date_tag <- Sys.getenv("SILICA_ESOM_HARMONIZATION_DATE", unset = "20260606")

args <- commandArgs(trailingOnly = TRUE)
esom_sites_file <- arg_or_default(
  args,
  1,
  file.path(box_root, "ESOM", "spatial-data", "ESOM_Sites.csv")
)
full_harmonized_file <- arg_or_default(
  args,
  2,
  file.path(data_root, "review", "harmonization", paste0("harmonized-spatial-drivers_", date_tag, ".csv"))
)
out_dir <- arg_or_default(
  args,
  3,
  file.path(data_root, "final-data", "esom")
)

if (!file.exists(esom_sites_file)) {
  stop("Missing ESOM sites file: ", esom_sites_file, call. = FALSE)
}
if (!file.exists(full_harmonized_file)) {
  stop("Missing full harmonized file: ", full_harmonized_file, call. = FALSE)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

esom_sites <- read_csv_clean(esom_sites_file)
if (!all(c("LTER", "Stream_Name") %in% names(esom_sites))) {
  stop("ESOM sites file must contain LTER and Stream_Name.", call. = FALSE)
}

full <- read_csv_clean(full_harmonized_file)
if (!all(c("LTER", "Stream_Name") %in% names(full))) {
  stop("Full harmonized file must contain LTER and Stream_Name.", call. = FALSE)
}

esom_sites$.site_key <- site_key(esom_sites$LTER, esom_sites$Stream_Name)
full$.site_key <- site_key(full$LTER, full$Stream_Name)

duplicates <- esom_sites %>%
  filter(duplicated(.site_key) | duplicated(.site_key, fromLast = TRUE)) %>%
  arrange(.site_key, LTER, Stream_Name)

esom_unique <- esom_sites %>%
  filter(!is.na(.site_key), .site_key != "||") %>%
  distinct(.site_key, .keep_all = TRUE)

full_unique <- full %>%
  filter(!is.na(.site_key), .site_key != "||") %>%
  distinct(.site_key, .keep_all = TRUE)

joined <- esom_unique %>%
  transmute(
    .site_key,
    ESOM_LTER = LTER,
    ESOM_Stream_Name = Stream_Name
  ) %>%
  left_join(full_unique, by = ".site_key") %>%
  select(-.site_key)

missing <- joined %>%
  filter(is.na(LTER) | is.na(Stream_Name)) %>%
  select(ESOM_LTER, ESOM_Stream_Name)

out_file <- file.path(out_dir, paste0("ESOM_final_combined_spatial_data_", date_tag, ".csv"))
missing_file <- file.path(out_dir, paste0("ESOM_MissingSpatialData_", date_tag, ".csv"))
duplicate_file <- file.path(out_dir, paste0("esom_duplicate_site_keys_removed_by_label_cleaning_", date_tag, ".csv"))
summary_file <- file.path(out_dir, paste0("ESOM_final_combined_spatial_data_summary_", date_tag, ".csv"))

write.csv(joined, out_file, row.names = FALSE, na = "")
write.csv(missing, missing_file, row.names = FALSE, na = "")
write.csv(duplicates, duplicate_file, row.names = FALSE, na = "")

summary <- data.frame(
  esom_site_rows = nrow(esom_sites),
  esom_unique_site_keys = nrow(esom_unique),
  full_harmonized_rows = nrow(full),
  matched_rows = nrow(joined) - nrow(missing),
  missing_rows = nrow(missing),
  duplicate_esom_rows = nrow(duplicates),
  stringsAsFactors = FALSE
)
write.csv(summary, summary_file, row.names = FALSE, na = "")

cat("WROTE:", out_file, "\n", sep = "")
cat("WROTE:", missing_file, "\n", sep = "")
cat("WROTE:", duplicate_file, "\n", sep = "")
cat("WROTE:", summary_file, "\n", sep = "")
print(summary, row.names = FALSE)
