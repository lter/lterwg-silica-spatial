#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
})

source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

env_or_default <- function(env_name, default_value) {
  value <- trimws(Sys.getenv(env_name, unset = ""))
  if (nzchar(value)) value else default_value
}

latest_existing <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) NA_character_ else paths[which.max(file.info(paths)$mtime)]
}

data_root <- env_or_default(
  "SILICA_QAQC_DATA_ROOT",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions"
)
date_tag <- env_or_default("SILICA_AUDIT_DATE", format(Sys.Date(), "%Y%m%d"))
audit_label <- env_or_default("SILICA_AUDIT_LABEL", "allreruns-esom-final-spatial-data-extractions")

combined_candidates <- c(
  env_or_default("SILICA_FINAL_COMBINED_FILE", ""),
  file.path(
    data_root,
    "si-extracted-data",
    "all_data_extractions",
    paste0("all-data_si-extract_4_", date_tag, "_", audit_label, ".csv")
  ),
  Sys.glob(file.path(
    data_root,
    "si-extracted-data",
    "all_data_extractions",
    "all-data_si-extract_4_*.csv"
  )),
  Sys.glob(file.path(
    getwd(),
    "generated_outputs",
    "final_combine",
    "*",
    "canonical_plus_legacy_*.csv"
  ))
)
combined_file <- latest_existing(combined_candidates[nzchar(combined_candidates)])

esom_file <- env_or_default(
  "SILICA_ESOM_FILE",
  file.path(data_root, "ESOM_Sites.csv")
)
out_dir <- env_or_default(
  "SILICA_GAP_AUDIT_DIR",
  file.path(data_root, "review", "harmonization")
)

if (is.na(combined_file) || !file.exists(combined_file)) {
  stop("Could not locate final combined file. Set SILICA_FINAL_COMBINED_FILE.", call. = FALSE)
}
if (!file.exists(esom_file)) {
  stop("Missing ESOM file: ", esom_file, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_loose_csv <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- make.unique(ifelse(is.na(names(x)) | trimws(names(x)) == "", "blank_col", names(x)))
  x
}

norm_blank <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "nan")] <- NA_character_
  x
}

has_nonmissing <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) {
    return(rep(FALSE, nrow(df)))
  }
  vals <- lapply(df[, cols, drop = FALSE], function(x) !is.na(norm_blank(x)))
  Reduce(`|`, vals)
}

site_key <- function(lter, stream_name) {
  paste(normalize_lter_key(lter), normalize_stream_key(stream_name), sep = "||")
}

driver_year_specs <- tibble::tribble(
  ~driver, ~year,
  "evapo", 2023L,
  "evapo", 2024L,
  "evapo", 2025L,
  "greenup", 2023L,
  "greenup", 2024L,
  "npp", 2023L,
  "npp", 2024L,
  "npp", 2025L,
  "snow", 2022L,
  "snow", 2023L,
  "snow", 2024L,
  "snow", 2025L
)

driver_year_cols <- function(driver, year) {
  switch(
    driver,
    evapo = sprintf("evapotrans_%s_kg_m2", year),
    greenup = sprintf("greenup_cycle%s_%sMMDD", 0:1, year),
    npp = sprintf("npp_%s_kgC_m2_year", year),
    snow = sprintf("snow_%s_num_days", year),
    character()
  )
}

final <- read_loose_csv(combined_file)
for (col in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) {
  if (!col %in% names(final)) final[[col]] <- NA_character_
}

final$.site_key <- site_key(final$LTER, final$Stream_Name)
final_values <- final %>%
  transmute(
    .site_key,
    matched_LTER = norm_blank(LTER),
    matched_Stream_Name = norm_blank(Stream_Name),
    matched_Discharge_File_Name = norm_blank(Discharge_File_Name),
    matched_Shapefile_Name = norm_blank(Shapefile_Name)
  )

for (i in seq_len(nrow(driver_year_specs))) {
  driver <- driver_year_specs$driver[[i]]
  year <- driver_year_specs$year[[i]]
  final_values[[paste0(driver, "_", year)]] <- has_nonmissing(final, driver_year_cols(driver, year))
}

site_values <- final_values %>%
  group_by(.site_key) %>%
  summarise(
    matched_LTER = first(na.omit(matched_LTER)),
    matched_Stream_Name = first(na.omit(matched_Stream_Name)),
    matched_Discharge_File_Name = paste(unique(na.omit(matched_Discharge_File_Name)), collapse = ";"),
    matched_Shapefile_Name = paste(unique(na.omit(matched_Shapefile_Name)), collapse = ";"),
    across(matches("^(evapo|greenup|npp|snow)_[0-9]{4}$"), any),
    .groups = "drop"
  )

esom <- read_loose_csv(esom_file) %>%
  clean_lter_column() %>%
  transmute(
    ESOM_LTER = norm_blank(LTER),
    ESOM_Stream_Name = norm_blank(Stream_Name),
    .site_key = site_key(LTER, Stream_Name)
  ) %>%
  filter(!is.na(.site_key), .site_key != "NA||NA") %>%
  distinct(.site_key, .keep_all = TRUE)

joined <- esom %>%
  left_join(site_values, by = ".site_key")

long <- joined %>%
  select(
    ESOM_LTER,
    ESOM_Stream_Name,
    matched_LTER,
    matched_Stream_Name,
    matched_Discharge_File_Name,
    matched_Shapefile_Name,
    matches("^(evapo|greenup|npp|snow)_[0-9]{4}$")
  ) %>%
  pivot_longer(
    matches("^(evapo|greenup|npp|snow)_[0-9]{4}$"),
    names_to = "driver_year",
    values_to = "has_value"
  ) %>%
  tidyr::separate(driver_year, into = c("driver", "year"), sep = "_", convert = TRUE) %>%
  mutate(
    has_value = dplyr::coalesce(has_value, FALSE),
    missing = !has_value,
    final_match_status = dplyr::case_when(
      is.na(matched_LTER) ~ "no_final_spatial_site_match",
      !nzchar(matched_Shapefile_Name) ~ "matched_without_shapefile_name",
      TRUE ~ "matched_with_shapefile_name"
    )
  )

gap_long <- long %>%
  filter(missing) %>%
  arrange(ESOM_LTER, ESOM_Stream_Name, driver, year)

gap_sites <- gap_long %>%
  mutate(gap = paste0(driver, "_", year)) %>%
  group_by(
    ESOM_LTER,
    ESOM_Stream_Name,
    matched_LTER,
    matched_Stream_Name,
    matched_Discharge_File_Name,
    matched_Shapefile_Name,
    final_match_status
  ) %>%
  summarise(
    n_missing_recent_driver_years = n(),
    missing_recent_driver_years = paste(gap, collapse = ";"),
    .groups = "drop"
  ) %>%
  arrange(desc(n_missing_recent_driver_years), ESOM_LTER, ESOM_Stream_Name)

summary_by_lter_driver <- gap_long %>%
  count(ESOM_LTER, driver, year, final_match_status, sort = TRUE, name = "n_sites")

summary_by_driver_year <- gap_long %>%
  count(driver, year, final_match_status, sort = FALSE, name = "n_sites")

out_files <- c(
  gap_long = file.path(out_dir, paste0(audit_label, "_esom_recent_dynamic_gap_long_", date_tag, ".csv")),
  gap_sites = file.path(out_dir, paste0(audit_label, "_esom_recent_dynamic_gap_sites_", date_tag, ".csv")),
  summary_by_lter_driver = file.path(out_dir, paste0(audit_label, "_esom_recent_dynamic_gap_summary_by_lter_driver_", date_tag, ".csv")),
  summary_by_driver_year = file.path(out_dir, paste0(audit_label, "_esom_recent_dynamic_gap_summary_by_driver_year_", date_tag, ".csv"))
)

write.csv(gap_long, out_files[["gap_long"]], row.names = FALSE, na = "")
write.csv(gap_sites, out_files[["gap_sites"]], row.names = FALSE, na = "")
write.csv(summary_by_lter_driver, out_files[["summary_by_lter_driver"]], row.names = FALSE, na = "")
write.csv(summary_by_driver_year, out_files[["summary_by_driver_year"]], row.names = FALSE, na = "")

cat("Using final combined file: ", combined_file, "\n", sep = "")
cat("Using ESOM file: ", esom_file, "\n", sep = "")
cat("WROTE:", out_files[["gap_long"]], "\n", sep = "")
cat("WROTE:", out_files[["gap_sites"]], "\n", sep = "")
cat("WROTE:", out_files[["summary_by_lter_driver"]], "\n", sep = "")
cat("WROTE:", out_files[["summary_by_driver_year"]], "\n", sep = "")

cat("\n=== Missing recent driver-years by driver/year and match status ===\n")
print(as.data.frame(summary_by_driver_year), row.names = FALSE)

cat("\n=== Top missing recent driver-years by LTER/driver/year ===\n")
print(as.data.frame(head(summary_by_lter_driver, 80)), row.names = FALSE)
