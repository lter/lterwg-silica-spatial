#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

env_or_default <- function(env_name, default_value) {
  value <- trimws(Sys.getenv(env_name, unset = ""))
  if (nzchar(value)) value else default_value
}

norm_blank <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

read_loose_csv <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- make.unique(ifelse(is.na(names(x)) | trimws(names(x)) == "", "blank_col", names(x)))
  x
}

data_root <- env_or_default(
  "SILICA_QAQC_DATA_ROOT",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions"
)
date_tag <- env_or_default("SILICA_ANNUAL_AUDIT_DATE", format(Sys.Date(), "%Y%m%d"))
audit_label <- env_or_default("SILICA_ANNUAL_AUDIT_LABEL", "final-annual-coverage")
combined_file <- env_or_default(
  "SILICA_FINAL_COMBINED_FILE",
  file.path(
    data_root,
    "si-extracted-data",
    "all_data_extractions",
    "all-data_si-extract_4_20260523_final-extract-merge-v4-followup-domain-spatial-data-extractions.csv"
  )
)
out_dir <- env_or_default(
  "SILICA_ANNUAL_AUDIT_DIR",
  file.path(data_root, "review", paste0("final_", date_tag, "_", audit_label))
)

if (!file.exists(combined_file)) {
  stop("Missing combined file: ", combined_file, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

x <- read_loose_csv(combined_file)
for (col in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) {
  if (!col %in% names(x)) x[[col]] <- NA_character_
  x[[col]] <- norm_blank(x[[col]])
}

driver_specs <- bind_rows(
  data.frame(driver = "airtemp", year = 2002:2025, column = paste0("temp_", 2002:2025, "_degC")),
  data.frame(driver = "evapo", year = 2002:2025, column = paste0("evapotrans_", 2002:2025, "_kg_m2")),
  data.frame(driver = "greenup_cycle0", year = 2002:2024, column = paste0("greenup_cycle0_", 2002:2024, "MMDD")),
  data.frame(driver = "greenup_cycle1", year = 2002:2024, column = paste0("greenup_cycle1_", 2002:2024, "MMDD")),
  data.frame(driver = "npp", year = 2002:2025, column = paste0("npp_", 2002:2025, "_kgC_m2_year")),
  data.frame(driver = "precip", year = 2002:2025, column = paste0("precip_", 2002:2025, "_mm_per_day")),
  data.frame(driver = "snow", year = 2002:2025, column = paste0("snow_", 2002:2025, "_num_days"))
)

driver_specs <- driver_specs %>%
  mutate(column_exists = column %in% names(x))

if (any(!driver_specs$column_exists)) {
  warning(
    "Missing expected annual columns: ",
    paste(driver_specs$column[!driver_specs$column_exists], collapse = ", "),
    call. = FALSE
  )
}

site_meta <- x %>%
  mutate(
    row_id = row_number(),
    has_shapefile_name = !is.na(Shapefile_Name),
    site_class = case_when(
      !has_shapefile_name ~ "missing_polygon",
      LTER == "MCM" ~ "known_mcm_spatial_gap",
      LTER == "Mali" & Shapefile_Name == "Niger_Bamako" ~ "pending_mali_appeears",
      TRUE ~ "polygon_site"
    )
  ) %>%
  select(row_id, LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, has_shapefile_name, site_class)

value_cols <- driver_specs$column[driver_specs$column_exists]
long_values <- x %>%
  mutate(row_id = row_number()) %>%
  select(row_id, all_of(value_cols)) %>%
  mutate(across(all_of(value_cols), as.character)) %>%
  pivot_longer(-row_id, names_to = "column", values_to = "value") %>%
  left_join(driver_specs, by = "column") %>%
  left_join(site_meta, by = "row_id") %>%
  mutate(
    value = norm_blank(value),
    missing = is.na(value),
    driver_family = case_when(
      driver %in% c("greenup_cycle0", "greenup_cycle1") ~ "greenup",
      TRUE ~ driver
    ),
    action_class = case_when(
      !missing ~ "complete",
      site_class == "missing_polygon" ~ "missing_polygon",
      site_class == "known_mcm_spatial_gap" ~ "known_mcm_spatial_gap",
      site_class == "pending_mali_appeears" & driver_family %in% c("evapo", "greenup", "npp") ~ "pending_mali_appeears",
      driver_family == "airtemp" ~ "needs_noaa_airtemp_update_or_merge",
      driver_family == "snow" ~ "needs_snow_domain_zero_or_raw_merge",
      TRUE ~ "needs_raw_extract_or_merge"
    )
  ) %>%
  select(
    LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
    site_class, driver, driver_family, year, column, missing, action_class
  )

missing_detail <- long_values %>%
  filter(missing) %>%
  arrange(action_class, driver_family, year, LTER, Stream_Name, Shapefile_Name)

summary_by_action <- missing_detail %>%
  count(action_class, driver_family, year, name = "n_missing_site_years") %>%
  arrange(action_class, driver_family, year)

summary_by_site <- missing_detail %>%
  group_by(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, site_class, action_class) %>%
  summarise(
    missing_driver_years = n(),
    missing_drivers = paste(sort(unique(driver_family)), collapse = ";"),
    missing_year_min = min(year, na.rm = TRUE),
    missing_year_max = max(year, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(action_class, desc(missing_driver_years), LTER, Stream_Name)

ready_summary <- data.frame(
  combined_file = combined_file,
  n_rows = nrow(x),
  n_polygon_rows = sum(site_meta$has_shapefile_name),
  n_missing_polygon_rows = sum(!site_meta$has_shapefile_name),
  n_missing_site_years = nrow(missing_detail),
  n_actionable_missing_site_years = sum(!missing_detail$action_class %in% c(
    "missing_polygon",
    "known_mcm_spatial_gap",
    "pending_mali_appeears"
  )),
  stringsAsFactors = FALSE
)

detail_file <- file.path(out_dir, paste0(audit_label, "_annual_missing_detail_", date_tag, ".csv"))
action_file <- file.path(out_dir, paste0(audit_label, "_annual_missing_summary_by_action_", date_tag, ".csv"))
site_file <- file.path(out_dir, paste0(audit_label, "_annual_missing_summary_by_site_", date_tag, ".csv"))
ready_file <- file.path(out_dir, paste0(audit_label, "_annual_readiness_summary_", date_tag, ".csv"))

write.csv(missing_detail, detail_file, row.names = FALSE, na = "")
write.csv(summary_by_action, action_file, row.names = FALSE, na = "")
write.csv(summary_by_site, site_file, row.names = FALSE, na = "")
write.csv(ready_summary, ready_file, row.names = FALSE, na = "")

cat("WROTE:", detail_file, "\n", sep = "")
cat("WROTE:", action_file, "\n", sep = "")
cat("WROTE:", site_file, "\n", sep = "")
cat("WROTE:", ready_file, "\n", sep = "")

cat("\n=== Annual readiness summary ===\n")
print(ready_summary, row.names = FALSE)
cat("\n=== Action summary ===\n")
print(summary_by_action, row.names = FALSE)
