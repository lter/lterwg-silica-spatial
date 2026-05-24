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
  paths <- unique(paths[nzchar(paths)])
  paths <- paths[file.exists(paths)]
  if (!length(paths)) NA_character_ else paths[which.max(file.info(paths)$mtime)]
}

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

data_root <- env_or_default(
  "SILICA_QAQC_DATA_ROOT",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions"
)
date_tag <- env_or_default("SILICA_AUDIT_DATE", format(Sys.Date(), "%Y%m%d"))
audit_label <- env_or_default("SILICA_AUDIT_LABEL", "allreruns-esom-final-spatial-data-extractions")
out_dir <- env_or_default(
  "SILICA_GAP_AUDIT_DIR",
  file.path(getwd(), "generated_outputs", "review", "harmonization")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

combined_file <- latest_existing(c(
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
))
esom_file <- env_or_default("SILICA_ESOM_FILE", file.path(data_root, "ESOM_Sites.csv"))
reference_file <- env_or_default(
  "SILICA_REFERENCE_FILE",
  file.path(data_root, "master", "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv")
)
raw_coverage_file <- env_or_default(
  "SILICA_RAW_COVERAGE_FILE",
  latest_existing(c(
    "/private/tmp/seine_elbe_germany_coverage_20260522/dynamic_raw_all_years_by_region_20260522.csv",
    "/private/tmp/seine_elbe_germany_coverage_20260522/dynamic_raw_recent_years_by_region_20260522.csv"
  ))
)
seine_target_coverage_file <- env_or_default(
  "SILICA_SEINE_TARGET_COVERAGE_FILE",
  "/private/tmp/seine_elbe_germany_coverage_20260522/seine_germany_dynamic_all_years_20260522.csv"
)
elbe_target_coverage_file <- env_or_default(
  "SILICA_ELBE_TARGET_COVERAGE_FILE",
  "/private/tmp/seine_elbe_germany_coverage_20260522/germany_elbe_dynamic_all_years_20260522.csv"
)

if (is.na(combined_file) || !file.exists(combined_file)) {
  stop("Could not locate final combined file. Set SILICA_FINAL_COMBINED_FILE.", call. = FALSE)
}
if (!file.exists(esom_file)) {
  stop("Missing ESOM file: ", esom_file, call. = FALSE)
}

complete_record_start_year <- 2002L

expected_columns <- bind_rows(
  tibble(
    family = "evapo",
    year = complete_record_start_year:2025,
    column = sprintf("evapotrans_%s_kg_m2", year),
    required_workflow_window = TRUE
  ),
  tidyr::expand_grid(year = complete_record_start_year:2024, cycle = 0:1) %>%
    transmute(
      family = "greenup",
      year,
      column = sprintf("greenup_cycle%s_%sMMDD", cycle, year),
      required_workflow_window = TRUE
    ),
  tibble(
    family = "npp",
    year = complete_record_start_year:2025,
    column = sprintf("npp_%s_kgC_m2_year", year),
    required_workflow_window = TRUE
  ),
  bind_rows(
    tibble(
      family = "snow",
      year = complete_record_start_year:2025,
      column = sprintf("snow_%s_num_days", year),
      required_workflow_window = TRUE
    ),
    tibble(
      family = "snow",
      year = complete_record_start_year:2025,
      column = sprintf("snow_%s_max_prop_area", year),
      required_workflow_window = TRUE
    )
  ),
  tibble(
    family = "precip",
    year = 1979:2025,
    column = sprintf("precip_%s_mm_per_day", year),
    required_workflow_window = TRUE
  ),
  tibble(
    family = "airtemp",
    year = 1948:2026,
    column = sprintf("temp_%s_degC", year),
    required_workflow_window = TRUE
  ),
  tibble(
    family = "static",
    year = NA_integer_,
    column = c(
      "elevation_median_m",
      "elevation_mean_m",
      "elevation_min_m",
      "elevation_max_m",
      "basin_slope_median_degree",
      "basin_slope_mean_degree",
      "basin_slope_min_degree",
      "basin_slope_max_degree",
      "major_rock",
      "major_soil",
      "major_land"
    ),
    required_workflow_window = TRUE
  )
)

driver_year_specs <- bind_rows(
  tibble(driver = "evapo", year = complete_record_start_year:2025),
  tibble(driver = "greenup", year = complete_record_start_year:2024),
  tibble(driver = "npp", year = complete_record_start_year:2025),
  tibble(driver = "snow", year = complete_record_start_year:2025)
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

region_map <- tibble::tribble(
  ~LTER_KEY, ~region,
  "amazon", "amazon",
  "and", "north-america-usa",
  "arc", "north-america-arctic",
  "australia", "australia",
  "bcczo", "north-america-usa",
  "canada", "canada",
  "coloradoalpine", "north-america-usa",
  "congo-basin", "congo",
  "east riversfa", "north-america-usa",
  "finnish environmental institute", "scandinavia",
  "germany", "germany",
  "gro", "amazon",
  "guadeloupe", "puerto-rico",
  "hbr", "north-america-usa",
  "hybam", "amazon",
  "knz", "north-america-usa",
  "krr", "north-america-usa",
  "lmp", "north-america-usa",
  "luq", "puerto-rico",
  "md", "australia",
  "niva", "scandinavia",
  "nwt", "north-america-usa",
  "pie", "north-america-usa",
  "seine", "germany",
  "sweden", "scandinavia",
  "uk", "united-kingdom",
  "umr", "north-america-usa",
  "usgs", "north-america-usa",
  "walker branch", "north-america-usa",
  "westernaustralia", "australia"
)

stream_region_overrides <- tibble::tribble(
  ~LTER_KEY, ~STREAM_KEY, ~stream_region,
  "lmp", "nor27", "north-america-usa",
  "krr", "s65c", "north-america-usa"
)

final <- read_loose_csv(combined_file)
for (col in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) {
  if (!col %in% names(final)) final[[col]] <- NA_character_
}

column_audit <- expected_columns %>%
  mutate(present = column %in% names(final))

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

ref_coords <- tibble(.site_key = character(), ref_Latitude = numeric(), ref_Longitude = numeric())
if (file.exists(reference_file)) {
  ref_raw <- read_loose_csv(reference_file)
  ref_coords <- ref_raw %>%
    transmute(
      .site_key = site_key(LTER, Stream_Name),
      ref_Latitude = suppressWarnings(as.numeric(Latitude)),
      ref_Longitude = suppressWarnings(as.numeric(Longitude))
    ) %>%
    filter(!is.na(.site_key), .site_key != "NA||NA") %>%
    distinct(.site_key, .keep_all = TRUE)
}

joined <- esom %>%
  left_join(site_values, by = ".site_key") %>%
  left_join(ref_coords, by = ".site_key") %>%
  mutate(
    effective_LTER = coalesce(matched_LTER, ESOM_LTER),
    effective_Stream_Name = coalesce(matched_Stream_Name, ESOM_Stream_Name),
    LTER_KEY = normalize_lter_key(effective_LTER),
    STREAM_KEY = normalize_stream_key(effective_Stream_Name),
    coord_region = infer_dynamic_region_from_coords(ref_Latitude, ref_Longitude)
  ) %>%
  left_join(stream_region_overrides, by = c("LTER_KEY", "STREAM_KEY")) %>%
  left_join(region_map, by = "LTER_KEY") %>%
  mutate(
    dynamic_region = coalesce(stream_region, coord_region, region),
    target_coverage_group = case_when(
      normalize_lter_key(ESOM_LTER) == "seine" ~ "seine_germany",
      normalize_lter_key(ESOM_LTER) == "germany" ~ "germany_elbe",
      TRUE ~ NA_character_
    )
  )

required_dynamic_long <- joined %>%
  select(
    ESOM_LTER,
    ESOM_Stream_Name,
    matched_LTER,
    matched_Stream_Name,
    matched_Discharge_File_Name,
    matched_Shapefile_Name,
    dynamic_region,
    target_coverage_group,
    matches("^(evapo|greenup|npp|snow)_[0-9]{4}$")
  ) %>%
  pivot_longer(
    matches("^(evapo|greenup|npp|snow)_[0-9]{4}$"),
    names_to = "driver_year",
    values_to = "has_value"
  ) %>%
  tidyr::separate(driver_year, into = c("driver", "year"), sep = "_", convert = TRUE) %>%
  mutate(
    has_value = coalesce(has_value, FALSE),
    missing = !has_value,
    final_match_status = case_when(
      is.na(matched_LTER) ~ "no_final_spatial_site_match",
      !nzchar(matched_Shapefile_Name) ~ "matched_without_shapefile_name",
      TRUE ~ "matched_with_shapefile_name"
    )
  )

raw_coverage <- tibble(driver = character(), dynamic_region = character(), year = integer())
if (file.exists(raw_coverage_file)) {
  raw_coverage <- read_loose_csv(raw_coverage_file) %>%
    transmute(
      driver,
      dynamic_region = normalize_region_key(region),
      year = suppressWarnings(as.integer(year)),
      region_file_count = suppressWarnings(as.integer(file_count)),
      region_expected_file_count = suppressWarnings(as.integer(expected_file_count)),
      raw_missing_or_incomplete = as.logical(missing_or_incomplete)
    )
}

read_target_coverage <- function(path, group) {
  if (!file.exists(path)) {
    return(tibble())
  }
  read_loose_csv(path) %>%
    transmute(
      target_coverage_group = group,
      driver,
      year = suppressWarnings(as.integer(year)),
      target_file_count = suppressWarnings(as.integer(file_count)),
      target_files_intersecting_bbox = suppressWarnings(as.integer(files_intersecting_target_bbox)),
      target_coverage_missing = target_file_count == 0 | target_files_intersecting_bbox == 0
    )
}

target_coverage <- bind_rows(
  read_target_coverage(seine_target_coverage_file, "seine_germany"),
  read_target_coverage(elbe_target_coverage_file, "germany_elbe")
)

known_pending <- tibble::tribble(
  ~target_coverage_group, ~driver, ~year, ~pending_status,
  "seine_germany", "evapo", 2023L, "appeears_submitted_pending",
  "germany_elbe", "evapo", 2023L, "appeears_submitted_pending",
  "seine_germany", "greenup", 2024L, "appeears_payload_corrected_needs_submit",
  "germany_elbe", "greenup", 2024L, "appeears_payload_corrected_needs_submit",
  "seine_germany", "npp", 2023L, "appeears_submitted_pending",
  "seine_germany", "npp", 2024L, "appeears_submitted_pending",
  "seine_germany", "npp", 2025L, "appeears_submitted_pending",
  "germany_elbe", "npp", 2023L, "appeears_submitted_pending",
  "germany_elbe", "npp", 2024L, "appeears_submitted_pending",
  "germany_elbe", "npp", 2025L, "appeears_submitted_pending",
  "seine_germany", "snow", 2024L, "appeears_submitted_pending",
  "seine_germany", "snow", 2025L, "appeears_submitted_pending",
  "germany_elbe", "snow", 2024L, "appeears_submitted_pending",
  "germany_elbe", "snow", 2025L, "appeears_submitted_pending"
)

gap_actions <- required_dynamic_long %>%
  filter(missing) %>%
  left_join(raw_coverage, by = c("dynamic_region", "driver", "year")) %>%
  left_join(target_coverage, by = c("target_coverage_group", "driver", "year")) %>%
  left_join(known_pending, by = c("target_coverage_group", "driver", "year")) %>%
  mutate(
    raw_status = case_when(
      final_match_status != "matched_with_shapefile_name" ~ "not_applicable_polygon_or_reference",
      dynamic_region == "puerto-rico" & driver == "snow" ~ "climatological_zero_snow_region",
      is.na(dynamic_region) ~ "unknown_dynamic_region",
      !is.na(raw_missing_or_incomplete) & !raw_missing_or_incomplete ~ "raw_inventory_complete_region",
      !is.na(target_coverage_missing) & target_coverage_missing ~ "raw_missing_for_target_aoi",
      !is.na(target_coverage_missing) & !target_coverage_missing & raw_missing_or_incomplete ~ "region_partial_but_target_coverage_present",
      is.na(raw_missing_or_incomplete) ~ "raw_coverage_not_audited",
      raw_missing_or_incomplete ~ "raw_missing_or_incomplete_region",
      TRUE ~ "raw_inventory_complete_region"
    ),
    blocker_class = case_when(
      final_match_status != "matched_with_shapefile_name" ~ "polygon_or_reference_gap",
      raw_status == "climatological_zero_snow_region" ~ "zero_fill_needed",
      raw_status %in% c("raw_missing_for_target_aoi", "raw_missing_or_incomplete_region", "unknown_dynamic_region", "raw_coverage_not_audited") ~ raw_status,
      TRUE ~ "extract_or_merge_missing"
    ),
    pending_status = coalesce(pending_status, "")
  ) %>%
  arrange(blocker_class, ESOM_LTER, ESOM_Stream_Name, driver, year)

site_actions <- gap_actions %>%
  mutate(gap = paste0(driver, "_", year)) %>%
  group_by(
    ESOM_LTER,
    ESOM_Stream_Name,
    matched_LTER,
    matched_Stream_Name,
    matched_Shapefile_Name,
    dynamic_region,
    final_match_status
  ) %>%
  summarise(
    n_missing_recent_driver_years = n(),
    missing_recent_driver_years = paste(gap, collapse = ";"),
    blocker_classes = paste(sort(unique(blocker_class)), collapse = ";"),
    pending_statuses = paste(sort(unique(pending_status[nzchar(pending_status)])), collapse = ";"),
    .groups = "drop"
  ) %>%
  arrange(desc(n_missing_recent_driver_years), ESOM_LTER, ESOM_Stream_Name)

summary <- gap_actions %>%
  count(blocker_class, raw_status, pending_status, dynamic_region, driver, year, sort = TRUE, name = "n_missing_site_years")

summarise_gap_groups <- function(x) {
  if (!nrow(x)) {
    return(tibble(
      blocker_class = character(),
      raw_status = character(),
      pending_status = character(),
      dynamic_region = character(),
      driver = character(),
      years = character(),
      n_missing_site_years = integer(),
      n_sites = integer(),
      sites = character()
    ))
  }

  x %>%
    mutate(
      site_label = paste(
        coalesce(matched_LTER, ESOM_LTER, ""),
        coalesce(matched_Stream_Name, ESOM_Stream_Name, ""),
        coalesce(matched_Shapefile_Name, ""),
        sep = " / "
      )
    ) %>%
    group_by(blocker_class, raw_status, pending_status, dynamic_region, driver) %>%
    summarise(
      years = paste(sort(unique(year)), collapse = ","),
      n_missing_site_years = n(),
      n_sites = n_distinct(site_label),
      sites = paste(sort(unique(site_label)), collapse = "; "),
      .groups = "drop"
    ) %>%
    arrange(blocker_class, dynamic_region, driver)
}

raw_action_summary <- gap_actions %>%
  filter(blocker_class %in% c("raw_missing_for_target_aoi", "raw_missing_or_incomplete_region")) %>%
  summarise_gap_groups()

unknown_region_summary <- gap_actions %>%
  filter(blocker_class %in% c("unknown_dynamic_region", "raw_coverage_not_audited")) %>%
  summarise_gap_groups()

extract_or_merge_summary <- gap_actions %>%
  filter(blocker_class == "extract_or_merge_missing") %>%
  summarise_gap_groups()

polygon_summary <- gap_actions %>%
  filter(blocker_class == "polygon_or_reference_gap") %>%
  count(blocker_class, final_match_status, ESOM_LTER, sort = TRUE, name = "n_missing_site_years")

readiness_summary <- bind_rows(
  tibble(
    check = "expected_columns_missing",
    n = sum(!column_audit$present),
    status = ifelse(sum(!column_audit$present) == 0, "PASS", "FAIL")
  ),
  tibble(
    check = "esom_required_dynamic_gap_site_count",
    n = n_distinct(site_actions$ESOM_LTER, site_actions$ESOM_Stream_Name),
    status = ifelse(n_distinct(site_actions$ESOM_LTER, site_actions$ESOM_Stream_Name) == 0, "PASS", "FAIL")
  ),
  tibble(
    check = "matched_shapefile_sites_with_required_dynamic_gaps",
    n = n_distinct(
      site_actions$ESOM_LTER[site_actions$final_match_status == "matched_with_shapefile_name"],
      site_actions$ESOM_Stream_Name[site_actions$final_match_status == "matched_with_shapefile_name"]
    ),
    status = ifelse(
      n_distinct(
        site_actions$ESOM_LTER[site_actions$final_match_status == "matched_with_shapefile_name"],
        site_actions$ESOM_Stream_Name[site_actions$final_match_status == "matched_with_shapefile_name"]
      ) == 0,
      "PASS",
      "FAIL"
    )
  ),
  tibble(
    check = "raw_missing_or_pending_required_dynamic_site_years",
    n = sum(gap_actions$blocker_class %in% c(
      "raw_missing_for_target_aoi",
      "raw_missing_or_incomplete_region",
      "unknown_dynamic_region",
      "raw_coverage_not_audited"
    )),
    status = ifelse(n == 0, "PASS", "FAIL")
  ),
  tibble(
    check = "extract_or_merge_missing_required_dynamic_site_years",
    n = sum(gap_actions$blocker_class == "extract_or_merge_missing"),
    status = ifelse(n == 0, "PASS", "FAIL")
  ),
  tibble(
    check = "zero_fill_required_dynamic_site_years",
    n = sum(gap_actions$blocker_class == "zero_fill_needed"),
    status = ifelse(n == 0, "PASS", "FAIL")
  )
)

out_files <- c(
  readiness_summary = file.path(out_dir, paste0(audit_label, "_final_run_readiness_summary_", date_tag, ".csv")),
  expected_columns = file.path(out_dir, paste0(audit_label, "_final_run_expected_column_audit_", date_tag, ".csv")),
  gap_actions = file.path(out_dir, paste0(audit_label, "_final_run_gap_actions_", date_tag, ".csv")),
  site_actions = file.path(out_dir, paste0(audit_label, "_final_run_site_actions_", date_tag, ".csv")),
  blocker_summary = file.path(out_dir, paste0(audit_label, "_final_run_blocker_summary_", date_tag, ".csv")),
  raw_action_summary = file.path(out_dir, paste0(audit_label, "_final_run_raw_appeears_or_download_needed_", date_tag, ".csv")),
  extract_or_merge_summary = file.path(out_dir, paste0(audit_label, "_final_run_extract_or_merge_needed_", date_tag, ".csv")),
  zero_fill_summary = file.path(out_dir, paste0(audit_label, "_final_run_zero_fill_needed_", date_tag, ".csv")),
  polygon_summary = file.path(out_dir, paste0(audit_label, "_final_run_polygon_or_reference_gaps_", date_tag, ".csv"))
)

write.csv(readiness_summary, out_files[["readiness_summary"]], row.names = FALSE, na = "")
write.csv(column_audit, out_files[["expected_columns"]], row.names = FALSE, na = "")
write.csv(gap_actions, out_files[["gap_actions"]], row.names = FALSE, na = "")
write.csv(site_actions, out_files[["site_actions"]], row.names = FALSE, na = "")
write.csv(summary, out_files[["blocker_summary"]], row.names = FALSE, na = "")
write.csv(raw_action_summary, out_files[["raw_action_summary"]], row.names = FALSE, na = "")
write.csv(extract_or_merge_summary, out_files[["extract_or_merge_summary"]], row.names = FALSE, na = "")
write.csv(
  gap_actions %>%
    filter(blocker_class == "zero_fill_needed") %>%
    summarise_gap_groups(),
  out_files[["zero_fill_summary"]],
  row.names = FALSE,
  na = ""
)
write.csv(polygon_summary, out_files[["polygon_summary"]], row.names = FALSE, na = "")

cat("Using final combined file: ", combined_file, "\n", sep = "")
cat("Using ESOM file: ", esom_file, "\n", sep = "")
cat("Using raw coverage file: ", ifelse(file.exists(raw_coverage_file), raw_coverage_file, "NOT_FOUND"), "\n", sep = "")
cat("WROTE:", out_files[["readiness_summary"]], "\n", sep = "")
cat("WROTE:", out_files[["expected_columns"]], "\n", sep = "")
cat("WROTE:", out_files[["gap_actions"]], "\n", sep = "")
cat("WROTE:", out_files[["site_actions"]], "\n", sep = "")
cat("WROTE:", out_files[["blocker_summary"]], "\n", sep = "")
cat("WROTE:", out_files[["raw_action_summary"]], "\n", sep = "")
cat("WROTE:", out_files[["extract_or_merge_summary"]], "\n", sep = "")
cat("WROTE:", out_files[["zero_fill_summary"]], "\n", sep = "")
cat("WROTE:", out_files[["polygon_summary"]], "\n", sep = "")

cat("\n=== Final Run Readiness ===\n")
print(as.data.frame(readiness_summary), row.names = FALSE)

cat("\n=== Raw/AppEEARS Or Download Needed ===\n")
if (nrow(raw_action_summary)) {
  print(
    as.data.frame(raw_action_summary %>%
      select(blocker_class, pending_status, dynamic_region, driver, years, n_missing_site_years, n_sites)),
    row.names = FALSE
  )
} else {
  cat("<none>\n")
}

if (nrow(unknown_region_summary)) {
  cat("\n=== Unknown Or Unaudited Dynamic Region ===\n")
  print(
    as.data.frame(unknown_region_summary %>%
      select(blocker_class, dynamic_region, driver, years, n_missing_site_years, n_sites)),
    row.names = FALSE
  )
}

cat("\n=== Extract Or Merge Needed (Raw Exists) ===\n")
if (nrow(extract_or_merge_summary)) {
  print(
    as.data.frame(extract_or_merge_summary %>%
      select(dynamic_region, driver, years, n_missing_site_years, n_sites) %>%
      arrange(desc(n_missing_site_years)) %>%
      head(40)),
    row.names = FALSE
  )
} else {
  cat("<none>\n")
}

zero_fill_summary <- gap_actions %>%
  filter(blocker_class == "zero_fill_needed") %>%
  summarise_gap_groups()

cat("\n=== Zero Fill Needed (No AppEEARS Pull) ===\n")
if (nrow(zero_fill_summary)) {
  print(
    as.data.frame(zero_fill_summary %>%
      select(dynamic_region, driver, years, n_missing_site_years, n_sites)),
    row.names = FALSE
  )
} else {
  cat("<none>\n")
}

cat("\n=== Polygon Or Reference Gaps ===\n")
if (nrow(polygon_summary)) {
  print(as.data.frame(head(polygon_summary, 40)), row.names = FALSE)
} else {
  cat("<none>\n")
}

if (any(readiness_summary$status == "FAIL")) {
  cat("\nVERDICT: NOT_FINAL_YET\n")
} else {
  cat("\nVERDICT: FINAL_READY\n")
}
