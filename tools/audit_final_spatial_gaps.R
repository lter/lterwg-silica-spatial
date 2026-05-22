#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
})

source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

env_or_default <- function(env_name, default_value) {
  value <- trimws(Sys.getenv(env_name, unset = ""))
  if (nzchar(value)) value else default_value
}

data_root <- env_or_default(
  "SILICA_QAQC_DATA_ROOT",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial_data_extractions"
)

date_tag <- env_or_default("SILICA_AUDIT_DATE", format(Sys.Date(), "%Y%m%d"))
audit_label <- env_or_default("SILICA_AUDIT_LABEL", "stablehydro4")
combined_file <- env_or_default(
  "SILICA_FINAL_COMBINED_FILE",
  file.path(
    data_root,
    "si-extracted-data",
    paste0("all-data_si-extract_3_", date_tag, "_stablehydro4-final-spatial-data-extractions.csv")
  )
)

esom_file <- env_or_default(
  "SILICA_ESOM_FILE",
  file.path(data_root, "ESOM_Sites.csv")
)

esom_missing_file <- env_or_default(
  "SILICA_ESOM_MISSING_FILE",
  file.path(data_root, "data_checking", "ESOM_MissingSpatialData.csv")
)

out_dir <- env_or_default(
  "SILICA_GAP_AUDIT_DIR",
  file.path(data_root, "review", "harmonization")
)
watershed_file <- env_or_default(
  "SILICA_WATERSHED_FILE",
  file.path(data_root, "silica-shapefiles", "site-coordinates", "silica-watersheds.shp")
)

if (!file.exists(combined_file)) {
  stop("Missing combined file: ", combined_file, call. = FALSE)
}
if (!file.exists(esom_file)) {
  stop("Missing ESOM file: ", esom_file, call. = FALSE)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

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

site_key <- function(lter, stream_name) {
  paste(normalize_lter_key(lter), normalize_stream_key(stream_name), sep = "||")
}

has_any_values <- function(df, cols) {
  if (!length(cols)) {
    return(rep(FALSE, nrow(df)))
  }
  vals <- lapply(df[, cols, drop = FALSE], function(x) {
    x <- norm_blank(x)
    !is.na(x) & x != ""
  })
  Reduce(`|`, vals)
}

collapse_names <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) "" else paste(x, collapse = ";")
}

read_watershed_shapefile_keys <- function(path) {
  if (!nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  x <- sf::st_read(path, quiet = TRUE)
  key_col <- if ("shp_nm" %in% names(x)) {
    "shp_nm"
  } else if ("Shapefile_Name" %in% names(x)) {
    "Shapefile_Name"
  } else {
    return(NULL)
  }
  unique(normalize_site_key(x[[key_col]]))
}

family_patterns <- c(
  evapo = "^evapotrans_",
  greenup = "^greenup_",
  precip = "^precip_",
  airtemp = "^temp_",
  snow = "^snow_",
  npp = "^npp_",
  elev = "^elevation_|^basin_slope_",
  lith = "^major_rock$|^rocks_",
  permafrost = "^permafrost_",
  soil = "^major_soil$|^soil_"
)
actionable_families <- setdiff(names(family_patterns), "permafrost")

final <- read_loose_csv(combined_file)
for (col in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) {
  if (!col %in% names(final)) final[[col]] <- NA_character_
}

watershed_shp_keys <- read_watershed_shapefile_keys(watershed_file)

final$.site_key <- site_key(final$LTER, final$Stream_Name)
final$.shp_key <- normalize_site_key(final$Shapefile_Name)
final$.has_shapefile_name <- !is.na(norm_blank(final$Shapefile_Name))
final$.shapefile_in_active_inventory <- if (is.null(watershed_shp_keys)) {
  final$.has_shapefile_name
} else {
  final$.has_shapefile_name & final$.shp_key %in% watershed_shp_keys
}

for (family in names(family_patterns)) {
  cols <- grep(family_patterns[[family]], names(final), value = TRUE)
  final[[paste0("has_", family)]] <- has_any_values(final, cols)
}

has_cols <- paste0("has_", names(family_patterns))
actionable_has_cols <- paste0("has_", actionable_families)

final_audit <- final %>%
  rowwise() %>%
  mutate(
    strict_missing_driver_families = collapse_names(names(family_patterns)[!c_across(all_of(has_cols))]),
    actionable_missing_driver_families = collapse_names(actionable_families[!c_across(all_of(actionable_has_cols))]),
    n_strict_missing_driver_families = sum(!c_across(all_of(has_cols))),
    n_actionable_missing_driver_families = sum(!c_across(all_of(actionable_has_cols))),
    any_spatial_driver_data = any(c_across(all_of(has_cols))),
    any_actionable_spatial_driver_data = any(c_across(all_of(actionable_has_cols))),
    gap_type = case_when(
      !.has_shapefile_name ~ "missing_polygon",
      !.shapefile_in_active_inventory & !any_actionable_spatial_driver_data ~ "missing_polygon",
      !any_actionable_spatial_driver_data ~ "no_actionable_spatial_driver_values",
      n_actionable_missing_driver_families > 0 ~ "partial_actionable_driver_gap",
      TRUE ~ "complete_actionable_spatial"
    ),
    polygon_note = case_when(
      !.has_shapefile_name ~ "missing_shapefile_name",
      !.shapefile_in_active_inventory ~ "shapefile_name_not_found_in_active_watershed_inventory",
      TRUE ~ ""
    ),
    permafrost_note = case_when(
      !has_permafrost ~ "permafrost_NA_not_counted_actionable_likely_outside_product_domain",
      TRUE ~ ""
    )
  ) %>%
  ungroup() %>%
  transmute(
    LTER,
    Stream_Name,
    Discharge_File_Name,
    Shapefile_Name,
    gap_type,
    polygon_note,
    n_actionable_missing_driver_families,
    actionable_missing_driver_families,
    n_strict_missing_driver_families,
    strict_missing_driver_families,
    permafrost_note,
    any_spatial_driver_data,
    any_actionable_spatial_driver_data,
    across(all_of(has_cols))
  )

final_summary <- final_audit %>%
  count(gap_type, actionable_missing_driver_families, sort = TRUE, name = "n_sites")

esom <- read_loose_csv(esom_file)
if (!all(c("LTER", "Stream_Name") %in% names(esom))) {
  stop("ESOM file needs LTER and Stream_Name columns: ", esom_file, call. = FALSE)
}

esom <- clean_lter_column(esom) %>%
  transmute(
    ESOM_LTER = norm_blank(clean_lter_label(LTER)),
    ESOM_Stream_Name = norm_blank(Stream_Name),
    .site_key = site_key(LTER, Stream_Name)
  ) %>%
  filter(!is.na(.site_key), .site_key != "NA||NA") %>%
  distinct(.site_key, .keep_all = TRUE)

if (file.exists(esom_missing_file)) {
  esom_missing <- read_loose_csv(esom_missing_file) %>%
    clean_lter_column() %>%
    transmute(.site_key = site_key(LTER, Stream_Name), previously_in_ESOM_MissingSpatialData = TRUE) %>%
    distinct(.site_key, .keep_all = TRUE)
} else {
  esom_missing <- data.frame(.site_key = character(), previously_in_ESOM_MissingSpatialData = logical())
}

final_for_esom <- final_audit %>%
  mutate(.site_key = site_key(LTER, Stream_Name)) %>%
  arrange(.site_key, gap_type != "complete_actionable_spatial", n_actionable_missing_driver_families) %>%
  distinct(.site_key, .keep_all = TRUE)

esom_audit <- esom %>%
  left_join(final_for_esom, by = ".site_key") %>%
  left_join(esom_missing, by = ".site_key") %>%
  mutate(
    previously_in_ESOM_MissingSpatialData = coalesce(previously_in_ESOM_MissingSpatialData, FALSE),
    gap_type = case_when(
      is.na(LTER) ~ "no_final_spatial_site_match",
      TRUE ~ gap_type
    ),
    actionable_missing_driver_families = ifelse(
      is.na(actionable_missing_driver_families),
      "all_spatial_families_no_final_site_match",
      actionable_missing_driver_families
    ),
    n_actionable_missing_driver_families = ifelse(
      is.na(n_actionable_missing_driver_families),
      length(actionable_families),
      n_actionable_missing_driver_families
    )
  ) %>%
  select(
    ESOM_LTER,
    ESOM_Stream_Name,
    matched_LTER = LTER,
    matched_Stream_Name = Stream_Name,
    Discharge_File_Name,
    Shapefile_Name,
    gap_type,
    polygon_note,
    n_actionable_missing_driver_families,
    actionable_missing_driver_families,
    n_strict_missing_driver_families,
    strict_missing_driver_families,
    permafrost_note,
    previously_in_ESOM_MissingSpatialData,
    starts_with("has_")
  )

esom_summary <- esom_audit %>%
  count(gap_type, actionable_missing_driver_families, previously_in_ESOM_MissingSpatialData, sort = TRUE, name = "n_sites")

out_files <- c(
  final_audit = file.path(out_dir, paste0(audit_label, "_final_spatial_gap_audit_", date_tag, ".csv")),
  final_summary = file.path(out_dir, paste0(audit_label, "_final_spatial_gap_summary_", date_tag, ".csv")),
  esom_audit = file.path(out_dir, paste0(audit_label, "_esom_spatial_gap_audit_", date_tag, ".csv")),
  esom_summary = file.path(out_dir, paste0(audit_label, "_esom_spatial_gap_summary_", date_tag, ".csv")),
  esom_open_sites = file.path(out_dir, paste0(audit_label, "_esom_spatial_gap_open_sites_", date_tag, ".csv"))
)

esom_open_sites <- esom_audit %>%
  filter(gap_type != "complete_actionable_spatial") %>%
  select(
    ESOM_LTER,
    ESOM_Stream_Name,
    matched_LTER,
    matched_Stream_Name,
    Discharge_File_Name,
    Shapefile_Name,
    gap_type,
    polygon_note,
    n_actionable_missing_driver_families,
    actionable_missing_driver_families,
    n_strict_missing_driver_families,
    strict_missing_driver_families,
    permafrost_note,
    previously_in_ESOM_MissingSpatialData,
    starts_with("has_")
  )

write.csv(final_audit, out_files[["final_audit"]], row.names = FALSE, na = "")
write.csv(final_summary, out_files[["final_summary"]], row.names = FALSE, na = "")
write.csv(esom_audit, out_files[["esom_audit"]], row.names = FALSE, na = "")
write.csv(esom_summary, out_files[["esom_summary"]], row.names = FALSE, na = "")
write.csv(esom_open_sites, out_files[["esom_open_sites"]], row.names = FALSE, na = "")

cat("WROTE:", out_files[["final_audit"]], "\n", sep = "")
cat("WROTE:", out_files[["final_summary"]], "\n", sep = "")
cat("WROTE:", out_files[["esom_audit"]], "\n", sep = "")
cat("WROTE:", out_files[["esom_summary"]], "\n", sep = "")
cat("WROTE:", out_files[["esom_open_sites"]], "\n", sep = "")

cat("\n=== ESOM spatial gap summary ===\n")
print(as.data.frame(esom_summary))

cat("\n=== Final spatial gap summary ===\n")
print(as.data.frame(final_summary))
