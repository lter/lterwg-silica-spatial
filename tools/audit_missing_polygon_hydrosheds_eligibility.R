librarian::shelf(dplyr)

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

site_key <- function(lter, stream_name) {
  paste(normalize_lter_key(lter), normalize_stream_key(stream_name), sep = "||")
}

data_root <- env_or_default(
  "SILICA_QAQC_DATA_ROOT",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions"
)
date_tag <- env_or_default("SILICA_POLYGON_AUDIT_DATE", format(Sys.Date(), "%Y%m%d"))
audit_label <- env_or_default("SILICA_POLYGON_AUDIT_LABEL", "final-missing-polygons")
combined_file <- env_or_default(
  "SILICA_FINAL_COMBINED_FILE",
  file.path(
    data_root,
    "si-extracted-data",
    "all_data_extractions",
    "all-data_si-extract_4_20260523_final-extract-merge-v4-followup-domain-spatial-data-extractions.csv"
  )
)
site_ref_file <- env_or_default(
  "SILICA_SITE_REF_FILE",
  file.path(data_root, "master", "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv")
)
out_dir <- env_or_default(
  "SILICA_POLYGON_AUDIT_DIR",
  file.path(data_root, "review", paste0("final_", date_tag, "_", audit_label))
)
hydrosheds_threshold_km2 <- suppressWarnings(as.numeric(
  env_or_default("SILICA_HYDROSHEDS_AREA_THRESHOLD_KM2", "100")
))

if (!file.exists(combined_file)) {
  stop("Missing combined file: ", combined_file, call. = FALSE)
}
if (!file.exists(site_ref_file)) {
  stop("Missing site reference file: ", site_ref_file, call. = FALSE)
}
if (is.na(hydrosheds_threshold_km2) || hydrosheds_threshold_km2 < 0) {
  stop("SILICA_HYDROSHEDS_AREA_THRESHOLD_KM2 must be a non-negative number.", call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

final <- read_loose_csv(combined_file)
for (col in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) {
  if (!col %in% names(final)) final[[col]] <- NA_character_
  final[[col]] <- norm_blank(final[[col]])
}

ref <- read_loose_csv(site_ref_file) %>%
  clean_lter_column()
for (col in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name", "Latitude", "Longitude", "drainSqKm", "Shapefile_Source")) {
  if (!col %in% names(ref)) ref[[col]] <- NA
}

ref_key <- ref %>%
  transmute(
    ref_key = site_key(LTER, Stream_Name),
    ref_LTER = norm_blank(LTER),
    ref_Stream_Name = norm_blank(Stream_Name),
    ref_Discharge_File_Name = norm_blank(Discharge_File_Name),
    ref_Shapefile_Name = norm_blank(Shapefile_Name),
    ref_Latitude = suppressWarnings(as.numeric(Latitude)),
    ref_Longitude = suppressWarnings(as.numeric(Longitude)),
    ref_drainSqKm = suppressWarnings(as.numeric(drainSqKm)),
    ref_Shapefile_Source = norm_blank(Shapefile_Source)
  ) %>%
  arrange(ref_key, desc(!is.na(ref_drainSqKm)), desc(!is.na(ref_Latitude) & !is.na(ref_Longitude))) %>%
  distinct(ref_key, .keep_all = TRUE)

missing <- final %>%
  mutate(final_key = site_key(LTER, Stream_Name)) %>%
  filter(is.na(Shapefile_Name)) %>%
  left_join(ref_key, by = c("final_key" = "ref_key")) %>%
  mutate(
    has_reference_match = !is.na(ref_LTER),
    has_reference_coordinates = !is.na(ref_Latitude) & !is.na(ref_Longitude),
    has_reference_drainage_area = !is.na(ref_drainSqKm),
    reference_has_shapefile_name = !is.na(ref_Shapefile_Name),
    reference_marks_hydrosheds = grepl("hydroshed", ref_Shapefile_Source, ignore.case = TRUE),
    eligible_for_hydrosheds = has_reference_coordinates &
      has_reference_drainage_area &
      ref_drainSqKm >= hydrosheds_threshold_km2,
    reason_missing = case_when(
      eligible_for_hydrosheds ~ "hydrosheds_candidate_has_coordinates_and_large_drainage_area",
      !has_reference_match ~ "no_matching_site_reference_row",
      !has_reference_coordinates & !has_reference_drainage_area ~ "site_reference_missing_coordinates_and_drainage_area",
      !has_reference_coordinates ~ "site_reference_missing_coordinates",
      !has_reference_drainage_area ~ "site_reference_missing_drainage_area",
      ref_drainSqKm < hydrosheds_threshold_km2 ~ "drainage_area_below_hydrosheds_threshold",
      TRUE ~ "not_hydrosheds_candidate"
    )
  ) %>%
  select(
    LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
    ref_LTER, ref_Stream_Name, ref_Discharge_File_Name, ref_Shapefile_Name,
    ref_Latitude, ref_Longitude, ref_drainSqKm, ref_Shapefile_Source,
    has_reference_match, has_reference_coordinates, has_reference_drainage_area,
    reference_has_shapefile_name, reference_marks_hydrosheds,
    eligible_for_hydrosheds, reason_missing
  ) %>%
  arrange(desc(eligible_for_hydrosheds), desc(ref_drainSqKm), LTER, Stream_Name)

summary <- missing %>%
  count(eligible_for_hydrosheds, reason_missing, sort = TRUE, name = "n_sites")

hydrosheds_subset <- missing %>%
  filter(eligible_for_hydrosheds) %>%
  transmute(
    LTER,
    Stream_Name,
    Discharge_File_Name = coalesce(Discharge_File_Name, ref_Discharge_File_Name),
    Shapefile_Name = ref_Shapefile_Name,
    Region = "",
    Watershed_Source = "hydrosheds",
    Force_HydroSHEDS = "TRUE",
    HydroSHEDS_Rule = "missing_polygon_large_reference_basin",
    drainSqKm = ref_drainSqKm,
    reason_missing
  )

detail_file <- file.path(out_dir, paste0(audit_label, "_hydrosheds_eligibility_detail_", date_tag, ".csv"))
summary_file <- file.path(out_dir, paste0(audit_label, "_hydrosheds_eligibility_summary_", date_tag, ".csv"))
subset_file <- file.path(out_dir, paste0(audit_label, "_hydrosheds_candidate_subset_", date_tag, ".csv"))

write.csv(missing, detail_file, row.names = FALSE, na = "")
write.csv(summary, summary_file, row.names = FALSE, na = "")
write.csv(hydrosheds_subset, subset_file, row.names = FALSE, na = "")

cat("WROTE:", detail_file, "\n", sep = "")
cat("WROTE:", summary_file, "\n", sep = "")
cat("WROTE:", subset_file, "\n", sep = "")

cat("\nhydrosheds_candidate_rows=", nrow(hydrosheds_subset), "\n", sep = "")
