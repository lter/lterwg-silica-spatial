librarian::shelf(dplyr, sf)

sf::sf_use_s2(FALSE)

source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

env_or_default <- function(env_name, default_value) {
  value <- trimws(Sys.getenv(env_name, unset = ""))
  if (nzchar(value)) value else default_value
}

read_loose_csv <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- make.unique(ifelse(is.na(names(x)) | trimws(names(x)) == "", "blank_col", names(x)))
  x
}

norm_blank <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

data_root <- env_or_default(
  "SILICA_QAQC_DATA_ROOT",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions"
)
date_tag <- env_or_default("SILICA_GEE_GLC_TARGET_DATE", format(Sys.Date(), "%Y%m%d"))
combined_file <- env_or_default(
  "SILICA_FINAL_COMBINED_FILE",
  file.path(
    data_root,
    "si-extracted-data",
    "all_data_extractions",
    "all-data_si-extract_4_20260523_final-extract-merge-v4-mali-dynamic-domain-spatial-data-extractions.csv"
  )
)
gee_lulc_file <- env_or_default(
  "SILICA_GEE_GLC_FILE",
  file.path(
    data_root,
    "spatial_data_harmonization",
    "master_datasets",
    "DSi_LULC_filled_interpolated_Simple.csv"
  )
)
watershed_file <- env_or_default(
  "SILICA_GEE_GLC_WATERSHED_FILE",
  "/private/tmp/final_v4_followup_extract_20260522/site-coordinates/silica-watersheds_20260522_final-v4-followup-extract-20260522.shp"
)
out_dir <- env_or_default(
  "SILICA_GEE_GLC_TARGET_DIR",
  file.path(getwd(), "generated_outputs", "rerun", paste0(date_tag, "_full_missing_gee_glc"))
)

if (!file.exists(combined_file)) stop("Missing combined file: ", combined_file, call. = FALSE)
if (!file.exists(gee_lulc_file)) stop("Missing GEE/GLC file: ", gee_lulc_file, call. = FALSE)
if (!file.exists(watershed_file)) stop("Missing watershed file: ", watershed_file, call. = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

combined <- read_loose_csv(combined_file)
gee <- read_loose_csv(gee_lulc_file)
watersheds <- sf::st_read(watershed_file, quiet = TRUE)

for (col in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) {
  if (!col %in% names(combined)) combined[[col]] <- NA_character_
  combined[[col]] <- norm_blank(combined[[col]])
}

if (!"Stream_Name" %in% names(gee)) {
  stop("GEE/GLC file needs Stream_Name: ", gee_lulc_file, call. = FALSE)
}
if (!all(c("LTER", "shp_nm") %in% names(watersheds))) {
  stop("Watershed file needs LTER and shp_nm columns: ", watershed_file, call. = FALSE)
}

gee_streams <- data.frame(
  gee_stream_raw = as.character(gee$Stream_Name),
  gee_stream_key = normalize_stream_key(gee$Stream_Name),
  stringsAsFactors = FALSE
) %>%
  filter(!is.na(gee_stream_key), nzchar(gee_stream_key)) %>%
  distinct()

duplicate_gee_keys <- gee_streams %>%
  count(gee_stream_key, name = "n_stream_name_variants") %>%
  filter(n_stream_name_variants > 1)
unique_gee_keys <- setdiff(unique(gee_streams$gee_stream_key), duplicate_gee_keys$gee_stream_key)

combined_keyed <- combined %>%
  mutate(
    .row_id = row_number(),
    .lter_key = normalize_lter_key(LTER),
    .stream_key = normalize_stream_key(Stream_Name),
    .shp_key = normalize_site_key(Shapefile_Name),
    has_gee_glc = Stream_Name %in% gee_streams$gee_stream_raw | .stream_key %in% unique_gee_keys
  )

watershed_keyed <- watersheds %>%
  mutate(
    .lter_key = normalize_lter_key(LTER),
    .shp_key = normalize_site_key(shp_nm)
  ) %>%
  select(.lter_key, .shp_key, geometry) %>%
  distinct(.lter_key, .shp_key, .keep_all = TRUE)

target_attrs <- combined_keyed %>%
  filter(!has_gee_glc, !is.na(.shp_key), nzchar(.shp_key)) %>%
  select(
    .row_id,
    LTER,
    Stream_Name,
    Discharge_File_Name,
    Shapefile_Name,
    .lter_key,
    .stream_key,
    .shp_key
  )

target_sf <- target_attrs %>%
  left_join(watershed_keyed, by = c(".lter_key", ".shp_key")) %>%
  filter(!sf::st_is_empty(geometry)) %>%
  sf::st_as_sf(crs = sf::st_crs(watersheds)) %>%
  arrange(LTER, Stream_Name, Shapefile_Name)

target_sf <- sf::st_make_valid(target_sf)
target_sf <- target_sf[!sf::st_is_empty(target_sf), ]

target_csv <- target_sf %>%
  sf::st_drop_geometry() %>%
  transmute(
    LTER,
    Stream_Name,
    Discharge_File_Name,
    Shapefile_Name,
    reason = "active watershed geometry, missing from existing GEE/GLC Stream_Name table"
  )

summary_by_lter <- target_csv %>%
  count(LTER, sort = TRUE, name = "n_sites")

csv_file <- file.path(out_dir, paste0("full_missing_gee_glc_targets_", date_tag, ".csv"))
summary_file <- file.path(out_dir, paste0("full_missing_gee_glc_targets_summary_", date_tag, ".csv"))
geojson_file <- file.path(out_dir, paste0("full_missing_gee_glc_targets_", date_tag, ".geojson"))

write.csv(target_csv, csv_file, row.names = FALSE, na = "")
write.csv(summary_by_lter, summary_file, row.names = FALSE, na = "")
if (file.exists(geojson_file)) unlink(geojson_file)
sf::st_write(target_sf %>% select(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, geometry), geojson_file, quiet = TRUE)

cat("WROTE:", csv_file, "\n", sep = "")
cat("WROTE:", summary_file, "\n", sep = "")
cat("WROTE:", geojson_file, "\n", sep = "")
cat("combined_rows=", nrow(combined), "\n", sep = "")
cat("missing_gee_glc_stream_rows=", sum(!combined_keyed$has_gee_glc), "\n", sep = "")
cat("missing_gee_glc_with_active_geometry=", nrow(target_csv), "\n", sep = "")
cat("duplicate_normalized_gee_stream_keys=", nrow(duplicate_gee_keys), "\n", sep = "")
