source(file.path(getwd(), "tools", "workflow_paths.R"))
load_workflow_packages(c("dplyr", "sf", "readxl", "readr"))

options(dplyr.summarise.inform = FALSE)
sf::sf_use_s2(FALSE)

data_root <- resolve_silica_data_root()
site_coord_dir <- silica_site_coordinates_dir(data_root)
review_root <- file.path(data_root, "review", "hydrosheds")
dir.create(review_root, recursive = TRUE, showWarnings = FALSE)

read_site_reference <- function(data_root, site_coord_dir) {
  candidates <- c(
    Sys.getenv("SILICA_SITE_REFERENCE_FILE", unset = ""),
    file.path(data_root, "spatial_data_harmonization", "master_datasets", "Site_Reference_Table - WRTDS_Reference_Table_LTER_V2_20260518.csv"),
    file.path(data_root, "master", "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv"),
    file.path(data_root, "master", "Site_Reference_Table - WRTDS_Reference_Table_LTER_V2.csv"),
    file.path(site_coord_dir, "silica-coords_RAW.xlsx")
  )
  candidates <- unique(candidates[nzchar(candidates)])
  hit <- candidates[file.exists(candidates)][1]
  if (is.na(hit) || !nzchar(hit)) {
    stop("Could not locate a site reference table. Set SILICA_SITE_REFERENCE_FILE.", call. = FALSE)
  }

  ext <- tolower(tools::file_ext(hit))
  if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(hit)
  } else {
    readr::read_csv(hit, show_col_types = FALSE)
  }
}

ref_raw <- read_site_reference(data_root, site_coord_dir)

required_cols <- c("LTER", "Stream_Name", "Latitude", "Longitude")
missing_cols <- setdiff(required_cols, names(ref_raw))
if (length(missing_cols) > 0) {
  stop("Reference table is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

ref <- ref_raw %>%
  dplyr::transmute(
    LTER = as.character(LTER),
    Stream_Name = as.character(Stream_Name),
    Shapefile_Name_ref = if ("Shapefile_Name" %in% names(.)) as.character(Shapefile_Name) else NA_character_,
    Latitude = suppressWarnings(as.numeric(Latitude)),
    Longitude = suppressWarnings(as.numeric(Longitude)),
    .LTER_KEY = normalize_lter_key(LTER),
    .STREAM_KEY = normalize_stream_key(Stream_Name)
  ) %>%
  dplyr::filter(!is.na(.LTER_KEY), !is.na(.STREAM_KEY), !is.na(Latitude), !is.na(Longitude)) %>%
  dplyr::distinct(.LTER_KEY, .STREAM_KEY, .keep_all = TRUE)

hydro_layer_override <- Sys.getenv("SILICA_HYDRO_LAYER", unset = "")
hydro_layer <- if (nzchar(hydro_layer_override)) {
  hydro_layer_override
} else {
  silica_sitecoord_existing_file(data_root, "silica-watersheds_hydrosheds", "shp")
}
combined_layer <- silica_watershed_file(data_root)

read_watershed_layer <- function(path) {
  x <- sf::st_read(path, quiet = TRUE)

  if ("shp_nm" %in% names(x)) {
    x <- dplyr::rename(x, Shapefile_Name = shp_nm)
  } else if (!("Shapefile_Name" %in% names(x))) {
    stop("Watershed layer is missing both shp_nm and Shapefile_Name: ", path, call. = FALSE)
  }

  x
}

try_read_watershed_layer <- function(path) {
  tryCatch(
    read_watershed_layer(path),
    error = function(e) {
      message("Skipping unusable watershed layer: ", path)
      message("  ", conditionMessage(e))
      NULL
    }
  )
}

sheds <- NULL
if (file.exists(hydro_layer)) {
  sheds <- try_read_watershed_layer(hydro_layer)
}
if (is.null(sheds)) {
  sheds <- read_watershed_layer(combined_layer)
}

stream_name_field <- dplyr::case_when(
  "Strm_Nm" %in% names(sheds) ~ "Strm_Nm",
  "Stream_Name" %in% names(sheds) ~ "Stream_Name",
  TRUE ~ NA_character_
)

if (is.na(stream_name_field)) {
  stop("Active silica-watersheds shapefile is missing both Strm_Nm and Stream_Name fields.", call. = FALSE)
}

hydro <- sheds %>%
  dplyr::mutate(
    LTER = as.character(LTER),
    Stream_Name = as.character(.data[[stream_name_field]]),
    .LTER_KEY = normalize_lter_key(LTER),
    .STREAM_KEY = normalize_stream_key(Stream_Name)
  ) %>%
  dplyr::left_join(
    ref %>% dplyr::select(.LTER_KEY, .STREAM_KEY, Latitude, Longitude, Shapefile_Name_ref),
    by = c(".LTER_KEY", ".STREAM_KEY")
  )

if (nrow(hydro) == 0) {
  stop("No HydroSHEDS polygons found in active hydrosheds shapefile.", call. = FALSE)
}

hydro_centroids <- sf::st_centroid(hydro)
site_points <- sf::st_as_sf(hydro, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

metric_crs <- 6933
hydro_metric <- sf::st_transform(hydro, metric_crs)
centroids_metric <- sf::st_transform(hydro_centroids, metric_crs)
site_metric <- sf::st_transform(site_points, metric_crs)

centroid_dist_m <- as.numeric(sf::st_distance(site_metric, centroids_metric, by_element = TRUE))
within_mat <- sf::st_within(site_points, hydro, sparse = FALSE)
point_in_poly <- if (nrow(within_mat) == ncol(within_mat)) {
  diag(within_mat)
} else {
  rep(NA, nrow(hydro))
}
poly_area_km2 <- as.numeric(sf::st_area(hydro_metric)) / 1e6

audit <- hydro %>%
  sf::st_drop_geometry() %>%
  dplyr::transmute(
    LTER,
    Stream_Name,
    Shapefile_Name,
    Shapefile_Name_ref,
    Latitude,
    Longitude,
    centroid_lon = sf::st_coordinates(hydro_centroids)[, 1],
    centroid_lat = sf::st_coordinates(hydro_centroids)[, 2],
    centroid_distance_km = centroid_dist_m / 1000,
    point_in_polygon = point_in_poly,
    polygon_area_km2 = poly_area_km2,
    flag_missing_site_coords = is.na(Latitude) | is.na(Longitude),
    flag_outside_polygon = !point_in_poly,
    flag_centroid_distance_gt_50km = !is.na(centroid_dist_m) & centroid_dist_m > 50000,
    flag_centroid_distance_gt_250km = !is.na(centroid_dist_m) & centroid_dist_m > 250000
  ) %>%
  dplyr::mutate(
    n_flags = rowSums(dplyr::across(dplyr::starts_with("flag_")), na.rm = TRUE),
    likely_problem = dplyr::case_when(
      flag_missing_site_coords ~ "missing_site_coords",
      flag_outside_polygon & flag_centroid_distance_gt_250km ~ "wrong_polygon_assignment",
      flag_outside_polygon ~ "site_not_inside_polygon",
      flag_centroid_distance_gt_250km ~ "centroid_very_far_from_site",
      flag_centroid_distance_gt_50km ~ "centroid_far_from_site",
      TRUE ~ "looks_ok"
    )
  ) %>%
  dplyr::arrange(dplyr::desc(n_flags), dplyr::desc(centroid_distance_km))

flagged <- audit %>%
  dplyr::filter(likely_problem != "looks_ok")

stamp <- format(Sys.Date(), "%Y%m%d")
full_file <- file.path(review_root, paste0("hydrosheds_assignment_audit_", stamp, ".csv"))
flag_file <- file.path(review_root, paste0("hydrosheds_assignment_audit_flagged_", stamp, ".csv"))

readr::write_csv(audit, full_file, na = "")
readr::write_csv(flagged, flag_file, na = "")

cat("WROTE:", full_file, "\n", sep = "")
cat("WROTE:", flag_file, "\n", sep = "")
cat("hydrosheds_rows=", nrow(audit), "\n", sep = "")
cat("flagged_rows=", nrow(flagged), "\n", sep = "")
