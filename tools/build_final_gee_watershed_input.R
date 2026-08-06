# Build the final GEE watershed package from the canonical release library

suppressPackageStartupMessages(library(sf))

source(file.path("tools", "identifier_helpers.R"))
source(file.path("tools", "workflow_paths.R"))

### Arguments

args <- commandArgs(trailingOnly = TRUE)
reference_path <- cli_value(
  args,
  "--reference",
  file.path(
    "generated_outputs", "final-integration", "final-spatial-20260805",
    "site_reference_table_final_candidate_20260805.csv"
  )
)
output_root <- cli_value(
  args,
  "--outdir",
  file.path("generated_outputs", "gee", "final-watersheds-20260805")
)
batch_size <- cli_integer(args, "--batch-size", "50", minimum = 1L)

data_root <- resolve_silica_data_root()
shape_root <- silica_shape_library_root(data_root)
new_sites_root <- resolve_silica_new_sites_root()
new_sites_shape_root <- file.path(new_sites_root, "aurora", "watershed_library")

reference_path <- require_input_file(reference_path, "canonical site reference")
prepare_output_dir(output_root)
output_root <- normalizePath(output_root, mustWork = TRUE)

### Canonical roster

reference <- read.csv(
  reference_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
reference <- reference[
  trimws(reference$Has_Spatial_Data) == "Yes" &
    nzchar(trimws(reference$Shapefile_Name)), ,
  drop = FALSE
]
reference$.shape_key <- normalize_site_key(reference$Shapefile_Name)
reference$.site_key <- paste(
  normalize_lter_key(reference$LTER),
  normalize_stream_key(reference$Stream_Name),
  normalize_site_key(reference$Discharge_File_Name),
  reference$.shape_key,
  sep = "__"
)
reference$canonical_row_id <- sprintf("site_%04d", seq_len(nrow(reference)))

geometry_rows <- reference[!duplicated(reference$.shape_key), , drop = FALSE]
existing_crosswalk_path <- file.path(
  output_root,
  "gee_watershed_site_crosswalk.csv"
)
existing_crosswalk <- if (file.exists(existing_crosswalk_path)) {
  read.csv(
    existing_crosswalk_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
} else {
  data.frame()
}
existing_ids <- if (nrow(existing_crosswalk)) {
  old <- unique(data.frame(
    shape_key = normalize_site_key(existing_crosswalk$Shapefile_Name),
    watershed_id = trimws(existing_crosswalk$watershed_id),
    stringsAsFactors = FALSE
  ))
  setNames(old$watershed_id, old$shape_key)
} else {
  character()
}
geometry_rows$watershed_id <- unname(existing_ids[geometry_rows$.shape_key])
used_numbers <- suppressWarnings(as.integer(sub(
  "^ws_", "", geometry_rows$watershed_id
)))
next_number <- if (any(is.finite(used_numbers))) max(used_numbers, na.rm = TRUE) else 0L
new_id_rows <- which(is.na(geometry_rows$watershed_id) | !nzchar(
  geometry_rows$watershed_id
))
geometry_rows$watershed_id[new_id_rows] <- sprintf(
  "ws_%04d",
  next_number + seq_along(new_id_rows)
)
geometry_rows$run_group <- sprintf(
  "batch_%03d",
  ceiling(seq_len(nrow(geometry_rows)) / batch_size)
)

bundle_path <- function(row) {
  relative <- file.path(
    paste0("data_release_", row$Spatial_Data_Version),
    row$Shapefile_Name,
    paste0(row$Shapefile_Name, ".shp")
  )
  candidates <- file.path(c(shape_root, new_sites_shape_root), relative)
  available <- candidates[file.exists(candidates)]
  if (!length(available)) {
    return(candidates[[1]])
  }
  available[[1]]
}

shape_paths <- vapply(
  seq_len(nrow(geometry_rows)),
  function(index) bundle_path(geometry_rows[index, , drop = FALSE]),
  character(1)
)
missing_shapes <- !file.exists(shape_paths)
if (any(missing_shapes)) {
  stop(
    "Missing ", sum(missing_shapes), " referenced watershed bundles:\n- ",
    paste(shape_paths[missing_shapes], collapse = "\n- ")
  )
}

### Geometry assembly

read_geometry <- function(path, fallback_epsg) {
  data <- st_read(path, quiet = TRUE)
  if (is.na(st_crs(data))) {
    epsg <- suppressWarnings(as.integer(fallback_epsg))
    if (is.na(epsg)) stop("Missing CRS for ", path)
    st_crs(data) <- epsg
  }
  geometry <- st_make_valid(st_geometry(data))
  if (any(st_geometry_type(geometry) == "GEOMETRYCOLLECTION")) {
    geometry <- st_collection_extract(geometry, "POLYGON", warn = FALSE)
  }
  if (!length(geometry)) stop("No polygon geometry in ", path)
  geometry <- st_transform(geometry, 4326)
  geometry <- st_union(geometry)
  st_cast(geometry, "MULTIPOLYGON", warn = FALSE)[[1]]
}

geometry_list <- vector("list", nrow(geometry_rows))
for (index in seq_len(nrow(geometry_rows))) {
  geometry_list[[index]] <- read_geometry(
    shape_paths[[index]],
    geometry_rows$Shapefile_CRS_EPSG[[index]]
  )
  if (index %% 100L == 0L || index == nrow(geometry_rows)) {
    cat("Assembled ", index, "/", nrow(geometry_rows), " watersheds\n", sep = "")
  }
}

geometry_sfc <- st_sfc(geometry_list, crs = 4326)
polygon_area_km2 <- as.numeric(st_area(geometry_sfc)) / 1e6

reference_groups <- split(seq_len(nrow(reference)), reference$.shape_key)
expected_area <- vapply(geometry_rows$.shape_key, function(key) {
  values <- suppressWarnings(as.numeric(reference$drainSqKm[reference_groups[[key]]]))
  values <- values[is.finite(values) & values >= 0]
  if (!length(values)) {
    return(NA_real_)
  }
  stats::median(values)
}, numeric(1))
site_count <- vapply(
  geometry_rows$.shape_key,
  function(key) length(reference_groups[[key]]),
  integer(1)
)
mismatch_documented <- vapply(geometry_rows$.shape_key, function(key) {
  rows <- reference[reference_groups[[key]], , drop = FALSE]
  notes <- paste(rows$Spatial_Notes, rows$CQ_Notes)
  any(!is.na(notes) & nzchar(trimws(notes)))
}, logical(1))
area_percent_difference <- ifelse(
  is.finite(expected_area) & expected_area > 0,
  100 * (polygon_area_km2 - expected_area) / expected_area,
  NA_real_
)
area_qa_status <- ifelse(
  !is.finite(expected_area),
  "no reference area",
  ifelse(
    abs(area_percent_difference) <= 15,
    "within 15 percent",
    ifelse(mismatch_documented, "documented mismatch", "unresolved mismatch")
  )
)

source_file <- vapply(shape_paths, function(path) {
  if (startsWith(path, paste0(shape_root, "/"))) {
    return(sub(paste0(shape_root, "/"), "", path, fixed = TRUE))
  }
  sub(
    paste0(new_sites_shape_root, "/"),
    "${SILICA_NEW_SITES_HANDOFF_ROOT}/aurora/watershed_library/",
    path,
    fixed = TRUE
  )
}, character(1))
source_type <- ifelse(
  startsWith(shape_paths, paste0(new_sites_shape_root, "/")),
  "august_new_site_intake",
  ifelse(
    normalize_site_key(geometry_rows$Shapefile_Name) %in% c(
      "vieuxhabitantsbarthole_rgealti5m",
      "vieuxhabitantssavanne_rgealti5m"
    ),
    "guadeloupe_correction",
    "canonical_release_bundle"
  )
)

gee <- st_sf(
  data.frame(
    watershed_id = geometry_rows$watershed_id,
    run_group = geometry_rows$run_group,
    LTER = geometry_rows$LTER,
    Shapefile_Name = geometry_rows$Shapefile_Name,
    Stream_Name = geometry_rows$Stream_Name,
    Discharge_File_Name = geometry_rows$Discharge_File_Name,
    spatial_release = suppressWarnings(as.integer(
      geometry_rows$Spatial_Data_Version
    )),
    canonical_site_count = site_count,
    expected_area_km2 = expected_area,
    polygon_area_km2 = polygon_area_km2,
    area_percent_difference = area_percent_difference,
    area_qa_status = area_qa_status,
    source_type = source_type,
    source_file = source_file,
    stringsAsFactors = FALSE
  ),
  geom = geometry_sfc
)

if (any(st_is_empty(gee)) || any(!st_is_valid(gee))) {
  stop("Final GEE package contains empty or invalid geometries.")
}
if (anyDuplicated(gee$watershed_id) || anyDuplicated(normalize_site_key(
  gee$Shapefile_Name
))) {
  stop("Final GEE package contains duplicate watershed identifiers.")
}

### Outputs

gpkg_path <- file.path(
  output_root,
  "silica_gee_watersheds_final_20260805.gpkg"
)
st_write(gee, gpkg_path, layer = "watersheds", quiet = TRUE, delete_dsn = TRUE)

shapefile_root <- file.path(output_root, "shapefile-final")
if (dir.exists(shapefile_root)) unlink(shapefile_root, recursive = TRUE)
dir.create(shapefile_root, recursive = TRUE, showWarnings = FALSE)
shapefile_path <- file.path(
  shapefile_root,
  "silica_gee_watersheds_final_20260805.shp"
)
shapefile_data <- gee
names(shapefile_data)[match(
  c(
    "watershed_id", "run_group", "Shapefile_Name", "Stream_Name",
    "Discharge_File_Name", "spatial_release", "canonical_site_count",
    "expected_area_km2", "polygon_area_km2", "area_percent_difference",
    "area_qa_status", "source_type", "source_file"
  ),
  names(shapefile_data)
)] <- c(
  "ws_id", "run_grp", "shp_name", "stream_nm", "q_file", "sp_rel",
  "site_n", "exp_km2", "poly_km2", "area_pdif", "area_qa", "src_type",
  "src_file"
)
st_write(
  shapefile_data,
  shapefile_path,
  quiet = TRUE,
  delete_layer = TRUE
)

zip_path <- file.path(
  output_root,
  "silica_gee_watersheds_final_20260805_shapefile.zip"
)
temporary_zip_path <- paste0(zip_path, ".tmp.zip")
if (file.exists(temporary_zip_path)) unlink(temporary_zip_path)
shapefile_files <- list.files(shapefile_root, full.names = TRUE)
old_directory <- getwd()
zip_status <- tryCatch(
  {
    setwd(shapefile_root)
    utils::zip(
      normalizePath(temporary_zip_path, mustWork = FALSE),
      basename(shapefile_files)
    )
  },
  finally = setwd(old_directory)
)
if (!identical(zip_status, 0L) || !file.exists(temporary_zip_path)) {
  stop("Could not create the final GEE shapefile ZIP.")
}
if (file.exists(zip_path)) unlink(zip_path)
if (!file.rename(temporary_zip_path, zip_path)) {
  stop("Could not install the final GEE shapefile ZIP.")
}
unlink(shapefile_root, recursive = TRUE)

crosswalk <- reference[, c(
  "canonical_row_id", ".site_key", "LTER", "Stream_Name",
  "Discharge_File_Name", "Shapefile_Name", "Spatial_Data_Version"
)]
crosswalk$watershed_id <- geometry_rows$watershed_id[
  match(reference$.shape_key, geometry_rows$.shape_key)
]
write.csv(
  crosswalk,
  file.path(output_root, "gee_watershed_site_crosswalk.csv"),
  row.names = FALSE,
  na = ""
)

area_qa <- st_drop_geometry(gee)[, c(
  "watershed_id", "LTER", "Stream_Name", "Shapefile_Name",
  "spatial_release", "canonical_site_count", "expected_area_km2",
  "polygon_area_km2", "area_percent_difference", "area_qa_status",
  "source_file"
)]

summary <- data.frame(
  check = c(
    "canonical_site_rows", "distinct_watershed_geometries",
    "valid_nonempty_geometries", "shared_watershed_geometries",
    "reference_area_within_15_percent", "reference_area_documented_mismatch",
    "reference_area_unresolved_mismatch", "reference_area_unavailable"
  ),
  value = c(
    nrow(reference), nrow(gee), sum(st_is_valid(gee) & !st_is_empty(gee)),
    sum(gee$canonical_site_count > 1L),
    sum(gee$area_qa_status == "within 15 percent"),
    sum(gee$area_qa_status == "documented mismatch"),
    sum(gee$area_qa_status == "unresolved mismatch"),
    sum(gee$area_qa_status == "no reference area")
  ),
  stringsAsFactors = FALSE
)
write.csv(
  summary,
  file.path(output_root, "gee_input_summary.csv"),
  row.names = FALSE
)
saveRDS(
  list(summary = summary, area_qa = area_qa, crosswalk = crosswalk),
  file.path(output_root, "gee_input_audit.rds")
)
unlink(file.path(output_root, "gee_watershed_area_qa.csv"))

print(summary, row.names = FALSE)
cat("GEE GeoPackage: ", gpkg_path, "\n", sep = "")
cat("GEE shapefile ZIP: ", zip_path, "\n", sep = "")
