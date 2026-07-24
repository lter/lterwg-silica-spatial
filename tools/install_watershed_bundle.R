#!/usr/bin/env Rscript

# Install one validated watershed in the canonical spatial-version library.
#
# This is the general replacement for site-specific "materialize" scripts.
# It accepts any polygon source, standardizes the CRS and file stem, verifies
# the written bundle, and records machine-readable provenance.
#
# Example:
#   Rscript tools/install_watershed_bundle.R \
#     --source-shp path/to/source.shp \
#     --bundle-name example_watershed \
#     --spatial-version 3 \
#     --source-name "Agency or PI dataset" \
#     --source-link "https://example.org/dataset"
#
# Use --dissolve only when the source intentionally contains multiple polygon
# features belonging to one watershed. Existing bundles are protected unless
# --overwrite is explicitly supplied.

suppressPackageStartupMessages({
  library(jsonlite)
  library(sf)
})

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else ""
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()
source(file.path(script_dir, "cli_helpers.R"))
source(file.path(script_dir, "workflow_paths.R"))

args <- commandArgs(trailingOnly = TRUE)
source_path <- cli_value(args, "--source-shp", required = TRUE)
bundle_name <- cli_value(args, "--bundle-name", required = TRUE)
spatial_version <- cli_integer(
  args,
  "--spatial-version",
  required = TRUE,
  minimum = 1L,
  maximum = 3L
)
output_root <- cli_value(args, "--output-root")
source_name <- cli_value(args, "--source-name", required = TRUE)
source_link <- cli_value(args, "--source-link", default = "")
notes <- cli_value(args, "--notes", default = "")
source_epsg <- cli_integer(args, "--source-epsg", minimum = 1L)
target_epsg <- cli_integer(args, "--target-epsg", default = "4326", minimum = 1L)
dissolve <- cli_has_flag(args, "--dissolve")
overwrite <- cli_has_flag(args, "--overwrite")

source_path <- require_input_file(source_path, "source shapefile")
if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", bundle_name)) {
  stop(
    "--bundle-name may contain only letters, numbers, periods, underscores, ",
    "and hyphens.",
    call. = FALSE
  )
}
if (is.null(output_root)) {
  data_root <- resolve_silica_data_root()
  output_root <- silica_shape_library_root(data_root)
}
output_root <- normalizePath(output_root, mustWork = TRUE)
version_dir <- file.path(output_root, paste0("data_release_", spatial_version))
dir.create(version_dir, recursive = TRUE, showWarnings = FALSE)
target_dir <- file.path(version_dir, bundle_name)
if (dir.exists(target_dir) && !overwrite) {
  stop(
    "Bundle already exists: ", target_dir,
    "\nReview it, then rerun with --overwrite if replacement is intentional.",
    call. = FALSE
  )
}

geometry <- suppressWarnings(st_read(source_path, quiet = TRUE))
if (!nrow(geometry)) {
  stop("Source contains no features.", call. = FALSE)
}
if (is.na(st_crs(geometry))) {
  if (is.null(source_epsg)) {
    stop("Source CRS is missing; provide --source-epsg.", call. = FALSE)
  }
  st_crs(geometry) <- source_epsg
}
geometry <- st_make_valid(geometry)
geometry <- geometry[!st_is_empty(geometry), , drop = FALSE]
if (!nrow(geometry)) {
  stop("Source contains no non-empty geometry.", call. = FALSE)
}

polygon_type <- grepl("POLYGON", as.character(st_geometry_type(geometry)))
if (!all(polygon_type)) {
  stop("Every source feature must be a polygon or multipolygon.", call. = FALSE)
}
if (nrow(geometry) > 1L && !dissolve) {
  stop(
    "Source contains ", nrow(geometry),
    " features. Confirm they form one watershed and rerun with --dissolve.",
    call. = FALSE
  )
}
if (dissolve) {
  geometry <- st_sf(
    bundle_name = bundle_name,
    geometry = st_union(st_geometry(geometry)),
    crs = st_crs(geometry)
  )
} else {
  geometry <- geometry[1, , drop = FALSE]
}
geometry <- st_transform(geometry, target_epsg)
geometry <- st_make_valid(geometry)

area_km2 <- sum(as.numeric(st_area(st_transform(geometry, 6933)))) / 1e6
if (!is.finite(area_km2) || area_km2 <= 0) {
  stop("Installed geometry would have an invalid area.", call. = FALSE)
}

temporary_dir <- tempfile(
  pattern = paste0(".", bundle_name, "_"),
  tmpdir = version_dir
)
dir.create(temporary_dir, recursive = TRUE)
on.exit(unlink(temporary_dir, recursive = TRUE, force = TRUE), add = TRUE)

temporary_shp <- file.path(temporary_dir, paste0(bundle_name, ".shp"))
suppressWarnings(st_write(
  geometry,
  temporary_shp,
  driver = "ESRI Shapefile",
  quiet = TRUE,
  delete_layer = TRUE
))
required_extensions <- c("shp", "shx", "dbf", "prj")
required_files <- file.path(
  temporary_dir,
  paste0(bundle_name, ".", required_extensions)
)
if (!all(file.exists(required_files))) {
  stop("Written bundle is missing a required shapefile component.", call. = FALSE)
}

verified <- suppressWarnings(st_read(temporary_shp, quiet = TRUE))
verified_area <- sum(
  as.numeric(st_area(st_transform(st_make_valid(verified), 6933)))
) / 1e6
relative_difference <- abs(verified_area - area_km2) / area_km2
if (!is.finite(relative_difference) || relative_difference > 1e-6) {
  stop("Read-back geometry area differs from the source geometry.", call. = FALSE)
}

manifest <- list(
  bundle_name = bundle_name,
  spatial_data_version = spatial_version,
  installed_crs_epsg = target_epsg,
  area_km2 = unname(verified_area),
  feature_count = nrow(verified),
  source = list(
    name = source_name,
    link = source_link,
    path_at_install = source_path,
    md5 = unname(tools::md5sum(source_path)),
    original_feature_count = nrow(st_read(source_path, quiet = TRUE))
  ),
  transformation = list(
    dissolved = dissolve,
    target_epsg = target_epsg
  ),
  notes = notes
)
write_json(
  manifest,
  file.path(temporary_dir, "provenance.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)

if (
  dir.exists(target_dir) &&
    unlink(target_dir, recursive = TRUE, force = TRUE) != 0L
) {
  stop("Could not replace existing bundle: ", target_dir, call. = FALSE)
}
if (!file.rename(temporary_dir, target_dir)) {
  stop("Could not install bundle: ", target_dir, call. = FALSE)
}

cat("Installed:", normalizePath(target_dir, winslash = "/", mustWork = TRUE), "\n")
cat("Spatial data version:", spatial_version, "\n")
cat("Area (km2):", sprintf("%.4f", verified_area), "\n")
cat("Source:", source_name, "\n")
