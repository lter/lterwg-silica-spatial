# Shared helpers for the main workflow, extractors, and watershed scripts.

source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

use_project_rlibs <- function() {
  project_lib <- file.path(getwd(), ".Rlibs")
  if (dir.exists(project_lib)) {
    .libPaths(unique(c(normalizePath(project_lib, mustWork = TRUE), .libPaths())))
  }
}

silica_default_box_data_root <- function() {
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions"
}

load_workflow_packages <- function(packages) {
  use_project_rlibs()
  for (pkg in packages) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE)
    )
  }
}

resolve_silica_data_root <- function() {
  env_root <- Sys.getenv("SILICA_DATA_ROOT", unset = "")
  if (nzchar(env_root) && dir.exists(env_root)) {
    return(normalizePath(env_root, mustWork = TRUE))
  }

  candidates <- c(
    file.path(getwd(), "spatial-data-extractions"),
    file.path(dirname(getwd()), "spatial-data-extractions"),
    file.path(getwd(), "spatial_data_extractions"),
    file.path(dirname(getwd()), "spatial_data_extractions"),
    file.path(getwd(), "si-watershed-extract"),
    file.path(dirname(getwd()), "si-watershed-extract"),
    "/home/shares/lter-si/si-watershed-extract",
    silica_default_box_data_root()
  )

  hit <- candidates[dir.exists(candidates)]
  if (!length(hit)) {
    stop("Could not find the silica data root. Set SILICA_DATA_ROOT.", call. = FALSE)
  }

  normalizePath(hit[[1]], mustWork = TRUE)
}

read_silica_site_reference <- function(site_coord_dir = NULL) {
  env_ref <- Sys.getenv("SILICA_BASE_FILE", unset = "")
  if (!nzchar(env_ref)) {
    env_ref <- Sys.getenv("SILICA_SITE_REF_FILE", unset = "")
  }

  candidates <- c(
    env_ref,
    if (!is.null(site_coord_dir)) file.path(site_coord_dir, "silica-coords_RAW.xlsx") else character()
  )
  candidates <- candidates[nzchar(candidates)]
  ref_path <- candidates[file.exists(candidates)][1]

  if (is.na(ref_path) || !nzchar(ref_path)) {
    stop(
      "Could not locate site reference table. Set SILICA_BASE_FILE or provide ",
      "silica-coords_RAW.xlsx in the site-coordinate directory.",
      call. = FALSE
    )
  }

  ext <- tolower(tools::file_ext(ref_path))
  if (ext == "csv") {
    return(clean_lter_column(utils::read.csv(ref_path, stringsAsFactors = FALSE, check.names = TRUE)))
  }

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package readxl is required to read site reference workbook: ", ref_path, call. = FALSE)
  }
  clean_lter_column(readxl::read_excel(path = ref_path))
}

silica_site_coordinates_dir <- function(root_path) {
  silica_output_dir_from_root(
    root_path = root_path,
    env_var = "SILICA_SITE_COORD_DIR",
    path_candidates = c(
      file.path(root_path, "site-coordinates"),
      file.path(root_path, "silica-shapefiles", "site-coordinates")
    )
  )
}

silica_raw_driver_data_dir <- function(root_path) {
  dir <- Sys.getenv("SILICA_RAW_DRIVER_DIR", unset = file.path(root_path, "raw-driver-data"))
  if (!dir.exists(dir)) {
    stop("Missing raw-driver-data directory: ", dir, call. = FALSE)
  }
  normalizePath(dir, mustWork = TRUE)
}

silica_extracted_data_dir <- function(root_path) {
  silica_output_dir_from_root(
    root_path = root_path,
    env_var = "SILICA_EXTRACTED_DIR",
    path_candidates = c(
      file.path(root_path, "extracted-data"),
      file.path(root_path, "si-extracted-data"),
      file.path(root_path, "silica-shapefiles", "extracted-data"),
      file.path(root_path, "silica-shapefiles", "si-extracted-data")
    )
  )
}

silica_hydrosheds_raw_dir <- function(root_path) {
  dir <- Sys.getenv("SILICA_HYDROSHEDS_RAW_DIR", unset = file.path(root_path, "hydrosheds-raw"))
  if (!dir.exists(dir)) {
    stop("Missing hydrosheds-raw directory: ", dir, call. = FALSE)
  }
  normalizePath(dir, mustWork = TRUE)
}

silica_watershed_file <- function(root_path) {
  silica_sitecoord_existing_file(root_path, "silica-watersheds", "shp")
}
