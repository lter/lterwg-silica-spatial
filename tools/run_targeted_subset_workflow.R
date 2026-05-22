
args <- commandArgs(trailingOnly = TRUE)
source(file.path(getwd(), "tools", "workflow_paths.R"))

get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit)) {
    return(default)
  }
  args[hit[1] + 1]
}

resolve_default_subset <- function() {
  review_root <- silica_review_root(resolve_silica_data_root())
  cand <- Sys.glob(file.path(review_root, "rerun", "targeted_rerun_subset_hydrosheds_fallback_*.csv"))
  if (!length(cand)) {
    cand <- Sys.glob(file.path(review_root, "rerun", "targeted_rerun_subset_amazon_hybam_westaus_*.csv"))
  }
  if (!length(cand)) {
    cand <- Sys.glob(file.path(review_root, "rerun", "targeted_rerun_subset_*.csv"))
  }
  if (!length(cand)) {
    stop("No targeted subset CSV found in ", file.path(review_root, "rerun"), ". Run hydrosheds mode from 02_run-workflow.R or build a subset file first.", call. = FALSE)
  }
  cand[which.max(file.info(cand)$mtime)]
}

subset_path <- get_arg("--subset", resolve_default_subset())
combine_full <- tolower(get_arg("--combine-full", "true")) == "true"

if (!file.exists(subset_path)) {
  stop("Subset file does not exist: ", subset_path, call. = FALSE)
}

subset_path <- normalizePath(subset_path, mustWork = TRUE)

message("Using subset file: ", subset_path)

set_default_env <- function(name, value) {
  current <- Sys.getenv(name, unset = NA_character_)
  if (is.na(current) || !nzchar(current)) {
    Sys.setenv(structure(value, names = name))
  }
}

Sys.setenv(
  SILICA_SITE_SUBSET_FILE = subset_path,
  SILICA_SKIP_DRIVE_AUTH = "TRUE",
  SILICA_SKIP_DRIVE_UPLOAD = "TRUE",
  SILICA_COMBINE_LOCAL_ONLY = "TRUE",
  SILICA_CANONICALIZE_OBIDOS = "TRUE",
  SILICA_MERGE_SUBSET_OUTPUTS = "TRUE"
)

set_default_env("SILICA_REBUILD_ARTISANAL", "TRUE")
set_default_env("SILICA_REBUILD_HYDROSHEDS", "TRUE")

library(googledrive)
assignInNamespace(
  x = "drive_upload",
  value = function(...) {
    message("Skipping drive_upload because SILICA_SKIP_DRIVE_UPLOAD=TRUE.")
    invisible(NULL)
  },
  ns = "googledrive"
)

run_isolated <- function(script) {
  source(script, local = new.env(parent = globalenv()))
}

skip_watershed_rebuild <- tolower(Sys.getenv("SILICA_SKIP_WATERSHED_REBUILD", "false")) == "true"
if (skip_watershed_rebuild) {
  message("Skipping watershed rebuild because SILICA_SKIP_WATERSHED_REBUILD=TRUE")
} else {
  message("Step 1: rebuild subset watersheds and assemble subset watershed inputs")
  run_isolated(file.path("02_watershed_delineation", "03_combine-artisanal-hydrosheds.R"))
}

run_static_drivers <- tolower(Sys.getenv("SILICA_RUN_STATIC_DRIVERS", "true")) == "true"
run_dynamic_drivers <- tolower(Sys.getenv("SILICA_RUN_DYNAMIC_DRIVERS", "true")) == "true"

static_scripts <- c(
  file.path("03_spatial_extraction", "extraction_scripts", "extract-soil.R"),
  file.path("03_spatial_extraction", "extraction_scripts", "extract-lithology.R"),
  file.path("03_spatial_extraction", "extraction_scripts", "extract-elevation.R"),
  file.path("03_spatial_extraction", "extraction_scripts", "extract-permafrost.R")
)

dynamic_scripts <- c(
  file.path("03_spatial_extraction", "extraction_scripts", "extract-precip.R"),
  file.path("03_spatial_extraction", "extraction_scripts", "extract-airtemp.R"),
  file.path("03_spatial_extraction", "extraction_scripts", "extract-npp.R"),
  file.path("03_spatial_extraction", "extraction_scripts", "extract-greenup.R"),
  file.path("03_spatial_extraction", "extraction_scripts", "extract-evapo.R"),
  file.path("03_spatial_extraction", "extraction_scripts", "extract-snowfrac.R")
)

extract_scripts <- c(
  if (run_static_drivers) static_scripts else character(0),
  if (run_dynamic_drivers) dynamic_scripts else character(0)
)

for (script in extract_scripts) {
  message("Step 2: running ", script)
  run_isolated(script)
}

if (combine_full) {
  message("Step 3: combine full extracted outputs")
  Sys.unsetenv("SILICA_SITE_SUBSET_FILE")
  run_isolated(file.path("04_combine_qaqc", "combine_from_site_ref_local.R"))
} else {
  message("Skipping full combine because --combine-full=false")
}

message("Targeted subset workflow complete.")
