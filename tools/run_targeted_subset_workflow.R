#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit)) {
    return(default)
  }
  args[hit[1] + 1]
}

resolve_default_subset <- function() {
  cand <- Sys.glob(file.path("qa", "targeted_rerun_subset_amazon_hybam_westaus_*.csv"))
  if (!length(cand)) {
    cand <- Sys.glob(file.path("qa", "targeted_rerun_subset_*.csv"))
  }
  if (!length(cand)) {
    stop("No targeted rerun subset CSV found in qa/. Run tools/build_targeted_rerun_manifest.R first.", call. = FALSE)
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

Sys.setenv(
  SILICA_SITE_SUBSET_FILE = subset_path,
  SILICA_SKIP_DRIVE_AUTH = "TRUE",
  SILICA_SKIP_DRIVE_UPLOAD = "TRUE",
  SILICA_COMBINE_LOCAL_ONLY = "TRUE",
  SILICA_CANONICALIZE_OBIDOS = "TRUE",
  SILICA_MERGE_SUBSET_OUTPUTS = "TRUE",
  SILICA_REBUILD_ARTISANAL = "TRUE",
  SILICA_REBUILD_HYDROSHEDS = "TRUE"
)

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

message("Step 1: rebuild subset watersheds and assemble subset watershed inputs")
run_isolated("wrangle-watersheds.R")

extract_scripts <- c(
  "extract-soil.R",
  "extract-lithology.R",
  "extract-elevation.R",
  "extract-permafrost.R",
  "extract-precip.R",
  "extract-airtemp.R",
  "extract-npp.R",
  "extract-greenup.R",
  "extract-evapo.R",
  "extract-snowfrac.R"
)

for (script in extract_scripts) {
  message("Step 2: running ", script)
  run_isolated(script)
}

if (combine_full) {
  message("Step 3: combine full extracted outputs")
  Sys.unsetenv("SILICA_SITE_SUBSET_FILE")
  run_isolated(file.path("tools", "combine_from_site_ref_local.R"))
} else {
  message("Skipping full combine because --combine-full=false")
}

message("Targeted subset workflow complete.")
