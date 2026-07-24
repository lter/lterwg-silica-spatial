## Optional wrapper around the numbered workflow stages.
## Direct stage-by-stage use is also supported.

args <- commandArgs(trailingOnly = TRUE)
source(file.path("tools", "workflow_paths.R"))

get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  args[hit[1] + 1]
}

config_file <- get_arg("--config", "01_run_config.R")
config_file <- normalizePath(config_file, mustWork = TRUE)
source(config_file, local = TRUE)

if (!exists("run_settings", inherits = FALSE) || !is.list(run_settings)) {
  stop("Config file must create a list called run_settings.", call. = FALSE)
}

setting <- function(name, default = NULL) {
  val <- run_settings[[name]]
  if (is.null(val) || (is.character(val) && !nzchar(val))) default else val
}

set_env_value <- function(name, value) {
  if (is.null(value)) return()
  if (is.logical(value)) value <- ifelse(isTRUE(value), "TRUE", "FALSE")
  do.call(Sys.setenv, stats::setNames(list(as.character(value)), name))
}

mode <- tolower(setting("mode", ""))
if (identical(mode, "combine_qaqc")) {
  mode <- "combine_review"
}
if (!mode %in% c("full", "subset", "update_years", "hydrosheds", "combine_review")) {
  stop("run_settings$mode must be one of: full, subset, update_years, hydrosheds, combine_review", call. = FALSE)
}

set_env_value("SILICA_OUTPUT_DATE", setting("output_date", format(Sys.Date(), "%Y%m%d")))
set_env_value("SILICA_RUN_LABEL", setting("run_label", ""))
set_env_value("SILICA_RUN_MODE", mode)
set_env_value(
  "SILICA_USE_CANONICAL_RELEASE_LIBRARY",
  setting("use_canonical_release_library", identical(mode, "full"))
)
set_env_value("SILICA_REFERENCE_RELEASE", setting("reference_release", 3))
set_env_value("SILICA_SKIP_DRIVE_AUTH", setting("skip_drive_auth", TRUE))
set_env_value("SILICA_SKIP_DRIVE_UPLOAD", setting("skip_drive_upload", TRUE))
set_env_value("SILICA_REBUILD_ARTISANAL", setting("rebuild_artisanal", TRUE))
set_env_value("SILICA_REBUILD_HYDROSHEDS", setting("rebuild_hydrosheds", TRUE))
set_env_value("SILICA_COMBINE_FULL", setting("combine_after_extract", FALSE))
set_env_value("SILICA_WRITE_DETAILED_REVIEW", setting("write_detailed_review", FALSE))
set_env_value("SILICA_DRY_RUN", setting("dry_run", FALSE))
set_env_value("SILICA_TARGET_YEAR_START", setting("start_year", NA))
set_env_value("SILICA_TARGET_YEAR_END", setting("end_year", NA))
target_years <- setting("target_years", integer(0))
if (length(target_years)) {
  set_env_value("SILICA_TARGET_YEARS", paste(target_years, collapse = ","))
}
set_env_value(
  "SILICA_HYDROSHEDS_AREA_THRESHOLD_KM2",
  setting("hydrosheds_area_threshold_km2", 100)
)

if (nzchar(setting("data_root", ""))) {
  set_env_value("SILICA_DATA_ROOT", setting("data_root"))
}
if (nzchar(setting("shape_library_root", ""))) {
  set_env_value(
    "SILICA_SHAPE_LIBRARY_ROOT",
    setting("shape_library_root")
  )
}
if (nzchar(setting("site_reference_file", ""))) {
  site_reference_file <- normalizePath(
    setting("site_reference_file"),
    mustWork = TRUE
  )
  set_env_value("SILICA_SITE_REF_FILE", site_reference_file)
}

resolved_data_root <- resolve_silica_data_root()

if (nzchar(setting("site_followup_file", ""))) {
  site_followup_file <- normalizePath(setting("site_followup_file"), mustWork = TRUE)
  set_env_value("SILICA_SITE_FOLLOWUP_FILE", site_followup_file)
}

qa_root <- setting("qa_root", "")
if (nzchar(qa_root)) {
  set_env_value("SILICA_QA_ROOT", qa_root)
} else {
  set_env_value("SILICA_QA_ROOT", file.path(resolved_data_root, "review"))
}

if (nzchar(setting("subset_file", ""))) {
  subset_file <- normalizePath(setting("subset_file"), mustWork = TRUE)
  set_env_value("SILICA_SITE_SUBSET_FILE", subset_file)
} else {
  subset_file <- ""
}

message("Run mode: ", mode)

if (mode == "full") {
  source(file.path("03_spatial_extraction", "modes", "full_run_all_sites.R"), echo = TRUE)
} else if (mode == "subset") {
  if (!nzchar(subset_file)) {
    stop("Subset mode needs run_settings$subset_file.", call. = FALSE)
  }
  source(file.path("03_spatial_extraction", "modes", "targeted_sites_run.R"), echo = TRUE)
} else if (mode == "update_years") {
  source(file.path("03_spatial_extraction", "modes", "update_years_run.R"), echo = TRUE)
} else if (mode == "hydrosheds") {
  source(file.path("03_spatial_extraction", "modes", "hydrosheds_sites_run.R"), echo = TRUE)
} else if (mode == "combine_review") {
  status <- system2(
    "Rscript",
    file.path("04_combine_qaqc", "combine-spatial-data.R")
  )
  if (!identical(status, 0L)) {
    stop("Combine step did not finish cleanly.", call. = FALSE)
  }

  status <- system2(
    "Rscript",
    file.path("04_combine_qaqc", "qaqc-spatial-data.R")
  )
  if (!identical(status, 0L)) {
    stop("Combine / QAQC step did not finish cleanly.", call. = FALSE)
  }
}
