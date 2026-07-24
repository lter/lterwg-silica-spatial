#!/usr/bin/env Rscript

# Convenience wrapper for the watershed-size old-versus-GEE QA workflow.
#
# Required:
#   --reference-driver-path /path/to/accepted_spatial_driver.csv
#
# Optional:
#   --input-dir /path/to/downloaded/gee/csvs
#   --organize-drive true
#   --drive-export-folder-id GOOGLE_DRIVE_FOLDER_ID
#
# All remaining options are forwarded to the general comparison runner.

source(file.path("tools", "cli_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)

has_flag <- function(flag) {
  cli_has_flag(args, flag)
}

default_arg <- function(flag, value) {
  if (has_flag(flag) || is.null(value) || !nzchar(as.character(value))) {
    character()
  } else {
    c(flag, as.character(value))
  }
}

reference_driver_path <- cli_value(
  args,
  "--reference-driver-path",
  default = env_value("SILICA_REFERENCE_DRIVER_PATH", default = ""),
  required = FALSE
)
reference_driver_path <- require_input_file(
  reference_driver_path,
  "accepted reference-driver table"
)

organize_drive <- cli_boolean(args, "--organize-drive", default = FALSE)
forwarded_args <- cli_without_option(args, "--organize-drive")
start_year <- cli_integer(args, "--start-year", default = 2001L)
end_year <- cli_integer(args, "--end-year", default = 2023L)
run_label <- cli_value(
  args,
  "--run-label",
  default = "comparison_sites_fine_scale"
)

if (organize_drive) {
  drive_folder_id <- cli_value(
    args,
    "--drive-export-folder-id",
    default = "",
    required = TRUE
  )
  organize_args <- c(
    "tools/gee_colab_helpers/post_export/organize_gee_exports_in_drive.R",
    forwarded_args,
    default_arg("--run-label", run_label),
    default_arg("--start-year", start_year),
    default_arg("--end-year", end_year),
    default_arg(
      "--drive-run-folder",
      paste0(
        "gee_exports_era5_land_watershed_size_comparison_sites_",
        start_year,
        "_",
        end_year
      )
    ),
    default_arg("--drive-export-folder-id", drive_folder_id)
  )

  message("Organizing completed GEE CSV exports in Google Drive.")
  organize_status <- system2("Rscript", organize_args)
  if (!identical(organize_status, 0L)) {
    stop("GEE export Drive organization failed.", call. = FALSE)
  }
}

qa_args <- c(
  "tools/gee_colab_helpers/post_run_qa/",
  "run_old_vs_gee_annual_comparison_qa.R"
)
qa_args <- c(
  paste0(qa_args, collapse = ""),
  forwarded_args,
  default_arg("--run-label", run_label),
  default_arg("--slug", "watershed_size_comparison"),
  default_arg("--plot-subject", "Watershed-size comparison sites"),
  default_arg("--start-year", start_year),
  default_arg("--end-year", end_year),
  default_arg("--reference-driver-path", reference_driver_path),
  default_arg("--write-site-plots", "false"),
  default_arg("--max-plot-sites", 40)
)

message("Running local watershed-size QA.")
status <- system2("Rscript", qa_args)
if (!identical(status, 0L)) {
  stop("Watershed-size comparison QA failed.", call. = FALSE)
}
