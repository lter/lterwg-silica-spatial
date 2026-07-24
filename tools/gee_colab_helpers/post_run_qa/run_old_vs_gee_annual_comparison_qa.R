suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

source(file.path("tools", "cli_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default = NULL) {
  cli_value(args, flag, default = default)
}

parse_bool_arg <- function(flag, default) {
  cli_boolean(args, flag, default = default)
}

regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

first_existing_dir <- function(paths) {
  paths <- unique(paths[nzchar(paths)])
  paths[dir.exists(paths)]
}

env_pair <- function(name, value) {
  paste0(name, "=", shQuote(as.character(value)))
}

default_drive_subfolder <- function(slug) {
  paste0(slug, " ERA5-Land comparison")
}

default_drive_export_subfolder <- function(run_label, start_year, end_year) {
  paste0("era5_land_", run_label, "_", start_year, "_", end_year)
}

upload_paths_to_drive <- function(paths, parent_folder_id, main_folder_name, csv_folder_name, account, overwrite) {
  if (!length(paths)) {
    return(invisible(NULL))
  }
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Install the googledrive package or rerun with --upload-to-drive FALSE.",
      call. = FALSE
    )
  }

  if (nzchar(account)) {
    googledrive::drive_auth(email = account)
  } else {
    googledrive::drive_auth()
  }

  drive_child_folder <- function(folder_name, parent) {
    folder_matches <- googledrive::drive_ls(parent) %>%
      filter(name == folder_name)

    if (nrow(folder_matches) > 0) {
      return(folder_matches[1, ])
    }

    googledrive::drive_mkdir(folder_name, path = parent)
  }

  main_folder <- drive_child_folder(main_folder_name, googledrive::as_id(parent_folder_id))
  csv_folder <- drive_child_folder(csv_folder_name, googledrive::as_id(main_folder))

  invisible(lapply(paths, function(path) {
    uploaded <- googledrive::drive_upload(
      media = path,
      path = googledrive::as_id(csv_folder),
      name = basename(path),
      overwrite = overwrite
    )
    message("Uploaded QA table to Google Drive: ", uploaded$name)
    uploaded
  }))
}

download_exports_from_drive <- function(file_names, drive_export_folder_id, drive_export_subfolder, download_dir, account, overwrite) {
  if (!length(file_names)) {
    return(invisible(character(0)))
  }
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Install the googledrive package, download the Colab CSVs manually, or rerun with --download-from-drive FALSE.",
      call. = FALSE
    )
  }

  if (nzchar(account)) {
    googledrive::drive_auth(email = account)
  } else {
    googledrive::drive_auth()
  }

  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
  downloaded_paths <- character(0)
  missing_files <- character(0)
  search_parent_ids <- drive_export_folder_id

  if (nzchar(drive_export_subfolder)) {
    subfolder_matches <- googledrive::drive_ls(
      googledrive::as_id(drive_export_folder_id),
      pattern = paste0("^", regex_escape(drive_export_subfolder), "$")
    )

    if (nrow(subfolder_matches)) {
      search_parent_ids <- c(subfolder_matches$id[[1]], drive_export_folder_id)
      message("Found Drive run folder: ", drive_export_subfolder)
    } else {
      message("Drive run folder not found yet, searching shared Drive export root: ", drive_export_subfolder)
    }
  }

  for (file_name in file_names) {
    target_path <- file.path(download_dir, file_name)
    if (file.exists(target_path) && !overwrite) {
      downloaded_paths <- c(downloaded_paths, target_path)
      next
    }

    matches <- data.frame()
    for (parent_id in search_parent_ids) {
      matches <- googledrive::drive_ls(
        googledrive::as_id(parent_id),
        pattern = paste0("^", regex_escape(file_name), "$")
      )
      if (nrow(matches)) {
        break
      }
    }

    if (!nrow(matches)) {
      missing_files <- c(missing_files, file_name)
      next
    }

    googledrive::drive_download(
      matches[1, ],
      path = target_path,
      overwrite = TRUE
    )
    downloaded_paths <- c(downloaded_paths, target_path)
    message("Downloaded from Google Drive: ", file_name)
  }

  if (length(missing_files)) {
    stop(
      "Could not find these expected CSVs in the Google Drive export folder: ",
      paste(missing_files, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(downloaded_paths)
}

run_label <- get_arg("--run-label", "comparison_sites_fine_scale")
comparison_slug <- get_arg("--slug", "comparison_sites")
plot_subject <- get_arg("--plot-subject", "Comparison sites")
start_year <- as.integer(get_arg("--start-year", "2001"))
end_year <- as.integer(get_arg("--end-year", "2023"))
input_dir <- get_arg("--input-dir", "")
search_dirs <- cli_values(args, "--search-dir")
reference_driver_path <- get_arg("--reference-driver-path", "")
download_from_drive <- parse_bool_arg("--download-from-drive", FALSE)
drive_export_folder_id <- get_arg("--drive-export-folder-id", "")
drive_export_subfolder <- get_arg(
  "--drive-export-subfolder",
  default_drive_export_subfolder(run_label, start_year, end_year)
)
upload_to_drive <- parse_bool_arg("--upload-to-drive", FALSE)
write_site_plots <- parse_bool_arg("--write-site-plots", TRUE)
max_plot_sites <- get_arg("--max-plot-sites", "80")
skip_comparison <- parse_bool_arg("--skip-comparison", FALSE)
drive_folder_id <- get_arg("--drive-folder-id", "")
drive_subfolder <- get_arg("--drive-subfolder", default_drive_subfolder(comparison_slug))
drive_plot_folder <- get_arg("--drive-plot-folder", "plots")
drive_csv_folder <- get_arg("--drive-csv-folder", "csv files")
drive_account <- get_arg("--drive-account", "")
drive_overwrite <- parse_bool_arg("--drive-overwrite", TRUE)

if (download_from_drive && !nzchar(drive_export_folder_id)) {
  stop(
    "--download-from-drive TRUE requires --drive-export-folder-id.",
    call. = FALSE
  )
}
if (upload_to_drive && !nzchar(drive_folder_id)) {
  stop(
    "--upload-to-drive TRUE requires --drive-folder-id.",
    call. = FALSE
  )
}

if (is.na(start_year) || is.na(end_year) || start_year > end_year) {
  stop("Expected --start-year and --end-year to define a valid year range.", call. = FALSE)
}

today_tag <- format(Sys.Date(), "%Y%m%d")
drive_download_folder <- file.path(
  "generated_outputs",
  paste0(comparison_slug, "_gee_drive_exports_", today_tag),
  paste0("era5_land_", run_label, "_", start_year, "_", end_year)
)
generated_output_dirs <- if (dir.exists("generated_outputs")) {
  list.dirs("generated_outputs", recursive = TRUE, full.names = TRUE)
} else {
  character(0)
}
output_folder <- file.path(
  "generated_outputs",
  paste0(comparison_slug, "_era5_spatial_driver_comparison_", today_tag)
)

era5_pattern <- paste0(
  "^era5_land_[0-9]{4}_",
  regex_escape(run_label),
  "_watershed_extract\\.csv$"
)

candidate_dirs <- first_existing_dir(c(
  input_dir,
  search_dirs,
  drive_download_folder,
  generated_output_dirs
))

files_by_dir <- lapply(candidate_dirs, function(folder) {
  list.files(folder, pattern = era5_pattern, full.names = TRUE)
})

folder_summary <- tibble(
  folder = candidate_dirs,
  n_files = lengths(files_by_dir),
  latest_file_time = as.POSIXct(vapply(
    files_by_dir,
    function(files) {
      if (!length(files)) {
        return(NA_real_)
      }
      max(file.info(files)$mtime)
    },
    numeric(1)
  ), origin = "1970-01-01")
) %>%
  filter(n_files > 0) %>%
  arrange(desc(n_files), desc(latest_file_time))

expected_years <- seq.int(start_year, end_year)
expected_file_names <- paste0(
  "era5_land_",
  expected_years,
  "_",
  run_label,
  "_watershed_extract.csv"
)

if (!nrow(folder_summary) && download_from_drive) {
  message("No local ERA5-Land CSVs found. Downloading expected exports from Google Drive.")
  download_exports_from_drive(
    file_names = expected_file_names,
    drive_export_folder_id = drive_export_folder_id,
    drive_export_subfolder = drive_export_subfolder,
    download_dir = drive_download_folder,
    account = drive_account,
    overwrite = drive_overwrite
  )

  candidate_dirs <- first_existing_dir(c(drive_download_folder, candidate_dirs))
  files_by_dir <- lapply(candidate_dirs, function(folder) {
    list.files(folder, pattern = era5_pattern, full.names = TRUE)
  })
  folder_summary <- tibble(
    folder = candidate_dirs,
    n_files = lengths(files_by_dir),
    latest_file_time = as.POSIXct(vapply(
      files_by_dir,
      function(files) {
        if (!length(files)) {
          return(NA_real_)
        }
        max(file.info(files)$mtime)
      },
      numeric(1)
    ), origin = "1970-01-01")
  ) %>%
    filter(n_files > 0) %>%
    arrange(desc(n_files), desc(latest_file_time))
}

if (!nrow(folder_summary)) {
  stop(
    "Could not find ERA5-Land exports for run label `",
    run_label,
    "`. Download the Colab CSVs first, or pass --input-dir.",
    call. = FALSE
  )
}

export_folder <- folder_summary$folder[[1]]
export_files <- sort(files_by_dir[[match(export_folder, candidate_dirs)]])
export_years <- as.integer(sub("^era5_land_([0-9]{4})_.*$", "\\1", basename(export_files)))
missing_years <- setdiff(expected_years, export_years)

if (length(missing_years) && download_from_drive) {
  message("Downloading missing ERA5-Land years from Google Drive: ", paste(missing_years, collapse = ", "))
  missing_file_names <- paste0(
    "era5_land_",
    missing_years,
    "_",
    run_label,
    "_watershed_extract.csv"
  )
  download_exports_from_drive(
    file_names = missing_file_names,
    drive_export_folder_id = drive_export_folder_id,
    drive_export_subfolder = drive_export_subfolder,
    download_dir = export_folder,
    account = drive_account,
    overwrite = drive_overwrite
  )
  export_files <- sort(list.files(export_folder, pattern = era5_pattern, full.names = TRUE))
  export_years <- as.integer(sub("^era5_land_([0-9]{4})_.*$", "\\1", basename(export_files)))
  missing_years <- setdiff(expected_years, export_years)
}

message("Using ERA5-Land export folder: ", normalizePath(export_folder))
message("Found ERA5-Land export files: ", length(export_files))
if (length(missing_years)) {
  stop(
    "Missing expected export years for ",
    run_label,
    ": ",
    paste(missing_years, collapse = ", "),
    call. = FALSE
  )
}

if (!skip_comparison) {
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  env <- c(
    env_pair("SILICA_ERA5_RUN_LABEL", run_label),
    env_pair("SILICA_ERA5_LTER_FILTER", "ALL"),
    env_pair("SILICA_ERA5_COMPARISON_SLUG", comparison_slug),
    env_pair("SILICA_ERA5_PLOT_SUBJECT", plot_subject),
    env_pair("SILICA_ERA5_INPUT_DIR", normalizePath(export_folder)),
    env_pair("SILICA_UPLOAD_ERA5_COMPARISON_TO_DRIVE", if (upload_to_drive) "TRUE" else "FALSE"),
    env_pair("SILICA_WRITE_ERA5_SITE_PLOTS", if (write_site_plots) "TRUE" else "FALSE"),
    env_pair("SILICA_ERA5_MAX_PLOT_SITES", max_plot_sites),
    env_pair("SILICA_ERA5_COMPARISON_DRIVE_FOLDER_ID", drive_folder_id),
    env_pair("SILICA_ERA5_COMPARISON_DRIVE_SUBFOLDER", drive_subfolder),
    env_pair("SILICA_ERA5_COMPARISON_PLOT_FOLDER", drive_plot_folder),
    env_pair("SILICA_ERA5_COMPARISON_CSV_FOLDER", drive_csv_folder),
    env_pair("SILICA_GOOGLE_DRIVE_ACCOUNT", drive_account),
    env_pair("SILICA_GOOGLE_DRIVE_OVERWRITE", if (drive_overwrite) "TRUE" else "FALSE")
  )
  if (nzchar(reference_driver_path)) {
    env <- c(env, env_pair("SILICA_REFERENCE_DRIVER_PATH", reference_driver_path))
  }

  status <- system2(
    "Rscript",
    "tools/plot_and_era5_modis_comparison.R",
    env = env
  )
  if (!identical(status, 0L)) {
    stop("Comparison plotting script failed.", call. = FALSE)
  }
}

comparison_points_file <- file.path(
  output_folder,
  paste0(comparison_slug, "_era5land_spatial_driver_comparison_points.csv")
)
site_stats_file <- file.path(
  output_folder,
  paste0(comparison_slug, "_era5land_spatial_driver_site_regression_stats.csv")
)

if (!file.exists(comparison_points_file) || !file.exists(site_stats_file)) {
  stop(
    "Expected comparison outputs were not found in ",
    output_folder,
    ". Run without --skip-comparison after CSVs are downloaded.",
    call. = FALSE
  )
}

points <- read_csv(comparison_points_file, show_col_types = FALSE)
site_stats <- read_csv(site_stats_file, show_col_types = FALSE)

qa_summary <- points %>%
  group_by(comparison, reference_product) %>%
  summarise(
    n_points = n(),
    n_lter = n_distinct(lter),
    n_sites = n_distinct(site_panel),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    missing_era5_values = sum(is.na(era5_value)),
    missing_reference_values = sum(is.na(reference_value)),
    native_scale_polygon_mean = sum(era5_fallback_method == "Native-scale polygon mean", na.rm = TRUE),
    fine_scale_polygon_retry = sum(era5_fallback_method == "Fine-scale polygon retry", na.rm = TRUE),
    centroid_fill = sum(era5_fallback_method == "Centroid fill", na.rm = TRUE),
    sites_with_shared_era5_values = n_distinct(site_panel[shared_era5_values %in% TRUE]),
    sites_with_shared_reference_values = n_distinct(site_panel[shared_reference_values %in% TRUE]),
    .groups = "drop"
  ) %>%
  left_join(
    site_stats %>%
      group_by(comparison) %>%
      summarise(
        median_site_r2 = if (all(is.na(r_squared))) NA_real_ else median(r_squared, na.rm = TRUE),
        min_site_r2 = if (all(is.na(r_squared))) NA_real_ else min(r_squared, na.rm = TRUE),
        max_site_r2 = if (all(is.na(r_squared))) NA_real_ else max(r_squared, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "comparison"
  )

fallback_by_site <- points %>%
  count(comparison, lter, stream_name, shapefile_name, site_panel, era5_fallback_method, name = "n_points") %>%
  arrange(comparison, lter, stream_name, shapefile_name, era5_fallback_method)

shared_value_groups <- bind_rows(
  points %>%
    filter(shared_era5_values %in% TRUE) %>%
    distinct(
      comparison,
      source = "ERA5-Land",
      shared_group = shared_era5_group,
      n_sites = n_sites_sharing_era5,
      shared_sites = shared_era5_sites
    ),
  points %>%
    filter(shared_reference_values %in% TRUE) %>%
    distinct(
      comparison,
      source = "old/reference driver",
      shared_group = shared_reference_group,
      n_sites = n_sites_sharing_reference,
      shared_sites = shared_reference_sites
    )
) %>%
  arrange(comparison, source, shared_group)

qa_summary_file <- file.path(output_folder, paste0(comparison_slug, "_era5land_qa_summary.csv"))
fallback_by_site_file <- file.path(output_folder, paste0(comparison_slug, "_era5land_fallback_by_site.csv"))
shared_value_groups_file <- file.path(output_folder, paste0(comparison_slug, "_era5land_shared_value_groups.csv"))

write_csv(qa_summary, qa_summary_file, na = "")
write_csv(fallback_by_site, fallback_by_site_file, na = "")
write_csv(shared_value_groups, shared_value_groups_file, na = "")

if (upload_to_drive) {
  upload_paths_to_drive(
    paths = c(qa_summary_file, fallback_by_site_file, shared_value_groups_file),
    parent_folder_id = drive_folder_id,
    main_folder_name = drive_subfolder,
    csv_folder_name = drive_csv_folder,
    account = drive_account,
    overwrite = drive_overwrite
  )
}

message("Wrote QA summary: ", normalizePath(qa_summary_file))
message("Wrote fallback-by-site summary: ", normalizePath(fallback_by_site_file))
message("Wrote shared-value groups: ", normalizePath(shared_value_groups_file))
message("Review folder: ", normalizePath(output_folder))
