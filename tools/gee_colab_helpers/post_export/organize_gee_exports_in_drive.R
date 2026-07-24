suppressPackageStartupMessages({
  library(googledrive)
})

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default = "") {
  hit <- which(args == flag)
  if (!length(hit)) {
    return(default)
  }
  if (hit[1] == length(args)) {
    stop("Missing value for ", flag, call. = FALSE)
  }
  args[hit[1] + 1]
}

parse_bool_arg <- function(flag, default) {
  value <- get_arg(flag, if (default) "TRUE" else "FALSE")
  toupper(value) %in% c("TRUE", "T", "1", "YES", "Y")
}

parse_years <- function(value) {
  if (!nzchar(value)) {
    return(integer())
  }
  years <- as.integer(strsplit(value, ",", fixed = TRUE)[[1]])
  years[!is.na(years)]
}

regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

exact_pattern <- function(x) {
  paste0("^", regex_escape(x), "$")
}

drive_ls_exact <- function(parent_id, name) {
  matches <- drive_ls(as_id(parent_id), pattern = exact_pattern(name))
  matches[matches$name == name, , drop = FALSE]
}

find_or_create_child_folder <- function(parent_id, folder_name) {
  matches <- drive_ls_exact(parent_id, folder_name)
  if (nrow(matches)) {
    return(matches[1, ])
  }

  drive_mkdir(folder_name, path = as_id(parent_id))
}

find_export_file <- function(file_name, parent_id, run_folder_id, search_all_drive) {
  run_matches <- drive_ls_exact(run_folder_id, file_name)
  if (nrow(run_matches)) {
    return(list(file = run_matches[1, ], status = "already_in_run_folder"))
  }

  parent_matches <- drive_ls_exact(parent_id, file_name)
  if (nrow(parent_matches)) {
    return(list(file = parent_matches[1, ], status = "found_in_export_root"))
  }

  if (search_all_drive) {
    all_matches <- drive_find(pattern = exact_pattern(file_name), n_max = 25)
    all_matches <- all_matches[all_matches$name == file_name, , drop = FALSE]
    if (nrow(all_matches)) {
      return(list(file = all_matches[1, ], status = "found_elsewhere_in_drive"))
    }
  }

  list(file = NULL, status = "missing")
}

run_label <- get_arg("--run-label", "comparison_sites_fine_scale")
run_lter_label <- get_arg("--run-lter-label", "")
start_year <- as.integer(get_arg("--start-year", "2001"))
end_year <- as.integer(get_arg("--end-year", "2023"))
years <- parse_years(get_arg("--years", ""))
years_to_skip <- parse_years(get_arg("--years-to-skip", ""))
drive_export_folder_id <- get_arg(
  "--drive-export-folder-id",
  Sys.getenv("SILICA_GEE_DRIVE_EXPORT_FOLDER_ID", unset = "")
)
drive_run_folder <- get_arg("--drive-run-folder", "")
drive_account <- get_arg("--drive-account", "")
search_all_drive <- parse_bool_arg("--search-all-drive", TRUE)
fail_on_missing <- parse_bool_arg("--fail-on-missing", FALSE)

if (!nzchar(drive_export_folder_id)) {
  stop(
    "Pass --drive-export-folder-id or set ",
    "SILICA_GEE_DRIVE_EXPORT_FOLDER_ID.",
    call. = FALSE
  )
}

if (!length(years)) {
  if (is.na(start_year) || is.na(end_year) || start_year > end_year) {
    stop("Expected --start-year and --end-year to define a valid year range.", call. = FALSE)
  }
  years <- seq.int(start_year, end_year)
}
years <- setdiff(years, years_to_skip)

run_group <- if (nzchar(run_lter_label)) {
  paste(run_lter_label, run_label, sep = "_")
} else {
  run_label
}

if (!nzchar(drive_run_folder)) {
  drive_run_folder <- paste0("era5_land_", run_group, "_", min(years), "_", max(years))
}

expected_csv_names <- paste0(
  "era5_land_",
  years,
  "_",
  run_group,
  "_watershed_extract.csv"
)

if (nzchar(drive_account)) {
  drive_auth(email = drive_account)
} else {
  drive_auth()
}

run_folder <- find_or_create_child_folder(drive_export_folder_id, drive_run_folder)
run_folder_id <- run_folder$id[[1]]
run_folder_url <- paste0("https://drive.google.com/drive/folders/", run_folder_id)

message("Shared GEE Drive folder ID: ", drive_export_folder_id)
message("Run folder: ", drive_run_folder)
message("Run folder URL: ", run_folder_url)

summary_rows <- lapply(expected_csv_names, function(file_name) {
  result <- find_export_file(
    file_name = file_name,
    parent_id = drive_export_folder_id,
    run_folder_id = run_folder_id,
    search_all_drive = search_all_drive
  )

  if (is.null(result$file)) {
    message("Not found yet: ", file_name)
    return(data.frame(file_name = file_name, status = "missing", stringsAsFactors = FALSE))
  }

  if (identical(result$status, "already_in_run_folder")) {
    message("Already in run folder: ", file_name)
    return(data.frame(file_name = file_name, status = result$status, stringsAsFactors = FALSE))
  }

  drive_mv(result$file, path = as_id(run_folder_id))
  message("Moved to run folder: ", file_name)
  data.frame(file_name = file_name, status = result$status, stringsAsFactors = FALSE)
})

summary <- do.call(rbind, summary_rows)
missing <- summary$file_name[summary$status == "missing"]

message("CSV files organized/found in run folder: ", sum(summary$status != "missing"))
message("CSV files not found yet: ", length(missing))

if (length(missing)) {
  message("Missing files:")
  message(paste(missing, collapse = "\n"))
  if (fail_on_missing) {
    stop("Some expected GEE CSV exports are missing.", call. = FALSE)
  }
}
