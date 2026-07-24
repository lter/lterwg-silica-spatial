#!/usr/bin/env Rscript

# Prepare isolated MODIS extraction inputs for accepted AppEEARS tasks.
#
# Each AppEEARS bundle is cropped for one watershed. Keeping every watershed in
# its own raw-data directory prevents a nearby watershed from being evaluated
# against another site's cropped raster. The prepared files live under
# generated_outputs, which is ignored by Git.
#
# Required inputs:
#   --run-root PATH       Repeat for each AppEEARS run directory.
#   --watershed-file PATH Repeat for each accepted watershed layer.
#
# Common optional inputs:
#   --task-name NAME      Prepare only the named task; may be repeated.
#   --status-file PATH    Task tracker keyed by task_name.
#   --region-map PATH     TSV or CSV with LTER and Region columns.
#   --start-year YEAR     First source year to stage (default: 2002).
#   --end-year YEAR       Last source year to stage (default: 2022).

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
})

source(file.path("tools", "cli_helpers.R"))
source(file.path("tools", "workflow_paths.R"))

args <- commandArgs(trailingOnly = TRUE)
repo_root <- silica_find_repo_root()
output_root <- cli_value(
  args,
  "--output-root",
  file.path(repo_root, "generated_outputs", "appeears", "targeted-extraction-inputs")
)
run_roots <- cli_values(args, "--run-root")
watershed_files <- cli_values(args, "--watershed-file")
if (!length(run_roots) || !length(watershed_files)) {
  stop(
    "Provide at least one --run-root and one --watershed-file.",
    call. = FALSE
  )
}
run_roots <- vapply(run_roots, require_input_dir, character(1), label = "run root")
watershed_files <- vapply(
  watershed_files,
  require_input_file,
  character(1),
  label = "watershed file"
)

status_file <- cli_value(
  args,
  "--status-file",
  file.path(repo_root, "tracking", "appeears_new_watershed_status.csv")
)
status_file <- require_input_file(status_file, "task status file")
start_year <- cli_integer(args, "--start-year", 2002L)
end_year <- cli_integer(args, "--end-year", 2022L)
if (start_year > end_year) {
  stop("--start-year cannot be later than --end-year.", call. = FALSE)
}
excluded_task_patterns <- cli_values(args, "--exclude-task-pattern")
region_map_path <- cli_value(
  args,
  "--region-map",
  file.path(
    repo_root,
    "03_spatial_extraction",
    "config",
    "dynamic_region_routes.tsv"
  )
)

required_run_files <- unlist(
  lapply(run_roots, function(run_root) {
    c(
      file.path(run_root, "download-lists"),
      file.path(run_root, "download_timing.csv")
    )
  }),
  use.names = FALSE
)
missing_required <- required_run_files[!file.exists(required_run_files)]
if (length(missing_required)) {
  stop(
    "Each run root must contain download-lists/ and download_timing.csv. Missing: ",
    paste(missing_required, collapse = ", "),
    call. = FALSE
  )
}

task_from_list <- function(path) {
  sub("-download-list\\.txt$", "", basename(path))
}

list_files <- unlist(lapply(run_roots, function(run_root) {
  list.files(
    file.path(run_root, "download-lists"),
    pattern = "-download-list\\.txt$",
    full.names = TRUE
  )
}), use.names = FALSE)

task_inputs <- data.frame(
  task_name = vapply(list_files, task_from_list, character(1)),
  list_file = normalizePath(list_files, mustWork = TRUE),
  stringsAsFactors = FALSE
)
task_inputs$run_root <- vapply(task_inputs$list_file, function(path) {
  normalizePath(dirname(dirname(path)), mustWork = TRUE)
}, character(1))
task_inputs$download_dir <- file.path(
  task_inputs$run_root, "downloads", task_inputs$task_name
)

requested_tasks <- unique(cli_values(args, "--task-name"))
if (length(requested_tasks)) {
  missing_tasks <- setdiff(requested_tasks, task_inputs$task_name)
  if (length(missing_tasks)) {
    stop(
      "No accepted download list found for requested task(s): ",
      paste(missing_tasks, collapse = ", "),
      call. = FALSE
    )
  }
  task_inputs <- task_inputs %>% filter(task_name %in% requested_tasks)
}

expected_tasks <- cli_integer(
  args,
  "--expected-task-count",
  if (length(requested_tasks)) length(requested_tasks) else nrow(task_inputs)
)
if (nrow(task_inputs) != expected_tasks || anyDuplicated(task_inputs$task_name)) {
  stop(
    "Expected ", expected_tasks, " unique accepted download lists; found ",
    nrow(task_inputs), ".",
    call. = FALSE
  )
}

for (pattern in excluded_task_patterns) {
  if (any(grepl(pattern, task_inputs$task_name))) {
    stop(
      "An excluded task matched --exclude-task-pattern ", shQuote(pattern), ".",
      call. = FALSE
    )
  }
}

status <- read.csv(status_file, stringsAsFactors = FALSE, check.names = FALSE)
targets <- task_inputs %>%
  left_join(status, by = "task_name")

if (any(is.na(targets$watershed_key))) {
  stop(
    "Some accepted tasks are missing from the watershed tracker: ",
    paste(targets$task_name[is.na(targets$watershed_key)], collapse = ", "),
    call. = FALSE
  )
}

# Require a clean, fully downloaded bundle before any extraction input is made.
download_checks <- lapply(run_roots, function(run_root) {
  timing_file <- file.path(run_root, "download_timing.csv")
  if (!file.exists(timing_file)) {
    stop("Download timing file is not ready: ", timing_file, call. = FALSE)
  }
  read.csv(timing_file, stringsAsFactors = FALSE, check.names = FALSE)
}) %>% bind_rows()

targets <- targets %>%
  left_join(
    download_checks %>% select(task_name, download_record_status = status),
    by = "task_name"
  )

not_complete <- targets %>%
  filter(is.na(download_record_status) | download_record_status != "complete")
if (nrow(not_complete)) {
  stop(
    "Downloads are not complete for ", nrow(not_complete), " accepted task(s): ",
    paste(not_complete$task_name, collapse = ", "),
    call. = FALSE
  )
}

product_rules <- data.frame(
  driver = c("greenup", "npp", "evapo", "snow"),
  directory = c(
    "raw-greenup-v061", "raw-npp-v061", "raw-evapo-v061", "raw-snow-v061"
  ),
  pattern = c(
    "^MCD12Q2\\.061_Greenup_[01]_",
    "^MOD17A3HGF\\.061_Npp_500m_",
    "^MOD16A2GF\\.061_ET_500m_",
    "^MOD10A2\\.061_Eight_Day_Snow_Cover_"
  ),
  stringsAsFactors = FALSE
)

parse_date <- function(filename) {
  stamp <- regmatches(filename, regexpr("[0-9]{8}T[0-9]{6}", filename))
  if (!length(stamp) || is.na(stamp) || !nzchar(stamp)) return(as.Date(NA))
  as.Date(substr(stamp, 1L, 8L), format = "%Y%m%d")
}

staged_name <- function(filename, date) {
  doy <- format(date, "%Y%j")
  sub("[0-9]{8}T[0-9]{6}", paste0("doy", doy), filename)
}

link_file <- function(source, destination) {
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(destination)) {
    if (file.info(source)$size != file.info(destination)$size) {
      stop("Existing staged file has the wrong size: ", destination, call. = FALSE)
    }
    return(invisible(destination))
  }
  linked <- file.link(source, destination)
  if (!linked) linked <- file.symlink(source, destination)
  if (!linked) stop("Could not link staged raster: ", source, call. = FALSE)
  invisible(destination)
}

region_map_path <- require_input_file(region_map_path, "region map")
region_map <- if (tolower(tools::file_ext(region_map_path)) == "tsv") {
  read.delim(
    region_map_path,
    sep = "\t",
    quote = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
} else {
  read.csv(
    region_map_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
assert_required_columns(region_map, c("LTER", "Region"), "region map")
if ("Stream_Name" %in% names(region_map)) {
  region_map <- region_map[
    is.na(region_map$Stream_Name) | !nzchar(trimws(region_map$Stream_Name)),
    ,
    drop = FALSE
  ]
}
region_map <- region_map[c("LTER", "Region")]
region_map$key <- normalize_lter_key(region_map$LTER)
if (anyDuplicated(region_map$key)) {
  stop("The region map contains duplicate LTER values.", call. = FALSE)
}

workflow_region <- function(lter) {
  key <- normalize_lter_key(lter)
  region <- region_map$Region[match(key, region_map$key)]
  if (is.null(region) || is.na(region) || !nzchar(region)) {
    stop(
      "No dynamic-driver region is defined for LTER ", shQuote(lter),
      ". Add it to the CSV supplied with --region-map.",
      call. = FALSE
    )
  }
  region
}

first_column <- function(data, candidates, default = NA_character_) {
  found <- candidates[candidates %in% names(data)]
  if (!length(found)) return(rep(default, nrow(data)))
  data[[found[[1]]]]
}

read_accepted_watersheds <- function(path) {
  data <- st_read(path, quiet = TRUE)
  assert_required_columns(
    data,
    c("LTER", "Stream_Name", "Shapefile_Name"),
    paste("watershed file", path)
  )
  if ("accepted" %in% names(data)) {
    data <- data[is.na(data$accepted) | as.logical(data$accepted), , drop = FALSE]
  }
  polygon_area <- as.numeric(st_area(st_transform(data, 6933))) / 1e6
  expected_area <- suppressWarnings(as.numeric(first_column(
    data,
    c("expected_area_km2", "reference_area_km2", "drainSqKm")
  )))
  reported_polygon_area <- suppressWarnings(as.numeric(first_column(
    data,
    c("polygon_area_km2", "real_area")
  )))
  reported_polygon_area[
    is.na(reported_polygon_area) | reported_polygon_area <= 0
  ] <- polygon_area[
    is.na(reported_polygon_area) | reported_polygon_area <= 0
  ]
  data %>%
    transmute(
      LTER = as.character(LTER),
      shp_nm = as.character(Shapefile_Name),
      Strm_Nm = as.character(Stream_Name),
      exp_area = expected_area,
      real_area = reported_polygon_area,
      Dsc_F_N = as.character(first_column(
        data,
        c("Discharge_File_Name", "discharge_file")
      ))
    )
}

accepted_watersheds <- do.call(
  rbind,
  lapply(watershed_files, read_accepted_watersheds)
)
if (anyDuplicated(accepted_watersheds$shp_nm)) {
  duplicates <- unique(
    accepted_watersheds$shp_nm[duplicated(accepted_watersheds$shp_nm)]
  )
  stop(
    "Accepted watershed names must be unique across --watershed-file inputs: ",
    paste(duplicates, collapse = ", "),
    call. = FALSE
  )
}

dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
site_manifest <- vector("list", nrow(targets))
raster_manifest <- vector("list", nrow(targets))

for (i in seq_len(nrow(targets))) {
  target <- targets[i, , drop = FALSE]
  task_name <- target$task_name[[1]]
  watershed_key <- target$watershed_key[[1]]
  site_slug <- gsub("(^-+|-+$)", "", gsub("[^a-z0-9]+", "-", tolower(watershed_key)))
  site_root <- file.path(output_root, "sites", site_slug)

  expected_urls <- trimws(readLines(target$list_file[[1]], warn = FALSE))
  expected_urls <- expected_urls[nzchar(expected_urls)]
  expected_names <- basename(expected_urls)
  if (anyDuplicated(expected_names)) {
    stop("Duplicate bundle filename in ", target$list_file[[1]], call. = FALSE)
  }

  source_files <- file.path(target$download_dir[[1]], expected_names)
  missing_downloads <- source_files[
    !file.exists(source_files) | is.na(file.info(source_files)$size) | file.info(source_files)$size <= 0
  ]
  if (length(missing_downloads)) {
    stop(
      "Downloaded bundle is incomplete for ", task_name, ": ",
      length(missing_downloads), " file(s) missing or empty.",
      call. = FALSE
    )
  }

  prepared_watershed <- accepted_watersheds %>%
    filter(shp_nm == target$Shapefile_Name[[1]])
  if (nrow(prepared_watershed) != 1L) {
    stop(
      "Could not identify exactly one accepted watershed for ",
      watershed_key,
      call. = FALSE
    )
  }

  watershed_path <- file.path(site_root, "accepted-watershed.gpkg")
  dir.create(site_root, recursive = TRUE, showWarnings = FALSE)
  if (file.exists(watershed_path)) file.remove(watershed_path)
  st_write(prepared_watershed, watershed_path, quiet = TRUE)

  site_values <- st_drop_geometry(prepared_watershed)
  site_region <- workflow_region(site_values$LTER[[1]])
  base_file <- file.path(site_root, "site-reference.csv")
  subset_file <- file.path(site_root, "site-subset.csv")
  write.csv(
    data.frame(
      LTER = site_values$LTER,
      Stream_Name = site_values$Strm_Nm,
      Discharge_File_Name = site_values$Dsc_F_N,
      Shapefile_Name = site_values$shp_nm
    ),
    base_file,
    row.names = FALSE,
    na = ""
  )
  write.csv(
    data.frame(
      LTER = site_values$LTER,
      Stream_Name = site_values$Strm_Nm,
      Shapefile_Name = site_values$shp_nm,
      Region = site_region
    ),
    subset_file,
    row.names = FALSE,
    na = ""
  )

  site_rasters <- list()
  for (j in seq_len(nrow(product_rules))) {
    rule <- product_rules[j, , drop = FALSE]
    selected <- grepl(rule$pattern[[1]], expected_names)
    selected_dates <- as.Date(vapply(expected_names[selected], function(name) {
      as.character(parse_date(name))
    }, character(1)))
    keep_year <- !is.na(selected_dates) &
      as.integer(format(selected_dates, "%Y")) >= start_year &
      as.integer(format(selected_dates, "%Y")) <= end_year
    selected_names <- expected_names[selected][keep_year]
    selected_dates <- selected_dates[keep_year]
    selected_sources <- file.path(target$download_dir[[1]], selected_names)

    if (!length(selected_sources)) {
      stop("No ", rule$driver[[1]], " rasters found for ", task_name, call. = FALSE)
    }

    destination_dir <- file.path(
      site_root, "raw-driver-data", rule$directory[[1]], site_region
    )
    legacy_target_dir <- file.path(
      site_root, "raw-driver-data", rule$directory[[1]], "target"
    )
    if (dir.exists(legacy_target_dir) && !dir.exists(destination_dir)) {
      if (!file.rename(legacy_target_dir, destination_dir)) {
        stop("Could not rename the staged geographic region for ", task_name, call. = FALSE)
      }
    }
    destination_names <- mapply(
      staged_name,
      selected_names,
      selected_dates,
      USE.NAMES = FALSE
    )
    if (anyDuplicated(destination_names)) {
      stop("Staged raster names are not unique for ", task_name, call. = FALSE)
    }
    destinations <- file.path(destination_dir, destination_names)
    Map(link_file, selected_sources, destinations)

    site_rasters[[j]] <- data.frame(
      watershed_key = watershed_key,
      task_name = task_name,
      driver = rule$driver[[1]],
      date = format(selected_dates, "%Y-%m-%d"),
      source_file = selected_sources,
      staged_file = destinations,
      stringsAsFactors = FALSE
    )
  }

  data_root <- tryCatch(resolve_silica_data_root(), error = function(...) "")
  snow_reference_source <- if (nzchar(data_root)) {
    file.path(
      data_root,
      "raw-driver-data", "raw-snow-v061", "snow_integer_codes.csv"
    )
  } else {
    ""
  }
  snow_reference_destination <- file.path(
    site_root, "raw-driver-data", "raw-snow-v061", "snow_integer_codes.csv"
  )
  if (file.exists(snow_reference_source)) {
    link_file(snow_reference_source, snow_reference_destination)
  } else if (!file.exists(snow_reference_destination)) {
    # MOD10A2 stores eight daily snow flags in one byte. Rebuilding this small
    # deterministic lookup keeps the isolated run portable when an older shared
    # copy is not retained.
    snow_reference <- data.frame(value = 0:255)
    for (day_index in 1:8) {
      snow_reference[[paste0("day_", day_index, "_snow_pres")]] <- as.integer(
        bitwAnd(snow_reference$value, bitwShiftL(1L, day_index - 1L)) > 0L
      )
    }
    snow_reference$snow_days <- rowSums(
      snow_reference[paste0("day_", 1:8, "_snow_pres")]
    )
    snow_reference <- snow_reference[
      c("value", "snow_days", paste0("day_", 1:8, "_snow_pres"))
    ]
    dir.create(dirname(snow_reference_destination), recursive = TRUE, showWarnings = FALSE)
    write.csv(snow_reference, snow_reference_destination, row.names = FALSE)
  }

  raster_manifest[[i]] <- bind_rows(site_rasters)
  site_manifest[[i]] <- data.frame(
    watershed_key = watershed_key,
    task_name = task_name,
    site_slug = site_slug,
    site_root = normalizePath(site_root, mustWork = TRUE),
    subset_file = normalizePath(subset_file, mustWork = TRUE),
    base_file = normalizePath(base_file, mustWork = TRUE),
    watershed_file = normalizePath(watershed_path, mustWork = TRUE),
    workflow_region = site_region,
    raw_driver_dir = normalizePath(file.path(site_root, "raw-driver-data"), mustWork = TRUE),
    output_dir = file.path(site_root, "extracted-data"),
    qa_dir = file.path(site_root, "qa"),
    stringsAsFactors = FALSE
  )
}

site_manifest <- bind_rows(site_manifest) %>% arrange(watershed_key)
raster_manifest <- bind_rows(raster_manifest) %>% arrange(watershed_key, driver, date)

write.csv(site_manifest, file.path(output_root, "run_manifest.csv"), row.names = FALSE, na = "")
write.csv(raster_manifest, file.path(output_root, "raster_manifest.csv"), row.names = FALSE, na = "")

driver_counts <- raster_manifest %>% count(watershed_key, driver, name = "raster_count")
if (nrow(driver_counts) != nrow(targets) * 4L) {
  stop("Prepared inputs do not contain all four products for every watershed.", call. = FALSE)
}
write.csv(driver_counts, file.path(output_root, "prepared_raster_counts.csv"), row.names = FALSE)

message(
  "Prepared isolated inputs for ", nrow(site_manifest), " accepted watersheds and ",
  nrow(raster_manifest), " product rasters."
)
