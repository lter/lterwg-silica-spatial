suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
})

source(file.path(getwd(), "tools", "workflow_paths.R"))
source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

path <- resolve_silica_data_root()
site_coord_dir <- silica_site_coordinates_dir(path)
extracted_dir <- silica_extracted_data_dir(path)
dry_run <- tolower(Sys.getenv("SILICA_DRY_RUN", unset = "false")) %in%
  c("true", "t", "1", "yes", "y")

target_years <- silica_target_years()
if (!length(target_years)) {
  stop("update_years mode needs start_year/end_year or target_years in 01_run_config.R.", call. = FALSE)
}

subset_path <- Sys.getenv("SILICA_SITE_SUBSET_FILE", unset = "")

read_candidate_sites <- function() {
  ref <- readxl::read_excel(file.path(site_coord_dir, "silica-coords_RAW.xlsx")) %>%
    dplyr::transmute(
      LTER = trimws(as.character(LTER)),
      Stream_Name = trimws(as.character(Stream_Name)),
      Discharge_File_Name = if ("Discharge_File_Name" %in% names(.)) trimws(as.character(Discharge_File_Name)) else NA_character_,
      Shapefile_Name = if ("Shapefile_Name" %in% names(.)) trimws(as.character(Shapefile_Name)) else NA_character_
    ) %>%
    dplyr::distinct()

  if (!nzchar(subset_path)) {
    return(ref)
  }

  subset_targets <- read.csv(subset_path, stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      LTER = trimws(as.character(LTER)),
      Stream_Name = trimws(as.character(Stream_Name)),
      Discharge_File_Name = if ("Discharge_File_Name" %in% names(.)) trimws(as.character(Discharge_File_Name)) else NA_character_,
      Shapefile_Name = if ("Shapefile_Name" %in% names(.)) trimws(as.character(Shapefile_Name)) else NA_character_
    ) %>%
    dplyr::distinct()

  ref %>%
    dplyr::semi_join(
      subset_targets %>% dplyr::select(LTER, Stream_Name) %>% dplyr::distinct(),
      by = c("LTER", "Stream_Name")
    )
}

read_existing_site_keys <- function() {
  candidates <- c(
    list.files(extracted_dir, pattern = "^all-data_si-extract_.*\\.csv$", full.names = TRUE),
    list.files(extracted_dir, pattern = "^all-data_si-extract_.*recombinedLocal.*\\.csv$", full.names = TRUE),
    list.files(extracted_dir, pattern = "^all-data_si-extract_.*fromSiteRef.*\\.csv$", full.names = TRUE)
  )
  candidates <- unique(candidates[file.exists(candidates)])

  if (length(candidates)) {
    newest <- candidates[which.max(file.info(candidates)$mtime)]
    return(
      read.csv(newest, stringsAsFactors = FALSE, check.names = FALSE) %>%
        dplyr::transmute(
          LTER = trimws(as.character(LTER)),
          Stream_Name = trimws(as.character(Stream_Name)),
          Discharge_File_Name = trimws(as.character(Discharge_File_Name)),
          Shapefile_Name = trimws(as.character(Shapefile_Name))
        ) %>%
        dplyr::distinct()
    )
  }

  driver_candidates <- list.files(extracted_dir, pattern = "^si-extract_.*\\.csv$", full.names = TRUE)
  driver_candidates <- driver_candidates[file.exists(driver_candidates)]
  if (!length(driver_candidates)) {
    return(data.frame(
      LTER = character(0),
      Stream_Name = character(0),
      Discharge_File_Name = character(0),
      Shapefile_Name = character(0),
      stringsAsFactors = FALSE
    ))
  }

  all_rows <- lapply(driver_candidates, function(path_i) {
    d <- read.csv(path_i, stringsAsFactors = FALSE, check.names = FALSE)
    for (nm in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) {
      if (!nm %in% names(d)) d[[nm]] <- NA_character_
    }
    d %>%
      dplyr::transmute(
        LTER = trimws(as.character(LTER)),
        Stream_Name = trimws(as.character(Stream_Name)),
        Discharge_File_Name = trimws(as.character(Discharge_File_Name)),
        Shapefile_Name = trimws(as.character(Shapefile_Name))
      )
  })

  dplyr::bind_rows(all_rows) %>% dplyr::distinct()
}

write_subset_file <- function(df, stem) {
  if (dry_run) {
    out_dir <- file.path(silica_review_root(path), "debug")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_path <- file.path(
      out_dir,
      paste0(
        "internal_",
        stem, "_",
        Sys.getenv("SILICA_OUTPUT_DATE", unset = format(Sys.Date(), "%Y%m%d")),
        ".csv"
      )
    )
  } else {
    out_path <- tempfile(
      pattern = paste0("silica_", stem, "_"),
      fileext = ".csv"
    )
  }

  write.csv(df, out_path, row.names = FALSE, na = "")
  out_path
}

run_subset_pass <- function(subset_csv, run_static, run_dynamic, year_values = integer(0), pass_label = "") {
  old_subset <- Sys.getenv("SILICA_SITE_SUBSET_FILE", unset = "")
  old_merge <- Sys.getenv("SILICA_MERGE_OUTPUTS", unset = "")
  old_static <- Sys.getenv("SILICA_RUN_STATIC_DRIVERS", unset = "")
  old_dynamic <- Sys.getenv("SILICA_RUN_DYNAMIC_DRIVERS", unset = "")
  old_years <- Sys.getenv("SILICA_TARGET_YEARS", unset = "")
  old_skip_watershed <- Sys.getenv("SILICA_SKIP_WATERSHED_REBUILD", unset = "")

  on.exit({
    Sys.setenv(
      SILICA_SITE_SUBSET_FILE = old_subset,
      SILICA_MERGE_OUTPUTS = old_merge,
      SILICA_RUN_STATIC_DRIVERS = old_static,
      SILICA_RUN_DYNAMIC_DRIVERS = old_dynamic,
      SILICA_TARGET_YEARS = old_years,
      SILICA_SKIP_WATERSHED_REBUILD = old_skip_watershed
    )
  }, add = TRUE)

  Sys.setenv(
    SILICA_SITE_SUBSET_FILE = subset_csv,
    SILICA_MERGE_OUTPUTS = "TRUE",
    SILICA_RUN_STATIC_DRIVERS = ifelse(run_static, "TRUE", "FALSE"),
    SILICA_RUN_DYNAMIC_DRIVERS = ifelse(run_dynamic, "TRUE", "FALSE"),
    SILICA_SKIP_WATERSHED_REBUILD = ifelse(run_static, "FALSE", "TRUE")
  )

  if (length(year_values)) {
    Sys.setenv(SILICA_TARGET_YEARS = paste(sort(unique(year_values)), collapse = ","))
  } else {
    Sys.unsetenv("SILICA_TARGET_YEARS")
  }

  message("Update-years pass: ", pass_label)
  if (dry_run) {
    message("Dry run only. Wrote internal split file: ", subset_csv)
    message("  static drivers: ", ifelse(run_static, "on", "off"))
    message("  dynamic drivers: ", ifelse(run_dynamic, "on", "off"))
    if (length(year_values)) {
      message("  target years: ", paste(sort(unique(year_values)), collapse = ", "))
    } else {
      message("  target years: full available record")
    }
    return(invisible(0L))
  }

  status <- system2(
    "Rscript",
    c(
      file.path("03_spatial_extraction", "wrappers", "run-targeted-subset-workflow.R"),
      "--subset", subset_csv,
      "--combine-full", "false"
    )
  )

  if (!identical(status, 0L)) {
    stop("Update-years pass failed: ", pass_label, call. = FALSE)
  }
}

candidates <- read_candidate_sites()
existing_keys <- read_existing_site_keys()

new_sites <- dplyr::anti_join(
  candidates,
  existing_keys %>% dplyr::select(LTER, Stream_Name) %>% dplyr::distinct(),
  by = c("LTER", "Stream_Name")
)

existing_sites <- dplyr::semi_join(
  candidates,
  existing_keys %>% dplyr::select(LTER, Stream_Name) %>% dplyr::distinct(),
  by = c("LTER", "Stream_Name")
)

if (nrow(existing_sites) > 0) {
  existing_subset <- write_subset_file(existing_sites, "update_existing_sites")
  run_subset_pass(
    subset_csv = existing_subset,
    run_static = FALSE,
    run_dynamic = TRUE,
    year_values = target_years,
    pass_label = paste0("existing sites, years ", paste(target_years, collapse = ","))
  )
}

if (nrow(new_sites) > 0) {
  new_subset <- write_subset_file(new_sites, "update_new_sites")
  run_subset_pass(
    subset_csv = new_subset,
    run_static = TRUE,
    run_dynamic = TRUE,
    year_values = integer(0),
    pass_label = "new sites, full available record"
  )
}

if (nrow(existing_sites) == 0 && nrow(new_sites) == 0) {
  message("No sites matched the update_years request.")
} else if (dry_run) {
  message("Dry run complete. No extraction scripts were started.")
  message("These internal split files are only written in dry-run mode so you can inspect the site split.")
}
