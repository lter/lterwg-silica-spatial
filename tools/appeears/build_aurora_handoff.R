# Build an Aurora handoff from an explicit site subset or completed MODIS QA
# AppEEARS and Aurora may run independently after their shared inputs pass QA

### Inputs

source(file.path("tools", "cli_helpers.R"))
source(file.path("tools", "identifier_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
reference_path <- require_input_file(
  cli_value(args, "--reference", required = TRUE),
  "final site-reference table"
)
qa_paths <- unique(cli_values(args, "--qa"))
status_paths <- unique(cli_values(args, "--status"))
selection_path <- cli_value(args, "--selection", "")
if (nzchar(selection_path)) {
  selection_path <- require_input_file(selection_path, "Aurora selection table")
}
uses_explicit_selection <- nzchar(selection_path)
uses_modis_qa <- length(qa_paths) > 0L || length(status_paths) > 0L
if (uses_explicit_selection == uses_modis_qa) {
  stop(
    "Choose one Aurora selection mode: --selection, or matching --qa and --status files.",
    call. = FALSE
  )
}
if (uses_modis_qa) {
  if (!length(qa_paths) || !length(status_paths)) {
    stop("MODIS selection requires both --qa and --status.", call. = FALSE)
  }
  qa_paths <- vapply(
    qa_paths,
    require_input_file,
    character(1),
    label = "MODIS QA file"
  )
  status_paths <- vapply(
    status_paths,
    require_input_file,
    character(1),
    label = "MODIS task table"
  )
}
shape_root <- require_input_dir(
  cli_value(args, "--shape-root", required = TRUE),
  "versioned watershed library"
)
output_root <- cli_value(args, "--output-root", required = TRUE)
prepare_output_dir(output_root)

### Reference table

read_reference <- function(path) {
  extension <- tolower(tools::file_ext(path))
  if (extension %in% c("tsv", "txt")) {
    data <- read.delim(
      path,
      sep = "\t",
      quote = "",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else if (extension == "csv") {
    data <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("The handoff reference table must be CSV or tab-separated text.", call. = FALSE)
  }

  unnamed <- is.na(names(data)) | !nzchar(trimws(names(data)))
  empty <- vapply(data, function(column) {
    all(is.na(column) | !nzchar(trimws(as.character(column))))
  }, logical(1))
  data[, !(unnamed & empty), drop = FALSE]
}

required_reference_columns <- c(
  "LTER", "Stream_Name", "GlASS_First_Release", "Spatial_Data_Version",
  "Has_Spatial_Data", "Shapefile_Name", "Discharge_File_Name"
)
reference <- read_reference(reference_path)
assert_required_columns(reference, required_reference_columns, "final site-reference table")

reference_text <- unlist(reference, use.names = FALSE)
google_drive_hits <- grepl(
  "drive[.]google|google drive|shapefiles google",
  reference_text,
  ignore.case = TRUE
)
if (any(google_drive_hits, na.rm = TRUE)) {
  stop("The final reference table still contains a Google Drive reference.", call. = FALSE)
}

reference$.handoff_key <- paste(
  normalize_lter_key(reference$LTER),
  normalize_site_key(reference$Shapefile_Name),
  sep = "__"
)
reference$.site_key <- paste(
  normalize_lter_key(reference$LTER),
  normalize_stream_key(reference$Stream_Name),
  sep = "__"
)
has_spatial <- trimws(reference$Has_Spatial_Data) == "Yes"

### Site selection

if (uses_explicit_selection) {
  selection <- read_reference(selection_path)
  assert_required_columns(selection, "LTER", "Aurora selection table")
  has_shape_key <- "Shapefile_Name" %in% names(selection) &&
    all(nzchar(trimws(selection$Shapefile_Name)))
  has_site_key <- "Stream_Name" %in% names(selection) &&
    all(nzchar(trimws(selection$Stream_Name)))
  if (!has_shape_key && !has_site_key) {
    stop(
      "The Aurora selection table needs nonblank LTER plus Shapefile_Name or Stream_Name.",
      call. = FALSE
    )
  }

  selection_key <- if (has_shape_key) {
    paste(
      normalize_lter_key(selection$LTER),
      normalize_site_key(selection$Shapefile_Name),
      sep = "__"
    )
  } else {
    paste(
      normalize_lter_key(selection$LTER),
      normalize_stream_key(selection$Stream_Name),
      sep = "__"
    )
  }
  reference_key <- if (has_shape_key) reference$.handoff_key else reference$.site_key
  selection_key <- unique(selection_key)
  selected <- reference[has_spatial & reference_key %in% selection_key, , drop = FALSE]
  missing_selection <- setdiff(
    selection_key,
    unique(reference_key[has_spatial])
  )
  if (length(missing_selection)) {
    stop(
      "Aurora selection rows are missing from the spatial reference table:\n- ",
      paste(missing_selection, collapse = "\n- "),
      call. = FALSE
    )
  }
  selection_mode <- if (has_shape_key) {
    "explicit shapefile subset"
  } else {
    "explicit site subset"
  }
} else {
  qa_outputs <- do.call(rbind, lapply(qa_paths, function(path) {
    qa <- readRDS(path)
    if (is.null(qa$outputs)) {
      stop("MODIS QA file lacks an outputs table: ", path, call. = FALSE)
    }
    assert_required_columns(
      qa$outputs,
      c("watershed_key", "status"),
      paste0("MODIS QA outputs in ", path)
    )
    qa$outputs
  }))

  qa_groups <- split(qa_outputs, qa_outputs$watershed_key)
  complete_keys <- names(qa_groups)[vapply(qa_groups, function(group) {
    nrow(group) > 0L && all(group$status == "complete")
  }, logical(1))]

  task_status <- do.call(rbind, lapply(status_paths, function(path) {
    status <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    assert_required_columns(
      status,
      c("watershed_key", "LTER", "Stream_Name"),
      paste0("MODIS task table ", path)
    )
    status[, c("watershed_key", "LTER", "Stream_Name"), drop = FALSE]
  }))
  completed_status <- task_status[
    task_status$watershed_key %in% complete_keys,
    c("watershed_key", "LTER", "Stream_Name"),
    drop = FALSE
  ]
  missing_status <- setdiff(complete_keys, unique(completed_status$watershed_key))
  if (length(missing_status)) {
    stop(
      "Completed MODIS watersheds are missing from the task tables:\n- ",
      paste(missing_status, collapse = "\n- "),
      call. = FALSE
    )
  }
  completed_status$.site_key <- paste(
    normalize_lter_key(completed_status$LTER),
    normalize_stream_key(completed_status$Stream_Name),
    sep = "__"
  )
  complete_site_keys <- unique(completed_status$.site_key)
  selected <- reference[
    has_spatial & reference$.site_key %in% complete_site_keys,
    ,
    drop = FALSE
  ]
  missing_sites <- setdiff(complete_site_keys, unique(selected$.site_key))
  if (length(missing_sites)) {
    stop(
      "Completed MODIS sites are missing from the final table:\n- ",
      paste(missing_sites, collapse = "\n- "),
      call. = FALSE
    )
  }
  selection_mode <- "completed MODIS QA"
}

if (!nrow(selected)) {
  stop("The selected Aurora roster is empty.", call. = FALSE)
}

### Bundle validation

selected$Spatial_Data_Version <- suppressWarnings(
  as.integer(selected$Spatial_Data_Version)
)
if (any(!selected$Spatial_Data_Version %in% 1:3)) {
  stop("Selected rows contain an invalid spatial-data version.", call. = FALSE)
}

geometry_rows <- selected[!duplicated(selected$.handoff_key), , drop = FALSE]
bundle_paths <- file.path(
  shape_root,
  paste0("data_release_", geometry_rows$Spatial_Data_Version),
  geometry_rows$Shapefile_Name
)
missing_bundles <- bundle_paths[!dir.exists(bundle_paths)]
if (length(missing_bundles)) {
  stop(
    "Selected watersheds are absent from the versioned library:\n- ",
    paste(missing_bundles, collapse = "\n- "),
    call. = FALSE
  )
}

shapefile_counts <- vapply(bundle_paths, function(path) {
  length(list.files(path, pattern = "[.]shp$", ignore.case = TRUE))
}, integer(1))
if (any(shapefile_counts != 1L)) {
  stop(
    "Each handoff bundle must contain exactly one .shp file:\n- ",
    paste(bundle_paths[shapefile_counts != 1L], collapse = "\n- "),
    call. = FALSE
  )
}

handoff_columns <- c(
  "LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name",
  "Spatial_Data_Version"
)
full_rows <- selected[selected$Spatial_Data_Version == 3L, handoff_columns, drop = FALSE]
update_rows <- selected[selected$Spatial_Data_Version < 3L, handoff_columns, drop = FALSE]
all_rows <- selected[, handoff_columns, drop = FALSE]

### Handoff outputs

write.csv(
  reference[, setdiff(names(reference), c(".handoff_key", ".site_key")), drop = FALSE],
  file.path(output_root, "site_reference_table.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  full_rows,
  file.path(output_root, "aurora_full_non_gee.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  update_rows,
  file.path(output_root, "aurora_climate_update_2023_2025.csv"),
  row.names = FALSE,
  na = ""
)
write.csv(
  all_rows,
  file.path(output_root, "aurora_all_selected.csv"),
  row.names = FALSE,
  na = ""
)

inventory <- list(
  built_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  selection_mode = selection_mode,
  selection_file = selection_path,
  qa_files = qa_paths,
  task_files = status_paths,
  selected_watershed_geometries = nrow(geometry_rows),
  selected_reference_rows = nrow(selected),
  full_non_gee_geometries = sum(geometry_rows$Spatial_Data_Version == 3L),
  climate_update_geometries = sum(geometry_rows$Spatial_Data_Version < 3L),
  bundle_paths = bundle_paths
)
saveRDS(inventory, file.path(output_root, "handoff_inventory.rds"))

cat(
  "Prepared", inventory$selected_watershed_geometries,
  "selected watershed geometries represented by",
  inventory$selected_reference_rows, "final-table rows.\n"
)
cat("Selection mode:", inventory$selection_mode, "\n")
cat(
  "Full non-GEE pass:", inventory$full_non_gee_geometries,
  if (inventory$full_non_gee_geometries == 1L) "watershed." else "watersheds.",
  "Climate-only 2023-2025 update:",
  inventory$climate_update_geometries,
  if (inventory$climate_update_geometries == 1L) "watershed.\n" else "watersheds.\n"
)
