#!/usr/bin/env Rscript

# Validate and install a frozen GlASS site-reference snapshot.
#
# The living table keeps exactly three version fields:
#   GlASS_First_Release, CQ_Data_Version, and Spatial_Data_Version.
# Spatial_Data_Version determines which canonical watershed library is used.
#
# Example:
#   Rscript tools/install_release_reference_table.R \
#     --input exports/site_reference.csv \
#     --release 3 \
#     --output /path/to/published_glass_versions/glass_v3/site_reference_table.csv
#
# If --output is omitted, set SILICA_DATA_ROOT and the script writes to the
# canonical release-reference directory. Existing files are protected unless
# --overwrite is supplied.

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else ""
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()
source(file.path(script_dir, "cli_helpers.R"))
source(file.path(script_dir, "workflow_paths.R"))

args <- commandArgs(trailingOnly = TRUE)
input_path <- cli_value(args, "--input", required = TRUE)
release <- cli_integer(
  args,
  "--release",
  required = TRUE,
  minimum = 1L,
  maximum = 3L
)
output_path <- cli_value(args, "--output")
overwrite <- cli_has_flag(args, "--overwrite")

input_path <- require_input_file(input_path, "site-reference table")
if (is.null(output_path)) {
  data_root <- resolve_silica_data_root()
  output_path <- file.path(
    silica_shape_library_root(data_root),
    "reference_tables",
    "published_glass_versions",
    paste0("glass_v", release),
    "site_reference_table.csv"
  )
}
if (file.exists(output_path) && !overwrite) {
  stop(
    "Output already exists: ", output_path,
    "\nReview it, then rerun with --overwrite if replacement is intentional.",
    call. = FALSE
  )
}

read_reference <- function(path) {
  extension <- tolower(tools::file_ext(path))
  separator <- if (extension %in% c("tsv", "txt")) "\t" else ","
  out <- read.delim(
    path,
    header = TRUE,
    sep = separator,
    quote = "\"",
    fill = TRUE,
    comment.char = "",
    colClasses = "character",
    check.names = FALSE,
    na.strings = character(),
    fileEncoding = "UTF-8"
  )
  out[] <- lapply(out, function(value) {
    value[is.na(value)] <- ""
    trimws(value)
  })
  out
}

rows <- read_reference(input_path)
required_columns <- c(
  "LTER", "Stream_Name", "GlASS_First_Release", "CQ_Data_Version",
  "Spatial_Data_Version", "Latitude", "Longitude", "drainSqKm",
  "drainSqKm_source", "Has_Spatial_Data", "Shapefile_Name",
  "Shapefile_CRS_EPSG", "Shapefile_Source"
)
assert_required_columns(rows, required_columns, label = input_path)

deprecated <- intersect(
  c("Spatial_Data_Release", "River_Network_Context"),
  names(rows)
)
if (length(deprecated)) {
  stop(
    "Remove deprecated publication column(s): ",
    paste(deprecated, collapse = ", "),
    call. = FALSE
  )
}

first_release <- suppressWarnings(as.integer(rows$GlASS_First_Release))
cq_version <- suppressWarnings(as.integer(rows$CQ_Data_Version))
spatial_version <- suppressWarnings(as.integer(rows$Spatial_Data_Version))
has_spatial <- tolower(rows$Has_Spatial_Data) == "yes"

invalid_first <- is.na(first_release) | !(first_release %in% 1:3)
invalid_cq <- is.na(cq_version) | !(cq_version %in% 1:3)
invalid_spatial <- has_spatial &
  (is.na(spatial_version) | !(spatial_version %in% 1:3))
unexpected_spatial <- !has_spatial & nzchar(rows$Spatial_Data_Version)
future_rows <- first_release > release
future_data <- cq_version > release | (!is.na(spatial_version) & spatial_version > release)

problems <- c(
  if (any(invalid_first)) {
    paste0("invalid GlASS_First_Release rows: ", sum(invalid_first))
  },
  if (any(invalid_cq)) {
    paste0("invalid CQ_Data_Version rows: ", sum(invalid_cq))
  },
  if (any(invalid_spatial)) {
    paste0("accepted spatial rows with invalid Spatial_Data_Version: ", sum(invalid_spatial))
  },
  if (any(unexpected_spatial)) {
    paste0("no-spatial rows retaining Spatial_Data_Version: ", sum(unexpected_spatial))
  },
  if (any(future_rows, na.rm = TRUE)) {
    paste0("sites first published after release ", release, ": ", sum(future_rows, na.rm = TRUE))
  },
  if (any(future_data, na.rm = TRUE)) {
    paste0("rows using data newer than release ", release, ": ", sum(future_data, na.rm = TRUE))
  }
)
if (length(problems)) {
  stop(
    "Snapshot validation failed:\n- ",
    paste(problems, collapse = "\n- "),
    call. = FALSE
  )
}

required_spatial_fields <- c(
  "Shapefile_Name", "Shapefile_CRS_EPSG", "Shapefile_Source"
)
for (field in required_spatial_fields) {
  missing <- has_spatial & !nzchar(rows[[field]])
  if (any(missing)) {
    stop(
      "Accepted spatial rows missing ", field, ": ", sum(missing),
      call. = FALSE
    )
  }
}

site_key <- paste(rows$LTER, rows$Stream_Name, sep = " / ")
duplicate_rows <- duplicated(site_key) | duplicated(site_key, fromLast = TRUE)
if (any(duplicate_rows)) {
  note_column <- function(field) {
    if (field %in% names(rows)) rows[[field]] else rep("", nrow(rows))
  }
  notes <- paste(
    note_column("CQ_Notes")[duplicate_rows],
    note_column("Spatial_Notes")[duplicate_rows]
  )
  documented <- grepl(
    "alias|duplicate|same physical|provenance",
    notes,
    ignore.case = TRUE
  )
  if (!all(documented)) {
    stop(
      "Duplicate site keys require an alias/provenance note:\n- ",
      paste(unique(site_key[duplicate_rows][!documented]), collapse = "\n- "),
      call. = FALSE
    )
  }
}

sort_index <- order(
  tolower(rows$LTER),
  tolower(rows$Stream_Name),
  seq_len(nrow(rows))
)
rows <- rows[sort_index, , drop = FALSE]

prepare_output_dir(output_path, is_file = TRUE)
temporary_path <- tempfile(
  pattern = "site_reference_",
  tmpdir = dirname(output_path),
  fileext = ".csv"
)
on.exit(unlink(temporary_path), add = TRUE)
write.csv(
  rows,
  temporary_path,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

installed <- read_reference(temporary_path)
if (nrow(installed) != nrow(rows) || !identical(names(installed), names(rows))) {
  stop("Round-trip validation failed; the output was not installed.", call. = FALSE)
}
if (file.exists(output_path) && !file.remove(output_path)) {
  stop("Could not replace existing output: ", output_path, call. = FALSE)
}
if (!file.rename(temporary_path, output_path)) {
  stop("Could not atomically install ", output_path, ".", call. = FALSE)
}

cat("Installed release:", release, "\n")
cat("Rows:", nrow(rows), "\n")
cat("Spatial rows:", sum(has_spatial), "\n")
cat("Output:", normalizePath(output_path, winslash = "/", mustWork = TRUE), "\n")
