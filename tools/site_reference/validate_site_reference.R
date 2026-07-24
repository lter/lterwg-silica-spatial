#!/usr/bin/env Rscript

# Validate a publication-ready GlASS site-reference table.
#
# This script does not modify the source table. It writes a compact issue log
# that identifies every problem by row, site, field, severity, and message.
#
# Example:
#   Rscript tools/site_reference/validate_site_reference.R \
#     --input data/site_reference.tsv \
#     --report outputs/site_reference_validation.tsv \
#     --require-major-basin

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else ""
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()
source(file.path(dirname(script_dir), "cli_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
input_path <- cli_value(args, "--input")
report_path <- cli_value(
  args,
  "--report",
  file.path("outputs", "site_reference_validation.tsv")
)
require_major_basin <- cli_has_flag(args, "--require-major-basin")
allow_errors <- cli_has_flag(args, "--allow-errors")

if (is.null(input_path) || !nzchar(input_path)) {
  stop("Provide --input PATH.", call. = FALSE)
}
if (!file.exists(input_path)) {
  stop("Input does not exist: ", input_path, call. = FALSE)
}

read_reference_table <- function(path) {
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

rows <- read_reference_table(input_path)
assert_required_columns(
  rows,
  c(
    "LTER", "Stream_Name", "GlASS_First_Release", "CQ_Data_Version",
    "Spatial_Data_Version", "Country", "Waterbody", "Latitude", "Longitude",
    "drainSqKm", "drainSqKm_source", "Use_WRTDS", "Has_Spatial_Data",
    "Shapefile_Name", "Shapefile_CRS_EPSG", "Shapefile_Source",
    "Shapefile_Link", "CQ_Notes", "Spatial_Notes"
  ),
  label = input_path
)
if (require_major_basin) {
  assert_required_columns(rows, "Major_Drainage_Basin", label = input_path)
}

issues <- list()
add_issue <- function(index, field, severity, message) {
  if (!length(index)) {
    return(invisible(NULL))
  }
  for (row_index in index) {
    issues[[length(issues) + 1L]] <<- data.frame(
      Roster_Row = row_index,
      LTER = rows$LTER[[row_index]],
      Stream_Name = rows$Stream_Name[[row_index]],
      Field = field,
      Severity = severity,
      Message = message,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }
  invisible(NULL)
}

blank <- function(value) !nzchar(trimws(value))
as_number <- function(value) suppressWarnings(as.numeric(value))
invalid_choice <- function(value, choices) {
  !blank(value) & !(tolower(value) %in% tolower(choices))
}

add_issue(which(blank(rows$LTER)), "LTER", "ERROR", "Network identifier is blank.")
add_issue(
  which(blank(rows$Stream_Name)),
  "Stream_Name",
  "ERROR",
  "Site or stream name is blank."
)

latitude <- as_number(rows$Latitude)
longitude <- as_number(rows$Longitude)
add_issue(
  which(blank(rows$Latitude) | !is.finite(latitude) | latitude < -90 | latitude > 90),
  "Latitude",
  "ERROR",
  "Latitude must be numeric and within -90 to 90."
)
add_issue(
  which(blank(rows$Longitude) | !is.finite(longitude) | longitude < -180 | longitude > 180),
  "Longitude",
  "ERROR",
  "Longitude must be numeric and within -180 to 180."
)

area <- as_number(rows$drainSqKm)
add_issue(
  which(!blank(rows$drainSqKm) & (!is.finite(area) | area <= 0)),
  "drainSqKm",
  "ERROR",
  "Drainage area must be a positive number or blank."
)
add_issue(
  which(!blank(rows$drainSqKm) & blank(rows$drainSqKm_source)),
  "drainSqKm_source",
  "ERROR",
  "A drainage-area value requires a source."
)
add_issue(
  which(blank(rows$drainSqKm) & !blank(rows$drainSqKm_source)),
  "drainSqKm_source",
  "WARNING",
  "A source is present but the drainage area is blank."
)

for (field in c("Use_WRTDS", "Has_Spatial_Data")) {
  add_issue(
    which(invalid_choice(rows[[field]], c("Yes", "No")) | blank(rows[[field]])),
    field,
    "ERROR",
    "Allowed values are Yes and No."
  )
}

version_fields <- c(
  "GlASS_First_Release",
  "CQ_Data_Version",
  "Spatial_Data_Version"
)
versions <- lapply(rows[version_fields], as_number)
names(versions) <- version_fields
for (field in version_fields[1:2]) {
  add_issue(
    which(blank(rows[[field]]) | !(versions[[field]] %in% 1:3)),
    field,
    "ERROR",
    "Version must be 1, 2, or 3."
  )
}
add_issue(
  which(!blank(rows$Spatial_Data_Version) &
    !(versions$Spatial_Data_Version %in% 1:3)),
  "Spatial_Data_Version",
  "ERROR",
  "Spatial data version must be 1, 2, 3, or blank."
)
add_issue(
  which(
    is.finite(versions$GlASS_First_Release) &
      is.finite(versions$CQ_Data_Version) &
      versions$CQ_Data_Version < versions$GlASS_First_Release
  ),
  "CQ_Data_Version",
  "ERROR",
  "CQ data cannot predate the site's first GlASS release."
)
add_issue(
  which(
    is.finite(versions$GlASS_First_Release) &
      is.finite(versions$Spatial_Data_Version) &
      versions$Spatial_Data_Version < versions$GlASS_First_Release
  ),
  "Spatial_Data_Version",
  "ERROR",
  "Spatial data cannot predate the site's first GlASS release."
)

has_spatial <- tolower(rows$Has_Spatial_Data) == "yes"
spatial_fields <- c(
  "Spatial_Data_Version", "Shapefile_Name", "Shapefile_CRS_EPSG",
  "Shapefile_Source"
)
for (field in spatial_fields) {
  add_issue(
    which(has_spatial & blank(rows[[field]])),
    field,
    "ERROR",
    paste0("Accepted spatial rows require ", field, ".")
  )
}
for (field in c(
  "Spatial_Data_Version", "Shapefile_Name", "Shapefile_CRS_EPSG",
  "Shapefile_Source", "Shapefile_Link"
)) {
  add_issue(
    which(!has_spatial & !blank(rows[[field]])),
    field,
    "ERROR",
    paste0("Rows without accepted spatial data must leave ", field, " blank.")
  )
}

epsg <- as_number(rows$Shapefile_CRS_EPSG)
add_issue(
  which(
    !blank(rows$Shapefile_CRS_EPSG) &
      (!is.finite(epsg) | epsg <= 0 | epsg != floor(epsg))
  ),
  "Shapefile_CRS_EPSG",
  "ERROR",
  "CRS must be a positive integer EPSG code."
)

for (field in c("drainSqKm_source", "Shapefile_Link")) {
  value <- rows[[field]]
  has_url <- grepl("https?://", value, ignore.case = TRUE)
  malformed_url <- has_url & grepl("\\s", sub(".*https?://", "", value))
  add_issue(
    which(malformed_url),
    field,
    "WARNING",
    "The source contains a URL with whitespace; verify that the link is valid."
  )
}

if ("USGSGageNumber" %in% names(rows)) {
  add_issue(
    which(grepl("[Ee][+-][0-9]+", rows$USGSGageNumber)),
    "USGSGageNumber",
    "ERROR",
    "Gauge identifiers must be stored as text, not scientific notation."
  )
}

legacy_fields <- intersect(
  c("Spatial_Data_Release", "River_Network_Context", "river_network_context"),
  names(rows)
)
for (field in legacy_fields) {
  add_issue(
    1L,
    field,
    "ERROR",
    paste0(
      "Remove deprecated column ", field,
      " from the publication table (the issue applies to the whole column)."
    )
  )
}

text_fields <- names(rows)[vapply(rows, is.character, logical(1))]
for (field in text_fields) {
  value <- rows[[field]]
  add_issue(
    which(grepl("project-provided", value, ignore.case = TRUE)),
    field,
    "ERROR",
    "Replace vague 'project-provided' wording with the documented PI, agency, repository, or an explicit statement that the originator is unknown."
  )
  add_issue(
    which(grepl("\uFFFD|√|¬", value)),
    field,
    "ERROR",
    "Text contains a likely character-encoding error."
  )
}

site_key <- paste(rows$LTER, rows$Stream_Name, sep = " / ")
duplicate_key <- duplicated(site_key) | duplicated(site_key, fromLast = TRUE)
add_issue(
  which(duplicate_key),
  "LTER + Stream_Name",
  "WARNING",
  "Duplicate site key: confirm that this is an intentional provenance alias."
)

if (require_major_basin) {
  add_issue(
    which(blank(rows$Major_Drainage_Basin)),
    "Major_Drainage_Basin",
    "ERROR",
    "Major drainage basin is required for publication."
  )
}

if (length(issues)) {
  report <- do.call(rbind, issues)
  severity_order <- match(report$Severity, c("ERROR", "WARNING"))
  report <- report[order(severity_order, report$Roster_Row, report$Field), ]
} else {
  report <- data.frame(
    Roster_Row = integer(),
    LTER = character(),
    Stream_Name = character(),
    Field = character(),
    Severity = character(),
    Message = character(),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

prepare_output_dir(report_path, is_file = TRUE)
write.table(
  report,
  report_path,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

error_count <- sum(report$Severity == "ERROR")
warning_count <- sum(report$Severity == "WARNING")
cat("Rows checked:", nrow(rows), "\n")
cat("Errors:", error_count, "\n")
cat("Warnings:", warning_count, "\n")
cat("Report:", normalizePath(report_path, winslash = "/", mustWork = TRUE), "\n")

if (error_count > 0L && !allow_errors) {
  quit(status = 1L, save = "no")
}
