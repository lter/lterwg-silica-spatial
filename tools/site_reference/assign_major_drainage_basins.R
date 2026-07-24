#!/usr/bin/env Rscript

# Assign a major drainage basin to each site-reference row.
#
# The reproducible default is a point-in-polygon lookup against GRDC Major
# River Basins. GRDC WMO Basins can be supplied as a fallback for small coastal
# systems. A version-controlled override table handles documented exceptions
# without embedding individual site names in this script.
#
# Required override columns:
#   LTER, Stream_Name, Major_Drainage_Basin, Assignment_Source
#
# Example:
#   Rscript tools/site_reference/assign_major_drainage_basins.R \
#     --input data/site_reference.tsv \
#     --major-basins data/grdc_major_basins.shp \
#     --wmo-basins data/grdc_wmo_basins.shp \
#     --overrides config/site_reference/major_basin_overrides.tsv \
#     --output outputs/major_drainage_basins.tsv \
#     --audit outputs/major_drainage_basin_audit.tsv \
#     --require-complete

suppressPackageStartupMessages(library(sf))

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else ""
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()
source(file.path(dirname(script_dir), "cli_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
input_path <- cli_value(args, "--input", required = TRUE)
major_path <- cli_value(args, "--major-basins", required = TRUE)
wmo_path <- cli_value(args, "--wmo-basins")
override_path <- cli_value(args, "--overrides")
output_path <- cli_value(
  args,
  "--output",
  file.path("outputs", "major_drainage_basins.tsv")
)
audit_path <- cli_value(
  args,
  "--audit",
  file.path("outputs", "major_drainage_basin_audit.tsv")
)
major_name_field <- cli_value(args, "--major-name-field", "RIVERBASIN")
wmo_name_field <- cli_value(args, "--wmo-name-field", "WMOBB_BASI")
require_complete <- cli_has_flag(args, "--require-complete")

required_paths <- c(input_path, major_path)
if (!is.null(wmo_path) && nzchar(wmo_path)) required_paths <- c(required_paths, wmo_path)
if (!is.null(override_path) && nzchar(override_path)) {
  required_paths <- c(required_paths, override_path)
}
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths)) {
  stop("Missing input(s):\n", paste(missing_paths, collapse = "\n"), call. = FALSE)
}

read_table <- function(path) {
  separator <- if (tolower(tools::file_ext(path)) %in% c("tsv", "txt")) "\t" else ","
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

rows <- read_table(input_path)
assert_required_columns(
  rows,
  c("LTER", "Stream_Name", "Latitude", "Longitude"),
  label = input_path
)
rows$.Roster_Row <- seq_len(nrow(rows))

latitude <- suppressWarnings(as.numeric(rows$Latitude))
longitude <- suppressWarnings(as.numeric(rows$Longitude))
coordinate_ok <- is.finite(latitude) & latitude >= -90 & latitude <= 90 &
  is.finite(longitude) & longitude >= -180 & longitude <= 180

points <- st_as_sf(
  data.frame(
    .Roster_Row = rows$.Roster_Row[coordinate_ok],
    Longitude = longitude[coordinate_ok],
    Latitude = latitude[coordinate_ok]
  ),
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

read_polygons <- function(path, name_field) {
  polygons <- st_make_valid(st_read(path, quiet = TRUE))
  if (!(name_field %in% names(polygons))) {
    stop(
      "Field ", name_field, " is absent from ", path, ". Available fields: ",
      paste(names(polygons), collapse = ", "),
      call. = FALSE
    )
  }
  st_transform(polygons, 4326)
}

lookup_polygon_name <- function(point_sf, polygon_sf, name_field) {
  hits <- suppressWarnings(st_intersects(point_sf, polygon_sf))
  values <- vapply(hits, function(index) {
    if (!length(index)) {
      return("")
    }
    candidates <- trimws(as.character(polygon_sf[[name_field]][index]))
    candidates <- candidates[nzchar(candidates) & !is.na(candidates)]
    if (!length(candidates)) "" else sort(candidates)[[1]]
  }, character(1))
  out <- rep("", nrow(rows))
  out[point_sf$.Roster_Row] <- values
  out
}

normalize_basin_name <- function(value) {
  value <- trimws(value)
  value <- sub("\\s*\\(also.*$", "", value, ignore.case = TRUE)
  title_case <- nzchar(value) & grepl("^[A-Z0-9 .,'/-]+$", value)
  value[title_case] <- tools::toTitleCase(tolower(value[title_case]))
  value
}

major <- read_polygons(major_path, major_name_field)
rows$Major_Drainage_Basin <- normalize_basin_name(
  lookup_polygon_name(points, major, major_name_field)
)
rows$Assignment_Source <- ifelse(
  nzchar(rows$Major_Drainage_Basin),
  "GRDC Major River Basins point-in-polygon assignment",
  ""
)

if (!is.null(wmo_path) && nzchar(wmo_path)) {
  wmo <- read_polygons(wmo_path, wmo_name_field)
  wmo_value <- normalize_basin_name(
    lookup_polygon_name(points, wmo, wmo_name_field)
  )
  use_fallback <- !nzchar(rows$Major_Drainage_Basin) & nzchar(wmo_value)
  rows$Major_Drainage_Basin[use_fallback] <- wmo_value[use_fallback]
  rows$Assignment_Source[use_fallback] <-
    "GRDC WMO Basins point-in-polygon fallback"
}

rows$Override_Applied <- "No"
if (!is.null(override_path) && nzchar(override_path)) {
  overrides <- read_table(override_path)
  assert_required_columns(
    overrides,
    c("LTER", "Stream_Name", "Major_Drainage_Basin", "Assignment_Source"),
    label = override_path
  )
  override_key <- paste(overrides$LTER, overrides$Stream_Name, sep = "\r")
  if (anyDuplicated(override_key)) {
    duplicates <- unique(override_key[duplicated(override_key)])
    stop(
      "Duplicate override key(s): ",
      paste(gsub("\r", " / ", duplicates), collapse = "; "),
      call. = FALSE
    )
  }
  row_key <- paste(rows$LTER, rows$Stream_Name, sep = "\r")
  match_index <- match(row_key, override_key)
  apply_override <- !is.na(match_index)
  rows$Major_Drainage_Basin[apply_override] <-
    overrides$Major_Drainage_Basin[match_index[apply_override]]
  rows$Assignment_Source[apply_override] <-
    overrides$Assignment_Source[match_index[apply_override]]
  rows$Override_Applied[apply_override] <- "Yes"

  unused <- !(override_key %in% row_key)
  if (any(unused)) {
    warning(
      "Unused override(s): ",
      paste(
        paste(overrides$LTER[unused], overrides$Stream_Name[unused], sep = " / "),
        collapse = "; "
      ),
      call. = FALSE
    )
  }
}

missing <- !nzchar(rows$Major_Drainage_Basin)
if (require_complete && any(missing)) {
  stop(
    "Unassigned site(s): ",
    paste(
      paste(rows$LTER[missing], rows$Stream_Name[missing], sep = " / "),
      collapse = "; "
    ),
    call. = FALSE
  )
}

output <- rows[, c("LTER", "Stream_Name", "Major_Drainage_Basin")]
audit_fields <- c(
  ".Roster_Row", "LTER", "Stream_Name", "Latitude", "Longitude",
  "Major_Drainage_Basin", "Assignment_Source", "Override_Applied"
)
audit <- rows[, audit_fields]

prepare_output_dir(output_path, is_file = TRUE)
prepare_output_dir(audit_path, is_file = TRUE)
write.table(
  output,
  output_path,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)
write.table(
  audit,
  audit_path,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

cat("Rows:", nrow(rows), "\n")
cat("Assigned:", sum(!missing), "\n")
cat("Overrides:", sum(rows$Override_Applied == "Yes"), "\n")
cat("Unassigned:", sum(missing), "\n")
cat("Output:", normalizePath(output_path, winslash = "/", mustWork = TRUE), "\n")
cat("Audit:", normalizePath(audit_path, winslash = "/", mustWork = TRUE), "\n")
