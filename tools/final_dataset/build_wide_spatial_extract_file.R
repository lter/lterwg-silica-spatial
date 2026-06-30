options(stringsAsFactors = FALSE)

box_root <- Sys.getenv(
  "SILICA_BOX_ROOT",
  unset = "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn"
)
date_tag <- Sys.getenv("SILICA_WIDE_EXTRACT_DATE", unset = "20260629")

data_root <- file.path(box_root, "spatial-data-extractions")
spatial_file_dir <- file.path(data_root, "spatial-data-files")
appeears_nasa_file_dir <- file.path(spatial_file_dir, "appeears-nasa")
annual_with_wrtds_dir <- file.path(appeears_nasa_file_dir, "annual-with-wrtds")

reference_path <- Sys.getenv(
  "SILICA_WIDE_REFERENCE_FILE",
  unset = file.path(appeears_nasa_file_dir, "all-data_si-extract_2_20250325.csv")
)
annual_path <- Sys.getenv(
  "SILICA_FINAL_ANNUAL_FILE",
  unset = file.path(annual_with_wrtds_dir, paste0("final_annual_dataset_", date_tag, ".csv"))
)
output_path <- Sys.getenv(
  "SILICA_WIDE_OUTPUT_FILE",
  unset = file.path(appeears_nasa_file_dir, paste0("all-data_si-extract_3_", date_tag, ".csv"))
)
google_drive_export_dir <- Sys.getenv("SILICA_GOOGLE_DRIVE_EXPORT_DIR", unset = "")

source(file.path("tools", "name_keys.R"))

read_csv_clean <- function(path) {
  read.csv(
    path,
    check.names = FALSE,
    stringsAsFactors = FALSE,
    na.strings = c("", "NA", "NaN")
  )
}

is_present <- function(x) {
  if (length(x) == 0) {
    return(FALSE)
  }
  !(is.na(x) | trimws(as.character(x)) == "")
}

first_value <- function(x) {
  present <- is_present(x)
  if (!any(present)) {
    return(NA)
  }
  x[present][1]
}

format_value <- function(x) {
  if (length(x) == 0 || is.na(x) || trimws(as.character(x)) == "") {
    return(NA)
  }
  as.character(x)
}

doy_to_date <- function(year, doy) {
  day <- suppressWarnings(as.integer(round(as.numeric(doy))))
  if (is.na(day) || day < 1 || day > 366) {
    return(NA)
  }
  format(as.Date(paste0(year, "-01-01")) + day - 1, "%Y-%m-%d")
}

site_key_from_row <- function(lter, stream_name) {
  site_key_from_parts(lter, stream_name)
}

site_key_from_final <- function(stream_id) {
  site_key_from_stream_id(stream_id)
}

has_spatial_data <- function(dat) {
  check_cols <- intersect(
    c(
      "precip", "temp", "snow_cover", "snow_num_days", "npp", "evapotrans",
      "greenup_day", "permafrost", "elevation", "RBI", "RCS", "basin_slope",
      "major_rock", "major_land",
      grep("^(rocks_|land_)", names(dat), value = TRUE)
    ),
    names(dat)
  )

  if (length(check_cols) == 0) {
    return(rep(FALSE, nrow(dat)))
  }

  rowSums(as.data.frame(lapply(dat[check_cols], is_present))) > 0
}

set_if_present <- function(out, row_id, col, value) {
  if (col %in% names(out) && is_present(value)) {
    out[row_id, col] <- format_value(value)
  }
  out
}

set_year_value <- function(out, row_id, prefix, year, suffix, value) {
  set_if_present(out, row_id, paste0(prefix, "_", year, suffix), value)
}

get_site_rows <- function(final_data, site_key) {
  final_data[final_data$.site_key == site_key, , drop = FALSE]
}

get_site_static <- function(site_rows, col) {
  if (!col %in% names(site_rows)) {
    return(NA)
  }
  first_value(site_rows[[col]])
}

get_site_mean <- function(site_rows, col) {
  if (!col %in% names(site_rows)) {
    return(NA)
  }
  values <- suppressWarnings(as.numeric(site_rows[[col]]))
  if (all(is.na(values))) {
    return(NA)
  }
  mean(values, na.rm = TRUE)
}

get_site_latest <- function(site_rows, col) {
  if (!col %in% names(site_rows)) {
    return(NA)
  }
  ordered <- site_rows[order(suppressWarnings(as.integer(site_rows$Year)), decreasing = TRUE), , drop = FALSE]
  first_value(ordered[[col]])
}

sum_site_static <- function(site_rows, cols) {
  present_cols <- intersect(cols, names(site_rows))
  if (length(present_cols) == 0) {
    return(NA)
  }
  values <- vapply(present_cols, function(col) {
    suppressWarnings(as.numeric(get_site_static(site_rows, col)))
  }, numeric(1))
  if (all(is.na(values))) {
    return(NA)
  }
  sum(values, na.rm = TRUE)
}

message("Reading reference layout: ", reference_path)
reference <- read_csv_clean(reference_path)

message("Reading final annual data: ", annual_path)
final_annual <- read_csv_clean(annual_path)

if (!"Stream_ID" %in% names(final_annual)) {
  stop("The final annual file must include Stream_ID")
}
if (!"Year" %in% names(final_annual)) {
  stop("The final annual file must include Year")
}

reference$.site_key <- site_key_from_row(reference$LTER, reference$Stream_Name)
reference <- reference[!duplicated(reference$.site_key), , drop = FALSE]

final_annual <- final_annual[has_spatial_data(final_annual), , drop = FALSE]
final_annual$.site_key <- site_key_from_final(final_annual$Stream_ID)
final_annual <- final_annual[is_present(final_annual$.site_key), , drop = FALSE]

site_first <- final_annual[!duplicated(final_annual$.site_key), c(".site_key", "Stream_ID"), drop = FALSE]
current_keys <- site_first$.site_key
reference_keys <- reference$.site_key[reference$.site_key %in% current_keys]
new_keys <- setdiff(current_keys, reference_keys)
new_keys <- site_first$.site_key[site_first$.site_key %in% new_keys][order(site_first$Stream_ID[site_first$.site_key %in% new_keys])]
site_keys <- c(reference_keys, new_keys)

reference_columns <- setdiff(names(reference), ".site_key")
current_extra_columns <- intersect(
  c(
    "Stream_ID", "hydrosheds_used", "drainage_area", "drainage_area_source",
    "RBI", "RCS",
    grep("^land_", names(final_annual), value = TRUE)
  ),
  names(final_annual)
)
current_extra_columns <- setdiff(current_extra_columns, reference_columns)
output_columns <- c(reference_columns, current_extra_columns)

out <- data.frame(
  matrix(NA_character_, nrow = length(site_keys), ncol = length(output_columns)),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
names(out) <- output_columns

static_map <- c(
  elevation_mean_m = "elevation",
  basin_slope_mean_degree = "basin_slope",
  permafrost_mean_m = "permafrost",
  major_rock = "major_rock",
  major_land = "major_land",
  rocks_volcanic = "rocks_volcanic",
  rocks_plutonic = "rocks_plutonic",
  rocks_siliclastic_sedimentary = "rocks_siliclastic_sedimentary",
  rocks_mixed_sedimentary = "rocks_mixed_sedimentary",
  rocks_carbonate_sedimentary = "rocks_carbonate_sedimentary",
  rocks_pyroclastics = "rocks_pyroclastics",
  rocks_evaporites = "rocks_evaporites",
  rocks_metamorphics = "rocks_metamorphics",
  rocks_no_data = "rocks_no_data",
  land_cropland = "land_Cropland",
  land_urban_and_built_up_land = "land_Impervious",
  land_barren_or_sparsely_vegetated = "land_Bare",
  land_shrubland_grassland = "land_Grassland_Shrubland"
)

month_keys <- c(
  jan = "jan", feb = "feb", mar = "mar", apr = "apr",
  may = "may", jun = "jun", jul = "jul", aug = "aug",
  sep = "sep", oct = "oct", nov = "nov", dec = "dec"
)

for (row_id in seq_along(site_keys)) {
  site_key <- site_keys[row_id]
  site_rows <- get_site_rows(final_annual, site_key)
  reference_row <- reference[reference$.site_key == site_key, reference_columns, drop = FALSE]

  if (nrow(reference_row) > 0) {
    out[row_id, reference_columns] <- lapply(reference_row[1, , drop = FALSE], as.character)
  } else {
    id_parts <- split_stream_id(get_site_static(site_rows, "Stream_ID"))
    out <- set_if_present(out, row_id, "LTER", id_parts$LTER)
    out <- set_if_present(out, row_id, "Stream_Name", id_parts$Stream_Name)
    out <- set_if_present(out, row_id, "Shapefile_Name", id_parts$Stream_Name)
    out <- set_if_present(out, row_id, "Discharge_File_Name", NA)
  }

  ordered_rows <- site_rows[order(suppressWarnings(as.integer(site_rows$Year))), , drop = FALSE]
  for (year_row_id in seq_len(nrow(ordered_rows))) {
    year_row <- ordered_rows[year_row_id, , drop = FALSE]
    year <- suppressWarnings(as.integer(year_row$Year))
    if (is.na(year)) {
      next
    }

    out <- set_year_value(out, row_id, "snow", year, "_num_days", year_row$snow_num_days)
    out <- set_year_value(out, row_id, "snow", year, "_max_prop_area", year_row$snow_cover)
    out <- set_year_value(out, row_id, "precip", year, "_mm_per_day", year_row$precip)
    out <- set_year_value(out, row_id, "temp", year, "_degC", year_row$temp)
    out <- set_year_value(out, row_id, "evapotrans", year, "_kg_m2", year_row$evapotrans)
    out <- set_year_value(out, row_id, "npp", year, "_kgC_m2_year", year_row$npp)
    out <- set_if_present(out, row_id, paste0("greenup_cycle0_", year, "MMDD"), doy_to_date(year, year_row$greenup_day))
  }

  for (month_label in names(month_keys)) {
    month_key <- month_keys[[month_label]]
    out <- set_if_present(
      out,
      row_id,
      paste0("snow_", month_label, "_num_days"),
      get_site_mean(site_rows, paste0("snow_", month_key, "_num_days"))
    )
    out <- set_if_present(
      out,
      row_id,
      paste0("snow_", month_label, "_avg_prop_area"),
      get_site_mean(site_rows, paste0("snow_", month_key, "_avg_prop_area"))
    )
  }

  for (target_col in names(static_map)) {
    out <- set_if_present(out, row_id, target_col, get_site_static(site_rows, static_map[[target_col]]))
  }

  out <- set_if_present(
    out,
    row_id,
    "land_wetland",
    sum_site_static(site_rows, c("land_Tidal_Wetland", "land_Wetland_Marsh"))
  )

  for (extra_col in current_extra_columns) {
    out <- set_if_present(out, row_id, extra_col, get_site_latest(site_rows, extra_col))
  }
}

row_key <- paste(
  norm_key(out$LTER),
  norm_key(out$Shapefile_Name),
  norm_key(out$Discharge_File_Name),
  sep = "||"
)
site_number_name <- grepl("^Site [0-9]+$", trimws(out$Stream_Name))
has_descriptive_match <- row_key %in% row_key[!site_number_name]
drop_site_number_duplicate <- site_number_name & has_descriptive_match

if (any(drop_site_number_duplicate)) {
  message("Dropping duplicate site-number labels: ", sum(drop_site_number_duplicate))
  out <- out[!drop_site_number_duplicate, , drop = FALSE]
}

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write.csv(out, output_path, row.names = FALSE, na = "")

message("Wrote: ", output_path)

if (nzchar(google_drive_export_dir)) {
  dir.create(google_drive_export_dir, recursive = TRUE, showWarnings = FALSE)
  google_drive_path <- file.path(google_drive_export_dir, basename(output_path))
  file.copy(output_path, google_drive_path, overwrite = TRUE)
  message("Copied to Google Drive folder: ", google_drive_path)
}

message("Rows: ", nrow(out))
message("Columns: ", ncol(out))
message("New sites in current file: ", length(new_keys))
