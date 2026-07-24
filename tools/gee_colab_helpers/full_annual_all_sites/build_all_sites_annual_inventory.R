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

is_true_flag <- function(x) {
  tolower(as.character(x)) %in% c("true", "1")
}

count_present_values <- function(data, columns) {
  if (!length(columns)) {
    return(rep(0L, nrow(data)))
  }
  rowSums(!is.na(data[columns]))
}

run_label <- get_arg("--run-label", "all_sites_fine_scale")
slug <- get_arg("--slug", sub("_fine_scale$", "", run_label))
start_year <- as.integer(get_arg("--start-year", "2000"))
end_year <- as.integer(get_arg("--end-year", "2025"))
input_dir <- get_arg("--input-dir", "")
search_dirs <- cli_values(args, "--search-dir")
output_dir <- get_arg(
  "--output-dir",
  file.path(
    "generated_outputs",
    paste0(slug, "_era5_land_inventory_", format(Sys.Date(), "%Y%m%d"))
  )
)
allow_missing_years <- parse_bool_arg("--allow-missing-years", FALSE)
write_parquet <- parse_bool_arg("--write-parquet", TRUE)

if (is.na(start_year) || is.na(end_year) || start_year > end_year) {
  stop("Expected --start-year and --end-year to define a valid year range.", call. = FALSE)
}

generated_output_dirs <- if (dir.exists("generated_outputs")) {
  list.dirs("generated_outputs", recursive = TRUE, full.names = TRUE)
} else {
  character(0)
}

era5_pattern <- paste0(
  "^era5_land_[0-9]{4}_",
  regex_escape(run_label),
  "_watershed_extract\\.csv$"
)

candidate_dirs <- first_existing_dir(c(
  input_dir,
  search_dirs,
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

if (!nrow(folder_summary)) {
  stop(
    "Could not find ERA5-Land exports for run label `",
    run_label,
    "`. Download the Colab CSVs first, pass --input-dir, or repeat ",
    "--search-dir for additional locations.",
    call. = FALSE
  )
}

export_folder <- folder_summary$folder[[1]]
export_files <- sort(files_by_dir[[match(export_folder, candidate_dirs)]])
export_years <- as.integer(sub("^era5_land_([0-9]{4})_.*$", "\\1", basename(export_files)))
expected_years <- seq.int(start_year, end_year)
missing_years <- setdiff(expected_years, export_years)

if (length(missing_years) && !allow_missing_years) {
  stop(
    "Missing expected export years for ",
    run_label,
    ": ",
    paste(missing_years, collapse = ", "),
    ". Use --allow-missing-years TRUE only for a partial inventory.",
    call. = FALSE
  )
}

message("Using ERA5-Land export folder: ", normalizePath(export_folder))
message("Found ERA5-Land export files: ", length(export_files))

product_dictionary <- tibble(
  variable = c(
    "precip",
    "temp",
    "evapotrans",
    "potential_evap",
    "snow_cover",
    "snow_water_equiv"
  ),
  source_column = c(
    "precip_mm",
    "temp_degC",
    "evapotrans_mm",
    "potential_evap_mm",
    "snow_cover_fraction",
    "snow_water_equiv_mm"
  ),
  variable_label = c(
    "Annual precipitation",
    "Annual mean air temperature",
    "Annual actual evapotranspiration",
    "Annual potential evapotranspiration",
    "Annual snow cover",
    "Annual snow-water equivalent"
  ),
  units = c("mm yr-1", "deg C", "mm yr-1", "mm yr-1", "fraction", "mm")
)

wide <- export_files %>%
  lapply(function(path) {
    read_csv(path, show_col_types = FALSE) %>%
      mutate(source_export_file = basename(path))
  }) %>%
  bind_rows()

if (!"used_centroid_fallback" %in% names(wide)) {
  wide$used_centroid_fallback <- NA
}
if (!"used_fine_scale_fallback" %in% names(wide)) {
  wide$used_fine_scale_fallback <- NA
}

present_value_columns <- intersect(product_dictionary$source_column, names(wide))
missing_value_columns <- setdiff(product_dictionary$source_column, names(wide))
if (length(missing_value_columns)) {
  warning(
    "These expected ERA5-Land value columns were not present and will be absent from the inventory: ",
    paste(missing_value_columns, collapse = ", "),
    call. = FALSE
  )
}

n_era5_variables_present <- count_present_values(wide, present_value_columns)

wide <- wide %>%
  mutate(
    inventory_run_label = run_label,
    inventory_built_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    used_centroid_fallback = if_else(
      is.na(used_centroid_fallback),
      NA_character_,
      as.character(used_centroid_fallback)
    ),
    used_fine_scale_fallback = if_else(
      is.na(used_fine_scale_fallback),
      NA_character_,
      as.character(used_fine_scale_fallback)
    ),
    era5_fallback_method = case_when(
      is_true_flag(used_fine_scale_fallback) ~ "Fine-scale polygon retry",
      is_true_flag(used_centroid_fallback) ~ "Centroid fill",
      TRUE ~ "Native-scale polygon mean"
    ),
    n_era5_variables_present = n_era5_variables_present
  ) %>%
  arrange(year, lter, site_id)

duplicate_site_years <- wide %>%
  count(site_id, year, name = "n_rows") %>%
  filter(n_rows > 1)

if (nrow(duplicate_site_years)) {
  stop(
    "The inventory has duplicate site-year rows. Inspect the raw exports before using it.",
    call. = FALSE
  )
}

id_columns <- intersect(
  c(
    "inventory_run_label",
    "inventory_built_at",
    "site_id",
    "lter",
    "shapefile_name",
    "stream_name",
    "Q_file_name",
    "run_group",
    "hydrosheds_used",
    "hydrosheds_id",
    "expected_area_km2",
    "drainage_area_source",
    "polygon_area_km2",
    "tiny_watershed",
    "source_type",
    "period",
    "year",
    "month",
    "used_fine_scale_fallback",
    "used_centroid_fallback",
    "era5_fallback_method",
    "source_export_file"
  ),
  names(wide)
)

long <- wide %>%
  select(all_of(id_columns), all_of(present_value_columns)) %>%
  pivot_longer(
    cols = all_of(present_value_columns),
    names_to = "source_column",
    values_to = "value"
  ) %>%
  left_join(product_dictionary, by = "source_column") %>%
  mutate(has_value = !is.na(value)) %>%
  select(
    all_of(id_columns),
    variable,
    variable_label,
    source_column,
    units,
    value,
    has_value
  ) %>%
  arrange(year, lter, site_id, variable)

site_year_coverage <- wide %>%
  transmute(
    site_id,
    lter,
    shapefile_name,
    stream_name,
    year,
    n_era5_variables_expected = length(present_value_columns),
    n_era5_variables_present,
    all_expected_variables_present = n_era5_variables_present == length(present_value_columns),
    era5_fallback_method,
    tiny_watershed,
    polygon_area_km2,
    source_export_file
  ) %>%
  arrange(year, lter, site_id)

variable_coverage <- long %>%
  group_by(variable, variable_label, units) %>%
  summarise(
    n_values_expected = n(),
    n_values_present = sum(has_value),
    n_sites = n_distinct(site_id),
    n_years = n_distinct(year),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    missing_values = sum(!has_value),
    .groups = "drop"
  ) %>%
  arrange(variable)

site_coverage <- site_year_coverage %>%
  group_by(site_id, lter, shapefile_name, stream_name) %>%
  summarise(
    n_years = n_distinct(year),
    first_year = min(year, na.rm = TRUE),
    last_year = max(year, na.rm = TRUE),
    n_site_years_with_all_expected_variables = sum(all_expected_variables_present),
    n_site_years_with_fine_scale_retry = sum(era5_fallback_method == "Fine-scale polygon retry", na.rm = TRUE),
    tiny_watershed = any(tiny_watershed %in% TRUE, na.rm = TRUE),
    min_polygon_area_km2 = suppressWarnings(min(polygon_area_km2, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    min_polygon_area_km2 = if_else(
      is.infinite(min_polygon_area_km2),
      NA_real_,
      min_polygon_area_km2
    )
  ) %>%
  arrange(lter, site_id)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

file_stem <- paste0(slug, "_era5land_annual_", start_year, "_", end_year)
wide_csv <- file.path(output_dir, paste0(file_stem, "_wide.csv"))
long_csv <- file.path(output_dir, paste0(file_stem, "_long.csv"))
site_year_coverage_csv <- file.path(output_dir, paste0(file_stem, "_site_year_coverage.csv"))
variable_coverage_csv <- file.path(output_dir, paste0(file_stem, "_variable_coverage.csv"))
site_coverage_csv <- file.path(output_dir, paste0(file_stem, "_site_coverage.csv"))
wide_rds <- file.path(output_dir, paste0(file_stem, "_wide.rds"))
long_rds <- file.path(output_dir, paste0(file_stem, "_long.rds"))

write_csv(wide, wide_csv, na = "")
write_csv(long, long_csv, na = "")
write_csv(site_year_coverage, site_year_coverage_csv, na = "")
write_csv(variable_coverage, variable_coverage_csv, na = "")
write_csv(site_coverage, site_coverage_csv, na = "")
saveRDS(wide, wide_rds)
saveRDS(long, long_rds)

if (write_parquet) {
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_parquet(wide, file.path(output_dir, paste0(file_stem, "_wide.parquet")))
    arrow::write_parquet(long, file.path(output_dir, paste0(file_stem, "_long.parquet")))
  } else {
    message("Skipped Parquet outputs because the arrow package is not installed.")
  }
}

message("Wrote wide inventory: ", normalizePath(wide_csv))
message("Wrote long query inventory: ", normalizePath(long_csv))
message("Wrote site-year coverage: ", normalizePath(site_year_coverage_csv))
message("Wrote variable coverage: ", normalizePath(variable_coverage_csv))
message("Wrote site coverage: ", normalizePath(site_coverage_csv))
message("Review folder: ", normalizePath(output_dir))
