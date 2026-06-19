librarian::shelf(dplyr)

source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

env_or_default <- function(env_name, default_value) {
  value <- trimws(Sys.getenv(env_name, unset = ""))
  if (nzchar(value)) value else default_value
}

read_loose_csv <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- make.unique(ifelse(is.na(names(x)) | trimws(names(x)) == "", "blank_col", names(x)))
  x
}

norm_blank <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

infer_region_for_targets <- function(x) {
  stream_region_overrides <- data.frame(
    .LTER_KEY = c("lmp", "krr"),
    .STREAM_KEY = c("nor27", "s65c"),
    Region = c("north-america-usa", "north-america-usa"),
    stringsAsFactors = FALSE
  )

  region_map <- data.frame(
    .LTER_KEY = c(
      "amazon", "and", "arc", "australia", "bcczo", "canada",
      "catalina jemez", "coloradoalpine", "congo-basin", "east riversfa",
      "finnish environmental institute", "germany", "gro", "guadeloupe",
      "hbr", "hybam", "knz", "krr", "krycklan", "lmp", "luq", "mali",
      "md", "niva", "nwt", "pie", "sagehen", "seine", "sweden", "uk",
      "umr", "usgs", "walker branch", "westernaustralia"
    ),
    Region = c(
      "amazon", "north-america-usa", "north-america-arctic", "australia",
      "north-america-usa", "canada", "north-america-usa",
      "north-america-usa", "congo", "north-america-usa", "scandinavia",
      "germany", "amazon", "puerto-rico", "north-america-usa", "amazon",
      "north-america-usa", "north-america-usa", "scandinavia",
      "north-america-usa", "puerto-rico", "mali", "australia",
      "scandinavia", "north-america-usa", "north-america-usa",
      "north-america-usa", "germany", "scandinavia", "united-kingdom",
      "north-america-usa", "north-america-usa", "north-america-usa",
      "australia"
    ),
    stringsAsFactors = FALSE
  )

  x %>%
    mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_stream_key(Stream_Name)
    ) %>%
    left_join(stream_region_overrides, by = c(".LTER_KEY", ".STREAM_KEY")) %>%
    left_join(region_map, by = ".LTER_KEY") %>%
    mutate(Region = coalesce(Region.x, Region.y)) %>%
    select(-any_of(c("Region.x", "Region.y")))
}

site_key <- function(lter, shp) {
  paste(normalize_lter_key(lter), normalize_site_key(shp), sep = "||")
}

missing_years_for_prefix <- function(df, prefix, suffix, years) {
  out <- vector("list", length(years))
  for (i in seq_along(years)) {
    year <- years[[i]]
    col <- paste0(prefix, year, suffix)
    if (!col %in% names(df)) {
      out[[i]] <- rep(TRUE, nrow(df))
    } else {
      val <- df[[col]]
      out[[i]] <- is.na(val) | trimws(as.character(val)) == ""
    }
  }
  names(out) <- as.character(years)
  out
}

date_tag <- env_or_default("SILICA_YEAR_FILL_DATE", format(Sys.Date(), "%Y%m%d"))
combined_file <- env_or_default(
  "SILICA_YEAR_FILL_COMBINED_FILE",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/final-data/full-dataset/all-data_si-extract_4_20260523_final-extract-merge-v4-airtemp2025-domain-spatial-data-extractions.csv"
)
subset_file <- env_or_default(
  "SILICA_YEAR_FILL_SUBSET_FILE",
  file.path(getwd(), "generated_outputs", "rerun", "active-final-run", paste0("final_v4_year_fill_subset_", date_tag, ".csv"))
)
out_dir <- env_or_default(
  "SILICA_YEAR_FILL_OUT_DIR",
  file.path(getwd(), "generated_outputs", "rerun", "active-final-run")
)

if (!file.exists(combined_file)) stop("Missing combined file: ", combined_file, call. = FALSE)
if (!file.exists(subset_file)) stop("Missing subset file: ", subset_file, call. = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

x <- read_loose_csv(combined_file)
subset <- read_loose_csv(subset_file)
for (col in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) {
  x[[col]] <- norm_blank(x[[col]])
  subset[[col]] <- norm_blank(subset[[col]])
}

subset_keys <- site_key(subset$LTER, subset$Shapefile_Name)
x <- x %>%
  mutate(.site_key = site_key(LTER, Shapefile_Name)) %>%
  filter(.site_key %in% subset_keys) %>%
  distinct(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, .keep_all = TRUE) %>%
  infer_region_for_targets()

evapo_missing <- missing_years_for_prefix(x, "evapotrans_", "_kg_m2", 2002:2025)
snow_missing <- missing_years_for_prefix(x, "snow_", "_num_days", 2002:2025)

zero_snow_lters <- normalize_lter_key(c(
  "Amazon", "Cameroon", "Congo Basin", "GRO", "Guadeloupe", "HYBAM",
  "KRR", "LMP", "LUQ", "Mali"
))

build_long <- function(missing_list, driver, skip_rows = rep(FALSE, nrow(x))) {
  rows <- vector("list", length(missing_list))
  for (i in seq_along(missing_list)) {
    year <- as.integer(names(missing_list)[[i]])
    keep <- missing_list[[i]] & !skip_rows & !is.na(x$Region) & nzchar(x$Region)
    rows[[i]] <- x[keep, c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name", "Region"), drop = FALSE] %>%
      mutate(driver = driver, year = year)
  }
  bind_rows(rows)
}

evapo_long <- build_long(evapo_missing, "evapo")
snow_long <- build_long(
  snow_missing,
  "snow",
  skip_rows = normalize_lter_key(x$LTER) %in% zero_snow_lters
)

write_driver_files <- function(driver_long, driver_name) {
  subset_out <- driver_long %>%
    distinct(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, Region) %>%
    mutate(
      Watershed_Source = "existing_final_inventory",
      Force_HydroSHEDS = "FALSE",
      Rerun_Reason = paste0("year_fill_missing_", driver_name)
    ) %>%
    select(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, Watershed_Source, Force_HydroSHEDS, Region, Rerun_Reason) %>%
    arrange(Region, LTER, Stream_Name, Shapefile_Name)

  region_year_out <- driver_long %>%
    distinct(driver, region = Region, year) %>%
    arrange(driver, region, year)

  subset_path <- file.path(out_dir, paste0("final_v4_year_fill_", driver_name, "_subset_", date_tag, ".csv"))
  region_year_path <- file.path(out_dir, paste0("final_v4_year_fill_", driver_name, "_region_years_", date_tag, ".csv"))
  write.csv(subset_out, subset_path, row.names = FALSE, na = "")
  write.csv(region_year_out, region_year_path, row.names = FALSE, na = "")

  cat("WROTE:", subset_path, "\n", sep = "")
  cat("WROTE:", region_year_path, "\n", sep = "")
  cat(driver_name, "_sites=", nrow(subset_out), "\n", sep = "")
  cat(driver_name, "_region_years=", nrow(region_year_out), "\n", sep = "")
}

write_driver_files(evapo_long, "evapo")
write_driver_files(snow_long, "snow")
