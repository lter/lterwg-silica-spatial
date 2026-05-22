#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 3) {
  stop(
    "Usage: Rscript tools/merge_glc_lulc_update.R <master_lulc_csv> <new_glc_csv> <output_csv>",
    call. = FALSE
  )
}

master_path <- args[[1]]
new_glc_path <- args[[2]]
output_path <- args[[3]]

simple_classes <- c(
  "Bare",
  "Cropland",
  "Filled_Value",
  "Forest",
  "Grassland_Shrubland",
  "Ice_Snow",
  "Impervious",
  "Salt_Water",
  "Tidal_Wetland",
  "Water",
  "Wetland_Marsh"
)

landclass_to_simple <- c(
  "Rainfed_cropland" = "Cropland",
  "Herbaceous_cover_cropland" = "Cropland",
  "Tree_or_shrub_cover_cropland" = "Cropland",
  "Irrigated_cropland" = "Cropland",
  "Open_evergreen_broadleaved_forest" = "Forest",
  "Closed_evergreen_broadleaved_forest" = "Forest",
  "Open_deciduous_broadleaved_forest" = "Forest",
  "Closed_deciduous_broadleaved_forest" = "Forest",
  "Open_evergreen_needle_leaved_forest" = "Forest",
  "Closed_evergreen_needle_leaved_forest" = "Forest",
  "Open_deciduous_needle_leaved_forest" = "Forest",
  "Closed_deciduous_needle_leaved_forest" = "Forest",
  "Open_mixed_leaf_forest" = "Forest",
  "Closed_mixed_leaf_forest" = "Forest",
  "Shrubland" = "Grassland_Shrubland",
  "Evergreen_shrubland" = "Grassland_Shrubland",
  "Deciduous_shrubland" = "Grassland_Shrubland",
  "Grassland" = "Grassland_Shrubland",
  "Sparse_vegetation" = "Grassland_Shrubland",
  "Sparse_shrubland" = "Grassland_Shrubland",
  "Sparse_herbaceous" = "Grassland_Shrubland",
  "Bare_areas" = "Bare",
  "Consolidated_bare_areas" = "Bare",
  "Unconsolidated_bare_areas" = "Bare",
  "Lichens_and_mosses" = "Bare",
  "Impervious_surfaces" = "Impervious",
  "Swamp" = "Wetland_Marsh",
  "Marsh" = "Wetland_Marsh",
  "Flooded_flat" = "Wetland_Marsh",
  "Mangrove" = "Tidal_Wetland",
  "Tidal_flat" = "Tidal_Wetland",
  "Salt_marsh" = "Tidal_Wetland",
  "Saline" = "Salt_Water",
  "Water_body" = "Water",
  "Permanent_ice_and_snow" = "Ice_Snow",
  "Filled_value" = "Filled_Value"
)

interp_series <- function(years, values) {
  stopifnot(length(years) == length(values))

  if (!all(c(1985, 1990, 1995) %in% years)) {
    stop("Expected five-year anchors 1985, 1990, and 1995 in new GLC input.")
  }

  annual_years <- years[years >= 2000]
  if (!identical(sort(annual_years), 2000:2022)) {
    stop("Expected annual GLC years 2000:2022 in new GLC input.")
  }

  approx(
    x = years,
    y = values,
    xout = 1985:2022,
    method = "linear",
    rule = 2
  )$y
}

master_lulc <- read_csv(master_path, show_col_types = FALSE) %>%
  select(any_of(c("X", "Stream_Name", "Year", "Simple_Class", "LandClass_sum")))

new_glc <- read_csv(new_glc_path, show_col_types = FALSE)

required_cols <- c("Stream_Name", "Year", "LandClass", "Area_m2")
missing_cols <- setdiff(required_cols, names(new_glc))
if (length(missing_cols) > 0) {
  stop("Missing required columns in new GLC file: ", paste(missing_cols, collapse = ", "))
}

updated_sites <- new_glc %>%
  distinct(Stream_Name) %>%
  arrange(Stream_Name) %>%
  pull(Stream_Name)

message("Updating ", length(updated_sites), " Stream_Name values.")

source_years <- new_glc %>%
  mutate(
    Simple_Class = unname(landclass_to_simple[LandClass])
  ) %>%
  filter(!is.na(Simple_Class)) %>%
  group_by(Stream_Name, Year, Simple_Class) %>%
  summarise(Area_m2 = sum(Area_m2, na.rm = TRUE), .groups = "drop") %>%
  group_by(Stream_Name, Year) %>%
  mutate(
    total_area_m2 = sum(Area_m2, na.rm = TRUE),
    LandClass_sum = if_else(total_area_m2 > 0, Area_m2 / total_area_m2, 0)
  ) %>%
  ungroup() %>%
  select(Stream_Name, Year, Simple_Class, LandClass_sum) %>%
  complete(
    Stream_Name,
    Year = c(1985, 1990, 1995, 2000:2022),
    Simple_Class = simple_classes,
    fill = list(LandClass_sum = 0)
  )

interpolated_1985_2022 <- source_years %>%
  group_by(Stream_Name, Simple_Class) %>%
  summarise(
    Year = 1985:2022,
    LandClass_sum = interp_series(Year, LandClass_sum),
    .groups = "drop"
  )

pre_1985 <- interpolated_1985_2022 %>%
  filter(Year == 1985) %>%
  select(-Year) %>%
  crossing(Year = 1900:1984) %>%
  select(Stream_Name, Year, Simple_Class, LandClass_sum)

updated_lulc <- bind_rows(pre_1985, interpolated_1985_2022) %>%
  arrange(Stream_Name, Year, Simple_Class)

combined_lulc <- master_lulc %>%
  filter(!Stream_Name %in% updated_sites) %>%
  select(-any_of("X")) %>%
  bind_rows(updated_lulc) %>%
  arrange(Stream_Name, Year, Simple_Class) %>%
  mutate(X = row_number()) %>%
  select(X, Stream_Name, Year, Simple_Class, LandClass_sum)

write_csv(combined_lulc, output_path)

message("Wrote: ", output_path)
