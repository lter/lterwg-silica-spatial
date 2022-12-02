## ------------------------------------------------------- ##
      # Silica WG - Extract Spatial Data - Land Cover
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Using the watershed shapefiles created in "id-watershed-polygons.R"
## Extract the following data: LAND COVER

## ------------------------------------------------------- ##
                    # Housekeeping ----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, stars, terra, exactextractr, NCEAS/scicomptools)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

# Load in site names with lat/longs
sites <- read.csv(file = file.path(path, "site-coordinates", 'silica-coords_ACTUAL.csv'))

# Check it out
dplyr::glimpse(sites)

# Grab the shapefiles the previous script (see PURPOSE section) created
sheds <- sf::st_read(dsn = file.path(path, "site-coordinates", "silica-watersheds.shp"))

# Check that out
dplyr::glimpse(sheds)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

## ------------------------------------------------------- ##
              # Land Cover - Extract ----
## ------------------------------------------------------- ##

# Read in dataset
lulc_raw <- terra::rast(x = file.path(path, "raw-driver-data", "raw-glcc-landcover-data",
                                      "global-glcc-wgs84", "gblulcgeo20.tif"))

# Exploratory plot for overlap
plot(lulc_raw, axes = T, reset = F)
plot(sheds, axes = T, add = T)

# Strip out land cover for our polygons
land_out <- exactextractr::exact_extract(x = lulc_raw, y = sheds,
                                          include_cols = c("river_id")) %>%
  # Above returns a list so switch it to a dataframe
  purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
  # Filter out NAs
  dplyr::filter(!is.na(value))

# Check that dataframe
dplyr::glimpse(land_out)

## ------------------------------------------------------- ##
                # Land Cover - Summarize ----
## ------------------------------------------------------- ##

# Load index
lulc_index <- read.csv(file = file.path(path, "raw-driver-data",
                                        "raw-glcc-landcover-data", "LULC_index.csv")) %>%
  # Rename the integer code column to match the extracted data
  dplyr::rename(value = lulc_code)

# Wrangle extracted data
land_v2 <- land_out %>%
  # Drop coverage fraction column
  dplyr::select(-coverage_fraction) %>%
  # Summarize to count number of pixels per category
  dplyr::group_by(river_id, value) %>%
  dplyr::summarize(pixel_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  # Attach index information
  dplyr::left_join(y = lulc_index, by = "value") %>%
  # Simplify data now that descriptive categories are included
  dplyr::select(-value) %>%
  dplyr::relocate(lulc_category, .after = river_id) %>%
  dplyr::mutate(lulc_category = tolower(gsub(pattern = " |-", 
                                             replacement = "_", 
                                             x = lulc_category))) %>%
  # Streamline category information
  dplyr::mutate(lulc_category = dplyr::case_when(
    lulc_category %in% c("dryland_cropland_and_pasture", 
                         "irrigated_cropland_and_pasture",
                         "cropland/grassland_mosaic",
                         "cropland/woodland_mosaic") ~ "cropland",
    lulc_category %in% c("herbaceous_wetland", "wooded_wetland") ~ "wetland",
    lulc_category %in% c("bare_ground_tundra", "wooded_tundra",
                         "mixed_tundra") ~ "tundra",
    lulc_category %in% c("shrubland", "grassland", "savanna", 
                         "mixed_shrubland/grassland") ~ "shrubland_grassland",
    TRUE ~ lulc_category)) %>%
  # Drop unwanted categories
  dplyr::filter(!lulc_category %in% c("snow_or_ice", "water_bodies")) %>%
  # Summarize again with simplified categories
  dplyr::group_by(river_id, lulc_category) %>%
  dplyr::summarize(land_total = sum(pixel_ct, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Get total pixel count per river (across category)
  dplyr::group_by(river_id) %>%
  dplyr::mutate(total_pixels = sum(land_total, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Calculate percent of total
  dplyr::mutate(perc_total = ((land_total / total_pixels) * 100)) %>%
  # Drop pixel count columns
  dplyr::select(-land_total, -total_pixels)

# Check remaining categories  
sort(unique(land_v2$lulc_category))

# Glimpse this object
dplyr::glimpse(land_v2)

# Make a version of this that is wide
land_wide <- land_v2 %>%
  tidyr::pivot_wider(names_from = lulc_category,
                     values_from = perc_total)

# Also identify the dominant land cover type(s) in each river's polygon
land_major <- land_v2 %>%
  # Filter to only max of each rock type per river
  dplyr::group_by(river_id) %>%
  dplyr::filter(perc_total == max(perc_total, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Remove the percent total
  dplyr::select(-perc_total) %>%
  # Get the columns into wide format where the column name and value are both the dominant land
  tidyr::pivot_wider(names_from = lulc_category,
                     values_from = lulc_category) %>%
  # Paste all the non-NAs (i.e., the dominant rocks) into a single column
  tidyr::unite(col = major_land, -river_id, na.rm = T, sep = "; ")

# Combine major with per-category
land_actual <- land_wide %>%
  dplyr::left_join(y = land_major, by = "river_id") %>%
  dplyr::relocate(major_land, .after = river_id)

# Glimpse it
dplyr::glimpse(land_actual)

## ------------------------------------------------------- ##
                # Land Cover - Export ----
## ------------------------------------------------------- ##

# Let's get ready to export
land_export <- sites %>%
  # Join the rock data
  dplyr::left_join(y = land_actual, by = c("river_id"))

# Check it out
dplyr::glimpse(land_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized lithology data
write.csv(x = land_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_landcover.csv"))

# End ----
