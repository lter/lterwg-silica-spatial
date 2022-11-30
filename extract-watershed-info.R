## ----------------------------------------------------------------------- ##
                        # LTER WG: Silica Synthesis
## ----------------------------------------------------------------------- ##
# Written by:
## Nick J Lyon + 

# Purpose:
## Using the watershed shapefiles created in "identify-watersheds.R"
## extract lithology and land cover data for each watershed

# Housekeeping ---------------------------------------------------------------

# Read needed libraries
library(tidyverse); library(sf); library(stars); library(terra); library(exactextractr)

# Clear environment
rm(list = ls())

# Set working directory to location of shared data
## Identify path
path <- file.path('/', "home", "shares", "lter-si", "si-watershed-extract")
## Set WD to path
setwd(path)
## Check that it worked
getwd()

# Site Coordinate & Watershed Shapefile Retrieval ----------------------------

# Load in site names with lat/longs
sites <- read.csv("tidy_SilicaSites.csv")

# Check it out
str(sites)

# Grab the shapefiles the previous script (see PURPOSE section) created
sheds <- sf::st_read('watershed-shapefiles/SilicaSynthesis_allWatersheds.shp')

# Check that out
str(sheds)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Lithology Data ------------------------------------------------------------

# Ultimately want both lithology and land cover but we'll start with rocks

# Lithology Extraction ---------------------------------------------------

# Pull in the raw lithology data
rocks_raw <- terra::rast("extracted-data/raw-lithology-data/glim_wgs84_0point5deg.txt.asc")

# Check CRS
crs(rocks_raw)
## Looks good

# Experimental plotting
plot(rocks_raw)

# Strip out rocks from our polygons
rocks_list <- exactextractr::exact_extract(x = rocks_raw, y = sheds, include_cols = c("LTER", "uniqueID"))
str(rocks_list)

# Get that to a dataframs
rocks_actual <- rocks_list %>%
  map_dfr(select, c(LTER, uniqueID, value))

# Check contents
head(rocks_actual)

# Lithology Summarization ---------------------------------------------------

# Bring in the index tying rock code integers with rock abbreviations
rock_index_raw <- read.table(file = "extracted-data/raw-lithology-data/Classnames.txt",
                             header = T, sep = ';')

# Fix this index to make it more usable
rock_index <- rock_index_raw %>%
  # Rename the most important columns
  dplyr::rename(rock_code = OBJECTID,
                rock_abbrev = xx) %>%
  # And get a more descriptive version of each of the rock types
  dplyr::mutate(
    rock_type = case_when(
      # Abbreviations found here:
      # https://www.clisap.de/fileadmin/B-Research/IA/IA5/LITHOMAP/
      rock_abbrev == 'su' ~ 'unconsolidated_sediments',
      rock_abbrev == 'ss' ~ 'siliciclastic_sedimentary_rocks',
      rock_abbrev == 'sm' ~ 'mixed_sedimentary_rocks',
      rock_abbrev == 'py' ~ 'pyroclastic',
      rock_abbrev == 'sc' ~ 'carbonate_sedimentary_rocks',
      rock_abbrev == 'ev' ~ 'evaporites',
      rock_abbrev == 'mt' ~ 'metamorphic_rocks',
      rock_abbrev == 'pa' ~ 'acid_plutonic_rocks',
      rock_abbrev == 'pi' ~ 'intermediate_plutonic_rocks',
      rock_abbrev == 'pb' ~ 'basic_plutonic_rocks',
      rock_abbrev == 'va' ~ 'acid_volcanic_rocks',
      rock_abbrev == 'vi' ~ 'intermediate_volcanic_rocks',
      rock_abbrev == 'vb' ~ 'basic_volcanic_rocks',
      rock_abbrev == 'ig' ~ 'ice_and_glaciers',
      rock_abbrev == 'wb' ~ 'water_bodies',
      rock_abbrev == 'nd' ~ 'no_data',
      TRUE ~ as.character(rock_abbrev) ) ) %>%
  # Remove unneeded columns
  select(rock_code, rock_type)

# Check that worked
head(rock_index)  
sort(unique(rock_index$rock_type))

# Process the extracted lithology information into a dataframe
rock_data_v1 <- rocks_actual %>%
  # Get value column named more informatively
  dplyr::rename(rock_code = value) %>%
  # Bring over the rock names from the index
  left_join(rock_index, by = "rock_code") %>%
  # Remove the now-unneeded code column
  dplyr::select(-rock_code) %>%
  dplyr::mutate(
    rock_type = case_when(
      rock_type == 'unconsolidated_sediments' ~ 'sedimentary',
      rock_type == 'siliciclastic_sedimentary_rocks' ~ 'sedimentary',
      rock_type == 'mixed_sedimentary_rocks' ~ 'sedimentary',
      rock_type == 'pyroclastic' ~ 'volcanic',
      rock_type == 'carbonate_sedimentary_rocks' ~ 'carbonate_evaporite',
      rock_type == 'evaporites' ~ 'carbonate_evaporite',
      rock_type == 'metamorphic_rocks' ~ 'metamorphic',
      rock_type == 'acid_plutonic_rocks' ~ 'plutonic',
      rock_type == 'intermediate_plutonic_rocks' ~ 'plutonic',
      rock_type == 'basic_plutonic_rocks' ~ 'plutonic',
      rock_type == 'acid_volcanic_rocks' ~ 'volcanic',
      rock_type == 'intermediate_volcanic_rocks' ~ 'volcanic',
      rock_type == 'basic_volcanic_rocks' ~ 'volcanic',
      T ~ as.character(rock_type)
    ) ) %>%
  # Group by LTER and uniqueID
  group_by(LTER, uniqueID, rock_type) %>%
  # Count the instances within each rock type
  ## 0.5° degree pixels within the watershed that contain this rock type
  summarise(rock_totals = n()) %>%
  # Make it a dataframe (to avoid a list of tibbles)
  as.data.frame() %>%
  # Remove the 'no_data' cells
  filter(rock_type != "no_data" &
           rock_type != 'ice_and_glaciers' &
           rock_type != 'water_bodies') %>%
  # Bin rock categories and re-summarise within consolidated categories
  # Group by LTER and uniqueID
  group_by(LTER, uniqueID) %>%
  # We'll want the totals as a percent (total pixels is not very intuitive)
  dplyr::mutate( total_shed_pixels = sum(rock_totals) ) %>%
  # Again, return a dataframe, not a tibble
  as.data.frame() %>%
  # Now ungroup
  ungroup() %>%
  # And calculate the percent of total for each row
  dplyr::mutate( perc_total = ((rock_totals / total_shed_pixels) * 100) ) %>%
  # Remove the two pixel count columns
  dplyr::select(-rock_totals, -total_shed_pixels)

# Now we want to split into two directions
## First: get a version where each rock type is its own column
rock_data_wide <- rock_data_v1 %>%
  # Pivot to wide format
  pivot_wider(id_cols = c(LTER, uniqueID),
              names_from = rock_type,
              values_from = perc_total)

## Second: get the *majority* rock for each watershed
rock_data_major <- rock_data_v1 %>%
  # Filter to only max of each rock type per uniqueID & LTER
  group_by(LTER, uniqueID) %>%
  filter(perc_total == max(perc_total)) %>%
  # Remove the percent total
  dplyr::select(-perc_total) %>%
  # Get the columns into wide format where the column name and value are both whatever the dominant rock was
  pivot_wider(id_cols = c(LTER, uniqueID),
              names_from = rock_type,
              values_from = rock_type) %>%
  # Paste all the non-NAs (i.e., the dominant rocks) into a single column
  unite(col = major_rock, -LTER:-uniqueID, na.rm = T, sep = "; ")

# Now attach the major rocks to the wide format one
rock_data_actual <- rock_data_wide %>%
  left_join(rock_data_major, by = c("LTER", "uniqueID")) %>%
  relocate(major_rock, .after = uniqueID)

# Examine
str(rock_data_actual)
head(rock_data_actual)
names(rock_data_actual)

# Lithology Export -----------------------------------------------------------

# Let's get ready to export
rock_export <- sites %>%
  left_join(rock_data_actual, by = c("LTER", "uniqueID"))

# Check it out
head(rock_export)
names(rock_export)

# Export the summarized lithology data
write.csv(x = rock_export,
          file = "extracted-data/SilicaSites_litho.csv",
          na = '', row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Land Cover Data ------------------------------------------------------------

# NLCD Raster CRS Transformation ---------------------------------------------

# Two of the NLCD rasters I downloaded manually don't match the CRS of the other watersheds
## Alaska doesn't match, presumably because it is from 2016 versus 2019 for the others
## The continental US (from which we'll cut UMR's bounding box) doesn't match presumably because downloading from the web app versus downloading the whole raster differs (though it shouldn't in an ideal world)

# Both of these need to be re-projected to the CRS of the other rasters and saved out for convenience.

# This is done here to (1) allow for a for loop to handle as many watersheds as possible and (2) because it is computationally intense, we only want to run it once and then leave it alone.

# All of the following lines were run once and then commented out but retained for posterity.

# Process Alaska Data (for ARC)
## 1) Read in 'raw' (i.e., direct from NLCD) file
# arc_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-ARC-bbox-2016/NLCD_2016_Land_Cover_AK_20200724_1UJcf1tMJdCcymwLngFW.tiff")
# crs(arc_raw)

## 2) We need to transform the CRS so that it matches the others
# arc_fix <- terra::project(arc_raw, y = 'epsg:5070')
# crs(arc_fix)

## 3) Write it out for later use
# terra::writeRaster(arc_fix, "extracted-data/raw-landcover-data/NLCD-ARC-bbox-2016/NLCD_ARC_bbox_2016_raw.tiff")

# Process Continental US Data (for UMR)
## 1) Get raw variant
# usa_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-ContinentalUS-2019/nlcd_2019_land_cover_l48_20210604.img")

## 2) Crop it (the entire CONUS raster is too big to transform)
# ext(usa_raw)
# usa_crop <- terra::crop(usa_raw, terra::ext(0, 1000000, 1750000, 3310005))
# ext(usa_crop)

## 3) Re-project this cropped (thus smaller) object into the preferred CRS
### Note this takes ~15 minutes so try not to re-run it if you can avoid it
# usa_fix <- terra::project(usa_crop, y = 'epsg:5070')
# crs(usa_fix)

## 4) Save out fixed raster
# terra::writeRaster(usa_fix, "extracted-data/raw-landcover-data/NLCD-UMR-bbox-2019/NLCD_UMR_bbox_2019_raw.tiff")

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Luquillo LTER (LUQ) Land Cover (LC) Processing ----------------------------------

# Prepare an sf object of just this LTER's watershed shapes
luq_sf <- sheds %>%
  filter(LTER == "LUQ") %>%
  # And transformed to the LC raster's CRS
  st_transform(crs = 5070)

# Read in the raster
pr_rast <- terra::rast("extracted-data/raw-landcover-data/NLCD-PuertoRico-2001/NLCD_PuertoRico_2001_raw.img")

# Make sure CRS are same
crs(pr_rast)
st_crs(luq_sf)

# Plot that to see how it looks
plot(pr_rast, axes = T, reset = F)
plot(luq_sf["uniqueID"], add = T)

# Extract data from the raster using the sf object
luq_lc_list <- exactextractr::exact_extract(x = pr_rast, y = luq_sf, include_cols = c("LTER", "uniqueID"))
str(luq_lc_list)

# Grab the NLCD index that connects integer codes to meaningful categories
nlcd_index <- read.csv("extracted-data/raw-landcover-data/NLCD_index.csv")
head(nlcd_index)

# Process our landcover information
luq_lc_actual <- luq_lc_list %>%
  # Make a dataframe from the selected list columns
  map_dfr(select, c(LTER, uniqueID, value)) %>%
  # Rename the land cover column
  rename(nlcd_code = value) %>%
  # Group by code, LTER, and uniqueID
  group_by(LTER, uniqueID, nlcd_code) %>%
  # Count pixels per code within the watershed
  summarise(cover_pixel_ct = n()) %>%
  # Grab all of the contents of the index
  left_join(nlcd_index, by = "nlcd_code") %>%
  # Keep only columns that we want
  select(LTER, uniqueID, nlcd_category, cover_pixel_ct) %>%
  # Export as dataframe
  as.data.frame()

# Check it out
head(luq_lc_actual)

# Save this out
write.csv(luq_lc_actual, "extracted-data/raw-landcover-data/NLCD-LUQ-landcover.csv", row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Arctic LTER (ARC) LC Processing -------------------------------------------

# Prepare an sf object of just this LTER's watershed shapes in the correct CRS
arc_sf <- sheds %>%
  filter(LTER == "ARC") %>%
  st_transform(crs = 5070)

# Read in the raster
ak_rast <- terra::rast("extracted-data/raw-landcover-data/NLCD-ARC-bbox-2016/NLCD_ARC_bbox_2016_raw.tiff")

# Make sure CRS are same
crs(ak_rast)
st_crs(arc_sf)

# Plot that to see how it looks
plot(ak_rast, axes = T, reset = F)
## For some reason the conversion made the whole world of blank no-data show up so just ignore that
plot(arc_sf["uniqueID"], add = T)

# Extract data from the raster using the sf object
arc_lc_list <- exactextractr::exact_extract(x = ak_rast, y = arc_sf, include_cols = c("LTER", "uniqueID"))
str(arc_lc_list)

# Grab the NLCD index that connects integer codes to meaningful categories
nlcd_index <- read.csv("extracted-data/raw-landcover-data/NLCD_index.csv")
head(nlcd_index)

# Process our landcover information
arc_lc_actual <- arc_lc_list %>%
  # Make a dataframe from the selected list columns
  map_dfr(select, c(LTER, uniqueID, value)) %>%
  # Rename the land cover column
  rename(nlcd_code = value) %>%
  # Group by code, LTER, and uniqueID
  group_by(LTER, uniqueID, nlcd_code) %>%
  # Count pixels per code within the watershed
  summarise(cover_pixel_ct = n()) %>%
  # Grab all of the contents of the index
  left_join(nlcd_index, by = "nlcd_code") %>%
  # Keep only columns that we want
  select(LTER, uniqueID, nlcd_category, cover_pixel_ct) %>%
  # Export as dataframe
  as.data.frame()

# Check it out
head(arc_lc_actual)

# Save this out
write.csv(arc_lc_actual, "extracted-data/raw-landcover-data/NLCD-ARC-landcover.csv", row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Continental US LC Processing --------------------------------------------
# This includes AND, HBR, KRR, LMP, LUQ, MCM, NWT, Sagehen, & UMR

# Because the process is the same for extracting data for all of our LTERs, let's do it in a for loop 

# Also, this for loop has a grouped `dplyr::summarize` call so we'll turn off the annoying dialogue that it returns before going into the loop
options(dplyr.summarise.inform = F)

# Grab the NLCD index that connects integer codes to meaningful categories
nlcd_index <- read.csv("extracted-data/raw-landcover-data/NLCD_index.csv")
head(nlcd_index)

# Now time for the for loop (the `setdiff` is needed to split off the ones that we either already have or will need to process via a different workflow)
for(lter in setdiff(unique(sites$LTER), c("ARC", "LUQ", "GRO", "MCM"))){

    # Print a start message (for diagnostic purposes)
  print(paste0(lter, " processing begun at ", Sys.time()))
  
  # Read in the pre-transformed raster
  bbox <- terra::rast(paste0("extracted-data/raw-landcover-data/NLCD-", lter ,"-bbox-2019/NLCD_", lter, "_bbox_2019_raw.tiff"))
  
  # Prepare the relevant subset of the sf object of our watersheds
  sf <- sheds %>%
    filter(LTER == lter) %>%
    st_transform(crs = 5070)
  
  # Extract data from the raster using the sf object
  lc_list <- exactextractr::exact_extract(x = bbox, y = sf, include_cols = c("LTER", "uniqueID"))

  # Now we have a smidge more processing to do before we should export
  lc_actual <- lc_list %>%
    # Make a single dataframe from the selected columns of each list element
    map_dfr(select, c(LTER, uniqueID, value)) %>%
    # Rename the land cover column
    rename(nlcd_code = value) %>%
    # Group by code, LTER, and uniqueID
    group_by(LTER, uniqueID, nlcd_code) %>%
    # Count pixels per code within the watershed
    summarise(cover_pixel_ct = n()) %>%
    # Grab all of the contents of the index
    left_join(nlcd_index, by = "nlcd_code") %>%
    # Keep only columns that we want
    select(LTER, uniqueID, nlcd_category, cover_pixel_ct) %>%
    # Export as dataframe
    as.data.frame()
  
  # This object is the one we want so save it to the server as a csv
  write.csv(lc_actual, paste0("extracted-data/raw-landcover-data/NLCD-", lter, "-landcover.csv"), row.names = F)
  
  # End with a conclusion message to mirror the start message
  print(paste0(lter, " processing completed at ", Sys.time()))  }
## Note that the part of the loop handling UMR takes a few minutes (3 min on NCEAS server)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Great Rivers Observatory (GRO) LC Processing -----------------------------
# Data source https://earthexplorer.usgs.gov/scene/metadata/full/5e83a1d8eecc8bb5/GLCCGBE20/

# Get relevant part of watershed polygons
gro_sf <- sheds %>%
  filter(LTER == "GRO")

# Read in data
## gbigbpgeo20 = International Geosphere Biosphere Programme Land Cover Classification  (IGBP)
globe <- terra::rast("extracted-data/raw-landcover-data/MODIS_GLCC_Global_1992/gbigbpgeo20.tif")

# Exploratory plot
plot(globe)

# Crop to band of interest (all longitudes but only northern-most latitudes)
ext(globe)
globe_crop <- terra::crop(globe, terra::ext(-179.999999999967, 179.999985600033,
                                            55, 89.9999999999667))

# Plot cropped version
plot(globe_crop)

# Check CRS
crs(globe_crop)
st_crs(gro_sf)
## Looks like they match right out of the box!

# Try to extract information
gro_lc_list <- exactextractr::exact_extract(x = globe_crop, y = gro_sf, include_cols = c("LTER", "uniqueID"))
str(gro_lc_list)

# Read in IGBP index
## Created manually from description here
## www.usgs.gov/media/files/global-land-cover-characteristics-data-base-readme-version2
igbp_index <- read.csv("extracted-data/raw-landcover-data/IGBP_index.csv")
head(igbp_index)

# Process this out of list form
gro_lc <- gro_lc_list %>%
  # Make a dataframe from the selected list columns
  map_dfr(select, c(LTER, uniqueID, value)) %>%
  # Group by LTER and uniqueID
  group_by(LTER, uniqueID, value) %>%
  # Count instances of each pixel category
  summarise(cover_pixel_ct = n()) %>%
  # Bring in category names
  rename(igbp_code = value) %>%
  left_join(igbp_index, by = "igbp_code") %>%
  # Fix weird spacing issue
  mutate(igbp_category = str_sub(igbp_category, 2, nchar(igbp_category))) %>%
  # Keep only desired columns
  select(LTER, uniqueID, igbp_category, cover_pixel_ct)

# Check it
unique(gro_lc$igbp_category)
head(gro_lc)

# Export this as is so that we can standardize cover categories between different sources of land cover data
write.csv(gro_lc, "extracted-data/raw-landcover-data/IGBP-GRO-landcover.csv", na = '', row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# McMurdo LTER (MCM) LC Processing -----------------------------
# Need to find a source for these data




# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Combine LC Data Across Watersheds ----------------------------------------

# We have--at this point--successfully summarized land cover data from most of our LTERs; now we want to combine our LTER-specific csvs into one 'main' variant

# Set a counter and empty list
nlcd_list <- list()
k <- 1

# Read in the csvs (not for MCM or GRO because we don't have them yet and when we do they'll come from different sources)
for(lter in setdiff(unique(sites$LTER), c("GRO", "MCM"))){
  
  # Grab a csv
  nlcd_data <- read.csv(paste0("extracted-data/raw-landcover-data/NLCD-", lter, "-landcover.csv"))
  
  # Add it to the kth element of our list
  nlcd_list[[k]] <- nlcd_data
  
  # Advance the counter
  k <- k + 1 }

# Switch from list to dataframe
nlcd_data <- nlcd_list %>%
  map_dfr(select, c(LTER, uniqueID, nlcd_category, cover_pixel_ct))
head(nlcd_data)

# Read in GRO land cover (from different source!)
gro_data <- read.csv("extracted-data/raw-landcover-data/IGBP-GRO-landcover.csv")
head(gro_data)

# Combine the two dataframes (without standardizing cover names)
lc_unmod <- nlcd_data %>%
  # Use bind_rows to account for difference in category names
  dplyr::bind_rows(gro_data) %>%
  # Move the GRO's category column to be next to the other one
  relocate(igbp_category, .before = cover_pixel_ct) %>%
  # Make a single column for both types of category
  mutate(cover_category = coalesce(nlcd_category, igbp_category)) %>%
  # Remove any NAs
  filter(!is.na(cover_category)) %>%
  # Standardize casing/special character use in that combined column
  mutate(cover_category = tolower(gsub("\\/| |\\-", "_", cover_category))) %>%
  # Remove unneeded columns
  select(-nlcd_category, -igbp_category) %>%
  # Calculate total pixels per stream
  group_by(LTER, uniqueID) %>%
  mutate(total_pixels = sum(cover_pixel_ct, na.rm = T)) %>%
  # And pivot to wide format
  pivot_wider(id_cols = -cover_category:-cover_pixel_ct,
              names_from = cover_category,
              values_from = cover_pixel_ct)

# Integrate the two dataframes (for real)
lc_data <- nlcd_data %>%
  # Use bind_rows to account for difference in category names
  dplyr::bind_rows(gro_data) %>%
  # Move the GRO's category column to be next to the other one
  relocate(igbp_category, .before = cover_pixel_ct) %>%
  # Make a single column for both types of category
  mutate(cover_category = coalesce(nlcd_category, igbp_category)) %>%
  # Remove any NAs
  filter(!is.na(cover_category)) %>%
  # Collapse seemingly synonymous categories into one another (and standardize casing/special characters)
  dplyr::mutate(
    cover_category = case_when(
      # Some additional combination is possible but these are the "safe" changes in my (Nick's) opinion
      ## NLCD category simplification
      nlcd_category == "Barren_Land" ~ "barren_land",
      nlcd_category == "Perennial_Ice_Snow" ~ "barren_land",
      nlcd_category == "Cultivated_Crops" ~ "cultivated_crops",
      nlcd_category == "Deciduous_Forest" ~ "deciduous_forest",
      nlcd_category == "Developed_High_Intensity" ~ "low_medium_intensity_developed",
      nlcd_category == "Developed_Low_Intensity" ~ "low_medium_intensity_developed",
      nlcd_category == "Developed_Medium_Intensity" ~ "low_medium_intensity_developed",
      nlcd_category == "Developed_Open_Space" ~ "low_medium_intensity_developed",
      nlcd_category == "Evergreen_Forest" ~ "evergreen_forest",
      nlcd_category == "Grassland_Herbaceous" ~ "grassland",
      nlcd_category == "Sedge_Herbaceous" ~ "grassland",
      nlcd_category == "Mixed_Forest" ~ "mixed_forest",
      nlcd_category == "Open_Water" ~ "open_water",
      nlcd_category == "Pasture_Hay" ~ "pasture_hay",
      nlcd_category == "Dwarf_Shrub" ~ "shrubland",
      nlcd_category == "Shrub_Scrub" ~ "shrubland",
      nlcd_category == "Emergent_Herbaceous_Wetlands" ~ "wetland",
      nlcd_category == "Woody_Wetlands" ~ "wetland",
      ## IGBP category simplification
      igbp_category == "Barren or Sparsely Vegetated " ~ "barren_land",
      igbp_category == "Snow and Ice" ~ "barren_land",
      igbp_category == "Cropland/Natural Vegetation Mosaic" ~ "cultivated_crops",
      igbp_category == "Croplands" ~ "cultivated_crops",
      igbp_category == "Deciduous Needleleaf Forest" ~ "deciduous_needleaf_forest",
      igbp_category == "Urban and Built-Up" ~ "low_medium_intensity_developed",
      igbp_category == "Evergreen Needleleaf Forest" ~ "evergreen_forest",
      igbp_category == "Grasslands" ~ "grassland",
      igbp_category == "Savannas" ~ "grassland",
      igbp_category == "Woody Savannas" ~ "grassland", # *NOTE JUDGMENT CALL*
      igbp_category == "Mixed Forest" ~ "mixed_forest",
      igbp_category == "Water Bodies" ~ "open_water",
      igbp_category == "Closed Shrublands" ~ "shrubland",
      igbp_category == "Open Shrublands" ~ "shrubland",
      igbp_category == "Permanent Wetlands" ~ "wetland",
      # _category == "" ~ "",
      T ~ as.character(cover_category) ) ) %>%
  # Re-summarize within our new categories
  ## Note that this drops the original category columns so you'd need to go back to find those
  group_by(LTER, uniqueID, cover_category) %>%
  summarise(cover_pixel_ct = sum(cover_pixel_ct)) %>%
  # Group by LTER and uniqueID (looking across categories within watersheds)
  group_by(LTER, uniqueID) %>%
  # Count the total pixels and get percent from that
  mutate(
    total_pixels = sum(cover_pixel_ct),
    perc_cover = round((cover_pixel_ct / total_pixels) * 100, digits = 2)
  ) %>%
  # Slim down to only needed columns (drops unspecified cols implicitly)
  select(LTER, uniqueID, cover_category, perc_cover)

# Look at it
head(lc_data)

# As with lithology, we need to process in two directions
## Wide format with all percents
lc_wide <- lc_data %>%
  pivot_wider(id_cols = c(LTER, uniqueID),
              names_from = cover_category,
              values_from = perc_cover)

## Second: get the *majority* cover for each watershed
lc_major <- lc_data %>%
  # Filter to only max of each cover type per uniqueID & LTER
  group_by(LTER, uniqueID) %>%
  filter(perc_cover == max(perc_cover)) %>%
  # Remove the percent total
  dplyr::select(-perc_cover) %>%
  # Get the columns into wide format where the column name and value are both whatever the dominant cover was
  pivot_wider(id_cols = c(LTER, uniqueID),
              names_from = cover_category,
              values_from = cover_category) %>%
  # Paste all the non-NAs (i.e., the dominant rocks) into a single column
  unite(col = major_cover, -LTER:-uniqueID, na.rm = T, sep = "; ")

# Now attach the major rocks to the wide format one
lc_actual <- lc_wide %>%
  left_join(lc_major, by = c("LTER", "uniqueID")) %>%
  relocate(major_cover, .after = uniqueID)

# Examine
head(lc_actual)

# LC Export -----------------------------------------------------------------

# Let's get ready to export
cover_export <- sites %>%
  left_join(lc_actual, by = c("LTER", "uniqueID"))

# Check it out
head(cover_export)

# Export this csv
write.csv(x = cover_export,
          file = "extracted-data/SilicaSites_landcover.csv",
          na = '', row.names = F)

# And export the unmodified variant too
write.csv(x = lc_unmod,
          file = "extracted-data/SilicaSites_landcover_raw_categories.csv",
          na = '', row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Lithology and Land Cover Combination --------------------------------------

# With both lithology and land cover data in hand, let's make a single 'one-stop shop' dataframe containing both

# Read in both
rocks <- read.csv("extracted-data/SilicaSites_litho.csv")
cover <- read.csv("extracted-data/SilicaSites_landcover.csv")

# Get a vector of shared column names
shared_cols <- intersect(names(rocks), names(cover))
shared_cols

# Combine and process the two
combo <- cover %>%
  # Left join by shared columns
  left_join(rocks, by = shared_cols) %>%
  # Re-order columns
  relocate(major_rock, .after = major_cover) %>%
  # We want to fix some column names but its faster to do this via long format
  pivot_longer(cols = volcanic:plutonic,
               names_to = "rock_types", values_to = "rocks_perc") %>%
  # Fix the column names by:
  mutate(
    ## Adding prefix "rocks_" to all columns (this will help differentiate between rock and cover data that might otherwise be ambiguous)
    rock_types = paste0("rocks_", rock_types) ) %>%
  # Pivot back to wide format
  pivot_wider(names_from = rock_types,
              values_from = rocks_perc) %>%
  # Now do the same for the cover categories
  pivot_longer(cols = deciduous_forest:pasture_hay,
               names_to = "cover_types", values_to = "cover_perc") %>%
  mutate(cover_types = paste0("cover_", cover_types)) %>%
  pivot_wider(names_from = cover_types,
              values_from = cover_perc) %>%
  # McMurdo uses expert knowledge rather than extracted information (for now)
  mutate(major_rock = ifelse(LTER == "MCM", yes = "glacial_drift", no = major_rock),
         major_cover = ifelse(LTER == "MCM", yes = "barren_land", no = major_cover)) %>%
  # And let's also add some more 'data source' information to this for later use
  mutate(
    rockSource = ifelse(LTER == "MCM", yes = "Expert knowledge",
                        no = "Hartmann & Moosedorf 2012"),
    rockSourceLink = ifelse(LTER == "MCM", yes = "-",
                            no = "https://doi.pangaea.de/10.1594/PANGAEA.788537"),
    coverSource = case_when(
      LTER == "AND" ~ "2019 NLCD Continental US (manual bounding box via web app)",
      LTER == "ARC" ~ "2016 NLCD Alaska",
      LTER == "GRO" ~ "International Geosphere Biosphere Programme (IGBP) Land Cover Classification",
      LTER == "HBR" ~ "2019 NLCD Continental US (manual bounding box via web app)",
      LTER == "KRR" ~ "2019 NLCD Continental US (manual bounding box via web app)",
      LTER == "LMP" ~ "2019 NLCD Continental US (manual bounding box via web app)",
      LTER == "LUQ" ~ "2001 NLCD Puerto Rico",
      LTER == "MCM" ~ "Expert knowledge",
      LTER == "NWT" ~ "2019 NLCD Continental US (manual bounding box via web app)",
      LTER == "Sagehen" ~ "2019 NLCD Continental US (manual bounding box via web app)",
      LTER == "UMR" ~ "2019 NLCD Continental US (full)",
      T ~ as.character(LTER) ),
    coverSourceLink = case_when(
      LTER == "AND" ~ "https://www.mrlc.gov/viewer/",
      LTER == "ARC" ~ "https://www.mrlc.gov/data/nlcd-2016-land-cover-alaska",
      LTER == "GRO" ~ "https://www.usgs.gov/centers/eros/science/usgs-eros-archive-land-cover-products-global-land-cover-characterization-glcc",
      LTER == "HBR" ~ "https://www.mrlc.gov/viewer/",
      LTER == "KRR" ~ "https://www.mrlc.gov/viewer/",
      LTER == "LMP" ~ "https://www.mrlc.gov/viewer/",
      LTER == "LUQ" ~ "https://www.mrlc.gov/data/nlcd-2001-land-cover-puerto-rico",
      LTER == "MCM" ~ "-",
      LTER == "NWT" ~ "https://www.mrlc.gov/viewer/",
      LTER == "Sagehen" ~ "https://www.mrlc.gov/viewer/",
      LTER == "UMR" ~ "https://www.mrlc.gov/data/nlcd-2019-land-cover-conus",
      T ~ as.character(LTER) ) ) %>%
  # And clarify the meaning of the original 'dataSource' columns
  rename(shapeSource = dataSource) %>%
  rename(shapeSourceLink = dataSourceLink) %>%
  # And move all of the source columns together
  relocate(ends_with('Source'), .after = everything()) %>%
  relocate(ends_with('SourceLink'), .after = everything())

# Check contents
names(combo)

# Check McMurdo fix
combo %>%
  filter(LTER == "MCM") %>%
  select(uniqueID, major_cover, major_rock, coverSource, rockSource)
  ## Looks great!

# Let's actually split the source information into a separate dataframe
bib <- combo %>%
  # Get just the desired columns
  select(LTER, contains('Source', ignore.case = F)) %>%
  # And move them around
  relocate(contains('rock'), .after = LTER) %>%
  relocate(contains('cover'), .after = LTER) %>%
  relocate(contains('shape'), .after = LTER) %>%
  # And strip to just one row per LTER
  unique() %>%
  # Change the McMurdo shape source info to latest word
  mutate(
    shapeSource = ifelse(LTER == "MCM",
                         yes = "No shapefile because glacial drainage likely more important than topography",
                         no = shapeSource),
    shapeSourceLink = ifelse(LTER == "MCM", yes = "-", no = shapeSourceLink) )

# And remove sources from the 'actual' data
actual <- combo %>%
  select(-contains('Source', ignore.case = F))

# Check the contents of the bibliography file
bib

# And make sure only intended columns were dropped from data object
setdiff(names(combo), names(actual))

# Export both files to share with the working group
write.csv(actual, file = "extracted-data/SilicaSites_litho_and_landcover.csv",
          na = '', row.names = F)
write.csv(bib, file = "extracted-data/SilicaSites_data_source_info.csv",
          na = '', row.names = F)

# (Further) simplify cover categories
simp <- combo %>%
  # Remove unneeded-columns
  select(LTER, stream, uniqueID, starts_with('cover_')) %>%
  # pivot to long format
  pivot_longer(cols = starts_with('cover_'),
               names_to = "cover_categories",
               values_to = "percents") %>%
  # Drop "cover_" prefixes
  mutate(cover_categories = gsub("cover_", "", cover_categories)) %>%
  # Streamline remaining categories
  mutate(cover_categories = case_when(
    cover_categories == "deciduous_forest" ~ "forest",
    cover_categories == "deciduous_needleaf_forest" ~ "forest",
    cover_categories == "evergreen_forest" ~ "forest",
    cover_categories == "mixed_forest" ~ "forest",
    cover_categories == "cultivated_crops" ~ "impacted",
    cover_categories == "low_medium_intensity_developed" ~ "impacted",
    cover_categories == "pasture_hay" ~ "impacted",
    cover_categories == "grassland" ~ "shrub_grass",
    cover_categories == "shrubland" ~ "shrub_grass",
    cover_categories == "barren_land" ~ "barren",
    cover_categories == "open_water" ~ "water",
    cover_categories == "wetland" ~ "wetland",
    T ~ as.character(cover_categories) ) ) %>%
  # Add the "cover_" prefix back on
  mutate(cover_categories = paste0("cover_", cover_categories)) %>%
  # Make it a dataframe rather than a list tibble
  as.data.frame() %>%
  # Group by everything except cover category info
  group_by(LTER, stream, uniqueID, cover_categories) %>%
  # Summarize through new categories
  summarise(percents = sum(percents, na.rm = T)) %>%
  # Pivot to wide format
  pivot_wider(names_from = cover_categories,
              values_from = percents) %>%
  # Bring rocks back in
  left_join(y = actual %>%
              select(LTER, stream, uniqueID,
                     starts_with('rocks_')),
            by = c("LTER", "stream", "uniqueID"))

# Examine that
names(simp)

# Export this to for later use
write.csv(simp,
          file = "extracted-data/SilicaSites_simplified_litho_and_landcover.csv",
          na = '', row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# End ------------------------------------------------------------------------
