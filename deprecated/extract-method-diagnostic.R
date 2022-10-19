## ----------------------------------------------------------------------- ##
                        # LTER WG: Silica Synthesis
## ----------------------------------------------------------------------- ##
# Written by:
## Nick J Lyon + 

# Purpose:
## 1 - Compare different methods of extraction of landcover data to ensure consistent outcomes regardless of package(s)/function(s) used
## 2 - Compare different coordinate reference systems (CRS) across the same extraction method to see if we should be re-projecting from WGS84 to something that accounts for changing degree:area relationship at high latitudes

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

# Grab the shapefiles for all of the watersheds
sheds_all <- sf::st_read('watershed-shapefiles/SilicaSynthesis_allWatersheds.shp')

# Simplify to only one watershed
sheds_sf <- sheds_all %>%
  select(uniqueID) %>%
  filter(uniqueID == "GRO_Yukon")

# Check that out
str(sheds_sf)

# Extraction Housekeeping -----------------------------------------------------

# Get a raster version
sheds_rast <- terra::vect(sheds_sf)

# Read in raster to extract from
data_all <- terra::rast("extracted-data/raw-landcover-data/MODIS_GLCC_Global_1992/gbigbpgeo20.tif")

# Crop to band of interest
st_bbox(sheds_sf)
ext(data_all)
data_rast <- terra::crop(data_all, terra::ext(-179, -100, 55, 79))
ext(data_rast)
st_bbox(sheds_sf)

# Check CRS of all objects
st_crs(sheds_sf) # WGS84
crs(sheds_rast) # WGS84
crs(data_rast) # WGS84
## Looks good!

# Plot them to be sure
# (`if` bit just to make the whole block run together)
if(1 == 1){
par(mfrow = c(2, 1))
## First data raster with sf overlay
plot(data_rast, axes = T, reset = F, main = 'sf on raster')
plot(sheds_sf, axes = T, add = T)
## Then data raster with raster overlay
plot(data_rast, axes = T, reset = F, main = 'raster on raster')
plot(sheds_rast, axes = T, add = T)
par(mfrow = c(1, 1))}
## Looks identical!

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sheds_sf', 'sheds_rast', 'data_rast')))

# Method 1: `exactextractr::exact_extract` ---------------------------------

# Strip data from a raster using an sf object
data_list <- exactextractr::exact_extract(x = data_rast, y = sheds_sf,
                                          include_cols = "uniqueID")

# Make that a dataframe
data_df_ext <- data_list %>%
  # Get into dataframe from list
  map_dfr(select, c(uniqueID, value)) %>%
  # Count instances of each pixel category
  group_by(uniqueID, value) %>%
  summarise(num_pixels_ext = n())

# Look at it
data_df_ext

# Method 2: `terra::extract` -----------------------------------------------

# Strip data from a raster using another raster
data_terra_v1 <- terra::extract(x = data_rast, y = sheds_rast)

# Process it to match the structure of the other method's extracted data
data_df_terra <- data_terra_v1 %>%
  # Get column names to match
  rename(uniqueID = ID, value = gbigbpgeo20) %>%
  # Re-create the uniqueID column
  mutate(uniqueID = ifelse(uniqueID == 1, yes = "GRO_Yukon", no = "GRO_Yukon")) %>%
  # Count instances of each pixel category
  group_by(uniqueID, value) %>%
  summarise(num_pixels_terra = n())

# Look at it
data_df_terra

# Combine Method Outputs for Comparison ------------------------------------

# Make them the same object to make it easier to compare
data_all <- data_df_ext %>%
  full_join(data_df_terra, by = c("uniqueID", "value")) %>%
  # Identify differences
  mutate(ext_vs_terra_diff = num_pixels_ext - num_pixels_terra)

# Check contents
data_all

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sheds_sf')))

# CRS Housekeeping -----------------------------------------------------------

# Prepare two CRS variants of the sf polygon
sheds_sf_4326 <- sheds_sf %>%
  st_transform(crs = 4326)
sheds_sf_5070 <- sheds_sf %>%
  st_transform(crs = 5070)

# Check them both
st_crs(sheds_sf_4326)
st_crs(sheds_sf_5070)

# Make them both rasters
sheds_rast_4326 <- terra::vect(sheds_sf_4326)
sheds_rast_5070 <- terra::vect(sheds_sf_5070)

# Check that they transferred correctly
crs(sheds_rast_4326)
crs(sheds_rast_5070)

# Read in the lithology data raster
data_rast_raw <- terra::rast("extracted-data/raw-lithology-data/glim_wgs84_0point5deg.txt.asc")

# Check CRS
crs(data_rast_raw)

# Re-project into the different CRS's to check it out
## NOTE: will do this once but save the product, then comment out the workflow and just read in the different CRS version
## 5070
# data_rast_5070 <- terra::project(data_rast_raw, y = 'epsg:5070', method = 'near')
# terra::writeRaster(data_rast_5070, "extracted-data/raw-lithology-data/lithology_epsg5070_raster.tiff", overwrite = T)
## 4326
# data_rast_4326 <- terra::project(data_rast_raw, y = 'epsg:4326', method = "near")
# terra::writeRaster(data_rast_4326, "extracted-data/raw-lithology-data/lithology_epsg4326_raster.tiff", overwrite = T)

# Read in the re-projected file and check the CRS
data_rast_5070 <- terra::rast("extracted-data/raw-lithology-data/lithology_epsg5070_raster.tiff")
crs(data_rast_5070)
data_rast_4326 <- terra::rast("extracted-data/raw-lithology-data/lithology_epsg4326_raster.tiff")
crs(data_rast_4326)

# Make an exploratory plot
# (`if` bit just to make the whole block run together)
if(1 == 1){
  par(mfrow = c(2, 1))
  ## First data raster with sf overlay
  plot(data_rast_5070, axes = T, reset = F, main = 'EPSG 5070')
  plot(sheds_rast_5070, axes = T, add = T)
  ## Then data raster with raster overlay
  plot(data_rast_4326, axes = T, reset = F, main = 'EPSG 4326')
  plot(sheds_rast_4326, axes = T, add = T)
  par(mfrow = c(1, 1))}

# Extract Data from both CRS Variants --------------------------------------

# Double check that the CRS match
crs(data_rast_4326) == crs(sheds_rast_4326)
## They don't....

# Check what they are
crs(data_rast_4326)
crs(sheds_rast_4326)

# Extract from EPSG 4326 first
data_df_4326 <- data_rast_4326 %>%
  terra::extract(y = sheds_rast_4326) %>%
  # Get column names to match
  rename(uniqueID = ID, value = glim_wgs84_0point5deg.txt) %>%
  # Re-create the uniqueID column
  mutate(uniqueID = ifelse(uniqueID == 1, yes = "GRO_Yukon", no = "GRO_Yukon")) %>%
  # Count instances of each pixel category
  group_by(uniqueID, value) %>%
  summarise(num_pixels_4326 = n())

# Look at it
data_df_4326

# Check the other CRS
crs(data_rast_5070) == crs(sheds_rast_5070)

# Now extract from EPSG 5070
data_df_5070 <- data_rast_5070 %>%
  terra::extract(y = sheds_rast_5070) %>%
  # Get column names to match
  rename(uniqueID = ID, value = glim_wgs84_0point5deg.txt) %>%
  # Re-create the uniqueID column
  mutate(uniqueID = ifelse(uniqueID == 1, yes = "GRO_Yukon", no = "GRO_Yukon")) %>%
  # Count instances of each pixel category
  group_by(uniqueID, value) %>%
  summarise(num_pixels_5070 = n())

# Look at this one
data_df_5070

# Combine Pixel Counts for Comparison --------------------------------------

# Make them the same object to make it easier to compare
data_both_crs <- data_df_4326 %>%
  full_join(data_df_5070, by = c("uniqueID", "value")) %>%
  # Identify differences
  mutate(num_diff = num_pixels_4326 - num_pixels_5070)

# We want percents
data_both_crs_perc <- data_both_crs %>%
  # Pivot to long format
  pivot_longer(cols = starts_with('num_pixels'),
               names_to = 'source', values_to = 'pixel_ct') %>%
  # Group and compute percent differences
  group_by(uniqueID, source) %>%
  mutate(total_pixels = sum(pixel_ct, na.rm = T)) %>%
  ungroup() %>%
  group_by(uniqueID, source, num_diff, value) %>%
  mutate(pixel_perc = (pixel_ct / total_pixels) * 100 ) %>%
  # Remove count columns
  select(-pixel_ct, -total_pixels) %>%
  # Edit the source column
  mutate(source = gsub("num_", "perc_", source)) %>%
  # Pivot to wide format
  pivot_wider(names_from = source, values_from = pixel_perc) %>%
  # Calculate percent difference
  mutate(perc_diff = round(perc_pixels_4326 - perc_pixels_5070, digits = 2))

# Check contents
data_both_crs_perc

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sheds_sf')))

# End ----------------------------------------------------------------------
