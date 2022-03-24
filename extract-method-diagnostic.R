## ----------------------------------------------------------------------- ##
                        # LTER WG: Silica Synthesis
## ----------------------------------------------------------------------- ##
# Written by:
## Nick J Lyon + 

# Purpose:
## Compare different methods of extraction of landcover data to ensure consistent outcomes regardless of package(s)/function(s) used

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

# Housekeeping -------------------------------------------------------------

# Grab the shapefiles for all of the watersheds
sheds_all <- sf::st_read('watershed-shapefiles/SilicaSynthesis_allWatersheds.shp')

# Simplify to only one watershed
sheds_sf <- sheds_all %>%
  select(uniqueID) %>%
  filter(uniqueID == "GRO_Yukon")

# Check that out
str(sheds_sf)

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

# End ----------------------------------------------------------------------
