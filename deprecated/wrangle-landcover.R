## ------------------------------------------------------- ##
      # Silica WG - Wrangle Spatial Data - Land Cover
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Do pre-extraction wrangling of landcover data

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
          # Land Cover - CRS Transformation ----
## ------------------------------------------------------- ##

# Two of the NLCD rasters I downloaded manually don't match the CRS of the other watersheds
## Alaska doesn't match, presumably because it is from 2016 versus 2019 for the others
## The continental US (from which we'll cut UMR's bounding box) doesn't match presumably because downloading from the web app versus downloading the whole raster differs (though it shouldn't in an ideal world)

# Both of these need to be re-projected to the CRS of the other rasters and saved out for convenience.

# This is done here to
## 1. Allow for a for loop to handle as many watersheds as possible and
## 2. because it is computationally intense, we only want to run it once and then leave it alone.

# All of the following lines were run once and then commented out but retained for posterity.

# Process Alaska Data (for ARC)
## 1) Read in 'raw' (i.e., direct from NLCD) file
arc_raw <- terra::rast(x = file.path(path, "extracted-data", "raw-landcover-data",
                                     "NLCD-ARC-bbox-2016", 
                                     "NLCD_2016_Land_Cover_AK_20200724_1UJcf1tMJdCcymwLngFW.tiff"))
crs(arc_raw)

## 2) We need to transform the CRS so that it matches the others
arc_fix <- terra::project(x = arc_raw, y = 'epsg:5070')
crs(arc_fix)

## 3) Write it out for later use
terra::writeRaster(x = arc_fix, filename = file.path(path, "extracted-data", 
                                                     "raw-landcover-data",
                                                     "NLCD-ARC-bbox-2016", 
                                                     "NLCD_ARC_bbox_2016_raw.tiff"))

# Process Continental US Data (for UMR)
## 1) Get raw variant
usa_raw <- terra::rast(x = file.path(path, "extracted-data", "raw-landcover-data",
                                     "NLCD-ContinentalUS-2019",
                                     "nlcd_2019_land_cover_l48_20210604.img"))

## 2) Crop it (the entire CONUS raster is too big to transform)
ext(usa_raw)
usa_crop <- terra::crop(x = usa_raw, y = terra::ext(0, 1000000, 1750000, 3310005))
ext(usa_crop)

## 3) Re-project this cropped (thus smaller) object into the preferred CRS
### Note this takes ~15 minutes so try not to re-run it if you can avoid it
usa_fix <- terra::project(usa_crop, y = 'epsg:5070')
crs(usa_fix)

## 4) Save out fixed raster
terra::writeRaster(x = usa_fix, filename = file.path(path, "extracted-data", 
                                                     "raw-landcover-data",
                                                     "NLCD-UMR-bbox-2019",
                                                     "NLCD_UMR_bbox_2019_raw.tiff"))

# End ----
