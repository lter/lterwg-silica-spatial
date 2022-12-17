## ------------------------------------------------------- ##
#       Silica WG - Extract Spatial Data - Greenup
## ------------------------------------------------------- ##
# Written by:
## Angel Chen

# Purpose:
## Using the watershed shapefiles created in "id-watershed-polygons.R"
## Extract the following data: GREENUP

## ------------------------------------------------------- ##
#                    Housekeeping ----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, ncdf4, stars, terra, exactextractr, 
                 NCEAS/scicomptools, googledrive)

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
#.    MCD12Q2 (v. 061) - Convert Files to Rasters ----
## ------------------------------------------------------- ##

# List all the raw hdf files
files <- dir(file.path(path, "raw-driver-data", "raw-greenup-data"), pattern = ".hdf")

# Make new filenames for the converted files
tif_filenames <- substr(files,1,41)
tif_filenames <- paste0(tif_filenames, ".tif")
tif_filenames

# Convert hdf files to rasters 
# The Greenup layer is the second layer so sd_index = 2
for (i in 1:length(files)){
  gdal_translate(src_dataset = file.path(path, "raw-driver-data", "raw-greenup-data", files[i]), dst_dataset = file.path(path, "raw-driver-data", "converted-greenup-data", tif_filenames[i]), sd_index = 2)
}

## ------------------------------------------------------- ##
#.      MCD12Q2 (v. 061) - Reproject Rasters ----
## ------------------------------------------------------- ##

# List all the tif files
tif_files <- dir(file.path(path, "raw-driver-data", "converted-greenup-data"), pattern = ".tif$")

# Make an empty list
raster_list <- list()

# Convert each raster from the MODIS Sinusoidal coordinate system to WGS84
for (i in 1:10){
  original_raster <- terra::rast(file.path(path, "raw-driver-data", "converted-greenup-data", tif_files[i]))
  reprojected_raster <- terra::project(original_raster, "+proj=longlat +datum=WGS84")
  raster_list[[i]] <- reprojected_raster
  
}


## ------------------------------------------------------- ##
#          Greenup Day - Extract ----
## ------------------------------------------------------- ##

# Make a list to house extracted information
full_out <- list()

