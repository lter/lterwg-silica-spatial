## ------------------------------------------------------- ##
            # Silica WG - Crop Spatial Data
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Crop spatial data retrieved using the AppEEARS database
## https://appeears.earthdatacloud.nasa.gov/

## ------------------------------------------------------- ##
                      # Housekeeping ----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, ncdf4, stars, terra)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

## ------------------------------------------------------- ##
                # List Files - Russia East ----
## ------------------------------------------------------- ##

# The bounding box for the "russia-east" appEEARS data extends slightly too far west
# Need to slice off a thin strip of the western edge of this 'tile'

# Make an empty list
file_list <- list()

# Identify all tiles of this region for each driver
for(driver in c("raw-evapo-modis16a2-v006")){

# Identify files
file_df <- data.frame("driver_id" = driver,
                      "files" = dir(path = file.path(path, "raw-driver-data", 
                                                     driver, "russia-east")))

# Add to list
file_list[[driver]] <- file_df }

# Wrangle the list
file_all <- file_list %>%
  # Unlist the loop's output
  purrr::map_dfr(.f = dplyr::select, dplyr::everything())

# Glimpse it
dplyr::glimpse(file_all)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'file_all')))

## ------------------------------------------------------- ##
                  # Crop - Russia East ----
## ------------------------------------------------------- ##

# Create needed export folders
for(driver in unique(file_all$driver_id)){
  dir.create(path = file.path(path, "raw-driver-data", driver, "cropped-russia-east"),
             showWarnings = F) }

# Identify number of files to crop
(file_total <- length(unique(file_all$files)))

# For each file
for(k in 1:file_total){

# Read in that raster
rast <- terra::rast(file.path(path, "raw-driver-data",  file_all$driver_id[k],
                              "russia-east", file_all$files[k]))

# Check extent
terra::ext(rast)

# Define desired extent
bbox <- terra::ext(143, 172, 60, 73)

# Crop using that bounding box
cropped_rast <- terra::crop(x = rast, y = bbox)

# Write that raster out
terra::writeRaster(x = cropped_rast, overwrite = T,
                   filename = file.path(path, "raw-driver-data",  file_all$driver_id[k],
                                        "cropped-russia-east", file_all$files[k]))

# Completion message
message("Completed processing for file ", k, " of ", file_total) }

# Test plot to see the issue and how we fixed it by cropping
## Grab the shapefiles of the watersheds
sheds <- sf::st_read(dsn = file.path(path, "site-coordinates", "silica-watersheds.shp"))
## Uncropped (slight overlap with another watershed)
plot(rast, axes = T, reset = F, main = "russia-east UNCROPPED")
plot(sheds, axes = T, add = T)
## Cropped
plot(cropped_rast, axes = T, reset = F, main = "russia-east CROPPED")
plot(sheds, axes = T, add = T)

# Clean up environment
rm(list = setdiff(ls(), c('path')))

# End ----
