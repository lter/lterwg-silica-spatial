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

## ------------------------------------------------------- ##
          # List Files - Russia West & Center ----
## ------------------------------------------------------- ##

# Had to download several separate bounding boxes to get Russian GRO rivers
# Need to crop these so there is no overlap between them

# Make an empty list
file_list <- list()

# Identify all tiles of this region for each driver
for(driver in c("raw-evapo-modis16a2-v006")){
  
  # Identify files for each region
  west_df <- data.frame("driver_id" = driver,
                        "region" = "russia-west",
                        "files" = dir(path = file.path(path, "raw-driver-data", 
                                                       driver, "russia-west")))
  west2_df <- data.frame("driver_id" = driver,
                         "region" = "russia-west-2",
                         "files" = dir(path = file.path(path, "raw-driver-data", 
                                                        driver, "russia-west-2")))
  ctr_df <- data.frame("driver_id" = driver,
                       "region" = "russia-center",
                       "files" = dir(path = file.path(path, "raw-driver-data", 
                                                      driver, "russia-center")))
  
  # Bind them together
  file_df <- west_df %>%
    dplyr::bind_rows(y = west2_df) %>%
    dplyr::bind_rows(y = ctr_df)
  
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
              # Crop - Russia West & Center ----
## ------------------------------------------------------- ##

# Create needed export folders
for(driver in unique(file_all$driver_id)){
  for(reg in unique(file_all$region)){
  dir.create(path = file.path(path, "raw-driver-data", driver,
                              paste0("cropped-", reg)),
             showWarnings = F) } }

# Identify number of files to crop
(file_total <- nrow(file_all))

# For each file
for(k in 1:file_total){

  # Read in that raster
  rast <- terra::rast(file.path(path, "raw-driver-data",  file_all$driver_id[k],
                                file_all$region[k], file_all$files[k]))
  
  # Define desired extent
  if(file_all$region[k] == "russia-west"){ bbox <- terra::ext(57, 87, 46, 73) }
  if(file_all$region[k] == "russia-west-2"){ bbox <- terra::ext(87, 122, 46, 73) }
  if(file_all$region[k] == "russia-center"){ bbox <- terra::ext(122, 142, 46, 73) }
  
  # Crop using that bounding box
  cropped_rast <- terra::crop(x = rast, y = bbox)
  
  # Write that raster out
  terra::writeRaster(x = cropped_rast, overwrite = T,
                     filename = file.path(path, "raw-driver-data", file_all$driver_id[k],
                                          paste0("cropped-", file_all$region[k]),
                                          file_all$files[k]))
  
  # Completion message
  message("Completed processing for file ", k, " of ", file_total) }

# Identify one of each of the new ones
test_files <- file_all %>%
  dplyr::group_by(driver_id, region) %>%
  dplyr::summarize(files = dplyr::first(files))

# Read in one of each raster
## Uncropped
rast1 <- terra::rast(file.path(path, "raw-driver-data",  test_files$driver_id[1],
                               test_files$region[1], test_files$files[1]))
rast2 <- terra::rast(file.path(path, "raw-driver-data",  test_files$driver_id[2],
                               test_files$region[2], test_files$files[2]))
rast3 <- terra::rast(file.path(path, "raw-driver-data",  test_files$driver_id[3],
                               test_files$region[3], test_files$files[3]))
## Cropped
rast4 <- terra::rast(file.path(path, "raw-driver-data",  test_files$driver_id[1],
                               paste0("cropped-", test_files$region[1]), test_files$files[1]))
rast5 <- terra::rast(file.path(path, "raw-driver-data",  test_files$driver_id[2],
                               paste0("cropped-", test_files$region[2]), test_files$files[2]))
rast6 <- terra::rast(file.path(path, "raw-driver-data",  test_files$driver_id[3],
                               paste0("cropped-", test_files$region[3]), test_files$files[3]))
# Grab the shapefiles of the watersheds
sheds <- sf::st_read(dsn = file.path(path, "site-coordinates", "silica-watersheds.shp"))

# Make an empty raster 'frame' around the whole area
frame_rast <- terra::rast(terra::ext(55, 140, 45, 80))

# Test plot to see the issue and how we fixed it by cropping
## Uncropped (slight overlap with each other)
suppressWarnings(plot(frame_rast, axes = T, reset = F, main = "Russia COMPOSITE (Uncropped)"))
plot(rast1, axes = T, add = T)
plot(rast2, axes = T, add = T)
plot(rast3, axes = T, add = T)
plot(sheds, axes = T, add = T)
## Cropped 
suppressWarnings(plot(frame_rast, axes = T, reset = F, main = "Russia COMPOSITE (Cropped)"))
plot(rast4, axes = T, add = T)
plot(rast5, axes = T, add = T)
plot(rast6, axes = T, add = T)
plot(sheds, axes = T, add = T)

# Clean up environment
rm(list = setdiff(ls(), c('path')))

# End ----
