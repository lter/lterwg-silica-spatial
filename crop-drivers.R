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
              # List Raw Files - Russia East ----
## ------------------------------------------------------- ##

# The bounding box for the "russia-east" appEEARS data extends slightly too far west
# Need to slice off a thin strip of the western edge of this 'tile'

# Make an empty list
file_list <- list()

# Identify all tiles of this region for each driver
for(driver in c("raw-evapo-modis16a2-v006", "raw-snow-modis10a2-v006", "raw-greenup", "raw-npp")){

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
         # Identify Cropped Files - Russia East ----
## ------------------------------------------------------- ##

# Make an empty list
crop_list <- list()

# Identify all tiles of this region for each driver
for(driver in c("raw-evapo-modis16a2-v006", "raw-snow-modis10a2-v006", "raw-greenup")){
  
  # Identify files
  crop_df <- data.frame("driver_id" = driver,
                        "files" = dir(path = file.path(path, "raw-driver-data", 
                                                       driver, "cropped-russia-east")))
  
  # Add to list
  crop_list[[driver]] <- crop_df }

# Wrangle the list
crop_all <- crop_list %>%
  # Unlist the loop's output
  purrr::map_dfr(.f = dplyr::select, dplyr::everything())

# Glimpse it
dplyr::glimpse(crop_all)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'file_all', 'crop_all')))

## ------------------------------------------------------- ##
                  # Crop - Russia East ----
## ------------------------------------------------------- ##

# Identify undone files
undone <- file_all %>%
  dplyr::filter(!files %in% crop_all$files)

# Create needed export folders
for(driver in unique(undone$driver_id)){
  dir.create(path = file.path(path, "raw-driver-data", driver, "cropped-russia-east"),
             showWarnings = F) }

# Identify number of files to crop
(file_total <- length(unique(undone$files)))

# For each file
for(k in 1:file_total){

# Read in that raster
rast <- terra::rast(file.path(path, "raw-driver-data",  undone$driver_id[k],
                              "russia-east", undone$files[k]))

# Define desired extent
bbox <- terra::ext(143, 172, 60, 73)

# Crop using that bounding box
cropped_rast <- terra::crop(x = rast, y = bbox)

# Write that raster out
terra::writeRaster(x = cropped_rast, overwrite = T,
                   filename = file.path(path, "raw-driver-data",  undone$driver_id[k],
                                        "cropped-russia-east", undone$files[k]))

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
    # List Raw Files - Russia West, West 2 & Center ----
## ------------------------------------------------------- ##

# Had to download several separate bounding boxes to get Russian GRO rivers
# Need to crop these so there is no overlap between them

# Make an empty list
file_list <- list()

# Identify all tiles of this region for each driver
for(driver in c("raw-evapo-modis16a2-v006", "raw-snow-modis10a2-v006", "raw-greenup", "raw-npp")){
  
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
  purrr::map_dfr(.f = dplyr::select, dplyr::everything()) %>%
  # Make a combination column of region + file name
  dplyr::mutate(id = paste0(region, "_", files))

# Glimpse it
dplyr::glimpse(file_all)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'file_all')))

## ------------------------------------------------------- ##
 # Identify Cropped Files - Russia West, West 2 & Center ----
## ------------------------------------------------------- ##

# Make an empty list
crop_list <- list()

# Identify all tiles of this region for each driver
for(driver in c("raw-evapo-modis16a2-v006", "raw-snow-modis10a2-v006", "raw-greenup")){
  
  # Identify files for each region
  crop_west_df <- data.frame("driver_id" = driver,
                        "region" = "russia-west",
                        "files" = dir(path = file.path(path, "raw-driver-data", 
                                                       driver, "cropped-russia-west")))
  crop_west2_df <- data.frame("driver_id" = driver,
                         "region" = "russia-west-2",
                         "files" = dir(path = file.path(path, "raw-driver-data", 
                                                        driver, "cropped-russia-west-2")))
  crop_ctr_df <- data.frame("driver_id" = driver,
                       "region" = "russia-center",
                       "files" = dir(path = file.path(path, "raw-driver-data", 
                                                      driver, "cropped-russia-center")))
  
  # Bind them together
  crop_df <- crop_west_df %>%
    dplyr::bind_rows(y = crop_west2_df) %>%
    dplyr::bind_rows(y = crop_ctr_df)
  
  # Add to list
  crop_list[[driver]] <- crop_df }

# Wrangle the list
crop_all <- crop_list %>%
  # Unlist the loop's output
  purrr::map_dfr(.f = dplyr::select, dplyr::everything()) %>%
  # Make a combination column of region + file name
  dplyr::mutate(id = paste0(region, "_", files))

# Glimpse it
dplyr::glimpse(crop_all)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'file_all', 'crop_all')))

## ------------------------------------------------------- ##
          # Crop - Russia West, West 2 & Center ----
## ------------------------------------------------------- ##

# Identify undone files
undone <- file_all %>%
  dplyr::filter(!id %in% crop_all$id)

# Create needed export folders
for(driver in unique(undone$driver_id)){
  for(reg in unique(undone$region)){
  dir.create(path = file.path(path, "raw-driver-data", driver,
                              paste0("cropped-", reg)),
             showWarnings = F) } }

# Identify number of files to crop
(file_total <- nrow(undone))

# For each file
for(k in 1:file_total){

  # Read in that raster
  rast <- terra::rast(file.path(path, "raw-driver-data",  undone$driver_id[k],
                                undone$region[k], undone$files[k]))
  
  # Define desired extent
  if(undone$region[k] == "russia-west"){ bbox <- terra::ext(57, 87, 46, 73) }
  if(undone$region[k] == "russia-west-2"){ bbox <- terra::ext(87, 122, 46, 73) }
  if(undone$region[k] == "russia-center"){ bbox <- terra::ext(122, 142, 46, 73) }
  
  # Crop using that bounding box
  cropped_rast <- terra::crop(x = rast, y = bbox)
  
  # Write that raster out
  terra::writeRaster(x = cropped_rast, overwrite = T,
                     filename = file.path(path, "raw-driver-data", undone$driver_id[k],
                                          paste0("cropped-", undone$region[k]),
                                          undone$files[k]))
  
  # Completion message
  message("Completed processing for file ", k, " of ", file_total) }

# Identify one file for each region we just cropped
test_files <- file_all %>%
  dplyr::group_by(driver_id, region) %>%
  dplyr::summarize(files = dplyr::first(files)) %>%
  dplyr::ungroup()

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
