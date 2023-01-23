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
#          MCD12Q2 (v. 061) - Identify Files ----
## ------------------------------------------------------- ##

# Make an empty list
file_list <- list()

# Identify files for each region
for(region in c("north-america-usa", "north-america-arctic",
                "cropped-russia-west", "cropped-russia-west-2",
                "cropped-russia-center", "cropped-russia-east",
                "puerto-rico", "scandinavia")){
  
  # Identify files in that folder
  file_df <- data.frame("region" = region,
                        "files" = dir(path = file.path(path, "raw-driver-data", 
                                                       "raw-greenup", region),
                                      pattern = "MCD12Q2.006_Greenup_"))
  
  # Add that set of files to the list
  file_list[[region]] <- file_df }

# Wrangle the list
file_all <- file_list %>%
  # Unlist the loop's output
  purrr::map_dfr(.f = dplyr::select, dplyr::everything()) %>%
  # Identify date from file name
  dplyr::mutate(date_raw = stringr::str_extract(string = files, 
                                                pattern = "_doy[[:digit:]]{7}")) %>%
  # Simplify that column
  dplyr::mutate(date = gsub(pattern = "_doy", replacement = "", x = date_raw)) %>%
  # Identify day of year & year
  dplyr::mutate(year = stringr::str_sub(string = date, start = 1, end = 4)) %>%
  # Drop 'raw' version
  dplyr::select(-date_raw)

# Glimpse it
dplyr::glimpse(file_all)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all')))

## ------------------------------------------------------- ##
#     .       Greenup - Bounding Box Check ----
## ------------------------------------------------------- ##
# Let's check to make sure each of my manual bounding boxes fits the sites for that region

# Filter to only one row per 'region'
(viz_files <- file_all %>% 
   # Find first file per region
   dplyr::group_by(region) %>%
   dplyr::summarize(files = dplyr::first(x = files)) %>%
   dplyr::ungroup() )

# Read in one raster of each region
rast1 <- terra::rast(file.path(path, "raw-driver-data",  "raw-greenup",
                               viz_files$region[1], viz_files$files[1]))
rast2 <- terra::rast(file.path(path, "raw-driver-data",  "raw-greenup",
                               viz_files$region[2], viz_files$files[2]))
rast3 <- terra::rast(file.path(path, "raw-driver-data",  "raw-greenup",
                               viz_files$region[3], viz_files$files[3]))
rast4 <- terra::rast(file.path(path, "raw-driver-data",  "raw-greenup",
                               viz_files$region[4], viz_files$files[4]))
rast5 <- terra::rast(file.path(path, "raw-driver-data",  "raw-greenup",
                               viz_files$region[5], viz_files$files[5]))
rast6 <- terra::rast(file.path(path, "raw-driver-data",  "raw-greenup",
                               viz_files$region[6], viz_files$files[6]))
rast7 <- terra::rast(file.path(path, "raw-driver-data",  "raw-greenup",
                               viz_files$region[7], viz_files$files[7]))
rast8 <- terra::rast(file.path(path, "raw-driver-data",  "raw-greenup",
                               viz_files$region[8], viz_files$files[8]))

# Plot each "tile" of data against the watersheds polygons
## Russia Composite (Cropped)
frame_rast <- terra::rast(terra::ext(55, 140, 45, 80))
suppressWarnings(plot(frame_rast, axes = T, reset = F, main = "Russia COMPOSITE"))
plot(rast1, axes = T, add = T)
plot(rast3, axes = T, add = T)
plot(rast4, axes = T, add = T)
plot(sheds, axes = T, add = T)

## Russia East (Cropped)
plot(rast2, axes = T, reset = F, main = viz_files$region[2])
plot(sheds, axes = T, add = T)

## North America Arctic
plot(rast5, axes = T, reset = F, main = viz_files$region[5])
plot(sheds, axes = T, add = T)

## USA
plot(rast6, axes = T, reset = F, main = viz_files$region[6])
plot(sheds, axes = T, add = T)

## Puerto Rico
plot(rast7, axes = T, reset = F, main = viz_files$region[7])
plot(sheds, axes = T, add = T)

## Scandinavia
plot(rast8, axes = T, reset = F, main = viz_files$region[8])
plot(sheds, axes = T, add = T)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all')))

## ------------------------------------------------------- ##
#              Greenup Day - Extract ----
## ------------------------------------------------------- ##

for (a_year in unique(file_all$year)){
  # Subset to one year
  one_year_data <- dplyr::filter(file_all, year == a_year)
  
  # Make a list to house extracted information for a year
  year_list <- list()
  
    for (i in 1:nrow(one_year_data)){
      # Read in the raster
      original_raster <- terra::rast(file.path(path, "raw-driver-data", "converted-greenup-data", one_year_data$files[i]))
      
      # Convert each raster from the MODIS Sinusoidal coordinate system to WGS84
      reprojected_raster <- terra::project(original_raster, "+proj=longlat +datum=WGS84")
      
      # Rename the 2 layers
      names(reprojected_raster) <- c("Onset_Greenness_Increase1", "Onset_Greenness_Increase2")
      
      # Extract all possible information from that dataframe
      ex_data <- exactextractr::exact_extract(x = reprojected_raster, y = sheds, 
                                              include_cols = c("river_id"),
                                              progress = FALSE) %>%
      # Unlist to dataframe
      purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
      # Drop coverage fraction column
      dplyr::select(-coverage_fraction) %>%         
      # Make new relevant columns
      dplyr::mutate(year = a_year,
                    .after = river_id)
      
      # Add this dataframe to the list we made 
      year_list[[i]] <- ex_data
      
      # End message
      message("Finished extracting raster ", i, " of ", nrow(one_year_data)) 
    }
  
  # Assemble a file name for this extraction
  export_name <- paste0("greenup_extract_", a_year, ".csv")
  
  # Wrangle the output of the within-year extraction
  full_data <- year_list %>%
    # Unlist to dataframe
    purrr::map_dfr(.f = dplyr::select, dplyr::everything()) %>%
    # Handle the summarization within river (potentially across multiple rasters' pixels)
    dplyr::group_by(river_id, year) %>%
    dplyr::summarize(mean1 = mean(Onset_Greenness_Increase1, na.rm = T),
                     mean2 = mean(Onset_Greenness_Increase2, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # Drop the year column
    dplyr::select(-year) %>%
    # Rename the columns
    dplyr::rename_with(.fn = ~paste0("greenup_cycle1_", a_year, "_days_since_jan1_1970"), .cols = mean1) %>%
    dplyr::rename_with(.fn = ~paste0("greenup_cycle2_", a_year, "_days_since_jan1_1970"), .cols = mean2) 
  
  # Export this file for a given year
  write.csv(x = full_data, row.names = F, na = '',
            file = file.path(path, "raw-driver-data", "converted-greenup-data",
                             "_partial-extracted", export_name))
  
  # End message
  message("Finished wrangling output for ", a_year) 
  
}

## ------------------------------------------------------- ##
#              Greenup Day - Export ----
## ------------------------------------------------------- ##

# Identify extracted data
done_files <- dir(file.path(path, "raw-driver-data", "converted-greenup-data", "_partial-extracted"))

# Make an empty list
full_out <- list()

# Read all of these files in
for(k in 1:length(done_files)){
  
  # Read in the kth file
  full_out[[k]] <- read.csv(file = file.path(path, "raw-driver-data", "converted-greenup-data", "_partial-extracted", done_files[k]))
  
  # Finish
  message("Retrieved file ", k, " of ", length(done_files))
  }

# Unlist that list
out_df <- full_out %>%
  purrr::reduce(dplyr::left_join, by = 'river_id')

# Glimpse it
dplyr::glimpse(out_df)

# Let's get ready to export
greenup_export <- sites %>%
  # Join the rock data
  dplyr::left_join(y = out_df, by = "river_id")

# Check it out
dplyr::glimpse(greenup_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized greenup data
write.csv(x = greenup_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_greenup.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_greenup.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1-X0WjsBg-BTS_ows1jj6n_UehSVE9zwU"))
