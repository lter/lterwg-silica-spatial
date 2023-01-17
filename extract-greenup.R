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

# Only need to run this section once - (DONE)

# List all the raw hdf files
# files <- dir(file.path(path, "raw-driver-data", "raw-greenup-data"), pattern = ".hdf")
# 
# # Make new filenames for the converted files
# tif_filenames <- substr(files,1,41)
# tif_filenames <- paste0(tif_filenames, ".tif")
# tif_filenames
# 
# # Convert hdf files to rasters 
# # The Greenup layer is the second layer so sd_index = 2
# for (i in 1:length(files)){
#   gdal_translate(src_dataset = file.path(path, "raw-driver-data", "raw-greenup-data", files[i]), dst_dataset = file.path(path, "raw-driver-data", "converted-greenup-data", tif_filenames[i]), sd_index = 2)
# }

## ------------------------------------------------------- ##
#.         MCD12Q2 (v. 061) - Identify Files ----
## ------------------------------------------------------- ##

# List all the tif files
tif_files <- dir(file.path(path, "raw-driver-data", "converted-greenup-data"), pattern = ".tif$")

file_all <- data.frame("files" = dir(path = file.path(path, "raw-driver-data", "converted-greenup-data"),
                                     pattern = ".tif$")) %>%
  # Identify date from file name
  dplyr::mutate(date_raw = stringr::str_extract(string = files, 
                                                pattern = "[[:digit:]]{7}")) %>%
  dplyr::mutate(year = stringr::str_sub(string = date_raw, start = 1, end = 4))


## ------------------------------------------------------- ##
#              Greenup Day - Extract ----
## ------------------------------------------------------- ##

for (a_year in c("2001", "2002")){
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
}
