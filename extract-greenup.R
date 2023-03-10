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
  # Identify year
  dplyr::mutate(year = stringr::str_sub(string = date, start = 1, end = 4)) %>%
  # Drop 'raw' version
  dplyr::select(-date_raw) %>%
  # Identify greenup cycle
  dplyr::mutate(cycle = stringr::str_extract(string = files, 
                                             pattern = "_[[:digit:]]{1}_")) %>%
  # Simplify that column
  dplyr::mutate(cycle = gsub(pattern = "_", replacement = "", x = cycle))

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
#           Greenup Day - Extract Cycle 0 ----
## ------------------------------------------------------- ##\

# Silence `dplyr::summarize` preemptively
options(dplyr.summarise.inform = F)

# Specify driver
focal_driver <- "raw-greenup"

# Identify files we've already extracted from
done_files <- data.frame("files" = dir(file.path(path, "raw-driver-data",
                                                  focal_driver,
                                                  "_partial-extracted"))) %>%
   tidyr::separate(col = files, remove = F,
                   into = c("junk", "junk2", "year", "cycle", "file_ext")) %>%
  dplyr::mutate(cycle = gsub(pattern = "[[:alpha:]]", replacement = "", x = cycle)) %>%
  # Make a year-cycle column
   dplyr::mutate(year_cycle = paste0(year, "_", cycle))

# Remove completed files from the set of all possible files
not_done <- file_all %>%
   dplyr::mutate(year_cycle = paste0(year, "_", cycle)) %>%
   dplyr::filter(!year_cycle %in% done_files$year_cycle)

# Create a definitive object of files to extract
file_set <- not_done # Uncomment if want to only do only undone extractions
# file_set <- file_all # Uncomment if want to do all extractions

# Split the files into cycle 0 and cycle 1 files
cycle0_files <- file_set %>%
  dplyr::filter(cycle == 0)

cycle1_files <- file_set %>%
  dplyr::filter(cycle == 1)

for (a_year in unique(cycle0_files$year)){
  # Subset to one year
  one_year_data <- dplyr::filter(cycle0_files, year == a_year)
  
  # Make a list to house extracted information for a year
  year_list <- list()
  
  for (i in 1:nrow(one_year_data)){
    # Read in the raster
    gr_raster <- terra::rast(file.path(path, "raw-driver-data", "raw-greenup", one_year_data$region[i], one_year_data$files[i]))
    
    # Extract all possible information from that dataframe
    ex_data <- exactextractr::exact_extract(x = gr_raster, y = sheds, 
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
  export_name <- paste0("greenup_extract_", a_year, "_cycle0", ".csv")
  
  # Wrangle the output of the within-year extraction
  full_data <- year_list %>%
    # Unlist to dataframe
    purrr::map_dfr(.f = dplyr::select, dplyr::everything()) %>%
    # Handle the summarization within river (potentially across multiple rasters' pixels)
    dplyr::group_by(river_id, year) %>%
    dplyr::summarize(greenup_cycle0_days_since_jan1_1970 = round(mean(value, na.rm = T))) %>%
    dplyr::ungroup() %>%
    # Convert the days since Jan 1, 1970 to the actual date
    dplyr::mutate(greenup_cycle0_YYYYMMDD = lubridate::as_date(greenup_cycle0_days_since_jan1_1970, origin ="1970-01-01")) %>%
    # Drop unnecessary columns
    dplyr::select(-year, -greenup_cycle0_days_since_jan1_1970)
    
  # Export this file for a given year
  write.csv(x = full_data, row.names = F, na = '',
            file = file.path(path, "raw-driver-data", "raw-greenup",
                             "_partial-extracted", export_name))
  
  # End message
  message("Finished wrangling output for ", a_year) 
}

## ------------------------------------------------------- ##
#           Greenup Day - Extract Cycle 1 ----
## ------------------------------------------------------- ##\

# Silence `dplyr::summarize` preemptively
options(dplyr.summarise.inform = F)

# Specify driver
focal_driver <- "raw-greenup"

# Identify files we've already extracted from
done_files <- data.frame("files" = dir(file.path(path, "raw-driver-data",
                                                 focal_driver,
                                                 "_partial-extracted"))) %>%
  tidyr::separate(col = files, remove = F,
                  into = c("junk", "junk2", "year", "cycle", "file_ext")) %>%
  dplyr::mutate(cycle = gsub(pattern = "[[:alpha:]]", replacement = "", x = cycle)) %>%
  # Make a year-cycle column
  dplyr::mutate(year_cycle = paste0(year, "_", cycle))

# Remove completed files from the set of all possible files
not_done <- file_all %>%
  dplyr::mutate(year_cycle = paste0(year, "_", cycle)) %>%
  dplyr::filter(!year_cycle %in% done_files$year_cycle)

# Create a definitive object of files to extract
file_set <- not_done # Uncomment if want to only do only undone extractions
# file_set <- file_all # Uncomment if want to do all extractions

# Split the files into cycle 0 and cycle 1 files
cycle0_files <- file_set %>%
  dplyr::filter(cycle == 0)

cycle1_files <- file_set %>%
  dplyr::filter(cycle == 1)

for (a_year in unique(cycle1_files$year)){
  # Subset to one year
  one_year_data <- dplyr::filter(cycle1_files, year == a_year)
  
  # Make a list to house extracted information for a year
  year_list <- list()
  
  for (i in 1:nrow(one_year_data)){
    # Read in the raster
    gr_raster <- terra::rast(file.path(path, "raw-driver-data", "raw-greenup", one_year_data$region[i], one_year_data$files[i]))
    
    # Extract all possible information from that dataframe
    ex_data <- exactextractr::exact_extract(x = gr_raster, y = sheds, 
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
  export_name <- paste0("greenup_extract_", a_year, "_cycle1", ".csv")
  
  # Wrangle the output of the within-year extraction
  full_data <- year_list %>%
    # Unlist to dataframe
    purrr::map_dfr(.f = dplyr::select, dplyr::everything()) %>%
    # Handle the summarization within river (potentially across multiple rasters' pixels)
    dplyr::group_by(river_id, year) %>%
    dplyr::summarize(greenup_cycle0_days_since_jan1_1970 = round(mean(value, na.rm = T))) %>%
    dplyr::ungroup() %>%
    # Convert the days since Jan 1, 1970 to the actual date
    dplyr::mutate(greenup_cycle0_YYYYMMDD = lubridate::as_date(greenup_cycle0_days_since_jan1_1970, origin ="1970-01-01")) %>%
    # Drop unnecessary columns
    dplyr::select(-year, -greenup_cycle0_days_since_jan1_1970)
  
  # Export this file for a given year
  write.csv(x = full_data, row.names = F, na = '',
            file = file.path(path, "raw-driver-data", "raw-greenup",
                             "_partial-extracted", export_name))
  
  # End message
  message("Finished wrangling output for ", a_year) 
}



## ------------------------------------------------------- ##
#              Greenup Day - Export ----
## ------------------------------------------------------- ##

# Identify the extracted cycle 0 data
done_cycle0 <- dir(file.path(path, "raw-driver-data", "raw-greenup", "_partial-extracted"), pattern = "cycle0") 

# Identify the extracted cycle 1 data
done_cycle1 <- dir(file.path(path, "raw-driver-data", "raw-greenup", "_partial-extracted"), pattern = "cycle1") 

# Make an empty list to store our cycle 0 tables --------------------------------------------------------------------------------
full_out_cycle0 <- list()

# Read all of these files in
for(k in 1:length(done_cycle0)){
  
  # Read in the kth file
  full_out_cycle0[[k]] <- read.csv(file = file.path(path, "raw-driver-data", "raw-greenup", "_partial-extracted", done_cycle0[k]))
  
  # Finish
  message("Retrieved file ", k, " of ", length(done_cycle0))
  }

# Unlist that list
out_df_cycle0 <- full_out_cycle0 %>%
  purrr::reduce(dplyr::left_join, by = 'river_id') 

# Rename columns
names(out_df_cycle0) <- c("river_id", "greenup_cycle0_2001MMDD", "greenup_cycle0_2002MMDD", "greenup_cycle0_2003MMDD",
                          "greenup_cycle0_2004MMDD", "greenup_cycle0_2005MMDD", "greenup_cycle0_2006MMDD", "greenup_cycle0_2007MMDD",
                          "greenup_cycle0_2008MMDD", "greenup_cycle0_2009MMDD", "greenup_cycle0_2010MMDD", "greenup_cycle0_2011MMDD",
                          "greenup_cycle0_2012MMDD", "greenup_cycle0_2013MMDD", "greenup_cycle0_2014MMDD", "greenup_cycle0_2015MMDD",
                          "greenup_cycle0_2016MMDD", "greenup_cycle0_2017MMDD", "greenup_cycle0_2018MMDD", "greenup_cycle0_2019MMDD")

# Glimpse it
dplyr::glimpse(out_df_cycle0)

# Make an empty list to store our cycle 1 tables --------------------------------------------------------------------------------
full_out_cycle1 <- list()

# Read all of these files in
for(k in 1:length(done_cycle1)){
  
  # Read in the kth file
  full_out_cycle1[[k]] <- read.csv(file = file.path(path, "raw-driver-data", "raw-greenup", "_partial-extracted", done_cycle1[k]))
  
  # Finish
  message("Retrieved file ", k, " of ", length(done_cycle1))
}

# Unlist that list
out_df_cycle1 <- full_out_cycle1 %>%
  purrr::reduce(dplyr::left_join, by = 'river_id')

# Rename columns
names(out_df_cycle1) <- c("river_id", "greenup_cycle1_2001MMDD", "greenup_cycle1_2002MMDD", "greenup_cycle1_2003MMDD",
                          "greenup_cycle1_2004MMDD", "greenup_cycle1_2005MMDD", "greenup_cycle1_2006MMDD", "greenup_cycle1_2007MMDD",
                          "greenup_cycle1_2008MMDD", "greenup_cycle1_2009MMDD", "greenup_cycle1_2010MMDD", "greenup_cycle1_2011MMDD",
                          "greenup_cycle1_2012MMDD", "greenup_cycle1_2013MMDD", "greenup_cycle1_2014MMDD", "greenup_cycle1_2015MMDD",
                          "greenup_cycle1_2016MMDD", "greenup_cycle1_2017MMDD", "greenup_cycle1_2018MMDD", "greenup_cycle1_2019MMDD")

# Glimpse it
dplyr::glimpse(out_df_cycle1)

# Join cycle 0 and cycle 1 tables together -------------------------------------------------------------------------------------
out_df <- dplyr::left_join(out_df_cycle0, out_df_cycle1)

# Move columns around
out_df_v2 <- out_df %>% 
  dplyr::relocate(contains("2001"), .after = river_id) %>%
  dplyr::relocate(contains("2002"), .after = greenup_cycle1_2001MMDD) %>%
  dplyr::relocate(contains("2003"), .after = greenup_cycle1_2002MMDD) %>%
  dplyr::relocate(contains("2004"), .after = greenup_cycle1_2003MMDD) %>%
  dplyr::relocate(contains("2005"), .after = greenup_cycle1_2004MMDD) %>%
  dplyr::relocate(contains("2006"), .after = greenup_cycle1_2005MMDD) %>%
  dplyr::relocate(contains("2007"), .after = greenup_cycle1_2006MMDD) %>%
  dplyr::relocate(contains("2008"), .after = greenup_cycle1_2007MMDD) %>%
  dplyr::relocate(contains("2009"), .after = greenup_cycle1_2008MMDD) %>%
  dplyr::relocate(contains("2010"), .after = greenup_cycle1_2009MMDD) %>%
  dplyr::relocate(contains("2011"), .after = greenup_cycle1_2010MMDD) %>%
  dplyr::relocate(contains("2012"), .after = greenup_cycle1_2011MMDD) %>%
  dplyr::relocate(contains("2013"), .after = greenup_cycle1_2012MMDD) %>%
  dplyr::relocate(contains("2014"), .after = greenup_cycle1_2013MMDD) %>%
  dplyr::relocate(contains("2015"), .after = greenup_cycle1_2014MMDD) %>%
  dplyr::relocate(contains("2016"), .after = greenup_cycle1_2015MMDD) %>%
  dplyr::relocate(contains("2017"), .after = greenup_cycle1_2016MMDD) %>%
  dplyr::relocate(contains("2018"), .after = greenup_cycle1_2017MMDD) 
  

# Let's get ready to export
greenup_export <- sites %>%
  # Join the greenup data
  dplyr::left_join(y = out_df_v2, by = "river_id")

# Check it out
dplyr::glimpse(greenup_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized greenup data
write.csv(x = greenup_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_greenup.csv"))

# Upload to GoogleDrive
# googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_greenup.csv"),
#                          overwrite = T,
#                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK"))

# End ----
