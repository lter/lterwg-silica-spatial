## ------------------------------------------------------- ##
        # Silica WG - Extract Spatial Data - Greenup
## ------------------------------------------------------- ##
# Written by:
## Angel Chen

# Purpose:
## Using the watershed shapefiles created in "wrangle-watersheds.R"
## Extract the following data: GREENUP

## ------------------------------------------------------- ##
                       # Housekeeping ----
## ------------------------------------------------------- ##
# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, stars, terra, exactextractr, NCEAS/scicomptools, 
                googledrive, readxl)

# Clear environment
rm(list = ls())

# Silence `summarize`
options(dplyr.summarise.inform = F)

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

# Load in site names with lat/longs
sites <- readxl::read_excel(path = file.path(path, "site-coordinates",
                                            "silica-coords_RAW.xlsx")) %>%
 ## Pare down to minimum needed columns
 dplyr::select(LTER, Stream_Name, Discharge_Site_Name, Shapefile_Name) %>%
 ## Drop duplicate rows (if any)
 dplyr::distinct() %>%
 ## Remove any watersheds without a shapefile
 dplyr::filter(!is.na(Shapefile_Name) & 
                 nchar(Shapefile_Name) != 0 &
                 !Shapefile_Name %in% c("?", "MISSING"))

# Check it out
dplyr::glimpse(sites)

# Grab the shapefiles the previous script (see PURPOSE section) created
sheds <- sf::st_read(dsn = file.path(path, "site-coordinates", "silica-watersheds.shp")) %>%
 # Expand names to what they were before
 dplyr::rename(Shapefile_Name = file_name,
               expert_area_km2 = exp_area,
               shape_area_km2 = real_area)

# Check that out
dplyr::glimpse(sheds)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

## ------------------------------------------------------- ##
           # MCD12Q2 (v. 061) - Identify Files ----
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
                # Greenup Day - Extract Prep ----
## ------------------------------------------------------- ##

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

## ------------------------------------------------------- ##
            # Greenup Day - Extract Cycle 0 ----
## ------------------------------------------------------- ##

# For each cycle 0 year
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
                                            include_cols = c("LTER", "Shapefile_Name"),
                                            progress = FALSE) %>%
      # Unlist to dataframe
      purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
      # Drop coverage fraction column
      dplyr::select(-coverage_fraction) %>%         
      # Make new relevant columns
      dplyr::mutate(year = a_year,
                    .after = Shapefile_Name)
    
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
    dplyr::group_by(LTER, Shapefile_Name, year) %>%
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
  message("Finished wrangling output for ", a_year) }

## ------------------------------------------------------- ##
            # Greenup Day - Extract Cycle 1 ----
## ------------------------------------------------------- ##

# Extract cycle 1 too
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
                                            include_cols = c("LTER", "Shapefile_Name"),
                                            progress = FALSE) %>%
      # Unlist to dataframe
      purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
      # Drop coverage fraction column
      dplyr::select(-coverage_fraction) %>%         
      # Make new relevant columns
      dplyr::mutate(year = a_year,
                    .after = Shapefile_Name)
    
    # Add this dataframe to the list we made 
    year_list[[i]] <- ex_data
    
    # End message
    message("Finished extracting raster ", i, " of ", nrow(one_year_data)) }
  
  # Assemble a file name for this extraction
  export_name <- paste0("greenup_extract_", a_year, "_cycle1", ".csv")
  
  # Wrangle the output of the within-year extraction
  full_data <- year_list %>%
    # Unlist to dataframe
    purrr::map_dfr(.f = dplyr::select, dplyr::everything()) %>%
    # Handle the summarization within river (potentially across multiple rasters' pixels)
    dplyr::group_by(LTER, Shapefile_Name, year) %>%
    dplyr::summarize(greenup_cycle1_days_since_jan1_1970 = round(mean(value, na.rm = T))) %>%
    dplyr::ungroup() %>%
    # Convert the days since Jan 1, 1970 to the actual date
    dplyr::mutate(greenup_cycle1_YYYYMMDD = lubridate::as_date(greenup_cycle1_days_since_jan1_1970, origin ="1970-01-01")) %>%
    # Drop unnecessary columns
    dplyr::select(-year, -greenup_cycle1_days_since_jan1_1970)
  
  # Export this file for a given year
  write.csv(x = full_data, row.names = F, na = '',
            file = file.path(path, "raw-driver-data", "raw-greenup",
                             "_partial-extracted", export_name))
  
  # End message
  message("Finished wrangling output for ", a_year) }

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all')))

## ------------------------------------------------------- ##
            # Greenup Day - Process Extraction ----
## ------------------------------------------------------- ##

# Identify the extracted cycle 0 data
done_cycle0 <- dir(file.path(path, "raw-driver-data", "raw-greenup", "_partial-extracted"), pattern = "cycle0") 

# Identify the extracted cycle 1 data
done_cycle1 <- dir(file.path(path, "raw-driver-data", "raw-greenup", "_partial-extracted"), pattern = "cycle1") 

# Make an empty list to store our cycle 0 tables
full_out_cycle0 <- list()
full_out_cycle1 <- list()

# Read all of these files in
for(k in 1:length(done_cycle0)){
  
  # Read in the kth file
  file <- read.csv(file = file.path(path, "raw-driver-data", "raw-greenup", "_partial-extracted", done_cycle0[k]))
  
  # Identify the year
  file_year <- stringr::str_sub(string = file$greenup_cycle0_YYYYMMDD[1],
                                start = 1, end = 4)
  
  # Rename the data column
  names(file) <- c("river_id", paste0("greenup_cycle0_", file_year, "MMDD"))
  
  # Add it to the list
  full_out_cycle0[[k]] <- file
  
  # Finish
  message("Retrieved file ", k, " of ", length(done_cycle0)) }

# Unlist that list
out_df_cycle0 <- full_out_cycle0 %>%
  purrr::reduce(dplyr::left_join, by = c("LTER", "Shapefile_Name")) 

# Glimpse it
dplyr::glimpse(out_df_cycle0)

# Read in all of the cycle 1 files
for(k in 1:length(done_cycle1)){
  
  # Read in the kth file
  file <- read.csv(file = file.path(path, "raw-driver-data", "raw-greenup", "_partial-extracted", done_cycle1[k]))
  
  # Drop empty years
  non_empty_years <- file %>%
    dplyr::filter(nchar(greenup_cycle1_YYYYMMDD) != 0)
    
  # Identify the year
  file_year <- stringr::str_sub(string = non_empty_years$greenup_cycle1_YYYYMMDD[1],
                                start = 1, end = 4)
  
  # Rename the data column
  names(file) <- c("river_id", paste0("greenup_cycle1_", file_year, "MMDD"))
  
  # Add it to the list
  full_out_cycle1[[k]] <- file
  
  # Finish
  message("Retrieved file ", k, " of ", length(done_cycle1)) }

# Unlist that list
out_df_cycle1 <- full_out_cycle1 %>%
  purrr::reduce(dplyr::left_join, by = c("LTER", "Shapefile_Name"))

# Glimpse it
dplyr::glimpse(out_df_cycle1)

## ------------------------------------------------------- ##
                    # Greenup Day - Export ----
## ------------------------------------------------------- ##

# Join cycle 0 and cycle 1 tables together
out_df <- out_df_cycle0 %>%
  dplyr::left_join(out_df_cycle1, by = c("LTER", "Shapefile_Name")) %>%
  # Move columns around
  dplyr::relocate(contains("2001"), contains("2002"), contains("2003"),
                  contains("2004"), contains("2005"), contains("2006"),
                  contains("2007"), contains("2008"), contains("2009"),
                  contains("2010"), contains("2011"), contains("2012"),
                  contains("2013"), contains("2014"), contains("2015"),
                  contains("2016"), contains("2017"), contains("2018"),
                  contains("2019"),
                  .after = Shapefile_Name)

# Glimpse this too
dplyr::glimpse(out_df)

# Let's get ready to export
greenup_export <- sites %>%
  # Join the greenup data
  dplyr::left_join(y = out_df_v2, by = c("LTER", "Shapefile_Name"))

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
                         path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK"))

# End ----
