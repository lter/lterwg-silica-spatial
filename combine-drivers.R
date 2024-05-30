## ------------------------------------------------------- ##
# Silica WG - Combine Drivers
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Identify all extracted driver data (i.e., spatial data) and combine into a single file
## This makes the analysis phase easier on the WG as they can skip redundant joining steps

## ------------------------------------------------------- ##
                    # Housekeeping ----
## ------------------------------------------------------- ##

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, NCEAS/scicomptools, googledrive, readxl, magrittr, supportR)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

# Load in shape area check
area_check <- read.csv(file.path(path, "shape_checks", "artisanal_shape_area_check.csv"))

# Check it out
dplyr::glimpse(area_check)

# Load in site names with lat/longs
sites <- readxl::read_excel(path = file.path(path, "site-coordinates",
                                             "silica-coords_RAW.xlsx")) %>%
  # Pare down to minimum needed columns
  dplyr::select(LTER, Stream_Name, Discharge_Site_Name, Shapefile_Name) %>%
  # Drop duplicate rows (if any)
  dplyr::distinct() %>%
  # Remove any watersheds without a shapefile
  dplyr::filter(!is.na(Shapefile_Name) & 
                  nchar(Shapefile_Name) != 0 &
                  !Shapefile_Name %in% c("?", "MISSING")) %>%
  # Attach area check information
  dplyr::left_join(area_check, by = c("LTER", "Shapefile_Name"))
  
# Check it out
dplyr::glimpse(sites)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites')))

## ------------------------------------------------------- ##
# Combine Extracted Data ----
## ------------------------------------------------------- ##
# List current extracted data
(extracted_data <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK"), pattern = ".csv") %>%
  dplyr::filter(name != "all-data_si-extract.csv"))

# Download all files
purrr::walk2(.x = extracted_data$id, .y = extracted_data$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path(path, "extracted-data", .y),
                                                overwrite = T))

# Make an empty list
data_list <- list()

# Read the files into the list
for(file_name in extracted_data$name){
  data_list[[file_name]] <- read.csv(file = file.path(path, "extracted-data", file_name))
}

# Duplicate the sites object
driver_df <- sites

# For each file
for(k in 1:length(data_list)){
  # Processing message
  message("Adding ", names(data_list[k]), " to 
          output dataframe")
  
  # Left join onto the driver dataframe and overwrite the object
  driver_df %<>%
    dplyr::left_join(y = data_list[[k]],
                     by = c("LTER", "Stream_Name", "Discharge_Site_Name", "Shapefile_Name"))
}

# Glimpse what we wind up with
dplyr::glimpse(driver_df)

# Check all column names (should be *a lot*)
names(driver_df)

# Check for dropped rivers (i.e., rows)
## Stream names (chemistry river names)
supportR::diff_check(old = unique(sites$Stream_Name), new = unique(driver_df$Stream_Name))
## Discharge file names (discharge river names)
supportR::diff_check(old = unique(sites$Discharge_Site_Name), 
                     new = unique(driver_df$Discharge_Site_Name))

# Export this
write.csv(x = driver_df, na = '', row.names = F,
          file = file.path(path, "extracted-data", "all-data_si-extract_2_20240511.csv"))

# And upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "all-data_si-extract_2_20240511.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK"))

# End ----
