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

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites')))

## ------------------------------------------------------- ##
# Combine Extracted Data ----
## ------------------------------------------------------- ##
# # List current extracted data
# extracted_data <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK"), pattern = ".csv") %>%
#   dplyr::filter(name != "all-data_si-extract.csv")
# 
# # Make an empty list
# data_list <- list()
# 
# # Download these files
# for(file_name in extracted_data$name){
#   
#   # Filter to desired filed
#   wanted <- extracted_data %>%
#     dplyr::filter(name == file_name)
#   
#   # Download
#   googledrive::drive_download(file = googledrive::as_id(wanted$id),
#                               path = file.path(path, "extracted-data", file_name),
#                               overwrite = T)
#   
#   # Read the CSV and add to the list
#   data_list[[file_name]] <- read.csv(file = file.path(path, "extracted-data", file_name))
#   
# } # End loop
# 
# # Get a duplicate of the 'sites' object
# out_df <- sites
# 
# # Now loop across remaining elements and left join each
# for(k in 1:length(data_list)){
#   
#   # Add each bit to the existing dataframe
#   out_df <- out_df %>%
#     # Left join by all non-data columns
#     dplyr::left_join(y = data_list[[k]],
#                      by = c("LTER", "Stream_Name", "Discharge_File_Name", "drainSqKm", 
#                             "river_id", "lat", "long")) %>%
#     # Drop duplicated columns
#     unique()
# }
# 
# # Check for dropped rivers (i.e., rows)
# ## Stream names (chemistry river names)
# setdiff(x = unique(sites$Stream_Name), y = unique(out_df$Stream_Name))
# setdiff(y = unique(sites$Stream_Name), x = unique(out_df$Stream_Name))
# ## Discharge file names (discharge river names)
# setdiff(x = unique(sites$Discharge_File_Name), y = unique(out_df$Discharge_File_Name))
# setdiff(y = unique(sites$Discharge_File_Name), x = unique(out_df$Discharge_File_Name))
# 
# # Take a look
# dplyr::glimpse(out_df)
# 
# # Export this
# write.csv(x = out_df, na = '', row.names = F,
#           file = file.path(path, "extracted-data", "all-data_si-extract.csv"))
# 
# # And upload to GoogleDrive
# googledrive::drive_upload(media = file.path(path, "extracted-data", "all-data_si-extract.csv"),
#                           overwrite = T,
#                           path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK"))

# End ----
