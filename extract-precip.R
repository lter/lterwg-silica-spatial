## ------------------------------------------------------- ##
      # Silica WG - Extract Spatial Data - Precipitation
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Using the watershed shapefiles created in "id-watershed-polygons.R"
## Extract the following data: PRECIPITATION (MONTHLY)

## ------------------------------------------------------- ##
                    # Housekeeping ----
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
                    # Precip - Extract ----
## ------------------------------------------------------- ##
# The GPCP Precip data has each month of each year as a separate netCDF file

# List all of these files
precip_files <- dir(path = file.path(path, "raw-driver-data", "raw-gpcp-precip"))

# Check it out
head(precip_files)

# Count number of files
(file_ct <- length(precip_files))

# Create an empty list to store extracted information in
out_list <- list()

# Now we'll do our extraction for each of these files
for(k in 1:file_ct){
  
  # Identify focal file
  focal_precip <- precip_files[k]
  
  # Read in the file as netCDF
  prec_nc <- ncdf4::nc_open(filename = file.path(path, "raw-driver-data", "raw-gpcp-precip",
                                                 focal_precip))
  
  # Read it as a raster too
  prec_rast <- terra::rast(x = file.path(path, "raw-driver-data", "raw-gpcp-precip", 
                                         focal_precip))
  
  # Rotate so longitude is from -180 to 180 (rather than 0 to 360)
  rotated <- terra::rotate(x = prec_rast$precip)
  
  # Identify time of this layer
  layer_time <- terra::time(x = rotated)
  
  # Strip out the relevant bit
  small_out_df <- exactextractr::exact_extract(x = rotated, y = sheds, 
                                               include_cols = c("river_id"),
                                               progress = F) %>%
    # Above returns a list so switch it to a dataframe
    purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
    # Filter out NAs
    dplyr::filter(!is.na(value)) %>%
    # Average precip within river ID
    dplyr::group_by(river_id) %>%
    dplyr::summarize(avg_mm_precip_per_day = mean(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # Add a column for what timestamp this is
    dplyr::mutate(time = layer_time, .before = dplyr::everything())
  
  # Add it to the list
  out_list[[focal_precip]] <- small_out_df
  
  # Success message
  message("Processing complete for ", layer_time, " (number ", k, " of ", file_ct, ")")
  
}

# Exploratory plot one of what we just extracted
plot(rotated, axes = T, reset = F)
plot(sheds, axes = T, add = T)

# Can also double check other aspects of what we've processed
print(prec_nc)
names(prec_rast)
print(rotated$precip)

## ------------------------------------------------------- ##
                    # Precip - Summarize ----
## ------------------------------------------------------- ##
# Unlist that list
full_out_df <- out_list %>%
  purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
  # Strip out year and month
  dplyr::mutate(year = stringr::str_sub(string = time, start = 1, end = 4),
                month = stringr::str_sub(string = time, start = 6, end = 7),
                .after = time)

# Glimpse it
dplyr::glimpse(full_out_df)

# Make sure we have all 12 months for every year
## Could easily have missed downloading one because they are separate files
full_out_df %>%
  # Count number of months for each year
  dplyr::group_by(year) %>%
  dplyr::summarize(month_ct = length(unique(month))) %>%
  dplyr::ungroup() %>%
  # Drop all years where we have 12 months
  dplyr::filter(month_ct != 12)

# Summarize within month across years
year_df <- full_out_df %>%
  # Do summarization
  dplyr::group_by(river_id, year) %>%
  dplyr::summarize(value = mean(avg_mm_precip_per_day, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Make more informative year column
  dplyr::mutate(name = paste0("precip_", year, "_mm")) %>%
  # Drop simple year column
  dplyr::select(-year) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = name,
                     values_from = value)

# Glimpse this
dplyr::glimpse(year_df)

# Then summarize within year across months
month_df <- full_out_df %>%
  # Do summarization
  dplyr::group_by(river_id, month) %>%
  dplyr::summarize(value = mean(avg_mm_precip_per_day, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Change month number to letters
  dplyr::mutate(month_simp = dplyr::case_when(
    month == "01" ~ "jan",
    month == "02" ~ "feb",
    month == "03" ~ "mar",
    month == "04" ~ "apr",
    month == "05" ~ "may",
    month == "06" ~ "jun",
    month == "07" ~ "jul",
    month == "08" ~ "aug",
    month == "09" ~ "sep",
    month == "10" ~ "oct",
    month == "11" ~ "nov",
    month == "12" ~ "dec")) %>%
  # Make more informative month column
  dplyr::mutate(name = paste0("precip_", month_simp, "_mm")) %>%
  # Drop simple month column
  dplyr::select(-month, -month_simp) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = name,
                     values_from = value)

# Glimpse this
dplyr::glimpse(month_df)

# Combine these dataframes
prec_actual <- year_df %>%
  dplyr::left_join(y = month_df, by = "river_id")

# Glimpse again
dplyr::glimpse(prec_actual)

## ------------------------------------------------------- ##
                  # Precip - Export ----
## ------------------------------------------------------- ##
# Let's get ready to export
prec_export <- sites %>%
  # Join the rock data
  dplyr::left_join(y = prec_actual, by = c("river_id"))

# Check it out
dplyr::glimpse(prec_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized lithology data
write.csv(x = prec_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_precip.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_precip.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1-X0WjsBg-BTS_ows1jj6n_UehSVE9zwU"))

## ------------------------------------------------------- ##
              # Combine Extracted Data ----
## ------------------------------------------------------- ##
# Clear environment
rm(list = setdiff(ls(), c('path', 'sites')))

# List current extracted data
extracted_data <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1-X0WjsBg-BTS_ows1jj6n_UehSVE9zwU")) %>%
  dplyr::filter(name != "all-data_si-extract.csv")

# Make an empty list
data_list <- list()

# Download these files
for(file_name in extracted_data$name){
  
  # Filter to desired filed
  wanted <- extracted_data %>%
    dplyr::filter(name == file_name)
  
  # Download
  googledrive::drive_download(file = googledrive::as_id(wanted$id),
                              path = file.path(path, "extracted-data", file_name),
                              overwrite = T)
  
  # Read the CSV and add to the list
  data_list[[file_name]] <- read.csv(file = file.path(path, "extracted-data", file_name))
  
} # End loop

# Get a duplicate of the 'sites' object
out_df <- sites

# Now loop across remaining elements and left join each
for(k in 1:length(data_list)){
  
  # Add each bit to the existing dataframe
  out_df <- out_df %>%
    # Left join by all non-data columns
    dplyr::left_join(y = data_list[[k]],
                     by = c("LTER", "Stream_Name", "Discharge_File_Name", "drainSqKm", 
                            "river_id", "lat", "long")) %>%
    # Drop duplicated columns
    unique()
}

# Check for dropped rivers (i.e., rows)
## Stream names (chemistry river names)
setdiff(x = unique(sites$Stream_Name), y = unique(out_df$Stream_Name))
setdiff(y = unique(sites$Stream_Name), x = unique(out_df$Stream_Name))
## Discharge file names (discharge river names)
setdiff(x = unique(sites$Discharge_File_Name), y = unique(out_df$Discharge_File_Name))
setdiff(y = unique(sites$Discharge_File_Name), x = unique(out_df$Discharge_File_Name))

# Take a look
dplyr::glimpse(out_df)

# Export this
write.csv(x = out_df, na = '', row.names = F,
          file = file.path(path, "extracted-data", "all-data_si-extract.csv"))

# And upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "all-data_si-extract.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1-X0WjsBg-BTS_ows1jj6n_UehSVE9zwU"))

# End ----
