## ------------------------------------------------------- ##
      # Silica WG - Extract Spatial Data - Soil Order
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Using the watershed shapefiles created in "id-watershed-polygons.R"
## Extract the following data: SOIL ORDER

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
              # Soil Order - Extract ----
## ------------------------------------------------------- ##
# Pull in the raw lithology data
soil_raw <- terra::rast(x = file.path(path, "raw-driver-data", "raw-soil", "TAXNWRB_250m_ll.tif"))

# Check CRS
crs(soil_raw)

# Experimental plotting
plot(soil_raw, reset = F, axes = T)
plot(sheds, add = T, axes = T)

# Strip out rocks from our polygons
soil_out <- exactextractr::exact_extract(x = soil_raw, y = sheds,
                                         include_cols = c("river_id")) %>%
  # Above returns a list so switch it to a dataframe
  purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
  # Filter out NAs
  dplyr::filter(!is.na(value))

# Check that dataframe
dplyr::glimpse(soil_out)

## ------------------------------------------------------- ##
                # Soil Order - Summarize ----
## ------------------------------------------------------- ##
# Summarize that dataframe to be more manageable
soil_v2 <- soil_out %>%
  # Count pixels per category and river
  dplyr::group_by(river_id, value) %>%
  dplyr::summarize(pixel_ct = dplyr::n()) %>%
  dplyr::ungroup()

# Glimpse this
dplyr::glimpse(soil_v2)

# Need to convert integer values into text


# Then identify "major" (i.e., dominant) soil types per river



## ------------------------------------------------------- ##
# Soil Order - Export ----
## ------------------------------------------------------- ##
# Let's get ready to export
soil_export <- sites %>%
  # Join the rock data
  dplyr::left_join(y = soil_actual, by = c("river_id"))

# Check it out
dplyr::glimpse(soil_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized lithology data
write.csv(x = soil_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_air-temp.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_air-temp.csv"),
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
