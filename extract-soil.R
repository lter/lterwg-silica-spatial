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
librarian::shelf(tidyverse, sf, stars, terra, exactextractr, NCEAS/scicomptools, googledrive)

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
soil_raw <- terra::rast(x = file.path(path, "raw-driver-data", "raw-soil", 
                                      "TAXOUSDA_250m_suborder_classes.tif"))

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
  dplyr::filter(!is.na(value)) %>%
  # Count pixels per category and river
  dplyr::group_by(river_id, value) %>%
  dplyr::summarize(pixel_ct = dplyr::n()) %>%
  dplyr::ungroup() 

# Check that dataframe
dplyr::glimpse(soil_out)

## ------------------------------------------------------- ##
              # Soil Order - Index Prep ----
## ------------------------------------------------------- ##
# Read in soil order index
soil_index_raw <- read.csv(file = file.path(path, "raw-driver-data", "raw-soil", 
                                        "TAXOUSDA_250m_suborder_classes_legend.csv"))

# Glimpse it
dplyr::glimpse(soil_index_raw)

# See if there are any differences between "Group" and "Generic"
unique(soil_index_raw$Group)
unique(soil_index_raw$Generic)

# Simplify this object to just what we need
soil_index <- soil_index_raw %>%
  # Coerce soil class columns to lowercase
  dplyr::mutate(specific_soil = tolower(x = Group),
                generic_soil = tolower(x = Generic)) %>%
  # Pare down to only desired columns
  ## Also rename integer code column to match how it is called in the extracted dataframe
  dplyr::select(value = Number, specific_soil, generic_soil) %>%
  # Drop the group column (pending Silica input to the contrary)
  dplyr::select(-specific_soil)

# Glimpse this as well
dplyr::glimpse(soil_index)

## ------------------------------------------------------- ##
                # Soil Order - Summarize ----
## ------------------------------------------------------- ##

# Compare values in index to extracted values from raster
## Just to make sure it seems like we got the correct legend
range(soil_out$value, na.rm = T)
range(soil_index$value, na.rm = T)

# Summarize that dataframe to be more manageable
soil_v2 <- soil_out %>%
  # Attach more descriptive names to these integer codes
  dplyr::left_join(y = soil_index, by = "value") %>%
  # Drop the integer code now
  dplyr::select(-value) %>%
  # Recalculate number of pixels per category with new informative groups
  dplyr::group_by(river_id, generic_soil) %>%
  dplyr::summarize(pixel_ct_v2 = sum(pixel_ct, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Calculate number of pixels of all types per river
  dplyr::group_by(river_id) %>%
  dplyr::mutate(total_pixels = sum(pixel_ct_v2, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Calculate percent of total per rock type
  dplyr::mutate(perc_total = (pixel_ct_v2 / total_pixels) * 100) %>%
  # Remove now-unneeded columns
  dplyr::select(-pixel_ct_v2, -total_pixels)

# Glimpse this
dplyr::glimpse(soil_v2)

# Check range of percents
range(as.integer(soil_v2$perc_total))

# Look at included soils
sort(unique(soil_v2$generic_soil))

# Pivot to wide format
soil_wide <- soil_v2 %>%
  # Add "soil" to each soil category before pivoting
  dplyr::mutate(generic_soil = paste0("soil_", generic_soil)) %>%
  # Now pivot
  tidyr::pivot_wider(names_from = generic_soil,
                     values_from = perc_total)

# Glimpse that
dplyr::glimpse(soil_wide)

# Then identify "major" (i.e., dominant) soil types per river
soil_major <- soil_v2 %>%
  # Filter to only max of each rock type per river
  dplyr::group_by(river_id) %>%
  filter(perc_total == max(perc_total)) %>%
  dplyr::ungroup() %>%
  # Remove the percent total
  dplyr::select(-perc_total) %>%
  # Pivot back to wide format
  tidyr::pivot_wider(names_from = generic_soil,
                     values_from = generic_soil) %>%
  # Paste all the non-NAs (i.e., the dominant rocks) into a single column
  tidyr::unite(col = major_soil, -river_id, na.rm = T, sep = "; ")

# Glimpse it
dplyr::glimpse(soil_major)

# Combine the full information to the "major" one
soil_actual <- soil_wide %>%
  dplyr::left_join(y = soil_major, by = c("river_id")) %>%
  dplyr::relocate(major_soil, .after = river_id)

# Examine
dplyr::glimpse(soil_actual)

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
          file = file.path(path, "extracted-data", "si-extract_soil.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_soil.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK"))

## ------------------------------------------------------- ##
                # Combine Extracted Data ----
## ------------------------------------------------------- ##
# Clear environment
rm(list = setdiff(ls(), c('path', 'sites')))

# List current extracted data
extracted_data <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK"), pattern = ".csv") %>%
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
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK"))

# End ----
