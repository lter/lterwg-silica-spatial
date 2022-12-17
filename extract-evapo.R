## ------------------------------------------------------- ##
   # Silica WG - Extract Spatial Data - Evapotranspiration
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Using the watershed shapefiles created in "id-watershed-polygons.R"
## Extract the following data: EVAPOTRANSPIRATION

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
        # MODIS16A2 (v. 061) - Identify Files ----
## ------------------------------------------------------- ##

# Make an empty list
file_list <- list()

# Identify files for each region
for(region in c("north-america-usa", "north-america-arctic", "puerto-rico",
                "russia-west", "russia-west-2", "russia-center", "russia-east", 
                "scandinavia")){
  
  # Identify files in that folder
  file_df <- data.frame("region" = region,
                        "files" = dir(path = file.path(path, "raw-driver-data", 
                                                       "raw-evapo", region),
                                      pattern = "MOD16A2.061_ET_500m_"))
  
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
  dplyr::mutate(year = stringr::str_sub(string = date, start = 1, end = 4),
                doy = stringr::str_sub(string = date, start = 5, end = 7)) %>%
  # Drop 'raw' version
  dplyr::select(-date_raw)

# Glimpse it
dplyr::glimpse(file_all)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all')))

## ------------------------------------------------------- ##
        # Evapotranspiration - Bounding Box Check ----
## ------------------------------------------------------- ##
# Let's check to make sure each of my manual bounding boxes fits the sites for that region

# Filter to only one day of year
(viz_files <- dplyr::filter(file_all, doy == "001"))

# Read in one raster of each region
rast1 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo",
                               viz_files$region[1], viz_files$files[1]))
rast2 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo",
                               viz_files$region[2], viz_files$files[2]))
rast3 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo",
                               viz_files$region[3], viz_files$files[3]))
rast4 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo",
                               viz_files$region[4], viz_files$files[4]))
rast5 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo",
                               viz_files$region[5], viz_files$files[5]))
rast6 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo",
                               viz_files$region[6], viz_files$files[6]))
rast7 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo",
                               viz_files$region[7], viz_files$files[7]))
# Plot each "tile" of data against the watersheds polygons
## USA
plot(rast1, axes = T, reset = F, main = viz_files$region[1])
plot(sheds, axes = T, add = T)

## North America Arctic
plot(rast2, axes = T, reset = F, main = viz_files$region[2])
plot(sheds, axes = T, add = T)

## Puerto Rico
plot(rast3, axes = T, reset = F, main = viz_files$region[3])
plot(sheds, axes = T, add = T)

## Russia - West
plot(rast4, axes = T, reset = F, main = viz_files$region[4])
plot(sheds, axes = T, add = T)

## Russia - Center
plot(rast5, axes = T, reset = F, main = viz_files$region[5])
plot(sheds, axes = T, add = T)

## Russia - East
plot(rast6, axes = T, reset = F, main = viz_files$region[6])
plot(sheds, axes = T, add = T)

## Scandinavia
plot(rast7, axes = T, reset = F, main = viz_files$region[7])
plot(sheds, axes = T, add = T)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all')))

## ------------------------------------------------------- ##
            # Evapotranspiration - Extract ----
## ------------------------------------------------------- ##

# Make a list to house extracted information
full_out <- list()

# Extract all possible information from each
## Note this results in *many* NAs for pixels outside of each bounding box's extent
# for(day_num in "001") {
for(day_num in unique(file_all$doy)){
  
  # Starting message
  message("Processing begun for day of year: ", day_num)
  
  # File dataframe of files to just that doy
  simp_df <- dplyr::filter(file_all, doy == day_num)

  # Make an empty list
  doy_list <- list()
  
  # Now read in & extract each raster of that day of year
  for(focal_region in unique(simp_df$region)){
    
    # Starting message
    message("Begun for region: ", focal_region)
    
    # Subset simp_df to that region
    very_simp_df <- simp_df %>% 
      dplyr::filter(region == focal_region)
    
    # Read in raster
    et_rast <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo",
                                     very_simp_df$region, very_simp_df$files))
    
    # Extract all possible information from that dataframe
    ex_data <- exactextractr::exact_extract(x = et_rast, y = sheds, 
                                            include_cols = c("river_id"),
                                            progress = FALSE) %>%
      # Unlist to dataframe
      purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
      # Average within river_id
      dplyr::group_by(river_id) %>%
      dplyr::summarize(mean_val = mean(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      # Create some other useful columns
      dplyr::mutate(year = as.numeric(very_simp_df$year),
                    day_of_year = as.numeric(day_num),
                    .after = river_id)
    
    # Add this dataframe to the list we made within the larger for loop
    doy_list[[focal_region]] <- ex_data
    
    # End message
    message("Finished region: ", focal_region) }
  
  # Wrangle the output of the within-day of year extraction
  full_data <- doy_list %>%
    # Unlist to dataframe
    purrr::map_dfr(.f = dplyr::select, dplyr::everything())
  
  # Add this to a second, larger list
  full_out[[day_num]] <- full_data
    
  # Ending message
  message("Processing ended for day of year: ", day_num) }

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'full_out')))

## ------------------------------------------------------- ##
            # Evapotranspiration - Summarize ----
## ------------------------------------------------------- ##
# Unlist that list
out_df <- full_out %>%
  purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
  # Drop any possible duplicate rows
  unique()
  
# Glimpse it
dplyr::glimpse(out_df)

# Summarize within month across years
year_df <- out_df %>%
  # Do summarization
  dplyr::group_by(river_id, year) %>%
  dplyr::summarize(value = mean(mean_val, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Make more informative year column
  dplyr::mutate(name = paste0("evapotrans_", year, "_kg_m2")) %>%
  # Drop simple year column
  dplyr::select(-year) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = name,
                     values_from = value)

# Glimpse this
dplyr::glimpse(year_df)

# Need to convert day of year into months to get a monthly value
month_v1 <- out_df %>%
  # Get months
  dplyr::mutate(month = dplyr::case_when(
    day_of_year <= 31 ~ "jan", # 31 days in January
    day_of_year > 31 & day_of_year <= 59 ~ "feb", # +28 in Feb (note ignored leap days...)
    day_of_year > 59 & day_of_year <= 90 ~ "mar", # +31
    day_of_year > 90 & day_of_year <= 120 ~ "apr", # +30
    day_of_year > 120 & day_of_year <= 151 ~ "may", # +31
    day_of_year > 151 & day_of_year <= 181 ~ "jun", # +30 
    day_of_year > 181 & day_of_year <= 212 ~ "jul", # +31
    day_of_year > 212 & day_of_year <= 243 ~ "aug", # +31
    day_of_year > 243 & day_of_year <= 273 ~ "sep", # +30
    day_of_year > 273 & day_of_year <= 304 ~ "oct", # +31
    day_of_year > 304 & day_of_year <= 334 ~ "nov", # +30 
    day_of_year > 334 ~ "dec"))

# Check that each month is (roughly) equivalent in number of 8 day samples
month_v1 %>%
  dplyr::group_by(month) %>%
  summarize(count = n())

# Finish averaging within "month"
month_df <- month_v1 %>%
  # Average within month / river
  dplyr::group_by(river_id, month) %>%
  dplyr::summarize(value = mean(mean_val, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Make more informative month column
  dplyr::mutate(name = paste0("evapotrans_", month, "_kg_m2")) %>%
  # Drop simple month column
  dplyr::select(-month) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = name,
                     values_from = value)

# Glimpse this
dplyr::glimpse(month_df)

# Combine these dataframes
et_actual <- year_df %>%
  dplyr::left_join(y = month_df, by = "river_id")

# Glimpse again
dplyr::glimpse(et_actual)

## ------------------------------------------------------- ##
            # Evapotranspiration - Export ----
## ------------------------------------------------------- ##
# Let's get ready to export
et_export <- sites %>%
  # Join the rock data
  dplyr::left_join(y = et_actual, by = c("river_id"))

# Check it out
dplyr::glimpse(et_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized lithology data
write.csv(x = et_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_evapo.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_evapo.csv"),
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
