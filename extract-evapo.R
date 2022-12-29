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
for(region in c("north-america-usa", "north-america-arctic",
                "cropped-russia-west", "cropped-russia-west-2",
                "cropped-russia-center", "cropped-russia-east",
                "puerto-rico", "scandinavia")){
  
  # Identify files in that folder
  file_df <- data.frame("region" = region,
                        "files" = dir(path = file.path(path, "raw-driver-data", 
                                                       "raw-evapo-modis16a2-v006", region),
                                      pattern = "MOD16A2.006_ET_500m_"))
  
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

# Filter to only one row per 'region'
(viz_files <- file_all %>% 
   # Find first file per region
   dplyr::group_by(region) %>%
   dplyr::summarize(files = dplyr::first(x = files)) %>%
   dplyr::ungroup() )

# Read in one raster of each region
rast1 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo-modis16a2-v006",
                               viz_files$region[1], viz_files$files[1]))
rast2 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo-modis16a2-v006",
                               viz_files$region[2], viz_files$files[2]))
rast3 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo-modis16a2-v006",
                               viz_files$region[3], viz_files$files[3]))
rast4 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo-modis16a2-v006",
                               viz_files$region[4], viz_files$files[4]))
rast5 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo-modis16a2-v006",
                               viz_files$region[5], viz_files$files[5]))
rast6 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo-modis16a2-v006",
                               viz_files$region[6], viz_files$files[6]))
rast7 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo-modis16a2-v006",
                               viz_files$region[7], viz_files$files[7]))
rast8 <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo-modis16a2-v006",
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
            # Evapotranspiration - Extract ----
## ------------------------------------------------------- ##

# Silence `dplyr::summarize` preemptively
options(dplyr.summarise.inform = F)

# Create folder to export to
dir.create(path = file.path(path, "raw-driver-data",  "raw-evapo-modis16a2-v006",
                            "_partial-extracted"),
           showWarnings = F)

# Identify files we've already extracted from
done_files <- data.frame("files" = dir(file.path(path, "raw-driver-data", 
                                                 "raw-evapo-modis16a2-v006",
                                                 "_partial-extracted"))) %>%
  tidyr::separate(col = files, remove = F,
                  into = c("junk", "junk2", "year", "doy", "file_ext")) %>%
  # Make a year-day column
  dplyr::mutate(year_day = paste0(year, "_", doy))

# Remove completed files from the set of all possible files
not_done <- file_all %>%
  dplyr::mutate(year_day = paste0(year, "_", doy)) %>%
  dplyr::filter(!year_day %in% done_files$year_day)

# Create a definitive object of files to extract
file_set <- not_done # Uncomment if want to only do only undone extractions
# file_set <- file_all # Uncomment if want to do all extractions

# Extract all possible information from each
## Note this results in *many* NAs for pixels in sheds outside of each bounding box's extent
# for(annum in "2001"){
for(annum in unique(file_set$year)){
  
  # Start message
  message("Processing begun for year: ", annum)
  
  # Subset to one year
  one_year <- dplyr::filter(file_set, year == annum)

  # Loop across day-of-year within year
  # for(day_num in "001") {
  for(day_num in unique(one_year$doy)){
    
    # Starting message
    message("Processing begun for day of year: ", day_num)
    
    # Assemble a file name for this extraction
    (export_name <- paste0("evapotrans_extract_", annum, "_", day_num, ".csv"))
    
    # File dataframe of files to just that doy
    simp_df <- dplyr::filter(one_year, doy == day_num)
    
    # Make an empty list
    doy_list <- list()
    
    # Now read in & extract each raster of that day of year
    for(j in 1:nrow(simp_df)){
      
      # Starting message
      message("Begun for file ", j, " of ", nrow(simp_df))
      
      # Read in raster
      et_rast <- terra::rast(file.path(path, "raw-driver-data",  "raw-evapo-modis16a2-v006",
                                       simp_df$region[j], simp_df$files[j]))
      
      # Extract all possible information from that dataframe
      ex_data <- exactextractr::exact_extract(x = et_rast, y = sheds, 
                                              include_cols = c("river_id"),
                                              progress = FALSE) %>%
        # Unlist to dataframe
        purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
        # Drop coverage fraction column
        dplyr::select(-coverage_fraction) %>%
        # Drop NA values that were "extracted"
        ## I.e., those that are outside of the current raster bounding nox
        dplyr::filter(!is.na(value)) %>%
        # Make new relevant columns
        dplyr::mutate(year = as.numeric(simp_df$year[j]),
                      doy = as.numeric(simp_df$doy[j]),
                      .after = river_id)
      
      # Add this dataframe to the list we made within the larger for loop
      doy_list[[j]] <- ex_data
      
      # End message
      message("Finished extracting raster ", j, " of ", nrow(simp_df)) }
    
    # Wrangle the output of the within-day of year extraction
    full_data <- doy_list %>%
      # Unlist to dataframe
      purrr::map_dfr(.f = dplyr::select, dplyr::everything()) %>%
      # Handle the summarization within river (potentially across multiple rasters' pixels)
      dplyr::group_by(river_id, year, doy) %>%
      dplyr::summarize(value = mean(value, na.rm = T)) %>%
      dplyr::ungroup()
    
    # Export this file for a given day
    write.csv(x = full_data, row.names = F, na = '',
              file = file.path(path, "raw-driver-data", "raw-evapo-modis16a2-v006",
                               "_partial-extracted", export_name))
    
    # Ending message
    message("Processing ended for day of year: ", day_num) } # Close day-of-year loop
  
  # Ending message
  message("Processing ended year: ", annum) } # Close year loop

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all')))

## ------------------------------------------------------- ##
            # Evapotranspiration - Summarize ----
## ------------------------------------------------------- ##

# Identify extracted data
done_files <- dir(file.path(path, "raw-driver-data", "raw-evapo-modis16a2-v006", "_partial-extracted"))

# Make an empty list
full_out <- list()

# Read all of these files in
for(k in 1:length(done_files)){
  
  # Read in the kth file
  full_out[[k]] <- read.csv(file = file.path(path, "raw-driver-data", "raw-evapo-modis16a2-v006", "_partial-extracted", done_files[k]))
  
  # Finish
  message("Retrieved file ", k, " of ", length(done_files))}

# Unlist that list
out_df <- full_out %>%
  purrr::map_dfr(dplyr::select, dplyr::everything())
  
# Glimpse it
dplyr::glimpse(out_df)

# Summarize within month across years
year_df <- out_df %>%
  # Rename value column in data
  dplyr::rename(mean_val = value) %>%
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
month_df <- out_df %>%
  # Get months
  dplyr::mutate(month = dplyr::case_when(
    doy <= 31 ~ "jan", # 31 days in January
    doy > 31 & doy <= 59 ~ "feb", # +28 in Feb (note ignored leap days...)
    doy > 59 & doy <= 90 ~ "mar", # +31
    doy > 90 & doy <= 120 ~ "apr", # +30
    doy > 120 & doy <= 151 ~ "may", # +31
    doy > 151 & doy <= 181 ~ "jun", # +30 
    doy > 181 & doy <= 212 ~ "jul", # +31
    doy > 212 & doy <= 243 ~ "aug", # +31
    doy > 243 & doy <= 273 ~ "sep", # +30
    doy > 273 & doy <= 304 ~ "oct", # +31
    doy > 304 & doy <= 334 ~ "nov", # +30 
    doy > 334 ~ "dec")) %>%
  # Rename value column in data
  dplyr::rename(mean_val = value) %>%
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
