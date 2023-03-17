## ------------------------------------------------------- ##
#       Silica WG - Extract Spatial Data - NPP
## ------------------------------------------------------- ##
# Written by:
## Angel Chen

# Purpose:
## Using the watershed shapefiles created in "id-watershed-polygons.R"
## Extract the following data: NPP

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
#          MOD17A3HGF v006 - Identify Files ----
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
                                                       "raw-npp", region),
                                      pattern = "MOD17A3HGF.006_Npp_"))
  
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
  dplyr::select(-date_raw) 

# Glimpse it
dplyr::glimpse(file_all)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all')))

## ------------------------------------------------------- ##
#     .       NPP - Bounding Box Check ----
## ------------------------------------------------------- ##
# Let's check to make sure each of my manual bounding boxes fits the sites for that region

# Filter to only one row per 'region'
(viz_files <- file_all %>% 
   # Find first file per region
   dplyr::group_by(region) %>%
   dplyr::summarize(files = dplyr::first(x = files)) %>%
   dplyr::ungroup() )

# Read in one raster of each region
rast1 <- terra::rast(file.path(path, "raw-driver-data",  "raw-npp",
                               viz_files$region[1], viz_files$files[1]))
rast2 <- terra::rast(file.path(path, "raw-driver-data",  "raw-npp",
                               viz_files$region[2], viz_files$files[2]))
rast3 <- terra::rast(file.path(path, "raw-driver-data",  "raw-npp",
                               viz_files$region[3], viz_files$files[3]))
rast4 <- terra::rast(file.path(path, "raw-driver-data",  "raw-npp",
                               viz_files$region[4], viz_files$files[4]))
rast5 <- terra::rast(file.path(path, "raw-driver-data",  "raw-npp",
                               viz_files$region[5], viz_files$files[5]))
rast6 <- terra::rast(file.path(path, "raw-driver-data",  "raw-npp",
                               viz_files$region[6], viz_files$files[6]))
rast7 <- terra::rast(file.path(path, "raw-driver-data",  "raw-npp",
                               viz_files$region[7], viz_files$files[7]))
rast8 <- terra::rast(file.path(path, "raw-driver-data",  "raw-npp",
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
#                    NPP - Extract ----
## ------------------------------------------------------- ##

# Silence `dplyr::summarize` preemptively
options(dplyr.summarise.inform = F)

# Specify driver
focal_driver <- "raw-npp"

# Identify files we've already extracted from
done_files <- data.frame("files" = dir(file.path(path, "raw-driver-data",
                                                 focal_driver,
                                                 "_partial-extracted"))) %>%
  tidyr::separate(col = files, remove = F,
                  into = c("junk", "junk2", "year", "file_ext")) 

# Remove completed files from the set of all possible files
not_done <- file_all %>%
  dplyr::filter(!year %in% done_files$year)

# Create a definitive object of files to extract
# file_set <- not_done # Uncomment if want to only do only undone extractions
file_set <- file_all # Uncomment if want to do all extractions

for (a_year in "2021"){
  # Subset to one year
  one_year_data <- dplyr::filter(file_set, year == a_year)
  
  # Make a list to house extracted information for a year
  year_list <- list()
  
  for (i in 1:nrow(one_year_data)){
      # Read in the raster
      npp_raster <- terra::rast(file.path(path, "raw-driver-data", "raw-npp", one_year_data$region[i], one_year_data$files[i]))
      
      # Extract all possible information from that dataframe
      ex_data <- exactextractr::exact_extract(x = npp_raster, y = sheds, 
                                              include_cols = c("river_id"),
                                              progress = FALSE) %>%
        # Unlist to dataframe
        purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
        # Drop coverage fraction column
        dplyr::select(-coverage_fraction) %>%
        # Drop NA values that were "extracted"
        ## I.e., those that are outside of the current raster bounding nox
        dplyr::filter(!is.na(value)) %>%
        # Drop invalid values (per product documentation page)
        dplyr::filter(value >= -30000 & value <= 32700) %>%
        # Make new relevant columns
        dplyr::mutate(year = a_year,
                      .after = river_id)
      
      # Add this dataframe to the list we made 
      year_list[[i]] <- ex_data
      
      # End message
      message("Finished extracting raster ", i, " of ", nrow(one_year_data)) 
  }

  # Assemble a file name for this extraction
  export_name <- paste0("npp_extract_", a_year, ".csv")
  
  # Wrangle the output of the within-year extraction
  full_data <- year_list %>%
    # Unlist to dataframe
    purrr::map_dfr(.f = dplyr::select, dplyr::everything()) %>%
    # Apply scaler value
    ## See "Layers" dropdown here: https://lpdaac.usgs.gov/products/mod17a3hgfv061/
    dplyr::mutate(descaled_val = value * 0.0001) %>%
    # Handle the summarization within river (potentially across multiple rasters' pixels)
    dplyr::group_by(river_id, year) %>%
    dplyr::summarize(npp = mean(descaled_val, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # Drop unnecessary columns
    dplyr::select(-year) %>% 
    dplyr::rename_with(.fn = ~paste0("npp_", a_year, "_kgC_m2_year"), .cols = npp)
  
  # Export this file for a given year
  write.csv(x = full_data, row.names = F, na = '',
            file = file.path(path, "raw-driver-data", "raw-npp",
                             "_partial-extracted", export_name))
  
  # End message
  message("Finished wrangling output for ", a_year) 

}

## ------------------------------------------------------- ##
#              NPP - Export ----
## ------------------------------------------------------- ##

# Identify extracted data
done_files <- dir(file.path(path, "raw-driver-data", focal_driver, "_partial-extracted"))

# Make an empty list
full_out <- list()

# Read all of these files in
for(k in 1:length(done_files)){
  
  # Read in the kth file
  full_out[[k]] <- read.csv(file = file.path(path, "raw-driver-data", focal_driver,
                                             "_partial-extracted", done_files[k]))
  
  # Finish
  message("Retrieved file ", k, " of ", length(done_files))}

# Unlist that list
out_df <- full_out %>%
  purrr::reduce(dplyr::left_join, by = 'river_id') 

# Glimpse it
dplyr::glimpse(out_df)

# Let's get ready to export
npp_export <- sites %>%
  # Join the npp data
  dplyr::left_join(y = out_df, by = "river_id")

# Check it out
dplyr::glimpse(npp_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized npp data
write.csv(x = npp_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_npp.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_npp.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK"))

## ------------------------------------------------------- ##
#              Combine Extracted Data ----
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
