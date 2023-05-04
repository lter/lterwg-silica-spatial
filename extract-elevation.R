## ------------------------------------------------------- ##
       # Silica WG - Extract Spatial Data - Elevation
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Using the watershed shapefiles created in "wrangle-watersheds.R"
## Extract the following data: ELEVATION

## ------------------------------------------------------- ##
                      # Housekeeping ----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, stars, terra, exactextractr, NCEAS/scicomptools, 
                 googledrive, readxl)

# Clear environment
rm(list = ls())

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
              # Elevation - Extract ----
## ------------------------------------------------------- ##

# Read in dataset
elev_raw <- terra::rast(x = file.path(path, "raw-driver-data", "raw-elevation", 
                                      "wc2.1_30s_elev.tif"))

# Exploratory plot for overlap
plot(elev_raw, axes = T, reset = F)
plot(sheds, axes = T, add = T)

# Strip out land cover for our polygons
elev_out <- exactextractr::exact_extract(x = elev_raw, y = sheds,
                                         include_cols = c("LTER", "Shapefile_Name")) %>%
  # Above returns a list so switch it to a dataframe
  purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
  # Filter out NAs
  dplyr::filter(!is.na(value))
  
# Check that dataframe
dplyr::glimpse(elev_out)

## ------------------------------------------------------- ##
                  # Elevation - Summarize ----
## ------------------------------------------------------- ##

# Wrangle extracted data
elev_actual <- elev_out %>%
  # Summarize elevation within river ID
  dplyr::group_by(LTER, Shapefile_Name) %>%
  dplyr::summarize(elevation_median_m = stats::median(value, na.rm = T),
                   elevation_mean_m = mean(value, na.rm = T),
                   elevation_min_m = min(value, na.rm = T),
                   elevation_max_m = max(value, na.rm = T)) %>%
  dplyr::ungroup()

# Glimpse this
dplyr::glimpse(elev_actual)

## ------------------------------------------------------- ##
                  # Elevation - Export ----
## ------------------------------------------------------- ##
# Let's get ready to export
elev_export <- sites %>%
  # Join the rock data
  dplyr::left_join(y = elev_actual, by = c("LTER", "Shapefile_Name"))

# Check it out
dplyr::glimpse(elev_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized lithology data
write.csv(x = elev_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_elevation.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", 
                                            "si-extract_elevation.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK"))

# End ----
