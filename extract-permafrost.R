## ------------------------------------------------------- ##
      # Silica WG - Extract Spatial Data - Permafrost
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Using the watershed shapefiles created in "wrangle-watersheds.R"
## Extract the following data: PERMAFROST

## ------------------------------------------------------- ##
# Housekeeping ----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, stars, terra, exactextractr, NCEAS/scicomptools, googledrive, readxl)

# Silence dplyr grouping message
options(dplyr.summarise.inform = F)

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
# Basin Slope - Extract & Summarize ----
## ------------------------------------------------------- ##
# Create an empty list to store results
slope_list <- list()

# For each watershed shapefile...
for (i in 1:nrow(sheds)){
  
  # Starting message
  message("Extracting slope for watershed ", i, " (", (nrow(sheds) - i), " remaining)")
  
  # Crop and mask the elevation raster to each shapefile
  cropped_raster <- terra::crop(x = elev_raw, y = terra::vect(sheds[i,]), mask = TRUE)
  
  # Calculate the slopes
  slope_raster <- terra::terrain(cropped_raster, v = "slope", unit = "degrees")
  
  # Extract the slopes into a dataframe
  slope_dataframe <- terra::as.data.frame(slope_raster)
  
  # Create a dummy dataframe if the extracted dataframe has 0 rows
  if (nrow(slope_dataframe) == 0){
    LTER_Shapefile_slope_dataframe <- data.frame(LTER = sheds[i,]$LTER,
                                                 Shapefile_Name = sheds[i,]$Shapefile_Name,
                                                 basin_slope_median_degree = NA,
                                                 basin_slope_mean_degree = NA,
                                                 basin_slope_min_degree = NA,
                                                 basin_slope_max_degree = NA)
    
    # Save the dataframe into our list
    slope_list[[i]] <- LTER_Shapefile_slope_dataframe
    
    # If the dataframe is NOT empty...
  } else {
    # Calculate slope statistics
    LTER_Shapefile_slope_dataframe <- slope_dataframe %>%
      # Keep LTER/shapefile name for summarizing and joining later
      dplyr::mutate(LTER = sheds[i,]$LTER,
                    Shapefile_Name = sheds[i,]$Shapefile_Name) %>%
      # Calculate slope statistics within those groups
      dplyr::group_by(LTER, Shapefile_Name) %>%
      dplyr::summarize(basin_slope_median_degree = stats::median(slope, na.rm = T),
                       basin_slope_mean_degree = spatialEco::mean_angle(slope, angle = "degree"),
                       basin_slope_min_degree = min(slope, na.rm = T),
                       basin_slope_max_degree = max(slope, na.rm = T))
    
    # Save the dataframe into our list
    slope_list[[i]] <- LTER_Shapefile_slope_dataframe }
  
} # Close loop

# Unlist into one big dataframe
slope_actual <- slope_list %>% purrr::map_dfr(.f = select, everything())

# Glimpse this
dplyr::glimpse(slope_actual)

# Here is an alternative way to extract the slope using the `starsExtra::slope()` function, 
# but it requires rasters with a projected CRS, not a geographic CRS.
# So I would need to change the CRS first if I wanted to use `starsExtra::slope()`.
# I opted to use the `terra::terrain()` function instead.

# cropped_raster <- terra::crop(x = elev_raw, y = sheds[1,])
# reproj_raster <- terra::project(cropped_raster, "EPSG:3857") # Web Mercator projection
# converted_raster <- stars::st_as_stars(reproj_raster)
# slope_raster <- starsExtra::slope(converted_raster)
# slope_dataframe <- as.data.frame(slope_raster)

## ------------------------------------------------------- ##
#           Elevation & Basin Slope - Export ----
## ------------------------------------------------------- ##
# Let's get ready to export
elev_export <- sites %>%
  # Join the rock data
  dplyr::left_join(y = elev_actual, by = c("LTER", "Shapefile_Name")) %>%
  # Join the slope data
  dplyr::left_join(y = slope_actual, by = c("LTER", "Shapefile_Name"))

# Check it out
dplyr::glimpse(elev_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized elevation and slope data
write.csv(x = elev_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_elevation.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", 
                                            "si-extract_elevation.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1Z-qlt9okoZ4eE-VVsbHiVVSu7V5nEkqK"))

# End ----

