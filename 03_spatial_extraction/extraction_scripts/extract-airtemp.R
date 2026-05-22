## ------------------------------------------------------- ##
   # Silica WG - Extract Spatial Data - Air Temperature
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Uses the watershed shapefiles built by "02_watershed_delineation/03_combine-artisanal-hydrosheds.R"
## Extract the following data: AIR TEMPERATURE (MONTHLY)

## ------------------------------------------------------- ##
                        # Housekeeping ----
## ------------------------------------------------------- ##

# Load needed libraries

# Do not clear the session/environment here. This script may be sourced by the
# workflow.

source(file.path(getwd(), "tools", "workflow_paths.R"))
load_workflow_packages(c("tidyverse", "sf", "stars", "terra", "exactextractr", "googledrive", "readxl", "ncdf4"))

# Silence `summarize`
options(dplyr.summarise.inform = F)

# Identify path to location of shared data
(path <- resolve_silica_data_root())
site_coord_dir <- silica_site_coordinates_dir(path)
raw_driver_dir <- silica_raw_driver_data_dir(path)
extracted_dir <- silica_extracted_data_dir(path)

# Load in site names with lat/longs
sites <- read_silica_site_reference(site_coord_dir) %>%
  ## Pare down to minimum needed columns
  dplyr::select(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name) %>%
  ## Drop duplicate rows (if any)
  dplyr::distinct() 
# Remove any watersheds without a shapefile
# dplyr::filter(!is.na(Shapefile_Name) &
#                 nchar(Shapefile_Name) != 0 &
#                 !Shapefile_Name %in% c("?", "MISSING"))

# Check it out
dplyr::glimpse(sites)

# Grab the shapefiles the previous script (see PURPOSE section) created
sheds <- sf::st_read(dsn = silica_watershed_file(path)) %>%
  # Expand names to what they were before
  dplyr::rename(Shapefile_Name = shp_nm,
                Stream_Name = Strm_Nm,
                expert_area_km2 = exp_area,
                shape_area_km2 = real_area)

## combine sites and sheds to get ALL sheds (including hydrosheds) and their metadata (from the sites dataframe)
sheds <- sheds %>%
  dplyr::left_join(y = sites, by = c("LTER", "Shapefile_Name"))

sheds$Stream_Name <- ifelse(!is.na(sheds$Stream_Name.x), sheds$Stream_Name.x, sheds$Stream_Name.y)
sheds$Discharge_File_Name <- ifelse(!is.na(sheds$Dsc_F_N), sheds$Dsc_F_N, sheds$Discharge_File_Name)
sheds <- sheds %>%
  dplyr::select(-dplyr::any_of(c(
    "Stream_Name.x", "Stream_Name.y", "expert_area_km2", "shape_area_km2",
    "exp_are", "hydrshd", "real_ar", "Dsc_F_N"
  )))

# Check that out
dplyr::glimpse(sheds)

# Optionally filter to a target site subset (set SILICA_SITE_SUBSET_FILE env var)
source(file = file.path(getwd(), "tools", "subset_and_output_helpers.R"))
subset_targets <- load_site_subset()
subset_data <- filter_to_target_sites(sites = sites, sheds = sheds, subset_targets = subset_targets)
sites <- subset_data$sites
sheds <- subset_data$sheds
merge_subset_outputs <- !is.null(subset_targets) &&
  tolower(Sys.getenv("SILICA_MERGE_SUBSET_OUTPUTS", "false")) == "true"


# Clean up environment
# rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))
## ------------------------------------------------------- ##
                  # Air Temp - Extract ----
## ------------------------------------------------------- ##
# Read in the netCDF file and examine for context on units / etc.
# air_nc <- ncdf4::nc_open(filename = file.path(path, "raw-driver-data", "raw-airtemp-monthly",
#                                               "air.mon.mean.nc"))
# 
# # Look at this
# print(air_nc)

# Read it as a raster too
## This format is more easily manipulable for our purposes
air_rast <- terra::rast(x = file.path(raw_driver_dir, "raw-airtemp-monthly",
                                      "air.mon.mean.nc"))

# Check names
names(air_rast)

# Check out just one of those
print(air_rast$air_99)

# Create an empty list to store this information in
out_list <- list()

# Identify how many layers are in this
(layer_ct <- length(names(air_rast)))

# We'll need to strip each layer separately
for(k in 1:layer_ct){
  
  # Build name of layer
  focal_layer <- paste0("air_", k)
  
  # Rotate so longitude is from -180 to 180 (rather than 0 to 360)
  rotated <- terra::rotate(x = air_rast[[focal_layer]])
  
  # Identify time of this layer
  layer_time <- terra::time(x = rotated)
  
  # Strip out the relevant bit
  small_out_df <- exactextractr::exact_extract(x = rotated, y = sheds, 
                                               include_cols = c("LTER", "Shapefile_Name"),
                                               progress = F) %>%
    # Above returns a list so switch it to a dataframe
    purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
    # Filter out NAs
    dplyr::filter(!is.na(value)) %>%
    # NOAA air.mon.mean.nc is already in degrees C
    dplyr::mutate(value_c = value) %>%
    # Average temperature within river ID
    dplyr::group_by(LTER, Shapefile_Name) %>%
    dplyr::summarize(value_avg = mean(value_c, na.rm = T)) %>%
    dplyr::ungroup() %>%
    # Add a column for what timestamp this is
    dplyr::mutate(time = layer_time, .before = dplyr::everything())
  
  # Add it to the list
  out_list[[focal_layer]] <- small_out_df
  
  # Success message
  message("Processing complete for ", layer_time, " (number ", k, " of ", layer_ct, ")") }

# Exploratory plot one of what we just extracted
# plot(rotated, axes = T, reset = F)
# plot(sheds, axes = T, add = T)

## ------------------------------------------------------- ##
                # Air Temp - Summarize ----
## ------------------------------------------------------- ##
# Unlist that list
full_out_df <- out_list %>%
  purrr::list_rbind(x = .) %>%
  # Strip out year and month
  dplyr::mutate(year = stringr::str_sub(string = time, start = 1, end = 4),
                month = stringr::str_sub(string = time, start = 6, end = 7),
                .after = time)

full_out_df <- filter_target_year_rows(full_out_df, year_col = "year")
  
# Glimpse it
dplyr::glimpse(full_out_df)

# Summarize within month across years
year_df <- full_out_df %>%
  # Do summarization
  dplyr::group_by(LTER, Shapefile_Name, year) %>%
  dplyr::summarize(value = mean(value_avg, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Make more informative year column
  dplyr::mutate(name = paste0("temp_", year, "_degC")) %>%
  # Drop simple year column
  dplyr::select(-year) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = name,
                     values_from = value)
  
# Glimpse this
dplyr::glimpse(year_df)

month_lookup <- c(
  "01" = "jan", "02" = "feb", "03" = "mar", "04" = "apr",
  "05" = "may", "06" = "jun", "07" = "jul", "08" = "aug",
  "09" = "sep", "10" = "oct", "11" = "nov", "12" = "dec"
)

month_df <- full_out_df %>%
  dplyr::group_by(LTER, Shapefile_Name, year, month) %>%
  dplyr::summarize(value = mean(value_avg, na.rm = TRUE), .groups = "drop") %>%
  dplyr::group_by(LTER, Shapefile_Name, month) %>%
  dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    month_label = unname(month_lookup[month]),
    name = paste0("temp_", month_label, "_degC")
  ) %>%
  dplyr::select(-month, -month_label) %>%
  tidyr::pivot_wider(names_from = name, values_from = value)

air_actual <- year_df %>%
  dplyr::left_join(month_df, by = c("LTER", "Shapefile_Name"))

## ------------------------------------------------------- ##
                    # Air Temp - Export ----
## ------------------------------------------------------- ##
# Let's get ready to export
air_export <- sheds %>%
  # Join the rock data
  dplyr::left_join(y = air_actual, by = c("LTER", "Shapefile_Name"))%>%
  sf::st_drop_geometry()  


# Check it out
dplyr::glimpse(air_export)

air_out_file <- silica_driver_output_file(path, "si-extract_air-temp")
legacy_air_files <- file.path(
  extracted_dir,
  c(
    "si-extract_air-temp_2_cameroon_sites.csv",
    "si-extract_air-temp_2_cameroon_site.csv",
    "si-extract_air-temp_cameroon_sites_2.csv"
  )
)
legacy_air_files <- legacy_air_files[file.exists(legacy_air_files)]

if (length(legacy_air_files) > 0) {
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  invisible(lapply(legacy_air_files, function(old_file) {
    new_file <- paste0(old_file, ".deprecated_", stamp)
    ok <- file.rename(old_file, new_file)
    if (!ok) {
      warning("Failed to rename legacy file: ", old_file, call. = FALSE)
    } else {
      message("Deprecated legacy file naming: ", basename(old_file), " -> ", basename(new_file))
    }
    invisible(ok)
  }))
}

# Export the summarized lithology data
write_subset_csv(
  df = air_export,
  output_path = air_out_file,
  key_cols = c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name"),
  subset_targets = subset_targets,
  na = ""
)

# Upload to GoogleDrive
googledrive::drive_upload(media = air_out_file,
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld"))

# End ----
