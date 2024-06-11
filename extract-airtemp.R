## ------------------------------------------------------- ##
   # Silica WG - Extract Spatial Data - Air Temperature
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Using the watershed shapefile created in "wrangle-watersheds.R"
## Extract the following data: AIR TEMPERATURE (MONTHLY)

## ------------------------------------------------------- ##
                        # Housekeeping ----
## ------------------------------------------------------- ##

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, stars, terra, exactextractr, NCEAS/scicomptools, 
                 googledrive, readxl, ncdf4)

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
sheds <- sf::st_read(dsn = file.path(path, "site-coordinates", "silica-watersheds.shp")) %>%
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
sheds <- sheds %>% select (-c(Stream_Name.x, Stream_Name.y, expert_area_km2, shape_area_km2, exp_are, hydrshd, real_ar, 
                              Dsc_F_N))

# Check that out
dplyr::glimpse(sheds)


# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))
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
air_rast <- terra::rast(x = file.path(path, "raw-driver-data", "raw-airtemp-monthly",
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
    # Convert from Kelvin to Celsius
    dplyr::mutate(value_c = value - 273.15) %>%
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

# Then summarize within year across months
month_df <- full_out_df %>%
  # Do summarization
  dplyr::group_by(LTER, Shapefile_Name, month) %>%
  dplyr::summarize(value = mean(value_avg, na.rm = T)) %>%
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
  dplyr::mutate(name = paste0("temp_", month_simp, "_degC")) %>%
  # Drop simple month column
  dplyr::select(-month, -month_simp) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = name,
                     values_from = value) %>%
  # Reorder months into chronological order
  dplyr::select(LTER, Shapefile_Name, dplyr::contains("_jan_"), dplyr::contains("_feb_"),
                dplyr::contains("_mar_"), dplyr::contains("_apr_"),
                dplyr::contains("_may_"), dplyr::contains("_jun_"),
                dplyr::contains("_jul_"), dplyr::contains("_aug_"),
                dplyr::contains("_sep_"), dplyr::contains("_oct_"),
                dplyr::contains("_nov_"), dplyr::contains("_dec_"))

# Glimpse this
dplyr::glimpse(month_df)

# Combine these dataframes
air_actual <- year_df %>%
  dplyr::left_join(y = month_df, by = c("LTER", "Shapefile_Name"))

# Glimpse again
dplyr::glimpse(air_actual)

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

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized lithology data
write.csv(x = air_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_air-temp_2.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_air-temp_2.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld"))

# End ----
