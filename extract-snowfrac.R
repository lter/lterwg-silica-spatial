## ------------------------------------------------------- ##
    # Silica WG - Extract Spatial Data - Snow Fraction
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Using the watershed shapefiles created in "wrangle-watersheds.R"
## Extract the following data: SNOW FRACTION

## ------------------------------------------------------- ##
                      # Housekeeping ----
## ------------------------------------------------------- ##
# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, stars, terra, exactextractr, NCEAS/scicomptools, 
                 googledrive, readxl)

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
sheds <- sheds %>% 
  dplyr::select (-c(Stream_Name.x, Stream_Name.y, expert_area_km2, shape_area_km2, exp_are, hydrshd, real_ar, 
                              Dsc_F_N))

# Check that out
dplyr::glimpse(sheds)


# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

## ------------------------------------------------------- ##
          # Snow Fraction - Identify Files ----
## ------------------------------------------------------- ##

# Make an empty list
file_list <- list()


## NEW SITES added for Data Release 2 ##
for(region in c("north-america-usa", "north-america-arctic",
                "cropped-russia-west", "cropped-russia-west-2",   
                "cropped-russia-center", "cropped-russia-east",
                "puerto-rico", "scandinavia",
                "amazon", "australia",  
                "canada",  "congo", 
                "germany", "united-kingdom")){
  
  # This part is new -- we want to allow old and new versions of MODIS
  # Identify files in that folder
  file_df <- data.frame("region" = region,
                        "files" = dir(path = file.path(path, "raw-driver-data", 
                                                       "raw-snow-v061", region))) %>% 
    dplyr::filter(stringr::str_detect(string=files, pattern="MOD10A2.061_Eight_Day_Snow_Cover_")) 
  
  
  # Add that set of files to the list
  file_list[[region]] <- file_df }

# Wrangle the list
file_all <- file_list %>%
  # Unlist the loop's output
  purrr::list_rbind() %>%
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
                # Snow Fraction - Extract ----
## ------------------------------------------------------- ##

# Specify driver
focal_driver <- "raw-snow-v061"

# Make a short name for that driver
driver_short <- "snow"

# Create folder to export to
dir.create(path = file.path(path, "raw-driver-data",  focal_driver, "_partial-extracted"),
           showWarnings = F)

# Read in reference table that converts integers to snow days
snow_reftable <- read.csv(file = file.path(path, "raw-driver-data", 
                                           focal_driver, "snow_integer_codes.csv"))
dplyr::glimpse(snow_reftable)

# Identify files we've already extracted from
done_files <- data.frame("files" = dir(file.path(path, "raw-driver-data", focal_driver,
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
# file_set <- file_all # Uncomment if want to (re-)do all extractions

# Extract all possible information from each
## Note this results in *many* NAs for pixels in sheds outside of each bounding box's extent
# for(annum in "2002"){
for(annum in sort(unique(file_set$year))){
  
  # Start message
  message("Processing begun for year: ", annum)
  
  # Subset to one year
  one_year <- dplyr::filter(file_set, year == annum)
  
  # Loop across day-of-year within year
  # for(day_num in "009") {
  for(day_num in sort(unique(one_year$doy))){
    
    # Starting message
    message("Processing begun for day of year: ", day_num)
    
    # Assemble a file name for this extraction
    (export_name <- paste0(driver_short, "_extract_", annum, "_", day_num, ".csv"))
    
    # File dataframe of files to just that doy
    simp_df <- dplyr::filter(one_year, doy == day_num)
    
    # Make an empty list
    doy_list <- list()
    
    # Now read in & extract each raster of that day of year
    for(j in 1:nrow(simp_df)){
      
      # Starting message
      message("Begun for file ", j, " of ", nrow(simp_df))
      
      # Read in raster
      snow_rast <- terra::rast(file.path(path, "raw-driver-data",  focal_driver,
                                       simp_df$region[j], simp_df$files[j]))
      
      # Extract all possible information from that dataframe
      ex_data <- exactextractr::exact_extract(x = snow_rast, y = sheds, 
                                              include_cols = c("LTER", "Shapefile_Name"),
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
                      .after = Shapefile_Name) %>%
        # Attach the reference table for understanding the 'value' integer
        dplyr::left_join(y = snow_reftable, by = "value")
      
      # Add this dataframe to the list we made within the larger for loop
      doy_list[[j]] <- ex_data
      
      # End message
      message("Finished extracting raster ", j, " of ", nrow(simp_df)) }
    
    # Wrangle the output of the within-day of year extraction
    full_data <- doy_list %>%
      # Unlist to dataframe
      purrr::list_rbind() %>%
      # Handle the summarization within river (potentially across multiple rasters' pixels)
      dplyr::group_by(LTER, Shapefile_Name, year, doy) %>%
      dplyr::summarize(
        total_snow_days = mean(snow_days, na.rm = T),
        snow_pres_day_1 = mean(day_1_snow_pres, na.rm = T),
        snow_pres_day_2 = mean(day_2_snow_pres, na.rm = T),
        snow_pres_day_3 = mean(day_3_snow_pres, na.rm = T),
        snow_pres_day_4 = mean(day_4_snow_pres, na.rm = T),
        snow_pres_day_5 = mean(day_5_snow_pres, na.rm = T),
        snow_pres_day_6 = mean(day_6_snow_pres, na.rm = T),
        snow_pres_day_7 = mean(day_7_snow_pres, na.rm = T),
        snow_pres_day_8 = mean(day_8_snow_pres, na.rm = T)) %>%
      dplyr::ungroup()
    
    # Export this file for a given day
    write.csv(x = full_data, row.names = F, na = '',
              file = file.path(path, "raw-driver-data", focal_driver,
                               "_partial-extracted", export_name))
    
    # Ending message
    message("Processing ended for day of year: ", day_num) } # Close day-of-year loop
  
  # Ending message
  message("Processing ended year: ", annum) } # Close year loop

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'focal_driver', 'file_all')))

## ------------------------------------------------------- ##
                # Snow Fraction - Summarize ----
## ------------------------------------------------------- ##

# Identify extracted data
done_files <- dir(file.path(path, "raw-driver-data", focal_driver, "_partial-extracted"))

# Make an empty list
full_out <- list()

# Read all of these files in
for(k in 1:length(done_files)){
  
  # Read in the kth file
  data_file <- read.csv(file = file.path(path, "raw-driver-data", focal_driver, "_partial-extracted", done_files[k]))
  
  # If the file is empty, make a dummy file instead
  ## Some of these rasters are totally blank (an error on MODIS/AppEEARS side, not ours)
  if(nrow(data_file) == 0){ 
    data_file <- data.frame("LTER" = "xxx",
                            "Shapefile_Name" = "xxx",
                            "year" = 999,
                            "doy" = 999,
                            "total_snow_days" = 999.9,
                            "snow_pres_day_1" = 999.9,
                            "snow_pres_day_2" = 999.9,
                            "snow_pres_day_3" = 999.9,
                            "snow_pres_day_4" = 999.9,
                            "snow_pres_day_5" = 999.9,
                            "snow_pres_day_6" = 999.9,
                            "snow_pres_day_7" = 999.9,
                            "snow_pres_day_8" = 999.9) }
  
  # Add it to the list
  full_out[[k]] <- data_file
  
  # Print 'done' message
  message("Retrieved file ", k, " of ", length(done_files))}

# Unlist that list
out_df <- full_out %>%
  purrr::map(dplyr::mutate, Shapefile_Name = as.character(Shapefile_Name)) %>%
  purrr::list_rbind() %>%
  # And drop the placeholder dataframes when the extracted file is empty
  ## Again, only happens because of an unsolvable issue with the raw data
  dplyr::filter(Shapefile_Name != "xxx") %>%
  # Also drop 2001 because only one 8-day period is included
  dplyr::filter(year > 2001)

# Glimpse it
dplyr::glimpse(out_df)

# Assign column prefix to match this driver
col_prefix <- "snow"

# Summarize within month across years
year_df <- out_df %>%
  # Pivot to long format
  tidyr::pivot_longer(cols = dplyr::starts_with("snow_pres_day_")) %>%
  # Summarize within day of year
  dplyr::group_by(LTER, Shapefile_Name, year, doy) %>%
  dplyr::summarize(
  ## Pick first 'total snow days' (i.e., number of snow days for that 8-day period)
  total_snow_days = dplyr::first(total_snow_days),
  ## And average across 'value' (i.e., prop landscape with snow)
  snow_frac_8day = mean(value, na.rm = T) ) %>%
  dplyr::ungroup() %>%
  # Now summarize across days of year within year
  ## Sum total days and get maximum snow fraction
  dplyr::group_by(LTER, Shapefile_Name, year) %>%
  dplyr::summarize(snow_days = sum(total_snow_days, na.rm = T),
                   snow_frac = max(snow_frac_8day, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Pivot even longer
  tidyr::pivot_longer(cols = dplyr::starts_with("snow_")) %>%
  # Make more informative year column
  dplyr::mutate(new_name = ifelse(test = (name == "snow_days"),
                                  yes = paste0(col_prefix, "_", year, "_num_days"),
                                  no = paste0(col_prefix, "_", year, "_max_prop_area"))) %>%
  # Drop simple year column and simple name column
  dplyr::select(-year, -name) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = new_name,
                     values_from = value) %>%
  # Reorder columns
  dplyr::select(LTER, Shapefile_Name, dplyr::contains("_num_days"), dplyr::contains("_max_prop_area"))

# Glimpse this
dplyr::glimpse(year_df)

# Need to convert day of year into months to get a monthly value
month_df <- out_df %>%
  # Drop unwanted columns
  dplyr::select(-year, -total_snow_days) %>%
  # Pivot longer
  tidyr::pivot_longer(cols = dplyr::starts_with("snow_pres_day_")) %>%
  # Wrangle the 'name' column
  dplyr::mutate(name_simp = gsub(pattern = "snow_pres_day_", replacement = "", name)) %>%
  # Subtract one from the name column so the first day (i.e., what's in the DOY column) is 0
  dplyr::mutate(sub_doy = as.numeric(name_simp) - 1) %>%
  # Add the sub DOY to the DOY to get each row as an actual (non-relative) day of year
  dplyr::mutate(doy_actual = (doy + sub_doy), .after = doy) %>%
  # Drop intermediary columns
  dplyr::select(-name, -name_simp, -sub_doy, -doy) %>%
  # Rename doy_actual more simply
  dplyr::rename(doy = doy_actual) %>%
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
  # Average across the same day of year of each month across years
  dplyr::group_by(LTER, Shapefile_Name, month, doy) %>%
  dplyr::summarize(mean_val = mean(value, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Now sum across the days of year within each month
  # Average within month / river
  dplyr::group_by(LTER, Shapefile_Name, month) %>%
  dplyr::summarize(snow_total = sum(mean_val, na.rm = T),
                   snow_avg = mean(mean_val, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Pivot even longer
  tidyr::pivot_longer(cols = dplyr::starts_with("snow_")) %>%
  # Make more informative month column
  dplyr::mutate(new_name = ifelse(test = (name == "snow_total"),
                                  yes = paste0(col_prefix, "_", month, "_num_days"),
                                  no = paste0(col_prefix, "_", month, "_avg_prop_area"))) %>%
  # Drop simple month column and simple name column
  dplyr::select(-month, -name) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = new_name,
                     values_from = value) %>%
  # Reorder months into chronological order
  dplyr::select(LTER, Shapefile_Name, dplyr::contains("_jan_"), dplyr::contains("_feb_"),
                dplyr::contains("_mar_"), dplyr::contains("_apr_"),
                dplyr::contains("_may_"), dplyr::contains("_jun_"),
                dplyr::contains("_jul_"), dplyr::contains("_aug_"),
                dplyr::contains("_sep_"), dplyr::contains("_oct_"),
                dplyr::contains("_nov_"), dplyr::contains("_dec_")) %>%
  # And reorder again to group by column units
  dplyr::select(LTER, Shapefile_Name, dplyr::contains("_num_days"), dplyr::contains("_prop_area"))

# Glimpse this
dplyr::glimpse(month_df)

# Combine these dataframes
snow_actual <- year_df %>%
  dplyr::left_join(y = month_df, by = c("LTER", "Shapefile_Name"))

# Glimpse again
dplyr::glimpse(snow_actual)

## ------------------------------------------------------- ##
                 # Snow Fraction - Export ----
## ------------------------------------------------------- ##
# Let's get ready to export
snow_export <- sheds %>%
  # Join the rock data
  dplyr::left_join(y = snow_actual, by = c("LTER", "Shapefile_Name")) %>%
  sf::st_drop_geometry()  

# Check it out
dplyr::glimpse(snow_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized snow data
write.csv(x = snow_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", 
                           paste0("si-extract_", col_prefix, "_2_v061.csv")))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", 
                                            paste0("si-extract_", col_prefix, "_2_v061.csv")),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld"))

# End ----
