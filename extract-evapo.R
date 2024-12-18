## ------------------------------------------------------- ##
   # Silica WG - Extract Spatial Data - Evapotranspiration
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon, Sidney A Bush

# Purpose:
## Using the watershed shapefiles created in "wrangle-watersheds.R"
## Extract the following data: EVAPOTRANSPIRATION

## ------------------------------------------------------- ##
                      # Housekeeping ----
## ------------------------------------------------------- ##

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, stars, terra, exactextractr, NCEAS/scicomptools, 
                 googledrive, readxl, dplyr)

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
        # MODIS16A2 (v. 061) - Identify Files ----
## ------------------------------------------------------- ##

# Make an empty list
file_list <- list()

## NEW SITES for Data Release 2 ##
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
                                                       "raw-evapo-v061", region))) %>% 
    dplyr::filter(stringr::str_detect(string=files, pattern="MOD16A2GF.061_ET_500m_")) 
  
  # Add that set of files to the list
  file_list[[region]] <- file_df }

# Wrangle the list
file_all <- file_list %>%
  purrr::list_rbind() %>%
  # Extract date from filename
  dplyr::mutate(date_raw = stringr::str_extract(string = files, pattern = "_doy[[:digit:]]{7}")) %>%
  dplyr::mutate(date = gsub("_doy", "", date_raw)) %>%
  dplyr::mutate(
    year = stringr::str_sub(string = date, start = 1, end = 4),
    doy = stringr::str_sub(string = date, start = 5, end = 7),
    year_day = as.numeric(paste0(year, doy)) # Combine year and doy
  ) %>%
  # Drop raw columns if no longer needed
  dplyr::select(-date_raw, -date)

# Glimpse it
dplyr::glimpse(file_all)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all')))

## ------------------------------------------------------- ##
            # Evapotranspiration - Extract ----
## ------------------------------------------------------- ##
# Specify driver
focal_driver <- "raw-evapo-v061"

# Make a short name for that driver
driver_short <- "evapotrans"

# Create folder to export to
dir.create(path = file.path(path, "raw-driver-data", focal_driver,
                            "_partial-extracted"),
           showWarnings = F)

# # Identify files we've already extracted from
# done_files <- data.frame("files" = dir(file.path(path, "raw-driver-data",
#                                                  focal_driver,
#                                                  "_partial-extracted"))) %>%
#   tidyr::separate(col = files, remove = F,
#                   into = c("junk", "junk2", "year", "doy", "file_ext")) %>%
#   # Make a year-day column
#   dplyr::mutate(year_day = paste0(year, "_", doy))
# 
# # Remove completed files from the set of all possible files
# not_done <- file_all %>%
#   dplyr::mutate(year_day = paste0(year, "_", doy)) %>%
#   dplyr::filter(!year_day %in% done_files$year_day)
# 
# # Create a definitive object of files to extract
# #file_set <- not_done # Uncomment if want to only do only undone extractions
# file_set <- file_all # Uncomment if want to do all extractions

## Trying to start after known corrupt file:
# Identify files we've already extracted from
done_files <- data.frame("files" = dir(file.path(path, "raw-driver-data",
                                                 focal_driver,
                                                 "_partial-extracted"))) %>%
  dplyr::mutate(doy = stringr::str_extract(files, "doy\\d{7}"), # Extract the DOY part
    year_day = as.numeric(stringr::str_sub(doy, 4, 10)) # Convert to numeric year_day
  )


# # Remove completed files from the set of all possible files
# not_done <- file_all %>%
#   dplyr::mutate(doy = stringr::str_extract(files, "doy\\d{7}")) %>%  # Extract the DOY part
#   dplyr::mutate(year_day = stringr::str_sub(doy, 4, 10))  # Extract the year and DOY

# start_after <- 2010065  # 2010_073 north-america-usa
# start_after <- 2017209 # 2017_217 amazon "Error: [readValues] cannot read values (potential fix: re-download original file)
start_after <- 2013081 # restarted on 12/13 
# For some reason, HYBAM site Saut_Maripoa gets dropped in 2013 (doy 089), and is never picked up again -- check loop for this:
# Something to check: skipped sites for specific year-doy-region combos in one year are not skipped for all years. this is likely happening for all 8-day MODIS data (snow)

not_done <- file_all %>%
  dplyr::filter(year_day > start_after)

## This is more robust, and should be used instead of the above line once known issues with doy/regions are determined
# not_done <- file_all %>%
#   dplyr::filter(!(year_day == 2010073 & region == "north-america-usa") & !(year_day == 2017217 & region == "amazon") |
#                      !year_day %in% c(2010073, 2017217) | !region %in% c("north-america-usa", "amazon"))

file_all %>%
  dplyr::group_by(region) %>%
  dplyr::summarize(total_files = n()) %>%
  left_join(not_done %>% dplyr::group_by(region) %>%
              dplyr::summarize(filtered_files = n()), by = "region") %>%
  print()

not_done %>%
  dplyr::group_by(region) %>%
  dplyr::summarize(n_files = n()) %>%
  print()


# Create a definitive object of files to extract
file_set <- not_done # Uncomment if want to only do only undone extractions
# file_set <- file_all # Uncomment if want to do all extractions


# Extract all possible information from each
# Note this results in *many* NAs for pixels in sheds outside of each bounding box's extent
# for(annum in "2001"){
for(annum in sort(unique(file_set$year))){

  # Start message
  message("Processing begun for year: ", annum)

  # Subset to one year
  one_year <- dplyr::filter(file_set, year == annum)

  # Loop across day-of-year within year
  # for(day_num in "001") {
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
      et_rast <- terra::rast(file.path(path, "raw-driver-data",  focal_driver,
                                       simp_df$region[j], simp_df$files[j]))

      # Extract all possible information from that dataframe
      ex_data <- exactextractr::exact_extract(x = et_rast, y = sheds,
                                              include_cols = c("LTER", "Shapefile_Name"),
                                              progress = FALSE) %>%
        # Unlist to dataframe
        purrr::list_rbind() %>%
        # Drop coverage fraction column
        dplyr::select(-coverage_fraction) %>%
        # Drop NA values that were "extracted"
        ## I.e., those that are outside of the current raster bounding nox
        dplyr::filter(!is.na(value)) %>%
        # Drop invalid values (per product documentation page)
        dplyr::filter(value >= -32767 & value <= 32700) %>%
        # Make new relevant columns
        dplyr::mutate(year = as.numeric(simp_df$year[j]),
                      doy = as.numeric(simp_df$doy[j]),
                      .after = Shapefile_Name)

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
      dplyr::summarize(value = mean(value, na.rm = T)) %>%
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
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all', 'focal_driver')))

## ------------------------------------------------------- ##
            # Evapotranspiration - Summarize ----
## ------------------------------------------------------- ##

# Identify extracted data
done_files <- dir(file.path(path, "raw-driver-data", focal_driver, "_partial-extracted"))

# Make an empty list
full_out <- list()

# Read all of these files in
for(k in 1:length(done_files)){
  
  # Read in the kth file
  full_out[[k]] <- read.csv(file = file.path(path, "raw-driver-data", focal_driver,
                                             "_partial-extracted", done_files[k])) %>%
    dplyr::mutate(Shapefile_Name = as.character(Shapefile_Name))
  
  # Finish
  message("Retrieved file ", k, " of ", length(done_files))}

# Wrangle output
## Updates to avoid logical error
out_df <- full_out %>%
  # Make sure character columns are characters
  purrr::map(.x = .,
             .f = dplyr::mutate, dplyr::across(.cols = dplyr::all_of(c("LTER")), 
                                               .fns = as.character)) %>%
  # Unlist the list
  purrr::list_rbind() %>% 
  # Rest of pipe as normal


# out_df <- full_out %>%
#   # Unlist that list
#   purrr::list_rbind() %>%
  # Account for scaler value
  ## See "Layers" dropdown here: https://lpdaac.usgs.gov/products/mod16a2v006/
  dplyr::mutate(unscaled_val = value * 0.1) %>%
  # Get a daily value (divide by 8 for all but last composite period and by 5 for that one)
  dplyr::mutate(daily_val = ifelse(test = (doy == 361),
                                   yes = (unscaled_val / 5),
                                   no = (unscaled_val / 8)) ) %>%
  # Drop old column
  dplyr::select(-unscaled_val, -value)

# Glimpse it
dplyr::glimpse(out_df)

# Make an empty list
next_list <- list()

# Now we need to get that daily value attached too all days in that 8-day increment
for(i in 1:7){
  
  # Increase all days of year by 1
  next_day <- out_df %>%
    dplyr::mutate(doy = doy + i)
  
  # Add to list
  next_list[[i]] <- next_day
  
  # Add success message
  message("Day ", i + 1, " dataframe created") }

# Unbind the list
out_df_v2 <- next_list %>%
  purrr::list_rbind() %>%
  # Attach the first day of each 8-day period (original value)
  dplyr::bind_rows(y = out_df) %>%
  # Remove extra days created by adding too much to the final 8-day increment (not truly 8 days)
  dplyr::filter(doy <= 365)

# Glimpse this as well
dplyr::glimpse(out_df_v2)

# Summarize within month across years
year_df <- out_df_v2 %>%
  # Do summarization
  dplyr::group_by(LTER, Shapefile_Name, year) %>%
  dplyr::summarize(value = sum(daily_val, na.rm = T)) %>%
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
  # Average within month / river
  dplyr::group_by(LTER, Shapefile_Name, month) %>%
  dplyr::summarize(value = sum(daily_val, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Make more informative month column
  dplyr::mutate(name = paste0("evapotrans_", month, "_kg_m2")) %>%
  # Drop simple month column
  dplyr::select(-month) %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = name,
                     values_from = value)%>%
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
et_actual <- year_df %>%
  dplyr::left_join(y = month_df, by = c("LTER", "Shapefile_Name"))

# Glimpse again
dplyr::glimpse(et_actual)

## ------------------------------------------------------- ##
            # Evapotranspiration - Export ----
## ------------------------------------------------------- ##
# Let's get ready to export
et_export <- sheds %>%
  # Join the rock data
  dplyr::left_join(y = et_actual, by = c("LTER", "Shapefile_Name"))%>%
  # this drops the geometry column, which causes issues on export
  sf::st_drop_geometry()  

# Check it out
dplyr::glimpse(et_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized data
write.csv(x = et_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_evapo_2-v061.csv")) ## Changed Nov 2024 to reflect new MODIS version for all sites


# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_evapo_2-v061.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld"))

# End ----
