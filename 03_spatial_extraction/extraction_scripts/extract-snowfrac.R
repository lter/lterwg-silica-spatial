## ------------------------------------------------------- ##
    # Silica WG - Extract Spatial Data - Snow Fraction
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Uses the watershed shapefiles built by "02_watershed_delineation/03_combine-artisanal-hydrosheds.R"
## Extract the following data: SNOW FRACTION

## ------------------------------------------------------- ##
                      # Housekeeping ----
## ------------------------------------------------------- ##
# Load needed libraries

# Do not clear the session/environment here. This script may be sourced by the
# workflow.

source(file.path(getwd(), "tools", "workflow_paths.R"))
load_workflow_packages(c("tidyverse", "sf", "stars", "terra", "exactextractr", "googledrive", "readxl"))

# Silence `summarize`
options(dplyr.summarise.inform = F)

# Identify path to location of shared data
(path <- resolve_silica_data_root())
site_coord_dir <- silica_site_coordinates_dir(path)
raw_driver_dir <- silica_raw_driver_data_dir(path)

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
          # Snow Fraction - Identify Files ----
## ------------------------------------------------------- ##

# Make an empty list
file_list <- list()


# ## NEW SITES added for Data Release 2 ##
default_regions <- c("north-america-usa", "north-america-arctic",
                     "cropped-russia-west", "cropped-russia-west-2",
                     "cropped-russia-center", "cropped-russia-east",
                     "puerto-rico", "scandinavia",
                     "amazon", "australia",
                     "canada", "congo",
                     "germany", "united-kingdom")

region_set <- resolve_target_regions(
  subset_targets = subset_targets,
  default_regions = default_regions
)

for(region in region_set){

# ## NEW SITES added for Data Release 2 ##
# for(region in c("congo")){
  
  # This part is new -- we want to allow old and new versions of MODIS
  # Identify files in that folder
  file_df <- data.frame("region" = region,
                        "files" = dir(path = file.path(raw_driver_dir,
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
# rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all')))

## ------------------------------------------------------- ##
                # Snow Fraction - Extract ----
## ------------------------------------------------------- ##

# Specify driver
focal_driver <- "raw-snow-v061"

# Make a short name for that driver
driver_short <- "snow"

partial_dir <- silica_partial_extract_dir(raw_driver_dir, focal_driver, subset_targets)

# Read in reference table that converts integers to snow days
snow_reftable <- read.csv(file = file.path(raw_driver_dir, focal_driver, "snow_integer_codes.csv"))
dplyr::glimpse(snow_reftable)

# Identify files we've already extracted from
done_files <- data.frame("files" = dir(partial_dir)) %>%
  tidyr::separate(col = files, remove = F,
                  into = c("junk", "junk2", "year", "doy", "file_ext")) %>%
  # Make a year-day column
  dplyr::mutate(year_day = paste0(year, "_", doy))

# Remove completed files from the set of all possible files
not_done <- file_all %>%
  dplyr::mutate(year_day = paste0(year, "_", doy)) %>%
  dplyr::filter(!year_day %in% done_files$year_day)

resume_partials <- tolower(Sys.getenv("SILICA_RESUME_PARTIALS", "false")) == "true"

# Create a definitive object of files to extract
file_set <- if (merge_subset_outputs && !resume_partials) file_all else not_done
file_set <- filter_target_year_rows(file_set, year_col = "year")
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
      raster_path <- file.path(raw_driver_dir, focal_driver, simp_df$region[j], simp_df$files[j])
      message("Reading snow raster: ", raster_path)
      snow_rast <- terra::rast(raster_path)
      
      # Extract all possible information from that dataframe
      ex_data <- tryCatch(
        {
          exactextractr::exact_extract(x = snow_rast, y = sheds,
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
        },
        error = function(e) {
          stop(
            "Could not read snow raster: ", raster_path, "\n",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )
      
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
    write_subset_csv(
      df = full_data,
      output_path = file.path(partial_dir, export_name),
      key_cols = c("LTER", "Shapefile_Name", "year", "doy"),
      subset_targets = subset_targets,
      na = ""
    )
    
    # Ending message
    message("Processing ended for day of year: ", day_num) } # Close day-of-year loop
  
  # Ending message
  message("Processing ended year: ", annum) } # Close year loop

# Clean up environment
# rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'focal_driver', 'file_all')))

## ------------------------------------------------------- ##
                # Snow Fraction - Summarize ----
## ------------------------------------------------------- ##

# Identify extracted data
done_files <- dir(partial_dir)

# Make an empty list
full_out <- list()

# Read all of these files in
for(k in 1:length(done_files)){
  
  # Read in the kth file
  data_file <- read.csv(file = file.path(partial_dir, done_files[k]))
  
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

# Keep the partial 2001 year by default. Set SILICA_SNOW_KEEP_PARTIAL_2001=FALSE
# only if you explicitly want to drop that incomplete first year.
keep_partial_2001 <- tolower(Sys.getenv("SILICA_SNOW_KEEP_PARTIAL_2001", "true")) == "true"

# Unlist that list
out_df <- full_out %>%
  purrr::map(dplyr::mutate, Shapefile_Name = as.character(Shapefile_Name)) %>%
  purrr::list_rbind() %>%
  # And drop the placeholder dataframes when the extracted file is empty
  ## Again, only happens because of an unsolvable issue with the raw data
  dplyr::filter(Shapefile_Name != "xxx") %>%
  dplyr::mutate(
    LTER = toupper(as.character(LTER)),
    Shapefile_Name = toupper(as.character(Shapefile_Name))
  )

if (!keep_partial_2001) {
  out_df <- out_df %>% dplyr::filter(year > 2001)
  message("Dropping partial year 2001 because SILICA_SNOW_KEEP_PARTIAL_2001=FALSE.")
} else {
  message("Keeping partial year 2001 in snow outputs (SILICA_SNOW_KEEP_PARTIAL_2001=TRUE).")
}

# Glimpse it
dplyr::glimpse(out_df)

# Assign column prefix to match this driver
col_prefix <- "snow"

month_lookup <- c(
  "01" = "jan", "02" = "feb", "03" = "mar", "04" = "apr",
  "05" = "may", "06" = "jun", "07" = "jul", "08" = "aug",
  "09" = "sep", "10" = "oct", "11" = "nov", "12" = "dec"
)

snow_daily <- out_df %>%
  # Pivot to long format
  tidyr::pivot_longer(
    cols = dplyr::starts_with("snow_pres_day_"),
    names_to = "day_name",
    values_to = "snow_prop_area"
  ) %>%
  dplyr::mutate(
    day_index = as.integer(stringr::str_extract(day_name, "[0-9]+$")),
    date = as.Date(paste0(year, "-01-01")) + (doy + day_index - 2),
    month = format(date, "%m"),
    snow_day_flag = ifelse(!is.na(snow_prop_area) & snow_prop_area > 0, 1, 0)
  )

# Summarize within month across years
year_df <- snow_daily %>%
  # Summarize within day of year
  dplyr::group_by(LTER, Shapefile_Name, year, doy) %>%
  dplyr::summarize(
  ## Pick first 'total snow days' (i.e., number of snow days for that 8-day period)
  total_snow_days = dplyr::first(total_snow_days),
  ## And average across 'value' (i.e., prop landscape with snow)
  snow_frac_8day = mean(snow_prop_area, na.rm = T), .groups = "drop") %>%
  dplyr::ungroup() %>%
  # Now summarize across days of year within year
  ## Sum total days and get maximum snow fraction
  dplyr::group_by(LTER, Shapefile_Name, year) %>%
  dplyr::summarize(snow_days = sum(total_snow_days, na.rm = T),
                   snow_frac = max(snow_frac_8day, na.rm = T),
                   .groups = "drop") %>%
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

month_df <- snow_daily %>%
  dplyr::group_by(LTER, Shapefile_Name, year, month) %>%
  dplyr::summarize(
    snow_num_days = sum(snow_day_flag, na.rm = TRUE),
    snow_avg_prop_area = mean(snow_prop_area, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols = c("snow_num_days", "snow_avg_prop_area"),
    names_to = "metric",
    values_to = "value"
  ) %>%
  dplyr::group_by(LTER, Shapefile_Name, month, metric) %>%
  dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  dplyr::mutate(
    month_label = unname(month_lookup[month]),
    new_name = ifelse(
      metric == "snow_num_days",
      paste0(col_prefix, "_", month_label, "_num_days"),
      paste0(col_prefix, "_", month_label, "_avg_prop_area")
    )
  ) %>%
  dplyr::select(-month, -metric, -month_label) %>%
  tidyr::pivot_wider(names_from = new_name, values_from = value)

snow_actual <- year_df %>%
  dplyr::left_join(month_df, by = c("LTER", "Shapefile_Name"))

snow_actual <- snow_actual %>%
  dplyr::mutate(
    LTER = toupper(as.character(LTER)),
    Shapefile_Name = toupper(as.character(Shapefile_Name))
  )

## ------------------------------------------------------- ##
                 # Snow Fraction - Export ----
## ------------------------------------------------------- ##
# Let's get ready to export
snow_export <- sheds %>%
  dplyr::mutate(
    LTER = toupper(as.character(LTER)),
    Shapefile_Name = toupper(as.character(Shapefile_Name))
  ) %>%
  # Join the rock data
  dplyr::left_join(y = snow_actual, by = c("LTER", "Shapefile_Name")) %>%
  sf::st_drop_geometry()  

# Check it out
dplyr::glimpse(snow_export)

# Create folder to export to
snow_out_file <- silica_driver_output_file(path, paste0("si-extract_", col_prefix, "_v061"))

# Export the summarized snow data
write_subset_csv(
  df = snow_export,
  output_path = snow_out_file,
  key_cols = c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name"),
  subset_targets = subset_targets,
  na = ""
)

# Upload to GoogleDrive
googledrive::drive_upload(media = snow_out_file,
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld"))

# End ----
