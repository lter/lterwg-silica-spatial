## ------------------------------------------------------- ##
        # Silica WG - Extract Spatial Data - Greenup
## ------------------------------------------------------- ##
# Written by:
## Angel Chen, Nick Lyon

# Purpose:
## Using the watershed shapefiles created in "wrangle-watersheds.R"
## Extract the following data: GREEN-UP

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
sheds <- sheds %>% select (-c(Stream_Name.x, Stream_Name.y, expert_area_km2, shape_area_km2, exp_are, hydrshd, real_ar, 
                              Dsc_F_N))

# Check that out
dplyr::glimpse(sheds)


# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

## ------------------------------------------------------- ##
           # Green-Up Day - Identify Files ----
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
                                                       "raw-greenup-v061", region))) %>% 
    dplyr::filter(stringr::str_detect(string=files, pattern="MCD12Q2.061_Greenup_")) 
  
  
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
  # Identify year
  dplyr::mutate(year = stringr::str_sub(string = date, start = 1, end = 4)) %>%
  # Drop 'raw' version
  dplyr::select(-date_raw) %>%
  # Identify greenup cycle
  dplyr::mutate(cycle = stringr::str_extract(string = files, 
                                             pattern = "_[[:digit:]]{1}_")) %>%
  # Simplify that column
  dplyr::mutate(cycle = gsub(pattern = "_", replacement = "", x = cycle))

# Glimpse it
dplyr::glimpse(file_all)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all')))

## ------------------------------------------------------- ##
                # Green-Up Day - Extract ----
## ------------------------------------------------------- ##

# Specify driver
focal_driver <- "raw-greenup-v061"

# Identify files we've already extracted from
done_files <- data.frame("files" = dir(file.path(path, "raw-driver-data",
                                                  focal_driver,
                                                  "_partial-extracted"))) %>%
   tidyr::separate(col = files, remove = F,
                   into = c("junk", "junk2", "year", "cycle", "file_ext")) %>%
  dplyr::mutate(cycle = gsub(pattern = "[[:alpha:]]", replacement = "", x = cycle)) %>%
  # Make a year-cycle column
   dplyr::mutate(year_cycle = paste0(year, "_", cycle))

# Remove completed files from the set of all possible files
not_done <- file_all %>%
   dplyr::mutate(year_cycle = paste0(year, "_", cycle)) %>%
   dplyr::filter(!year_cycle %in% done_files$year_cycle)

# Create a definitive object of files to extract
file_set <- not_done # Uncomment if want to only do only undone extractions
# file_set <- file_all # Uncomment if want to do all extractions

# For both of the cycles (0 & 1)
for(a_cycle in 0:1){
  
  # Global start message
  message("Processing begun for cycle ", a_cycle)
  
  # Subset to correct cycle
  cycle_files <- dplyr::filter(file_set, cycle == a_cycle)
  
  # For each year in that cycle
  for (a_year in sort(unique(cycle_files$year))){
    
    # Starting message
    message("Beginning cycle ", a_cycle, " extraction for ", a_year)
    
    # Subset to one year
    one_year_data <- dplyr::filter(cycle_files, year == a_year)
    
    # Make a list to house extracted information for a year
    year_list <- list()
    
    # Loop across region
    for (i in 1:nrow(one_year_data)){
      # Message
      message("Processing raster ", i, " of ", nrow(one_year_data))
      
      # Read in the raster
      gr_raster <- terra::rast(file.path(path, "raw-driver-data", "raw-greenup-v061",
                                         one_year_data$region[i], one_year_data$files[i]))
      
      # Extract all possible information from that dataframe
      ex_data <- exactextractr::exact_extract(x = gr_raster, y = sheds, 
                                              include_cols = c("LTER", "Shapefile_Name"),
                                              progress = FALSE) %>%
        # Unlist to dataframe
        purrr::list_rbind() %>%
        # Drop coverage fraction column
        dplyr::select(-coverage_fraction) %>%         
        # Make new relevant columns
        dplyr::mutate(cycle = a_cycle,
                      year = a_year,
                      .after = Shapefile_Name)
      
      # Add this dataframe to the list we made 
      year_list[[i]] <- ex_data
      
      # End message
      message("Finished extracting raster ", i, " of ", nrow(one_year_data)) }
    
    # Assemble a file name for this extraction
    export_name <- paste0("greenup_extract_", a_year, "_cycle", a_cycle, ".csv")
    
    # Wrangle the output of the within-year extraction
    full_data <- year_list %>%
      # Unlist to dataframe
      purrr::list_rbind() %>%
      # Handle the summarization within river (potentially across multiple rasters' pixels)
      dplyr::group_by(LTER, Shapefile_Name, cycle, year) %>%
      dplyr::summarize(days_since_jan1_1970 = floor(mean(value, na.rm = T))) %>%
      dplyr::ungroup() %>%
      # Convert the days since Jan 1, 1970 to the actual date
      dplyr::mutate(greenup_cycle_YYYYMMDD = lubridate::as_date(days_since_jan1_1970, 
                                                                origin = "1970-01-01")) %>%
      # Drop unnecessary column(s)
      dplyr::select(-days_since_jan1_1970)
    
    # Export this file for a given year
    write.csv(x = full_data, row.names = F, na = '',
              file = file.path(path, "raw-driver-data", "raw-greenup-v061",
                               "_partial-extracted", export_name))
    
    # End message
    message("Finished wrangling output for ", a_year) } 
  
  # Global end message
  message("Finished wrangling outputs for cycle ", a_cycle) }

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all')))

## ------------------------------------------------------- ##
             # Green-Up Day - Summarize ----
## ------------------------------------------------------- ##

# Identify extracted files
done_greenup <- dir(file.path(path, "raw-driver-data", "raw-greenup-v061", "_partial-extracted"))

# Make an empty list for storing data
out_list <- list()

# Read all of these files in
for(k in 1:length(done_greenup)){
  
  # Read in the kth file
  file_v1 <- read.csv(file = file.path(path, "raw-driver-data", "raw-greenup-v061", "_partial-extracted", done_greenup[k]))
  
  # Wrangle that file a bit
  file_v2 <- file_v1 %>%
    # Pivot longer
    tidyr::pivot_longer(cols = greenup_cycle_YYYYMMDD,
                        names_to = "junk", values_to = "date") %>%
    # Assemble a more informative date column name
    dplyr::mutate(name = paste0("greenup_cycle", cycle, "_", year, "MMDD")) %>%
    # Drop (now) unwanted columns
    dplyr::select(-junk, -cycle, -year) %>%
    # Pivot wider again
    tidyr::pivot_wider(names_from = name, values_from = date)
  
  # Add it to the list
  out_list[[k]] <- file_v2
  
  # Finish
  message("Retrieved file ", k, " of ", length(done_greenup)) }

## ------------------------------------------------------- ##
                    # Green-Up Day - Export ----
## ------------------------------------------------------- ##

# Wrangle output list
out_df <- out_list %>%
  # Unlist via left joining
  purrr::reduce(dplyr::left_join, by = c("LTER", "Shapefile_Name")) %>%
  # Move columns around
  dplyr::relocate(contains("2001"), contains("2002"), contains("2003"),
                  contains("2004"), contains("2005"), contains("2006"),
                  contains("2007"), contains("2008"), contains("2009"),
                  contains("2010"), contains("2011"), contains("2012"),
                  contains("2013"), contains("2014"), contains("2015"),
                  contains("2016"), contains("2017"), contains("2018"),
                  contains("2019"),
                  .after = Shapefile_Name) 

# Glimpse this too
dplyr::glimpse(out_df)

# Let's get ready to export
greenup_export <- sheds %>%
  # Join the greenup data
  dplyr::left_join(y = out_df, by = c("LTER", "Shapefile_Name"))%>%
  # this drops the geometry column, which causes issues on export
  sf::st_drop_geometry()  

# Check it out
dplyr::glimpse(greenup_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized greenup data
write.csv(x = greenup_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_greenup_2_v061.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_greenup_2_v061.csv"),
                         overwrite = T,
                         path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld"))

# End ----
