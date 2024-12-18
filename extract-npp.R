## ------------------------------------------------------- ##
#       Silica WG - Extract Spatial Data - NPP
## ------------------------------------------------------- ##
# Written by:
## Angel Chen, Nick Lyon

# Purpose:
## Using the watershed shapefiles created in "wrangle-watersheds.R"
## Extract the following data: NPP

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
            # MOD17A3HGF -v061 - Identify Files ----
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
                                                       "raw-npp-v061", region))) %>% 
    dplyr::filter(stringr::str_detect(string=files, pattern="MOD17A3HGF.061_Npp_500m")) 
  
  
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
  dplyr::select(-date_raw) 

# Glimpse it
dplyr::glimpse(file_all)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds', 'file_all')))

## ------------------------------------------------------- ##
                      # NPP - Extract ----
## ------------------------------------------------------- ##

# Specify driver
focal_driver <- "raw-npp-v061"

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
#file_set <- not_done # Uncomment if want to only do only undone extractions
file_set <- file_all # Uncomment if want to do all extractions

# Loop across years...
for (a_year in unique(file_set$year)){
  
  # Starting message
  message("Processing begun for year ", a_year)
  
  # Subset to one year
  one_year_data <- dplyr::filter(file_set, year == a_year)
  
  # Make a list to house extracted information for a year
  year_list <- list()
  
  for (i in 1:nrow(one_year_data)){
      # Read in the raster
      npp_raster <- terra::rast(file.path(path, "raw-driver-data", "raw-npp-v061", one_year_data$region[i], one_year_data$files[i]))
      
      # Extract all possible information from that dataframe
      ex_data <- exactextractr::exact_extract(x = npp_raster, y = sheds, 
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
        dplyr::filter(value >= -30000 & value <= 32700) %>%
        # Make new relevant columns
        dplyr::mutate(year = a_year,
                      .after = Shapefile_Name)
      
      # Add this dataframe to the list we made 
      year_list[[i]] <- ex_data
      
      # End message
      message("Finished extracting raster ", i, " of ", nrow(one_year_data), " (", a_year, ")") 
  }

  # Assemble a file name for this extraction
  export_name <- paste0("npp_extract_", a_year, ".csv")
  
  # Wrangle the output of the within-year extraction
  full_data <- year_list %>%
    # Unlist to dataframe
    purrr::list_rbind() %>%
    # Apply scaler value
    ## See "Layers" dropdown here: https://lpdaac.usgs.gov/products/mod17a3hgfv061/
    # dplyr::mutate(descaled_val = value * 0.0001) %>%
    # Handle the summarization within river (potentially across multiple rasters' pixels)
    dplyr::group_by(LTER, Shapefile_Name, year) %>%
    #dplyr::summarize(npp = mean(descaled_val, na.rm = T)) %>% ## This is mean per pixel 
    dplyr::summarize(npp = mean(value, na.rm = T)) %>% ## This is mean per pixel -- checking to see if data are already scaled
    dplyr::ungroup() %>%
    # Drop unnecessary columns
    dplyr::select(-year) %>% 
    dplyr::rename_with(.fn = ~paste0("npp_", a_year, "_kgC_m2_year"), .cols = npp)
  
  # Export this file for a given year
  write.csv(x = full_data, row.names = F, na = '',
            file = file.path(path, "raw-driver-data", "raw-npp-v061",
                             "_partial-extracted", export_name))
  
  # End message
  message("Finished wrangling output for ", a_year) }

## ------------------------------------------------------- ##
                      # NPP - Export ----
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
  purrr::reduce(dplyr::left_join, by = c("LTER", "Shapefile_Name")) 

# Glimpse it
dplyr::glimpse(out_df)

# Let's get ready to export
npp_export <- sheds %>%
  # Join the npp data
  dplyr::left_join(y = out_df, by = c("LTER", "Shapefile_Name"))%>%
  # this drops the geometry column, which causes issues on export
  sf::st_drop_geometry()  

# Check it out
dplyr::glimpse(npp_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized npp data
write.csv(x = npp_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_npp_2_v061.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_npp_2_v061.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld"))

# End ----
