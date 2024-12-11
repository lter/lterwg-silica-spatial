## ------------------------------------------------------- ##
# Silica WG - Combine Drivers
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Identify all extracted driver data (i.e., spatial data) and combine into a single file
## This makes the analysis phase easier on the WG as they can skip redundant joining steps

## ------------------------------------------------------- ##
                    # Housekeeping ----
## ------------------------------------------------------- ##

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, NCEAS/scicomptools, googledrive, readxl, magrittr, supportR)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

# Load in shape area check
area_check <- read.csv(file.path(path, "shape_checks", "artisanal_shape_area_check_2.csv"))

# Check it out
dplyr::glimpse(area_check)

# Load in site names with lat/longs
sites <- readxl::read_excel(path = file.path(path, "site-coordinates",
                                             "silica-coords_RAW.xlsx")) %>%
  ## Pare down to minimum needed columns
  dplyr::select(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name) %>%
  ## Drop duplicate rows (if any)
  dplyr::distinct() 


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
  dplyr::left_join(y = sites, by = c("LTER", "Shapefile_Name"))%>%
  # Remove any watersheds without a shapefile
  dplyr::filter(!is.na(Shapefile_Name) &
                nchar(Shapefile_Name) != 0 &
                !Shapefile_Name %in% c("?", "MISSING"))

sheds$Stream_Name <- ifelse(!is.na(sheds$Stream_Name.x), sheds$Stream_Name.x, sheds$Stream_Name.y)
sheds$Discharge_File_Name <- ifelse(!is.na(sheds$Dsc_F_N), sheds$Dsc_F_N, sheds$Discharge_File_Name)
sheds <- sheds %>% select (-c(Stream_Name.x, Stream_Name.y, expert_area_km2, shape_area_km2, exp_are, hydrshd, real_ar, 
                              Dsc_F_N))

# Check that out
dplyr::glimpse(sheds)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sheds')))

## ------------------------------------------------------- ##
# Combine Extracted Data ----
## ------------------------------------------------------- ##
# List current extracted data
(extracted_data <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld"), pattern = ".csv") %>%
  dplyr::filter(name != "all-data_si-extract.csv"))

# Download all files
purrr::walk2(.x = extracted_data$id, .y = extracted_data$name,
             .f = ~ googledrive::drive_download(file = googledrive::as_id(.x), 
                                                path = file.path(path, "extracted-data", .y),
                                                overwrite = T))

# Make an empty list
data_list <- list()

# Read the files into the list
for(file_name in extracted_data$name){
  data_list[[file_name]] <- read.csv(file = file.path(path, "extracted-data", file_name))
}

# Duplicate the sheds object
driver_df <- sheds

# For each file
for(k in 1:length(data_list)){
  # Processing message
  message("Adding ", names(data_list[k]), " to 
          output dataframe")
  
  # Left join onto the driver dataframe and overwrite the object
  driver_df %<>%
    dplyr::left_join(y = data_list[[k]],
                     by = c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name"))
}

# Glimpse what we wind up with
dplyr::glimpse(driver_df)

# Check all column names (should be *a lot*)
names(driver_df)

# Check for dropped rivers (i.e., rows)
## Stream names (chemistry river names)
supportR::diff_check(old = unique(sheds$Stream_Name), new = unique(driver_df$Stream_Name))
## Discharge file names (discharge river names)
supportR::diff_check(old = unique(sheds$Discharge_File_Name), 
                     new = unique(driver_df$Discharge_File_Name))

## We need to drop the geometry column before export because it messes up the .csv file
driver_df <-sf::st_drop_geometry(driver_df)

## Lastly, we need to add the umlauts, commas, hyphens, etc. back to the Finnish names
## These needed to be removed to ensure that the data extractions workflow ran properly
driver_df$Stream_Name[driver_df$Stream_Name == "Kiiminkij 13010 4tien s"] <- "Kiiminkij 13010 4-tien s"
driver_df$Stream_Name[driver_df$Stream_Name == "Lestijoki 10800 8tien s"] <- "Lestijoki 10800 8-tien s"
driver_df$Stream_Name[driver_df$Stream_Name == "Mustijoki 42  6010"] <- "Mustijoki 4,2  6010"
driver_df$Stream_Name[driver_df$Stream_Name == "Mustionjoki 49  15500"] <- "Mustionjoki 4,9  15500"
driver_df$Stream_Name[driver_df$Stream_Name == "Porvoonjoki 115  6022"] <- "Porvoonjoki 11,5  6022"
driver_df$Stream_Name[driver_df$Stream_Name == "SIMOJOKI AS 13500"] <- "SIMOJOKI AS. 13500"
driver_df$Stream_Name[driver_df$Stream_Name == "Vantaa 42  6040"] <- "Vantaa 4,2  6040"

# Export this
write.csv(x = driver_df, na = '', row.names = F,
          file = file.path(path, "extracted-data", "all-data_si-extract_2_202412.csv"))

# And upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "all-data_si-extract_2_202412.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld"))

# End ----
