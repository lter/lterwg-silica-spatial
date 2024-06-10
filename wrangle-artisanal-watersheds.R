## ------------------------------------------------------- ##
# Silica WG - Wrangle Watershed Shapefiles
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon
# Edited by Sidney Bush -- for data release 2
## LATER: will need to add HydroSHEDS sites for: Murray-Darling (Australia) and Canada sites


# Purpose:
## Wrangle "artisanal" watershed shapefiles into a single file to extract driver data
## Artisanal = mixed provenance / provided by WG participants

## ------------------------------------------------------- ##
# Housekeeping -----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, magrittr, googledrive, sf, supportR, NCEAS/scicomptools, readxl)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

# Define the Drive folder for exporting checks / diagnostics to
check_folder <- googledrive::as_id("https://drive.google.com/drive/u/1/folders/1-IawEkFjfkrzAlgolvS1KTTm0pEW9K9h")


## ------------------------------------------------------- ##
# Reference Table Acquisition ----
## ------------------------------------------------------- ##

# Grab ID of the GoogleSheet with site coordinates
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AIPkWhVuXjqFUk9PVA")) %>%
  dplyr::filter(name == "Site_Reference_Table") %>%
  googledrive::drive_download(file = ., overwrite = T,
                              path = file.path(path, "site-coordinates",
                                               "silica-coords_RAW.xlsx"))

# Read in site coordinates (i.e., ref table)
coord_df <- readxl::read_excel(path = file.path(path, "site-coordinates",
                                                "silica-coords_RAW.xlsx")) %>%
  ## Pare down to minimum needed columns
  dplyr::select(LTER, Shapefile_Name, Stream_Name, drainSqKm, Latitude, Longitude, Shapefile_CRS_EPSG) %>%
  ## Drop duplicate rows (if any)
  dplyr::distinct() %>%
  ## Rename some columns
  dplyr::rename(expert_area_km2 = drainSqKm,
                crs_code = Shapefile_CRS_EPSG) |>
  filter(is.na(Shapefile_Name)!=T|
           is.na(Shapefile_Name)==T& !LTER %in% c("ARC","BcCZO", "BNZ", "Cameroon", "Catalina Jemez", "Coal Creek11", "Finnish Environmental Institute", 
                                                  "HYBAM", "KNZ", "KRR", "LMP", "LUQ", "MCM", "PIE", "Tanguro(Jankowski)", "USGS")
         & (!is.na(Latitude)&!is.na(Longitude)))

# Glimpse this
dplyr::glimpse(coord_df)
coord_df|> filter(is.na(Shapefile_Name)) |> pull(LTER) |> unique()

## ------------------------------------------------------- ##
# Acquire Shapefiles ----
## ------------------------------------------------------- ##

# Identify all shapefiles currently in Aurora
server_files <- data.frame("files" = dir(path = file.path(path, 'artisanal-shapefiles-2'))) %>%
  # Split off file type
  dplyr::mutate(file_type = stringr::str_sub(string = files, start = nchar(files) - 3,
                                             nchar(files)))

# Check that out
dplyr::glimpse(server_files)

# What file types are included?
sort(unique(server_files$file_type))

# Normally, `googledrive::drive_ls()` is the way we recommend to download files
# Unfortunately, it doesn't work super well for >500 files
# So, I've downloaded the watersheds manually and re-uploaded to Aurora
# This will need to be re-done if any files change / more shapefiles are added

# See Drive folder to download here:
## https://drive.google.com/drive/folders/1TLEFKLWUpTKwxKiZv9nqhdwccflPuF9g

## ------------------------------------------------------- ##
# Combine Shapefiles ----
## ------------------------------------------------------- ##

# Identify just the .shp files
raw_sheds <- server_files %>%
  # Filter to only shp
  dplyr::filter(file_type == ".shp") %>%
  # Pull out just the name column
  dplyr::pull(files) %>%
  ## Drop file extension to match with reference table
  gsub(pattern = "\\.shp", replacement = "", x = .)

# Compare shapefiles we have with those that are named in the reference table
supportR::diff_check(old = unique(coord_df$Shapefile_Name), 
                     new = unique(raw_sheds))
## Any 'in old but not new' = shapefiles named in reference table but not on Aurora
## Any 'in new but not old' = shapefiles in Aurora that aren't in the reference table

# Wrangle river coordinates
good_sheds <- coord_df %>%
  # Filter to only shapefiles in ref. table & on Aurora
  dplyr::filter(Shapefile_Name %in% raw_sheds) %>%
  # Drop any non-unique rows (shouldn't be any but good to double check)
  dplyr::distinct() %>%
  # Condense what remains to ensure no duplicates
  dplyr::group_by(LTER, Shapefile_Name, crs_code) %>%
  dplyr::summarize(expert_area_km2 = mean(expert_area_km2, na.rm = T),
                   Latitude = dplyr::first(Latitude),
                   Longitude = dplyr::first(Longitude)) %>%
  dplyr::ungroup()

# Check that out
dplyr::glimpse(good_sheds)

# Double check that there are no duplicated shapefile names
good_sheds %>%
  dplyr::group_by(Shapefile_Name) %>%
  dplyr::summarize(name_ct = dplyr::n()) %>%
  dplyr::filter(name_ct != 1)
# Kemijoen vesistöalue was duplicated four times but I think that's not a mistake

# Create an empty object to store combined shapefiles
all_shps <- NULL

# Turn off spherical processing
sf::sf_use_s2(F)

# For each shapefile we have:
for(focal_name in sort(unique(good_sheds$Shapefile_Name))){
  
  # Read in the shapefile
  focal_shp_raw <- sf::st_read(file.path(path, "artisanal-shapefiles-2",
                                         paste0(focal_name, ".shp")), quiet = T)
  
  
  # If CRS is missing (for some reason), manually set what the CRS *should* be
  if(is.na(sf::st_crs(focal_shp_raw))){
    sf::st_crs(focal_shp_raw) <- dplyr::filter(coord_df, Shapefile_Name == focal_name)$crs_code
  }
  
  # Make sure CRS is WGS84 (EPSG code 4326)
  focal_shp_wgs84 <- focal_shp_raw %>%
    sf::st_transform(crs = 4326)
  
  # Make sure the polygon geometry is consistently named
  sf::st_geometry(obj = focal_shp_wgs84) <- "geom"
  
  # Calculate shapefile area
  focal_area <- focal_shp_wgs84 %>%
    sf::st_area(x = .) %>%
    units::set_units(x = ., km^2)
  
  # Identify other relevant information we'll want to attach to the shapefile
  focal_info <- good_sheds %>%
    dplyr::filter(Shapefile_Name == focal_name)
  
  # Wrangle raw shapfiles as needed
  focal_shp <- focal_shp_wgs84 %>%
    ## Attach relevant information into this object
    dplyr::mutate(LTER = focal_info$LTER,
                  Shapefile_Name = focal_info$Shapefile_Name,
                  expert_area_km2 = focal_info$expert_area_km2,
                  shape_area_km2 = as.numeric(focal_area)) %>%
    ## Pare down to only this information (+ geometry)!
    ### This approach flexibly ignores whatever idiosyncratic columns may be in the raw object
    dplyr::select(LTER, Shapefile_Name, expert_area_km2, shape_area_km2, geom) %>%
    ## Keep only unique rows
    dplyr::distinct() %>%
    ## Filter to only the largest sub-shape (if there are more than one)
    dplyr::filter(shape_area_km2 == max(shape_area_km2)) %>%
    ## Drop Z/M axes (if they are included)
    sf::st_zm(drop = TRUE, what = "ZM") %>%
    ## Make sure this is a polygon
    sf::st_cast("MULTIPOLYGON")
  
  # Attach this shape to the rest of them
  all_shps %<>%
    dplyr::bind_rows(focal_shp)
  
  # Loop end message
  message("Completed processing for shapefile: ", focal_name, " (", focal_shp$LTER, ")") }

# Exploratory plot
plot(all_shps["Shapefile_Name"], axes = T, main = NULL)

# Tidy up environment
rm(list = setdiff(ls(), c("path", "coord_df", "all_shps", "check_folder")))


## ------------------------------------------------------- ##
# Export Results ----
## ------------------------------------------------------- ##

# Long column names get coerced into abbreviations if left alone
final_shps <- all_shps %>%
  # Rename the columns more succinctly
  dplyr::rename(shp_nm = Shapefile_Name,
                exp_area = expert_area_km2,
                real_area = shape_area_km2)

# Take one last look
dplyr::glimpse(final_shps)

# Export the combine shapefile for all rivers
sf::st_write(obj = final_shps, delete_layer = T,
             dsn = file.path(path, "site-coordinates", "silica-watersheds_artisanal.shp"))

# Process the shapefile object a bit to make a flat table variant
shps_df <- all_shps %>%
  # Drop geometry
  sf::st_drop_geometry(x = .) %>%
  # Calculate some area difference metrics
  dplyr::mutate(
    area_diff_km2 = round((shape_area_km2 - expert_area_km2), digits = 2),
    area_diff_perc = round((area_diff_km2 / expert_area_km2) * 100, digits = 2),
    area_diff_direction = dplyr::case_when(
      abs(area_diff_perc) < 5 ~ "under 5% mismatch",
      abs(area_diff_perc) >= 5 & expert_area_km2 <= 5 ~ "mismatch but watershed tiny (<5km2)",
      area_diff_perc <= -5 & expert_area_km2 > 5 ~ "underestimated",
      area_diff_perc >= 5 & expert_area_km2 > 5 ~ "overestimated"))

# Glimpse it
dplyr::glimpse(shps_df)

# How many shapefiles where area has >5% mismatch with expert know-how?
shps_df %>%
  dplyr::group_by(area_diff_direction) %>%
  dplyr::summarize(file_ct = dplyr::n())

# Create a checks folder if it doesn't exist yet
dir.create(path = file.path(path, "shape_checks"), showWarnings = F)

# Export locally
write.csv(shps_df, file = file.path(path, "shape_checks", "artisanal_shape_area_check_2.csv"), 
          row.names = F, na = '')

# Upload to Drive
googledrive::drive_upload(media = file.path(path, "shape_checks", "artisanal_shape_area_check_2.csv"), 
                          overwrite = T, 
                          path = check_folder)

# Tidy up environment
rm(list = ls()); gc()

# End ----
