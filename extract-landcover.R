## ------------------------------------------------------- ##
      # Silica WG - Extract Spatial Data - Land Cover
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Using the watershed shapefile created in "wrangle-watersheds.R"
## Extract the following data: LAND COVER

## ------------------------------------------------------- ##
                    # Housekeeping ----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, stars, terra, exactextractr, NCEAS/scicomptools, 
                 googledrive, readxl)

# Clear environment
rm(list = ls())

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
              # Land Cover - Extract ----
## ------------------------------------------------------- ##

# Read in dataset
lulc_raw <- terra::rast(x = file.path(path, "raw-driver-data", "raw-glcc-landcover-data",
                                      "global-glcc-wgs84", "gblulcgeo20.tif"))

# Exploratory plot for overlap
# plot(lulc_raw, axes = T, reset = F)
# plot(sheds["LTER"], axes = T, add = T)

# Strip out land cover for our polygons
land_out <- exactextractr::exact_extract(x = lulc_raw, y = sheds,
                                          include_cols = c("LTER", "Shapefile_Name")) %>%
  # Above returns a list so switch it to a dataframe
  purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
  # Filter out NAs
  dplyr::filter(!is.na(value)) %>%
  # Summarize to count number of pixels per category
  dplyr::group_by(LTER, Shapefile_Name, value) %>%
  dplyr::summarize(pixel_ct = dplyr::n()) %>%
  dplyr::ungroup()

# Check that dataframe
dplyr::glimpse(land_out)

## ------------------------------------------------------- ##
                # Land Cover - Summarize ----
## ------------------------------------------------------- ##

# Load index
lulc_index <- read.csv(file = file.path(path, "raw-driver-data",
                                        "raw-glcc-landcover-data", "LULC_index.csv")) %>%
  # Rename the integer code column to match the extracted data
  dplyr::rename(value = lulc_code)

# Wrangle extracted data
land_v2 <- land_out %>%
  # Attach index information
  dplyr::left_join(y = lulc_index, by = "value") %>%
  # Simplify data now that descriptive categories are included
  dplyr::select(-value) %>%
  dplyr::relocate(lulc_category, .after = Shapefile_Name) %>%
  dplyr::mutate(lulc_category = tolower(gsub(pattern = " |-", 
                                             replacement = "_", 
                                             x = lulc_category))) %>%
  # Streamline category information
  dplyr::mutate(lulc_category = dplyr::case_when(
    lulc_category %in% c("dryland_cropland_and_pasture", 
                         "irrigated_cropland_and_pasture",
                         "cropland/grassland_mosaic",
                         "cropland/woodland_mosaic") ~ "cropland",
    lulc_category %in% c("herbaceous_wetland", "wooded_wetland") ~ "wetland",
    lulc_category %in% c("bare_ground_tundra", "wooded_tundra",
                         "mixed_tundra") ~ "tundra",
    lulc_category %in% c("shrubland", "grassland", "savanna", 
                         "mixed_shrubland/grassland") ~ "shrubland_grassland",
    TRUE ~ lulc_category)) %>%
  # Drop unwanted categories
  dplyr::filter(!lulc_category %in% c("snow_or_ice", "water_bodies")) %>%
  # Summarize again with simplified categories
  dplyr::group_by(LTER, Shapefile_Name, lulc_category) %>%
  dplyr::summarize(land_total = sum(pixel_ct, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Get total pixel count per river (across category)
  dplyr::group_by(LTER, Shapefile_Name) %>%
  dplyr::mutate(total_pixels = sum(land_total, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Calculate percent of total
  dplyr::mutate(perc_total = ((land_total / total_pixels) * 100)) %>%
  # Drop pixel count columns
  dplyr::select(-land_total, -total_pixels)

# Check remaining categories  
sort(unique(land_v2$lulc_category))

# Glimpse this object
dplyr::glimpse(land_v2)

# Make a version of this that is wide
land_wide <- land_v2 %>%
  # Add "land" to each of these before they become columns
  dplyr::mutate(lulc_category = paste0("land_", lulc_category)) %>%
  # Now pivot wider
  tidyr::pivot_wider(names_from = lulc_category,
                     values_from = perc_total)

# Also identify the dominant land cover type(s) in each river's polygon
land_major <- land_v2 %>%
  # Filter to only max of each rock type per river
  dplyr::group_by(LTER, Shapefile_Name) %>%
  dplyr::filter(perc_total == max(perc_total, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Remove the percent total
  dplyr::select(-perc_total) %>%
  # Collapse ties for 'major' into one row
  dplyr::group_by(LTER, Shapefile_Name) %>%
  dplyr::summarize(major_land = paste(lulc_category, collapse = "; ")) %>%
  dplyr::ungroup()

# Combine major with per-category
land_actual <- land_wide %>%
  dplyr::left_join(y = land_major, by = c("LTER", "Shapefile_Name")) %>%
  dplyr::relocate(major_land, .after = Shapefile_Name)

# Glimpse it
dplyr::glimpse(land_actual)

## ------------------------------------------------------- ##
                # Land Cover - Export ----
## ------------------------------------------------------- ##
# Let's get ready to export
land_export <- sheds %>%
  # Join the land data
  dplyr::left_join(y = land_actual, by = c("LTER", "Shapefile_Name"))%>%
  # this drops the geometry column, which causes issues on export
  sf::st_drop_geometry()  

# Check it out
dplyr::glimpse(land_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized lithology data
write.csv(x = land_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_landcover_2.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_landcover_2.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld"))

# End ----
