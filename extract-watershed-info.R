## ----------------------------------------------------------------------- ##
                        # LTER WG: Silica Synthesis
## ----------------------------------------------------------------------- ##

# Purpose:
## Using the watershed shapefiles created in "identify-watersheds.R"
## extract lithology and land cover data for each watershed

# Written by:
## Set up + lithology extraction by Nick Lyon

# Housekeeping ---------------------------------------------------------------

# Read needed libraries
library(tidyverse); library(sf); library(stars); library(terra); library(exactextractr)

# Clear environment
rm(list = ls())

# Set working directory to location of shared data
## Identify path
path <- file.path('/', "home", "shares", "lter-si", "si-watershed-extract")
## Set WD to path
setwd(path)
## Check that it worked
getwd()

# Site Coordinate & Watershed Shapefile Retrieval ----------------------------

# Load in site names with lat/longs
sites <- read.csv("tidy_SilicaSites.csv")

# Check it out
str(sites)

# Grab the shapefiles the previous script (see PURPOSE section) created
sheds <- sf::st_read('watershed-shapefiles/SilicaSynthesis_allWatersheds.shp')

# Check that out
str(sheds)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Lithology Data ------------------------------------------------------------

# Ultimately want both lithology and land cover but we'll start with rocks

# Lithology Pre-Processing ---------------------------------------------------

# Pull in the raw lithology data
rocks_raw <- stars::read_stars("extracted-data/raw-lithology-data/glim_wgs84_0point5deg.txt.asc")

# Convert it to an sf object
rocks_sf <- sf::st_as_sf(rocks_raw)

# Examine it
str(rocks_sf)
rocks_sf$geometry
st_crs(rocks_sf)
  ## CRS is missing!

# Prepare the lithology dataset for extraction
rocks_actual <- rocks_sf %>%
  # Because the structure call shows that it is WGS84 we can set the NA without fear
  sf::st_set_crs(value = 4326) %>%
  # Name the data column more descriptively
  dplyr::rename(rock_code = glim_wgs84_0point5deg.txt.asc)

# Check it now
str(rocks_actual)
st_crs(rocks_actual)
  ## Looks good!

# Plot just to make sure they seem to be stacking correctly
plot(rocks_actual["rock_code"], main = "All Lithology Information", axes = T, reset = F)
## Note that plotting the global lithology takes a minute
plot(sheds["LTER"], axes = T, add = T)
  ## Looks about right!

# Preemptively turn off s2 processing
sf::sf_use_s2(use_s2 = F)

# Lithology Extraction ------------------------------------------------------

# Strip out the lithology data from within our watershed polygons
rocky_sheds <- sheds %>%
  # Identify intersections between watersheds and rocks
  ## Note this line takes a minute
  st_intersection(rocks_actual)

# Plot it for exploratory purposes
plot(rocky_sheds["rock_code"], main = "Lithology Extraction", axes = T)
## If you use the "Zoom" button you'll see there are colors in there
## In the plotting pane you can only see the edges of all the cells

# Lithology Summarization ---------------------------------------------------

# Bring in the index tying rock code integers with rock abbreviations
rock_index_raw <- read.table(file = "extracted-data/raw-lithology-data/Classnames.txt",
                             header = T, sep = ';')

# Fix this index to make it more usable
rock_index <- rock_index_raw %>%
  # Rename the most important columns
  dplyr::rename(rock_code = OBJECTID,
                rock_abbrev = xx) %>%
  # And get a more descriptive version of each of the rock types
  dplyr::mutate(
    rock_type = case_when(
      # Abbreviations found here:
      # https://www.clisap.de/fileadmin/B-Research/IA/IA5/LITHOMAP/
      rock_abbrev == 'su' ~ 'unconsolidated_sediments',
      rock_abbrev == 'ss' ~ 'siliciclastic_sedimentary_rocks',
      rock_abbrev == 'sm' ~ 'mixed_sedimentary_rocks',
      rock_abbrev == 'py' ~ 'pyroclastic',
      rock_abbrev == 'sc' ~ 'carbonate_sedimentary_rocks',
      rock_abbrev == 'ev' ~ 'evaporites',
      rock_abbrev == 'mt' ~ 'metamorphic_rocks',
      rock_abbrev == 'pa' ~ 'acid_plutonic_rocks',
      rock_abbrev == 'pi' ~ 'intermediate_plutonic_rocks',
      rock_abbrev == 'pb' ~ 'basic_plutonic_rocks',
      rock_abbrev == 'va' ~ 'acid_volcanic_rocks',
      rock_abbrev == 'vi' ~ 'intermediate_volcanic_rocks',
      rock_abbrev == 'vb' ~ 'basic_volcanic_rocks',
      rock_abbrev == 'ig' ~ 'ice_and_glaciers',
      rock_abbrev == 'wb' ~ 'water_bodies',
      rock_abbrev == 'nd' ~ 'no_data',
      TRUE ~ as.character(rock_abbrev) ) ) %>%
  # Remove unneeded columns
  select(rock_code, rock_type)

# Check that worked
head(rock_index)  
sort(unique(rock_index$rock_type))
  
# Process the extracted lithology information into a dataframe
rock_data_v1 <- rocky_sheds %>%
  # Remove the truly spatial part of the data to make it easier to work with
  st_drop_geometry() %>%
  # Bring over the rock names from the index
  left_join(rock_index, by = "rock_code") %>%
  # Remove the now-unneeded code column
  dplyr::select(-rock_code) %>%
  # Group by LTER and uniqueID
  group_by(LTER, uniqueID, rock_type) %>%
  # Count the instances within each rock type
  ## 0.5° degree pixels within the watershed that contain this rock type
  summarise(rock_totals = n()) %>%
  # Make it a dataframe (to avoid a list of tibbles)
  as.data.frame() %>%
  # Remove the 'no_data' cells
  filter(rock_type != "no_data") %>%
  # # Group by LTER and uniqueID
  group_by(LTER, uniqueID) %>%
  # We'll want the totals as a percent (total pixels is not very intuitive)
  dplyr::mutate( total_shed_pixels = sum(rock_totals) ) %>%
  # Again, return a dataframe, not a tibble
  as.data.frame() %>%
  # Now ungroup
  ungroup() %>%
  # And calculate the percent of total for each row
  dplyr::mutate( perc_total = ((rock_totals / total_shed_pixels) * 100) ) %>%
  # Remove the two pixel count columns
  dplyr::select(-rock_totals, -total_shed_pixels)

# Now we want to split into two directions
## First: get a version where each rock type is its own column
rock_data_wide <- rock_data_v1 %>%
  # Pivot to wide format
  pivot_wider(id_cols = c(LTER, uniqueID),
              names_from = rock_type,
              values_from = perc_total)

## Second: get the *majority* rock for each watershed
rock_data_major <- rock_data_v1 %>%
  # Filter to only max of each rock type per uniqueID & LTER
  group_by(LTER, uniqueID) %>%
  filter(perc_total == max(perc_total)) %>%
  # Remove the percent total
  dplyr::select(-perc_total) %>%
  # Get the columns into wide format where the column name and value are both whatever the dominant rock was
  pivot_wider(id_cols = c(LTER, uniqueID),
              names_from = rock_type,
              values_from = rock_type) %>%
  # Paste all the non-NAs (i.e., the dominant rocks) into a single column
  unite(col = major_rock, -LTER:-uniqueID, na.rm = T, sep = "; ")

# Now attach the major rocks to the wide format one
rock_data_actual <- rock_data_wide %>%
  left_join(rock_data_major, by = c("LTER", "uniqueID")) %>%
  relocate(major_rock, .after = uniqueID)

# Examine
str(rock_data_actual)
head(rock_data_actual)
names(rock_data_actual)

# Lithology Export -----------------------------------------------------------

# Let's get ready to export
rock_export <- sites %>%
  left_join(rock_data_actual, by = c("LTER", "uniqueID"))

# Check it out
head(rock_export)
names(rock_export)

# Export both this and the shapefile that contains the cropped rock data
write.csv(x = rock_export,
          file = "extracted-data/SilicaSites_withLithologyData.csv",
          na = '', row.names = F)
st_write(obj = rocky_sheds,
         dsn = "extracted-data/SilicaSynthesis_LithologyPolygons.shp",
         delete_layer = T)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Land Cover Data ------------------------------------------------------------

# NLCD Raster CRS Transformation ---------------------------------------------

# The NLCD rasters I downloaded manually do not match the CRS of the watershed shapefiles we want to align them to.
# Transforming the rasters is straightforward but computationally intensive so we will do all of those here and write out the transformed raster so that our "actual" landcover processing can just read in a raster of landcover data with the correct CRS
# Rather than needing to slow the code down every time we re-run the script to re-do the same transformation

# All of the following lines were run once and then commented out but retained for posterity.

# Process Alaska Data (for ARC)
## 1) Read in 'raw' (i.e., direct from NLCD) file
# ak_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-Alaska-2016/NLCD_2016_Land_Cover_AK_20200724.img")

## 2) Transform that raster to use the same CRS as the sf object
# ak_fix <- terra::project(x = ak_raw, y = "epsg:4326")
### Note that this takes 4-5 minutes

## 3) Save that modified raster for subsequent use
# terra::writeRaster(x = ak_fix, filename = "extracted-data/raw-landcover-data/NLCD-Alaska-2016/NLCD_Alaska_2016_WGS84.tiff", overwrite = T)

# Process Puerto Rico (LUQ) file
# pr_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-PuertoRico-2001/pr_landcover_wimperv_10-28-08_se5.img")
# pr_fix <- terra::project(x = pr_raw, y = "epsg:4326")
# terra::writeRaster(pr_fix, "extracted-data/raw-landcover-data/NLCD-PuertoRico-2001/NLCD_PuertoRico_2001_WGS84.tiff")

# All subsequent watersheds I had to draw a bounding box around the rough area and download the data so they have a different starting format

# Process AND bounding box raster
# and_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-AND-bbox-2019/NLCD_2019_Land_Cover_L48_20210604_x0irNUwuHgiIccFQinR7.tiff")
# and_fix <- terra::project(x = and_raw, y = "epsg:4326")
# terra::writeRaster(and_fix, "extracted-data/raw-landcover-data/NLCD-AND-bbox-2019/NLCD_AND_bbox_2019_WGS84.tiff")

# Process HBR bounding box raster
# hbr_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-HBR-bbox-2019/NLCD_2019_Land_Cover_L48_20210604_S7g5pOegmluHZTosWHHI.tiff")
# hbr_fix <- terra::project(x = hbr_raw, y = "epsg:4326")
# terra::writeRaster(hbr_fix, "extracted-data/raw-landcover-data/NLCD-HBR-bbox-2019/NLCD_HBR_bbox_2019_WGS84.tiff")

# Process LMP bounding box
# lmp_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-LMP-bbox-2019/NLCD_2019_Land_Cover_L48_20210604_cnpqFMtiv6oDEXEplYtp.tiff")
# lmp_fix <- terra::project(x = lmp_raw, y = "epsg:4326")
# terra::writeRaster(lmp_fix, "extracted-data/raw-landcover-data/NLCD-LMP-bbox-2019/NLCD_LMP_bbox_2019_WGS84.tiff")

# Process KRR bbox
# krr_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-KRR-bbox-2019/NLCD_2019_Land_Cover_L48_20210604_kpuczfO6eW1kYSWAeVXR.tiff")
# krr_fix <- terra::project(x = krr_raw, y = "epsg:4326")
# terra::writeRaster(krr_fix, "extracted-data/raw-landcover-data/NLCD-KRR-bbox-2019/NLCD_KRR_bbox_2019_WGS84.tiff")

# Process NWT bbox
# nwt_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-NWT-bbox-2019/NLCD_2019_Land_Cover_L48_20210604_2JttWZu6aZxdthItgKGn.tiff")
# nwt_fix <- terra::project(x = nwt_raw, y = "epsg:4326")
# terra::writeRaster(nwt_fix, "extracted-data/raw-landcover-data/NLCD-NWT-bbox-2019/NLCD_NWT_bbox_2019_WGS84.tiff")

# Process Sagehen bbox
# sagehen_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-Sagehen-bbox-2019/NLCD_2019_Land_Cover_L48_20210604_YLhilf2MDvLnp2ilVR3P.tiff")
# sagehen_fix <- terra::project(x = sagehen_raw, y = "epsg:4326")
# terra::writeRaster(sagehen_fix, "extracted-data/raw-landcover-data/NLCD-Sagehen-bbox-2019/NLCD_Sagehen_bbox_2019_WGS84.tiff")

# Process UMR bbox
## This one differs because it was too large to draw a bbox for via the web app so we need to use the whole USA raster
# usa_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-ContinentalUS-2019/NLCD_ContinentalUS_2019_WGS84.tiff")
## Crop the US to a bounding box (xmin, xmax, ymin, ymax)
# usa_crop <- terra::crop(usa_raw, terra::ext(-97.5, -87, 37, 49))
## Convert it to our preferred CRS
# umr_fix <- terra::project(x = usa_crop, y = "epsg:4326")
## Write it
# terra::writeRaster(umr_fix, "extracted-data/raw-landcover-data/NLCD-UMR-bbox-2019/NLCD_UMR_bbox_2019_WGS84.tiff")

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Luquillo LTER (LUQ) Land Cover (LC) Processing ----------------------------------

# Prepare an sf object of just this LTER's watershed shapes
luq_sf <- sheds %>%
  filter(LTER == "LUQ")

# Read in the raster
pr_fix <- terra::rast("extracted-data/raw-landcover-data/NLCD-PuertoRico-2001/NLCD_PuertoRico_2001_WGS84.tiff")

# Make sure CRS are same
crs(pr_fix)
st_crs(luq_sf)

# Plot that to see how it looks
plot(pr_fix, axes = T, reset = F)
plot(luq_sf["uniqueID"], add = T)

# Extract data from the raster using the sf object
luq_lc_list <- exactextractr::exact_extract(x = pr_fix, y = luq_sf, include_cols = c("LTER", "uniqueID"))
str(luq_lc_list)

# Grab the NLCD index that connects integer codes to meaningful categories
nlcd_index <- read.csv("extracted-data/raw-landcover-data/NLCD_index.csv")
head(nlcd_index)

# Process our landcover information
luq_lc_actual <- luq_lc_list %>%
  # Make a dataframe from the selected list columns
  map_dfr(select, c(LTER, uniqueID, value)) %>%
  # Rename the land cover column
  rename(nlcd_code = value) %>%
  # Group by code, LTER, and uniqueID
  group_by(LTER, uniqueID, nlcd_code) %>%
  # Count pixels per code within the watershed
  summarise(cover_pixel_ct = n()) %>%
  # Grab all of the contents of the index
  left_join(nlcd_index, by = "nlcd_code") %>%
  # Keep only columns that we want
  select(LTER, uniqueID, nlcd_category, cover_pixel_ct) %>%
  # Export as dataframe
  as.data.frame()

# Check it out
head(luq_lc_actual)

# Save this out
write.csv(luq_lc_actual, "extracted-data/raw-landcover-data/NLCD-LUQ-landcover.csv", row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Arctic LTER (ARC) LC Processing -------------------------------------------

# Prepare an sf object of just this LTER's watershed shapes
arc_sf <- sheds %>%
  filter(LTER == "ARC")

# Read in the raster
ak_fix <- terra::rast("extracted-data/raw-landcover-data/NLCD-Alaska-2016/NLCD_Alaska_2016_WGS84.tiff")

# Make sure CRS are same
crs(ak_fix)
st_crs(arc_sf)

# Plot that to see how it looks
plot(ak_fix, axes = T, reset = F)
## For some reason the conversion made the whole world of blank no-data show up so just ignore that
plot(arc_sf["uniqueID"], add = T)

# Extract data from the raster using the sf object
arc_lc_list <- exactextractr::exact_extract(x = ak_fix, y = arc_sf, include_cols = c("LTER", "uniqueID"))
str(arc_lc_list)

# Grab the NLCD index that connects integer codes to meaningful categories
nlcd_index <- read.csv("extracted-data/raw-landcover-data/NLCD_index.csv")
head(nlcd_index)

# Process our landcover information
arc_lc_actual <- arc_lc_list %>%
  # Make a dataframe from the selected list columns
  map_dfr(select, c(LTER, uniqueID, value)) %>%
  # Rename the land cover column
  rename(nlcd_code = value) %>%
  # Group by code, LTER, and uniqueID
  group_by(LTER, uniqueID, nlcd_code) %>%
  # Count pixels per code within the watershed
  summarise(cover_pixel_ct = n()) %>%
  # Grab all of the contents of the index
  left_join(nlcd_index, by = "nlcd_code") %>%
  # Keep only columns that we want
  select(LTER, uniqueID, nlcd_category, cover_pixel_ct) %>%
  # Export as dataframe
  as.data.frame()

# Check it out
head(arc_lc_actual)

# Save this out
write.csv(arc_lc_actual, "extracted-data/raw-landcover-data/NLCD-ARC-landcover.csv", row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Continental US LC Processing --------------------------------------------
# This includes AND, HBR, KRR, LMP, LUQ, MCM, NWT, Sagehen, & UMR

# Because the process is the same for extracting data for all of our LTERs, let's do it in a for loop 

# Also, this for loop has a grouped `dplyr::summarize` call so we'll turn off the annoying dialogue that it returns before going into the loop
options(dplyr.summarise.inform = F)

# Grab the NLCD index that connects integer codes to meaningful categories
nlcd_index <- read.csv("extracted-data/raw-landcover-data/NLCD_index.csv")
head(nlcd_index)

# Now time for the for loop (the `setdiff` is needed to split off the ones that we either already have or will need to process via a different workflow)
for(lter in setdiff(unique(sites$LTER), c("ARC", "LUQ", "GRO", "MCM"))){
  
  # Print a start message (for diagnostic purposes)
  print(paste0(lter, " processing begun at ", Sys.time()))
  
  # Read in the pre-transformed raster
  bbox <- terra::rast(paste0("extracted-data/raw-landcover-data/NLCD-", lter ,"-bbox-2019/NLCD_", lter, "_bbox_2019_WGS84.tiff"))
  
  # Prepare the relevant subset of the sf object of our watersheds
  sf <- sheds %>%
    filter(LTER == lter)
  
  # Extract data from the raster using the sf object
  lc_list <- exactextractr::exact_extract(x = bbox, y = sf, include_cols = c("LTER", "uniqueID"))
  
  # That created a list with one element per watershed in this LTER and we'll want a dataframe
  lc_df <- do.call(rbind, lc_list)
  
  # Now we have a smidge more processing to do before we should export
  lc_actual <- lc_list %>%
    # Make a dataframe from the selected list columns
    map_dfr(select, c(LTER, uniqueID, value)) %>%
    # Rename the land cover column
    rename(nlcd_code = value) %>%
    # Group by code, LTER, and uniqueID
    group_by(LTER, uniqueID, nlcd_code) %>%
    # Count pixels per code within the watershed
    summarise(cover_pixel_ct = n()) %>%
    # Grab all of the contents of the index
    left_join(nlcd_index, by = "nlcd_code") %>%
    # Keep only columns that we want
    select(LTER, uniqueID, nlcd_category, cover_pixel_ct) %>%
    # Export as dataframe
    as.data.frame()
  
  # This object is the one we want so save it to the server as a csv
  write.csv(lc_actual, paste0("extracted-data/raw-landcover-data/NLCD-", lter, "-landcover.csv"), row.names = F)
  
  # End with a conclusion message to mirror the start message
  print(paste0(lter, " processing completed at ", Sys.time()))
  
  }

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Great Rivers Observatory (GRO) LC Processing -----------------------------
# Possible source https://earthexplorer.usgs.gov/scene/metadata/full/5e83a1d8eecc8bb5/GLCCGBE20/

## Tried this path 3/3/22

# Get relevant part of watershed polygons
gro_sf <- sheds %>%
  filter(LTER == "GRO")

# Read in data
## gblulcgeo20 = USGS Land Use and Land Cover Classification (LULC)
globe <- terra::rast("extracted-data/raw-landcover-data/MODIS_GLCC_Global_1992/gblulcgeo20.tif")

# Exploratory plot
plot(globe)

# Crop to band of interest (all longitudes but only northern-most latitudes)
ext(globe)
globe_crop <- terra::crop(globe, terra::ext(-179.999999999967, 179.999985600033,
                                            60, 89.9999999999667))

# Plot cropped version
plot(globe_crop)

# Check CRS
crs(globe_crop)
st_crs(gro_sf)
## Looks like they match right out of the box!

# Try to extract information
gro_lc_list <- exactextractr::exact_extract(x = globe_crop, y = gro_sf, include_cols = c("LTER", "uniqueID"))
str(gro_lc_list)

# Read in LULC index
## Created manually from description here
## www.usgs.gov/media/files/global-land-cover-characteristics-data-base-readme-version2
lulc_index <- read.csv("extracted-data/raw-landcover-data/LULC_index.csv")
head(lulc_index)

# Process this out of list form
gro_lc <- gro_lc_list %>%
  # Make a dataframe from the selected list columns
  map_dfr(select, c(LTER, uniqueID, value)) %>%
  # Group by LTER and uniqueID
  group_by(LTER, uniqueID, value) %>%
  # Count instances of each pixel category
  summarise(cover_pixel_ct = n()) %>%
  # Bring in category names
  rename(lulc_code = value) %>%
  left_join(lulc_index, by = "lulc_code") %>%
  # Keep only desired columns
  select(LTER, uniqueID, lulc_category, cover_pixel_ct)

# Check it
unique(gro_lc$lulc_category)
head(gro_lc)

# Export this as is so that we can standardize cover categories between different sources of land cover data
write.csv(gro_lc, "extracted-data/raw-landcover-data/LULC-GRO-landcover.csv", na = '', row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# McMurdo LTER (MCM) LC Processing -----------------------------
# Need to find a source for these data




# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Combine LC Data Across Watersheds ----------------------------------------

# We have--at this point--successfully summarized land cover data from most of our LTERs; now we want to combine our LTER-specific csvs into one 'main' variant

# Set a counter and empty list
nlcd_list <- list()
k <- 1

# Read in the csvs (not for MCM or GRO because we don't have them yet and when we do they'll come from different sources)
for(lter in setdiff(unique(sites$LTER), c("GRO", "MCM"))){
  
  # Grab a csv
  nlcd_data <- read.csv(paste0("extracted-data/raw-landcover-data/NLCD-", lter, "-landcover.csv"))
  
  # Add it to the kth element of our list
  nlcd_list[[k]] <- nlcd_data
  
  # Advance the counter
  k <- k + 1 }

# Switch from list to dataframe
nlcd_data <- nlcd_list %>%
  map_dfr(select, c(LTER, uniqueID, nlcd_category, cover_pixel_ct))
head(nlcd_data)

# Read in GRO land cover (from different source!)
gro_data <- read.csv("extracted-data/raw-landcover-data/LULC-GRO-landcover.csv")
head(gro_data)

# Integrate the two dataframes
lc_data <- nlcd_data %>%
  # Use bind_rows to account for difference in category names
  dplyr::bind_rows(gro_data) %>%
  # Move the GRO's category column to be next to the other one
  relocate(lulc_category, .before = cover_pixel_ct) %>%
  # Make a single column for both types of category
  mutate(cover_category = coalesce(nlcd_category, lulc_category)) %>%
  # Remove any NAs
  filter(!is.na(cover_category)) %>%
  # Standardize casing/special character use in that combined column
  mutate(cover_category = tolower(gsub("\\/| ", "_", cover_category))) %>%
  # And collapse (seemingly) synonymous categories into one another
  mutate(
    cover_category = case_when(
      # Some additional combination is possible but these are the "safe" changes in my (Nick's) opinion
      cover_category == "deciduous_broadleaf_forest" ~ "deciduous_forest",
      cover_category == "deciduous_needleleaf_forest" ~ "deciduous_forest",
      cover_category == "evergreen_needleleaf_forest" ~ "evergreen_forest",
      cover_category == "barren_or_sparsely_vegetated" ~ "barren_land",
      cover_category == "snow_or_ice" ~ "perennial_ice_snow",
      cover_category == "dwarf_shrub" ~ "shrubland",
      cover_category == "shrub_scrub" ~ "shrubland",
      cover_category == "woody_wetlands" ~ "wooded_wetland",
      # cover_category == "" ~ "",
      T ~ as.character(cover_category) ) ) %>%
  # Re-summarize within our new categories
  ## Note that this drops the original category columns so you'd need to go back to find those
  group_by(LTER, uniqueID, cover_category) %>%
  summarise(cover_pixel_ct = sum(cover_pixel_ct)) %>%
  
  # Group by LTER and uniqueID (looking across categories within watersheds)
  group_by(LTER, uniqueID) %>%
  # Count the total pixels and get percent from that
  mutate(
    total_pixels = sum(cover_pixel_ct),
    perc_cover = round((cover_pixel_ct / total_pixels) * 100, digits = 2)
  ) %>%
  # Slim down to only needed columns (drops unspecified cols implicitly)
  select(LTER, uniqueID, cover_category, perc_cover)

# Look at it
head(lc_data)

# As with lithology, we need to process in two directions
## Wide format with all percents
lc_wide <- lc_data %>%
  pivot_wider(id_cols = c(LTER, uniqueID),
              names_from = cover_category,
              values_from = perc_cover)

## Second: get the *majority* cover for each watershed
lc_major <- lc_data %>%
  # Filter to only max of each cover type per uniqueID & LTER
  group_by(LTER, uniqueID) %>%
  filter(perc_cover == max(perc_cover)) %>%
  # Remove the percent total
  dplyr::select(-perc_cover) %>%
  # Get the columns into wide format where the column name and value are both whatever the dominant cover was
  pivot_wider(id_cols = c(LTER, uniqueID),
              names_from = cover_category,
              values_from = cover_category) %>%
  # Paste all the non-NAs (i.e., the dominant rocks) into a single column
  unite(col = major_cover, -LTER:-uniqueID, na.rm = T, sep = "; ")

# Now attach the major rocks to the wide format one
lc_actual <- lc_wide %>%
  left_join(lc_major, by = c("LTER", "uniqueID")) %>%
  relocate(major_cover, .after = uniqueID)

# Examine
head(lc_actual)

# LC Export -----------------------------------------------------------------

# Let's get ready to export
cover_export <- sites %>%
  left_join(lc_actual, by = c("LTER", "uniqueID"))

# Check it out
head(cover_export)

# Export this csv
write.csv(x = cover_export,
          file = "extracted-data/SilicaSites_withCoverData.csv",
          na = '', row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Lithology and Land Cover Combination --------------------------------------

# With both lithology and land cover data in hand, let's make a single 'one-stop shop' dataframe containing both

# Read in both
rocks <- read.csv("extracted-data/SilicaSites_withLithologyData.csv")
cover <- read.csv("extracted-data/SilicaSites_withCoverData.csv")

# Get a vector of shared column names (note that both data types have a "water_bodies" category)
shared_cols <- setdiff(intersect(names(rocks), names(cover)), "water_bodies")
shared_cols

# Combine and process the two
combo <- cover %>%
  # Left join by shared columns
  left_join(rocks, by = shared_cols) %>%
  # Re-order columns
  relocate(major_rock, .after = major_cover) %>%
  # We want to fix some column names but its faster to do this via long format
  pivot_longer(cols = basic_volcanic_rocks:ice_and_glaciers,
               names_to = "rock_types", values_to = "rocks_perc") %>%
  # Fix the column names by:
  mutate(
    ## Removing "_rocks" from names
    rock_types = gsub("\\_rocks", "", rock_types),
    ## Adding prefix "rocks_" to all columns (this will help differentiate between rock and cover data that might otherwise be ambiguous)
    ## Remove '.y' on ambiguous water bodies column
    rock_types = gsub("water_bodies.y", "water_bodies", rock_types),
    rock_types = paste0("rocks_", rock_types) ) %>%
  # Pivot back to wide format
  pivot_wider(id_cols = -rock_types:-rocks_perc,
              names_from = rock_types,
              values_from = rocks_perc) %>%
  # Now do the same for the cover categories
  pivot_longer(cols = deciduous_forest:pasture_hay,
               names_to = "cover_types", values_to = "cover_perc") %>%
  mutate(cover_types = gsub("water_bodies.x", "water_bodies", cover_types),
         cover_types = paste0("cover_", cover_types)) %>%
  pivot_wider(id_cols = -cover_types:-cover_perc,
              names_from = cover_types, values_from = cover_perc) %>%
  # McMurdo uses expert knowledge rather than extracted information (for now)
  mutate(major_rock = ifelse(LTER == "MCM", yes = "glacial_drift", no = major_rock),
         major_cover = ifelse(LTER == "MCM", yes = "barren_land", no = major_cover)) %>%
  # And let's also add some more 'data source' information to this for later use
  mutate(
    rockSource = ifelse(LTER == "MCM", yes = "Expert knowledge",
                        no = "Hartmann & Moosedorf 2012"),
    rockSourceLink = ifelse(LTER == "MCM", yes = "-",
                            no = "https://doi.pangaea.de/10.1594/PANGAEA.788537"),
    coverSource = case_when(
      LTER == "AND" ~ "2019 NLCD Continental US (manual bounding box via web app)",
      LTER == "ARC" ~ "2016 NLCD Alaska",
      LTER == "GRO" ~ "USGS Land Use/Land Cover (LULC) System",
      LTER == "HBR" ~ "2019 NLCD Continental US (manual bounding box via web app)",
      LTER == "KRR" ~ "2019 NLCD Continental US (manual bounding box via web app)",
      LTER == "LMP" ~ "2019 NLCD Continental US (manual bounding box via web app)",
      LTER == "LUQ" ~ "2001 NLCD Puerto Rico",
      LTER == "MCM" ~ "Expert knowledge",
      LTER == "NWT" ~ "2019 NLCD Continental US (manual bounding box via web app)",
      LTER == "Sagehen" ~ "2019 NLCD Continental US (manual bounding box via web app)",
      LTER == "UMR" ~ "2019 NLCD Continental US (full)",
      T ~ as.character(LTER) ),
    coverSourceLink = case_when(
      LTER == "AND" ~ "https://www.mrlc.gov/viewer/",
      LTER == "ARC" ~ "https://www.mrlc.gov/data/nlcd-2016-land-cover-alaska",
      LTER == "GRO" ~ "https://www.usgs.gov/centers/eros/science/usgs-eros-archive-land-cover-products-global-land-cover-characterization-glcc",
      LTER == "HBR" ~ "https://www.mrlc.gov/viewer/",
      LTER == "KRR" ~ "https://www.mrlc.gov/viewer/",
      LTER == "LMP" ~ "https://www.mrlc.gov/viewer/",
      LTER == "LUQ" ~ "https://www.mrlc.gov/data/nlcd-2001-land-cover-puerto-rico",
      LTER == "MCM" ~ "-",
      LTER == "NWT" ~ "https://www.mrlc.gov/viewer/",
      LTER == "Sagehen" ~ "https://www.mrlc.gov/viewer/",
      LTER == "UMR" ~ "https://www.mrlc.gov/data/nlcd-2019-land-cover-conus",
      T ~ as.character(LTER) ) ) %>%
  # And clarify the meaning of the original 'dataSource' columns
  rename(shapeSource = dataSource) %>%
  rename(shapeSourceLink = dataSourceLink) %>%
  # And move all of the source columns together
  relocate(ends_with('Source'), .after = everything()) %>%
  relocate(ends_with('SourceLink'), .after = everything())

# Check contents
names(combo)

# Check McMurdo fix
combo %>%
  filter(LTER == "MCM") %>%
  select(uniqueID, major_cover, major_rock, coverSource, rockSource)
  ## Looks great!

# Export this to share with the working group
write.csv(combo, file = "extracted-data/SilicaSites_allData.csv",
          na = '', row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# End ------------------------------------------------------------------------
