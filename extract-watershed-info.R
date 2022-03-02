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
      rock_abbrev == 'ig' ~ 'ice_and_glacers',
      rock_abbrev == 'wb' ~ 'water_bodies',
      rock_abbrev == 'nd' ~ 'no_data',
      TRUE ~ as.character(rock_abbrev) ) )

# Check that worked
head(rock_index)  
sort(unique(rock_index$rock_type))
  
# Process the extracted lithology information into a dataframe
rock_data_v1 <- rocky_sheds %>%
  # Remove the truly spatial part of the data to make it easier to work with
  st_drop_geometry() %>%
  # Bring over the rock names from the index
  dplyr::mutate(
    rock_type = rock_index$rock_type[match(rock_code, rock_index$rock_code)]
  ) %>%
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
luq_lc_v1 <- exactextractr::exact_extract(x = pr_fix, y = luq_sf, include_cols = c("LTER", "uniqueID"))
str(luq_lc_v1)

# That output a list so let's strip it to a dataframe to make it more manageable
luq_lc_v2 <- do.call(rbind, luq_lc_v1)

# Process our landcover information
luq_lc_actual <- luq_lc_v2 %>%
  # Rename the landcover column
  rename(nlcd_code = value) %>%
  # Group by category
  group_by(LTER, uniqueID, nlcd_code) %>%
  # Count pixels per code within the watershed
  summarise(cover_pixel_ct = n()) %>%
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
arc_lc_v1 <- exactextractr::exact_extract(x = ak_fix, y = arc_sf, include_cols = c("LTER", "uniqueID"))
str(arc_lc_v1)

# That output a list so let's strip it to a dataframe to make it more manageable
arc_lc_v2 <- do.call(rbind, arc_lc_v1)

# Process our landcover information
arc_lc_actual <- arc_lc_v2 %>%
  # Rename the landcover column
  rename(nlcd_code = value) %>%
  # Group by category
  group_by(LTER, uniqueID, nlcd_code) %>%
  # Count pixels per code within the watershed
  summarise(cover_pixel_ct = n()) %>%
  # Export as dataframe
  as.data.frame()

# Check it out
head(arc_lc_actual)

# Save this out
write.csv(arc_lc_actual, "extracted-data/raw-landcover-data/NLCD-ARC-landcover.csv", row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Continental US* LC Processing --------------------------------------------

# Read in pre-transformed raster
hbr_bbox <- rast("extracted-data/raw-landcover-data/NLCD-HBR-bbox-2019/NLCD_HBR_bbox_2019_WGS84.tiff")

# Prepare an sf object of just this LTER's watershed shapes
hbr_sf <- sheds %>%
  filter(LTER == "HBR")

# # Make sure CRS are same
crs(hbr_bbox)
st_crs(hbr_sf)

# Plot that to see how it looks
plot(hbr_bbox, axes = T, reset = F)
plot(hbr_sf["uniqueID"], add = T)

# Extract data from the raster using the sf object
hbr_lc_v1 <- exactextractr::exact_extract(x = hbr_bbox, y = hbr_sf, include_cols = c("LTER", "uniqueID"))
str(hbr_lc_v1)

# Create a more manageable dataframe from that list
hbr_lc_v2 <- do.call(rbind, hbr_lc_v1)
str(hbr_lc_v2)

# Read in legend connecting codes to 'real' categories
nlcd_index <- read.csv("extracted-data/raw-landcover-data/NLCD_index.csv")
str(nlcd_index)

# Process our landcover information
hbr_lc_actual <- hbr_lc_v2 %>%
  # Rename the landcover column
  rename(nlcd_code = value) %>%
  # Attach the NLCD index
  left_join(nlcd_index, by = "nlcd_code") %>%
  # Group by category and LTER/uniqueID
  group_by(LTER, uniqueID, nlcd_category) %>%
  # Count pixels per code within the watershed
  summarise(cover_pixel_ct = n()) %>%
  # Export as dataframe
  as.data.frame()

# Check it out
head(hbr_lc_actual)

# Save this out
write.csv(hbr_lc_actual, "extracted-data/raw-landcover-data/NLCD-HBR-landcover.csv", row.names = F)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))








# Combine LC Data Across Watersheds ----------------------------------

# We have--at this point--successfully grabbed the shapes and summarized dataframes of land cover data from all of our LTERs
# Now, we will combine these within data type into two global files (one shape, and one dataframe)

# Re-call which objects we need
# arc_lc <- read.csv("extracted-data/raw-landcover-data/NLCD-ARC-landcover.csv")

## LUQ dataframe
# luq_lc_actual
## LUQ shapes
# str(luq_lc_shed)














# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# End ------------------------------------------------------------------------
