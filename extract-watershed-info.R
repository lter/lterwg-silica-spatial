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
library(tidyverse); library(sf); library(stars); library(terra)

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
          row.names = F)
st_write(obj = rocky_sheds,
         dsn = "extracted-data/SilicaSynthesis_LithologyPolygons.shp",
         delete_layer = T)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Land Cover Data ------------------------------------------------------------

# Now it is time for land cover processing

# Luquillo (LUQ) Land Cover (LC) Processing ----------------------------------

# Subset our global sheds object to just the LUQ watersheds
luq_shapes <- sheds %>%
  filter(LTER == "LUQ")

# Read it in (note this takes awhile [~30 sec] because it is a *big* file)
pr_raw <- stars::read_stars("extracted-data/raw-landcover-data/NLCD-PuertoRico-2001/pr_landcover_wimperv_10-28-08_se5.img")
## These files are actually so large that we can't really work with them so we need to subset them before continuing

# We'll create a bounding box from each LTER and cut out the general area we'll want
luq_bbox <- sites %>%
  # Take the LTER of interest (PR is where we're starting so that's LUQ)
  filter(LTER == "LUQ") %>%
  # Make it spatial but adopt the CRS of the raw land cover data
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  # Transform the data to use the same CRS as the raw land cover data
  sf::st_transform(crs = st_crs(pr_raw)) %>%
  # Get a buffer around it for big drainage basins (`dist` in arc degrees)
  sf::st_buffer(dist = 750) %>%
  # Get the bounding box that contains those points
  sf::st_bbox()

# Subset the correct chunk
pr_crop <- pr_raw[luq_bbox]

# Make a plot to test whether the crop worked
plot(pr_crop, axes = T)

# Make this small subset into an sf object
pr_sf <- pr_crop %>%
  sf::st_as_sf() %>%
  # Transform into WGS84
  st_transform(crs = 4326) %>%
  # Rename the data object
  dplyr::rename(landcover = pr_landcover_wimperv_10.28.08_se5.img)

# Plot it again
plot(pr_sf, axes = T, lab = c(3, 3, 3), main = "PR NLCD Data", reset = F)
plot(filter(sheds, LTER == "LUQ")["LTER"], add = T)
  ## They overlap completely!

# Make an empty list and counter set at 1
lc_list <- list()
j <- 1

# This object is so large that we cannot process it as one thing so we'll need to loop through it (note that we need to chop forest out to make the loop run)
# Note that this loop takes 14 minutes *on NCEAS' server* so DO NOT ATTEMPT TO RUN on a laptop (or do and buckle in for a long wait)
for(cover in unique(pr_sf$landcover)){
  
  # Print a start message
  print(paste0("Category '", cover, "' processing begun at ", Sys.time()))
  
  # Split out one of the cover categories
  cover_sub <- pr_sf %>%
    filter(landcover == cover)
  
  # Use our watershed to crop out the pixels of this cover category inside one (or more) watersheds
  cover_shed <- luq_shapes %>%
    st_intersection(cover_sub)
  
  # Add the cropped information into the list at the jth position
  lc_list[[j]] <- cover_shed
  
  # Advance the counter
  j <- j + 1
  
  # Print a success message
  print(paste0("Category '", cover, "' successfully cropped at ", Sys.time()))
  }

# Collapse the list back into a single object
luq_lc_shed <- do.call(rbind, lc_list)
str(luq_lc_shed)

# Plot it
plot(luq_lc_shed["landcover"], axes = T, lab = c(3, 3, 3), main = "LUQ NLCD Data")
## It worked!

# Process the extracted land cover information into a dataframe
luq_lc_data_v1 <- luq_lc_shed %>%
  # Remove the truly spatial part of the data to make it easier to work with
  st_drop_geometry() %>%
  # Count instances of each class within unique ID
  group_by(LTER, uniqueID, landcover) %>%
  dplyr::summarise(cover_pixel_ct = n()) %>%
  # Make it a dataframe (to avoid a list of tibbles)
  as.data.frame() %>%
  # Group by LTER and uniqueID
  group_by(LTER, uniqueID) %>%
  # We'll want the totals as a percent (total pixels is not very intuitive)
  dplyr::mutate( total_pixels = sum(cover_pixel_ct) ) %>%
  # Again, return a dataframe, not a tibble
  as.data.frame() %>%
  # Now ungroup
  ungroup() %>%
  # And calculate the percent of total for each row
  dplyr::mutate(
    perc_total = ((cover_pixel_ct / total_pixels) * 100),
    # While we're here, fix the typo in "Herbaceous"
    landcover = gsub("Herbaceuous", "Herbaceous", landcover) ) %>%
  # Remove the two pixel count columns (implicitly)
  dplyr::select(LTER, uniqueID, landcover, perc_total)

# Now we want to split into two directions
## First: get a version where each rock type is its own column
luq_lc_wide <- luq_lc_data_v1 %>%
  # Pivot to wide format
  pivot_wider(id_cols = c(LTER, uniqueID),
              names_from = landcover,
              values_from = perc_total)

## Second: get the *majority* land cover for each watershed
luq_lc_major <- luq_lc_data_v1 %>%
  # Filter to only max of each LC type per uniqueID & LTER
  group_by(LTER, uniqueID) %>%
  filter(perc_total == max(perc_total)) %>%
  # Remove the percent total
  dplyr::select(-perc_total) %>%
  # Get the columns into wide format where the column name and value are both whatever the dominant LC was
  pivot_wider(id_cols = c(LTER, uniqueID),
              names_from = landcover,
              values_from = landcover) %>%
  # Paste all the non-NAs (i.e., the dominant LCs) into a single column
  unite(col = major_lc, -LTER:-uniqueID, na.rm = T, sep = "; ")

# Now attach the major rocks to the wide format one
luq_lc_actual <- luq_lc_wide %>%
  left_join(luq_lc_major, by = c("LTER", "uniqueID")) %>%
  relocate(major_lc, .after = uniqueID)

# Examine
head(luq_lc_actual)
# Looks great!

# Export both for later use
write.csv(x = luq_lc_actual, file = "extracted-data/raw-landcover-data/LUQ-tidy-landcover/LUQ_withLandCover.csv", row.names = F)
st_write(obj = luq_lc_shed, dsn = "extracted-data/raw-landcover-data/LUQ-tidy-landcover/LUQ_LandcoverPolygons.shp", delete_layer = T)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# Arctic LTER (ARC) Land Cover (LC) Processing ----------------------------------

# METHOD VARIANT No. 1 --------

# Prepare a raster of this LTER's watershed shapes
arc_sf <- sheds %>%
  # Filter to just this LTER
  filter(LTER == "ARC")

# Make it a raster
arc_rast <- terra::vect(arc_sf)

# Read in the raster of a rough (*manually drawn*) bounding box for this LTER
# ak_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-ARC-bbox-2016/NLCD_2016_Land_Cover_AK_20200724_1UJcf1tMJdCcymwLngFW.tiff")
ak_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-Alaska-2016/NLCD_2016_Land_Cover_AK_20200724.img")

# Transform the new raster's CRS to match the land cover file
arc_actual <- terra::project(x = arc_rast, y = ak_raw)

# Make sure CRS are same
crs(arc_actual)
crs(ak_raw)

# Plot that to see how it looks
plot(ak_raw, axes = T, reset = F)
plot(arc_actual, add = T)

# Extract data
arc_extract <- terra::extract(x = ak_raw, y = arc_actual, fun = sum, na.rm = T)
plot(arc_extract)


# METHOD VARIANT No. 2 --------

# Prepare a raster of this LTER's watershed shapes
arc_sf <- sheds %>%
  # Filter to just this LTER
  filter(LTER == "ARC")

# Make it a raster
arc_rast <- terra::vect(arc_sf)

# Read in the raster of landcover for all of Alaska
ak_raw <- terra::rast("extracted-data/raw-landcover-data/NLCD-Alaska-2016/NLCD_2016_Land_Cover_AK_20200724.img")

# Transform the landcover raster to use the CRS of our watersheds
## Note that it takes ~ 4 minutes
ak_fix <- terra::project(x = ak_raw, y = "epsg:4326")

# Make sure CRS are same
crs(ak_fix)
st_crs(arc_sf)

# Plot that to see how it looks
plot(ak_fix, axes = T, reset = F)
plot(arc_sf["uniqueID"], add = T)

# Extract data
library(exactextractr)
?exactextractr::exact_extract

arc_lc_v1 <- exactextractr::exact_extract(x = ak_fix, y = arc_sf)

str(arc_lc_v1)



# Combine LC Data Across Watersheds ----------------------------------

# We have--at this point--successfully grabbed the shapes and summarized dataframes of land cover data from all of our LTERs
# Now, we will combine these within data type into two global files (one shape, and one dataframe)

# Re-call which objects we need
## LUQ dataframe
# luq_lc_actual
## LUQ shapes
# str(luq_lc_shed)














# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

# End ------------------------------------------------------------------------
