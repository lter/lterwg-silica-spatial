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
library(tidyverse); library(sf); library(stars)

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


# End ------------------------------------------------------------------------
