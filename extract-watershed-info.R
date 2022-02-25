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
rocks.raw <- stars::read_stars("extracted-data/raw-lithology-data/glim_wgs84_0point5deg.txt.asc")

# Convert it to an sf object
rocks.sf <- sf::st_as_sf(rocks.raw)

# Examine it
str(rocks.sf)
rocks.sf$geometry
st_crs(rocks.sf)
  ## CRS is missing!

# Prepare the lithology dataset for extraction
rocks.actual <- rocks.sf %>%
  # Because the structure call shows that it is WGS84 we can set the NA without fear
  sf::st_set_crs(value = 4326) %>%
  # Name the data column more descriptively
  dplyr::rename(rock.code = glim_wgs84_0point5deg.txt.asc)

# Check it now
str(rocks.actual)
st_crs(rocks.actual)
  ## Looks good!

# Plot just to make sure they seem to be stacking correctly
plot(rocks.actual["rock.code"], main = "All Lithology Information", axes = T, reset = F)
plot(sheds["LTER"], axes = T, add = T)
  ## Looks about right!

# Preemptively turn off s2 processing
sf::sf_use_s2(use_s2 = F)

# Lithology Extraction ------------------------------------------------------

# Strip out the lithology data from within our watershed polygons
rocky.sheds <- sheds %>%
  # Identify intersections between watersheds and rocks
  ## Note this line takes a minute
  st_intersection(rocks.actual)

# Plot it for exploratory purposes
plot(rocky.sheds["rock.code"], main = "Lithology Extraction", axes = T)
## If you use the "Zoom" button you'll see there are colors in there
## In the plotting pane you can only see the edges of all the cells

# Lithology Summarization ---------------------------------------------------

# Bring in the index tying rock code integers with rock abbreviations
rock.index.raw <- read.table(file = "extracted-data/raw-lithology-data/Classnames.txt",
                             header = T, sep = ';')

# Fix this index to make it more usable
rock.index <- rock.index.raw %>%
  # Rename the most important columns
  dplyr::rename(rock.code = OBJECTID,
                rock.abbrev = xx) %>%
  # And get a more descriptive version of each of the rock types
  dplyr::mutate(
    rock.type = case_when(
      # Abbreviations found here:
      # https://www.clisap.de/fileadmin/B-Research/IA/IA5/LITHOMAP/
      rock.abbrev == 'su' ~ 'unconsolidated_sediments',
      rock.abbrev == 'ss' ~ 'siliciclastic_sedimentary_rocks',
      rock.abbrev == 'sm' ~ 'mixed_sedimentary_rocks',
      rock.abbrev == 'py' ~ 'pyroclastic',
      rock.abbrev == 'sc' ~ 'carbonate_sedimentary_rocks',
      rock.abbrev == 'ev' ~ 'evaporites',
      rock.abbrev == 'mt' ~ 'metamorphic_rocks',
      rock.abbrev == 'pa' ~ 'acid_plutonic_rocks',
      rock.abbrev == 'pi' ~ 'intermediate_plutonic_rocks',
      rock.abbrev == 'pb' ~ 'basic_plutonic_rocks',
      rock.abbrev == 'va' ~ 'acid_volcanic_rocks',
      rock.abbrev == 'vi' ~ 'intermediate_volcanic_rocks',
      rock.abbrev == 'vb' ~ 'basic_volcanic_rocks',
      rock.abbrev == 'ig' ~ 'ice_and_glacers',
      rock.abbrev == 'wb' ~ 'water_bodies',
      rock.abbrev == 'nd' ~ 'no_data',
      TRUE ~ as.character(rock.abbrev) ) )

# Check that worked
head(rock.index)  
sort(unique(rock.index$rock.type))
  
# Process the extracted lithology information into a dataframe
rock.data.v1 <- rocky.sheds %>%
  # Remove the truly spatial part of the data to make it easier to work with
  st_drop_geometry() %>%
  # Bring over the rock names from the index
  dplyr::mutate(
    rock.type = rock.index$rock.type[match(rock.code, rock.index$rock.code)]
  ) %>%
  # Remove the now-unneeded code column
  dplyr::select(-rock.code) %>%
  # Group by LTER and uniqueID
  group_by(LTER, uniqueID, rock.type) %>%
  # Count the instances within each rock type
  ## 0.5° degree pixels within the watershed that contain this rock type
  summarise(rock.totals = n()) %>%
  # Make it a dataframe (to avoid a list of tibbles)
  as.data.frame() %>%
  # Remove the 'no_data' cells
  filter(rock.type != "no_data") %>%
  # # Group by LTER and uniqueID
  group_by(LTER, uniqueID) %>%
  # We'll want the totals as a percent (total pixels is not very intuitive)
  dplyr::mutate( total.shed.pixels = sum(rock.totals) ) %>%
  # Again, return a dataframe, not a tibble
  as.data.frame() %>%
  # Now ungroup
  ungroup() %>%
  # And calculate the percent of total for each row
  dplyr::mutate( perc.total = ((rock.totals / total.shed.pixels) * 100) ) %>%
  # Remove the two pixel count columns
  dplyr::select(-rock.totals, -total.shed.pixels)

# Now we want to split into two directions
## First: get a version where each rock type is its own column
rock.data.wide <- rock.data.v1 %>%
  # Pivot to wide format
  pivot_wider(id_cols = c(LTER, uniqueID),
              names_from = rock.type,
              values_from = perc.total)

## Second: get the *majority* rock for each watershed
rock.data.major <- rock.data.v1 %>%
  # Filter to only max of each rock type per uniqueID & LTER
  group_by(LTER, uniqueID) %>%
  filter(perc.total == max(perc.total)) %>%
  # Remove the percent total
  dplyr::select(-perc.total) %>%
  # Get the columns into wide format where the column name and value are both whatever the dominant rock was
  pivot_wider(id_cols = c(LTER, uniqueID),
              names_from = rock.type,
              values_from = rock.type) %>%
  # Paste all the non-NAs (i.e., the dominant rocks) into a single column
  unite(col = major_rock, -LTER:-uniqueID, na.rm = T, sep = "; ")

# Now attach the major rocks to the wide format one
rock.data.actual <- rock.data.wide %>%
  left_join(rock.data.major, by = c("LTER", "uniqueID")) %>%
  relocate(major_rock, .after = uniqueID)

# Examine
str(rock.data.actual)
head(rock.data.actual)
names(rock.data.actual)

# Lithology Export -----------------------------------------------------------

# Let's get ready to export
rock.export <- sites %>%
  left_join(rock.data.actual, by = c("LTER", "uniqueID"))

# Check it out
head(rock.export)
names(rock.export)

# Export both this and the shapefile that contains the cropped rock data
write.csv(x = rock.export,
          file = "extracted-data/SilicaSites_withLithologyData.csv",
          row.names = F)
st_write(obj = rocky.sheds,
         dsn = "extracted-data/SilicaSynthesis_LithologyPolygons.shp",
         delete_layer = T)


# End ------------------------------------------------------------------------
