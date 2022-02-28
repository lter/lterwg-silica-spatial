## ----------------------------------------------------------------------- ##
                          # LTER WG: Silica Synthesis
## ----------------------------------------------------------------------- ##

# Purpose:
## Identify watershed boundaries for later use in gathering additional data
## Note that step-by-step comments are only included for processing AND LTER
## Other LTERs follow same process so commenting is somewhat reduced

# streamstats::delineateWatershed component written by Kristen Peach (see 'delineateWatershed.Rmd')
# Streamlined/consolidated and rest written by Nick Lyon

# Housekeeping ----------------------------------------------------------

# Read needed libraries
library(tidyverse); library(sf); library(nngeo)

# Clear environment
rm(list = ls())

# Set working directory to location of shared data
## Identify path
path <- file.path('/', "home", "shares", "lter-si", "si-watershed-extract")
## Set WD to path
setwd(path)
## Check that it worked
getwd()

# Site coordinate retrieval and preparation ----------------------------------

# Load in site names with lat/longs
sites_raw <- read.csv("Longtermsites_DA_Coordinates_2.14.22.csv")

# Do some needed pre-processing
sites <- sites_raw %>%
  dplyr::mutate(
    # Remove spaces from all stream sites' (within LTER) names
    SITE = gsub(' ', '-', SITE),
    # Create unique within LTER site names
    uniqueID = as.factor(paste0(LTER.x, "_", SITE)),
    # Make lat/long numeric
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    # Fix a missing negative for three sites
    ## We have determined elsewhere that these sites are __ degrees longitude WEST
    ## Meaning they should be negative for use as coordinates in R
    Longitude = ifelse(LTER.x == "Sagehen" |
                         uniqueID == "GRO_Mackenzie" | uniqueID == "GRO_Yukon",
                       yes = (Longitude * -1),
                       no = Longitude),
    # And simplify Sagehen's unique ID
    uniqueID = as.factor(ifelse(uniqueID == "Sagehen_Sagehen",
                                yes = gsub("Sagehen\\_Sagehen", "Sagehen", uniqueID),
                                no = as.character(uniqueID))) ) %>%
  # Simplify column naming
  dplyr::rename(LTER = LTER.x,
                stream = SITE,
                long = Longitude,
                lat = Latitude,
                drainSqKm_original = drainSqKm) %>%
  dplyr::mutate(
    # Add data source name to sites object
    dataSource = case_when(
      LTER == "AND" ~ "USGS StreamStats App",
      LTER == "ARC" ~ "Univ of Alaska Fairbanks",
      LTER == "GRO" ~ "Great Rivers Observatory",
      LTER == "HBR" ~ "USGS StreamStats App",
      LTER == "KRR" ~ "Florida Geospatial Open Data Portal",
      LTER == "LMP" ~ "USGS StreamStats App",
      LTER == "LUQ" ~ "USGS StreamStats App",
      LTER == "MCM" ~ "McMurdo Information Manager",
      LTER == "NWT" ~ "USGS StreamStats App",
      LTER == "Sagehen" ~ "USGS StreamStats App",
      LTER == "UMR" ~ "USGS StreamStats App",
      T ~ as.character(LTER) ),
    # Also add the link to the source
    dataSourceLink = case_when(
      LTER == "AND" ~ "https://streamstats.usgs.gov/ss/",
      LTER == "ARC" ~ "https://www.uaf.edu/toolik/gis/data/index.php#",
      LTER == "GRO" ~ "https://drive.google.com/drive/folders/1S-MAqA9ahewFhOb8yHEmMc-JC9jX6rwC?usp=sharing",
      LTER == "HBR" ~ "https://streamstats.usgs.gov/ss/",
      LTER == "KRR" ~ "https://geodata.floridagio.gov/datasets/sfwmd::north-south-everglades-and-neepp-boundaries/about",
      LTER == "LMP" ~ "https://streamstats.usgs.gov/ss/",
      LTER == "LUQ" ~ "https://streamstats.usgs.gov/ss/",
      LTER == "MCM" ~ "[Shared via email]",
      LTER == "NWT" ~ "https://streamstats.usgs.gov/ss/",
      LTER == "Sagehen" ~ "https://streamstats.usgs.gov/ss/",
      LTER == "UMR" ~ "https://streamstats.usgs.gov/ss/",
      T ~ as.character(LTER) ) ) %>%
  # Move some columns to be more intuitive
  dplyr::select(LTER, stream, uniqueID, Biome1, Biome2, dataSource, dataSourceLink, drainSqKm_original, lat, long)


filter(sites, LTER == "Sagehen" | uniqueID == "GRO_Mackenzie" | uniqueID == "GRO_Yukon")

# Examine output
head(sites)

# Get an explicitly spatial version of the sites dataframe
sites_spatial <- sf::st_as_sf(sites, coords = c("long", "lat"), crs = 4326)
str(sites_spatial)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Andrews Forest LTER (AND) ----------------------------------------------

# Check what sites are within this LTER
sites %>%
  filter(LTER == "AND") %>%
  dplyr::select(uniqueID, lat, long)

# AND Process Raw Shapefiles ----------------------------------------------

# Read in each raw shapefile
## All downloaded from: https://streamstats.usgs.gov/ss/
and_gsmack <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSMACK/globalwatershed.shp')
and_gsws02 <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSWS02/globalwatershed.shp')
and_gsws06 <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSWS06/globalwatershed.shp')
and_gsws07 <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSWS07/globalwatershed.shp')
and_gsws08 <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSWS08/globalwatershed.shp')
and_gsws09 <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSWS09/globalwatershed.shp')
and_gsws10 <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSWS10/globalwatershed.shp')

# Add the uniqueID the shape corresponds to and remove any unneeded layers in the polygon
and_gsmack_v2 <- and_gsmack %>%
  mutate(uniqueID = "AND_GSMACK") %>%
  dplyr::select(-Name)
and_gsws02_v2 <- and_gsws02 %>%
  mutate(uniqueID = "AND_GSWS02") %>%
  dplyr::select(-Name)
and_gsws06_v2 <- and_gsws06 %>%
  mutate(uniqueID = "AND_GSWS06") %>%
  dplyr::select(-Name)
and_gsws07_v2 <- and_gsws07 %>%
  mutate(uniqueID = "AND_GSWS07") %>%
  dplyr::select(-Name)
and_gsws08_v2 <- and_gsws08 %>%
  mutate(uniqueID = "AND_GSWS08") %>%
  dplyr::select(-Name)
and_gsws09_v2 <- and_gsws09 %>%
  mutate(uniqueID = "AND_GSWS09") %>%
  dplyr::select(-Name)
and_gsws10_v2 <- and_gsws10 %>%
  mutate(uniqueID = "AND_GSWS10") %>%
  dplyr::select(-Name)

# Combine them
and_all_watersheds <- rbind(and_gsmack_v2, and_gsws02_v2, and_gsws06_v2,
                            and_gsws07_v2, and_gsws08_v2, and_gsws09_v2,
                            and_gsws10_v2)
str(and_all_watersheds)

# AND Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
and_spatial <- sites %>%
  filter(LTER == "AND") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(and_spatial)
and_spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Preemptively fix any holes inside of polygons (more of an issue for bigger polygons)
and_shapes <- and_all_watersheds %>%
  # Group by uniqueID
  group_by(uniqueID) %>%
  # Close holes
  nngeo::st_remove_holes() %>%
  # Fix geometry naming (st_remove_holes change 'geometry' into 'geom')
  dplyr::rename(geometry = geom) %>%
  # Ungroup
  ungroup()

# Make an exploratory graph to see if everything looks okay
plot(and_shapes["uniqueID"], main = "Andrews Forest Sites", axes = T, reset = F, lab = c(2, 2, 3))
plot(and_spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T, las = 2)

# Check relevant shapefile geometry
and_shapes$geometry

# Save out shapefile
st_write(obj = and_shapes, dsn = "watershed-shapefiles/AND_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Hubbard Brook LTER (HBR) ---------------------------------------------
# Check what sites are within this LTER
sites %>%
  filter(LTER == "HBR") %>%
  dplyr::select(uniqueID, lat, long)

# HBR Process Raw Shapefiles ----------------------------------------------

# Read in each raw shapefile
## All downloaded from: https://streamstats.usgs.gov/ss/
hbr_ws1 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws1/globalwatershed.shp')
hbr_ws2 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws2/globalwatershed.shp')
hbr_ws3 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws3/globalwatershed.shp')
hbr_ws4 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws4/globalwatershed.shp')
hbr_ws5 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws5/globalwatershed.shp')
hbr_ws6 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws6/globalwatershed.shp')
hbr_ws7 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws7/globalwatershed.shp')
hbr_ws8 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws8/globalwatershed.shp')
hbr_ws9 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws9/globalwatershed.shp')

# Add the uniqueID the shape corresponds to and remove any unneeded layers in the polygon
hbr_ws1_v2 <- hbr_ws1 %>%
  mutate(uniqueID = "HBR_ws1") %>%
  dplyr::select(-Name)
hbr_ws2_v2 <- hbr_ws2 %>%
  mutate(uniqueID = "HBR_ws2") %>%
  dplyr::select(-Name)
hbr_ws3_v2 <- hbr_ws3 %>%
  mutate(uniqueID = "HBR_ws3") %>%
  dplyr::select(-Name)
hbr_ws4_v2 <- hbr_ws4 %>%
  mutate(uniqueID = "HBR_ws4") %>%
  dplyr::select(-Name)
hbr_ws5_v2 <- hbr_ws5 %>%
  mutate(uniqueID = "HBR_ws5") %>%
  dplyr::select(-Name)
hbr_ws6_v2 <- hbr_ws6 %>%
  mutate(uniqueID = "HBR_ws6") %>%
  dplyr::select(-Name)
hbr_ws7_v2 <- hbr_ws7 %>%
  mutate(uniqueID = "HBR_ws7") %>%
  dplyr::select(-Name)
hbr_ws8_v2 <- hbr_ws8 %>%
  mutate(uniqueID = "HBR_ws8") %>%
  dplyr::select(-Name)
hbr_ws9_v2 <- hbr_ws9 %>%
  mutate(uniqueID = "HBR_ws9") %>%
  dplyr::select(-Name)

# Combine them
hbr_all_watersheds <- rbind(hbr_ws1_v2, hbr_ws2_v2, hbr_ws3_v2,
                            hbr_ws4_v2, hbr_ws5_v2, hbr_ws6_v2,
                            hbr_ws7_v2, hbr_ws8_v2, hbr_ws9_v2)
str(hbr_all_watersheds)

# HBR Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
hbr_spatial <- sites %>%
  filter(LTER == "HBR") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(hbr_spatial)
hbr_spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Preemptively fix any holes inside of polygons (more of an issue for bigger polygons)
hbr_shapes <- hbr_all_watersheds %>%
  # Group by uniqueID
  group_by(uniqueID) %>%
  # Close holes
  nngeo::st_remove_holes() %>%
  # Fix geometry naming (st_remove_holes change 'geometry' into 'geom')
  dplyr::rename(geometry = geom) %>%
  # Ungroup
  ungroup()

# Make an exploratory graph to see if everything looks okay
plot(hbr_shapes["uniqueID"], main = "Hubbard Brook Sites", axes = T, reset = F, lab = c(2, 2, 2))
plot(hbr_spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# Check relevant shapefile geometry
hbr_shapes$geometry

# Save out shapefile
st_write(obj = hbr_all_watersheds, dsn = "watershed-shapefiles/HBR_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Upper Mississippi River (UMR) ---------------------------------------------
# Check what sites are within this LTER
sites %>%
  filter(LTER == "UMR") %>%
  dplyr::select(uniqueID, lat, long)

# UMR Process Raw Shapefiles ----------------------------------------------

# Read in each raw shapefile
## All downloaded from: https://streamstats.usgs.gov/ss/
umr_I080.2M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_I080.2M/globalwatershed.shp')
umr_SG16.2C <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_SG16.2C/globalwatershed.shp')
umr_CH00.1M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_CH00.1M/globalwatershed.shp')
umr_CN00.1M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_CN00.1M/globalwatershed.shp')
umr_CU11.6M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_CU11.6M/globalwatershed.shp')

umr_LM00.5M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_LM00.5M/globalwatershed.shp')
umr_M078.0B <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_M078.0B/globalwatershed.shp')
umr_M556.4A <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_M556.4A/globalwatershed.shp')
umr_M701.1B <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_M701.1B/globalwatershed.shp')
umr_M764.3A <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_M764.3A/globalwatershed.shp')

umr_M786.2C <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_M786.2C/globalwatershed.shp')
umr_MQ02.1M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_MQ02.1M/globalwatershed.shp')
umr_WP02.6M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_WP02.6M/globalwatershed.shp')
umr_BK01.0M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_BK01.0M/globalwatershed.shp')
umr_M241.4K <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_M241.4K/globalwatershed.shp')

# Add the uniqueID the shape corresponds to and remove any unneeded layers in the polygon
umr_I080.2M_v2 <- umr_I080.2M %>%
  mutate(uniqueID = "UMR_I080.2M") %>%
  dplyr::select(-Name)
umr_SG16.2C_v2 <- umr_SG16.2C %>%
  mutate(uniqueID = "UMR_SG16.2C") %>%
  dplyr::select(-Name)
umr_CH00.1M_v2 <- umr_CH00.1M %>%
  mutate(uniqueID = "UMR_CH00.1M") %>%
  dplyr::select(-Name)
umr_CN00.1M_v2 <- umr_CN00.1M %>%
  mutate(uniqueID = "UMR_CN00.1M") %>%
  dplyr::select(-Name)
umr_CU11.6M_v2 <- umr_CU11.6M %>%
  mutate(uniqueID = "UMR_CU11.6M") %>%
  dplyr::select(-Name)

umr_LM00.5M_v2 <- umr_LM00.5M %>%
  mutate(uniqueID = "UMR_LM00.5M") %>%
  dplyr::select(-Name)
umr_M078.0B_v2 <- umr_M078.0B %>%
  mutate(uniqueID = "UMR_M078.0B") %>%
  dplyr::select(-Name)
umr_M556.4A_v2 <- umr_M556.4A %>%
  mutate(uniqueID = "UMR_M556.4A") %>%
  dplyr::select(-Name)
umr_M701.1B_v2 <- umr_M701.1B %>%
  mutate(uniqueID = "UMR_M701.1B") %>%
  dplyr::select(-Name)
umr_M764.3A_v2 <- umr_M764.3A %>%
  mutate(uniqueID = "UMR_M764.3A") %>%
  dplyr::select(-Name)

umr_M786.2C_v2 <- umr_M786.2C %>%
  mutate(uniqueID = "UMR_M786.2C") %>%
  dplyr::select(-Name)
umr_MQ02.1M_v2 <- umr_MQ02.1M %>%
  mutate(uniqueID = "UMR_MQ02.1M") %>%
  dplyr::select(-Name)
umr_WP02.6M_v2 <- umr_WP02.6M %>%
  mutate(uniqueID = "UMR_WP02.6M") %>%
  dplyr::select(-Name)
umr_BK01.0M_v2 <- umr_BK01.0M %>%
  mutate(uniqueID = "UMR_BK01.0M") %>%
  dplyr::select(-Name)
umr_M241.4K_v2 <- umr_M241.4K %>%
  mutate(uniqueID = "UMR_M241.4K") %>%
  dplyr::select(-Name)

# Combine them
umr_all_watersheds <- rbind(umr_I080.2M_v2, umr_SG16.2C_v2, umr_CH00.1M_v2,
                            umr_CN00.1M_v2, umr_CU11.6M_v2, umr_LM00.5M_v2,
                            umr_M078.0B_v2, umr_M556.4A_v2, umr_M701.1B_v2, 
                            umr_M764.3A_v2, umr_M786.2C_v2, umr_MQ02.1M_v2,
                            umr_WP02.6M_v2, umr_BK01.0M_v2, umr_M241.4K_v2)
str(umr_all_watersheds)

# UMR Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
umr_spatial <- sites %>%
  filter(LTER == "UMR") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(umr_spatial)
umr_spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Preemptively fix any holes inside of polygons (more of an issue for bigger polygons)
umr_shapes <- umr_all_watersheds %>%
  # Group by uniqueID
  group_by(uniqueID) %>%
  # Close holes
  nngeo::st_remove_holes() %>%
  # Fix geometry naming (st_remove_holes change 'geometry' into 'geom')
  dplyr::rename(geometry = geom) %>%
  # Ungroup
  ungroup()

# Make an exploratory graph to see if everything looks okay
## Takes a little longer to run because of the size of some of them
plot(umr_shapes["uniqueID"], main = "Upper Mississippi River", axes = T, reset = F, lab = c(3, 3, 3))
plot(umr_spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# Check relevant shapefile geometry
umr_shapes$geometry

# Save out shapefile
st_write(obj = umr_shapes, dsn = "watershed-shapefiles/UMR_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Lamprey River (LMP) --------------------------------------------------------
# Check what sites are within this location (technically not an LTER)
sites %>%
  filter(LTER == "LMP") %>%
  dplyr::select(uniqueID, lat, long)

# LMP Process Raw Shapefiles -------------------------------------------------

# Read in needed raw shapefile(s)
## Downloaded from: https://streamstats.usgs.gov/ss/
lmp_73 <- sf::st_read('watershed-shapefiles/LMP_raw-shapefiles/LMP_LMP73/globalwatershed.shp')

# Add the uniqueID the shape corresponds to and remove any unneeded layers in the polygon
lmp_73_v2 <- lmp_73 %>%
  mutate(uniqueID = "LMP_LMP73") %>%
  dplyr::select(-Name)

# For consistency, make this a new object with a different name
lmp_all_watersheds <- lmp_73_v2

# Check it
str(lmp_all_watersheds)

# LMP Final Checks & Export --------------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
lmp_spatial <- sites %>%
  filter(LTER == "LMP") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(lmp_spatial)
lmp_spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Preemptively fix any holes inside of polygons (more of an issue for bigger polygons)
lmp_shapes <- lmp_all_watersheds %>%
  # Group by uniqueID
  group_by(uniqueID) %>%
  # Close holes
  nngeo::st_remove_holes() %>%
  # Fix geometry naming (st_remove_holes change 'geometry' into 'geom')
  dplyr::rename(geometry = geom) %>%
  # Ungroup
  ungroup()

# Make an exploratory graph to see if everything looks okay
plot(lmp_shapes["uniqueID"], main = "Lamprey River Sites", axes = T, reset = F, lab = c(3, 3, 3))
plot(lmp_spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# Check relevant shapefile geometry
lmp_shapes$geometry

# Save out shapefile
st_write(obj = lmp_shapes, dsn = "watershed-shapefiles/LMP_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Niwot Ridge LTER (NWT) -----------------------------------------------------
# Check what sites are within this LTER
sites %>%
  filter(LTER == "NWT") %>%
  dplyr::select(uniqueID, lat, long)

# NWT Process Raw Shapefiles ----------------------------------------------

# Read in needed raw shapefile(s)
## Downloaded from: https://streamstats.usgs.gov/ss/
nwt_albion <- sf::st_read('watershed-shapefiles/NWT_raw-shapefiles/NWT_ALBION/globalwatershed.shp')
nwt_martin <- sf::st_read('watershed-shapefiles/NWT_raw-shapefiles/NWT_MARTINELLI/globalwatershed.shp')
nwt_saddle <- sf::st_read('watershed-shapefiles/NWT_raw-shapefiles/NWT_SADDLE-STREAM-007/globalwatershed.shp')

# Saddle polygon is reading as a multipolygon because of pixels touching each other only on the corners (see bottom of shape)
str(nwt_saddle)
nwt_explore <- nwt_saddle %>%
  # Note in the warning that st_cast() creates duplicate geometries
  sf::st_cast("POLYGON") %>%
  # Add a column for row to account for these diffs
  dplyr::mutate(rowId = seq_along(Name))

# Plot for exploratory purposes
plot(nwt_explore["rowId"], axes = T, lab = c(2, 2, 3))
nwt_explore
## Not clear if we *need* it to be a polygon so let's leave that alone for now

# Add the uniqueID the shape corresponds to and remove any unneeded layers in the polygon
nwt_albion_v2 <- nwt_albion %>%
  mutate(uniqueID = "NWT_ALBION") %>%
  dplyr::select(-Name)
nwt_martin_v2 <- nwt_martin %>%
  mutate(uniqueID = "NWT_MARTINELLI") %>%
  dplyr::select(-Name)
nwt_saddle_v2 <- nwt_saddle %>%
  mutate(uniqueID = "NWT_SADDLE-STREAM-007") %>%
  dplyr::select(-Name)

# Combine them
nwt_all_watersheds <- rbind(nwt_albion_v2, nwt_martin_v2, nwt_saddle_v2)
str(nwt_all_watersheds)

# NWT Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
nwt_spatial <- sites %>%
  filter(LTER == "NWT") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(nwt_spatial)
nwt_spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Preemptively fix any holes inside of polygons (more of an issue for bigger polygons)
nwt_shapes <- nwt_all_watersheds %>%
  # Group by uniqueID
  group_by(uniqueID) %>%
  # Close holes
  nngeo::st_remove_holes() %>%
  # Fix geometry naming (st_remove_holes change 'geometry' into 'geom')
  dplyr::rename(geometry = geom) %>%
  # Ungroup
  ungroup()

# Make an exploratory graph to see if everything looks okay
plot(nwt_shapes["uniqueID"], main = "Niwot Ridge LTER Sites", axes = T, reset = F, lab = c(3, 1, 3))
plot(nwt_spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)
## Little jagged ones may be an issue down the line but they look prima facie reasonable

# Check relevant shapefile geometry
nwt_shapes$geometry

# Save out shapefile
st_write(obj = nwt_shapes, dsn = "watershed-shapefiles/NWT_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Sagehen -------------------------------------------------------------------
# Check what sites are within this LTER
sites %>%
  filter(LTER == "Sagehen") %>%
  dplyr::select(uniqueID, lat, long)

# Sagehen Process Raw Shapefiles ----------------------------------------------

# Read in needed raw shapefile(s)
## Downloaded from: https://streamstats.usgs.gov/ss/
sagehen <- sf::st_read('watershed-shapefiles/Sagehen_raw-shapefiles/Sagehen/globalwatershed.shp')

# Add the uniqueID the shape corresponds to and remove any unneeded layers in the polygon
sagehen_v2 <- sagehen %>%
  mutate(uniqueID = "Sagehen") %>%
  dplyr::select(-Name)

# For consistency, call it a new name
sagehen_all_watersheds <- sagehen_v2

# Examine
str(sagehen_all_watersheds)

# Sagehen Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
sagehen_spatial <- sites %>%
  filter(LTER == "Sagehen") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(sagehen_spatial)
sagehen_spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Preemptively fix any holes inside of polygons (more of an issue for bigger polygons)
sagehen_shapes <- sagehen_all_watersheds %>%
  # Group by uniqueID
  group_by(uniqueID) %>%
  # Close holes
  nngeo::st_remove_holes() %>%
  # Fix geometry naming (st_remove_holes change 'geometry' into 'geom')
  dplyr::rename(geometry = geom) %>%
  # Ungroup
  ungroup()

# Make an exploratory graph to see if everything looks okay
## Takes a little longer to run because of the size of some of them
plot(sagehen_shapes["uniqueID"], main = "Sagehen Sites", axes = T, reset = F, lab = c(3, 3, 3))
plot(sagehen_spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# Check relevant shapefile geometry
sagehen_shapes$geometry

# Save out shapefile
st_write(obj = sagehen_shapes, dsn = "watershed-shapefiles/Sagehen_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Arctic Great Rivers Observatory (GRO) ------------------------------------
## Data link: https://drive.google.com/drive/folders/1S-MAqA9ahewFhOb8yHEmMc-JC9jX6rwC?usp=sharing

# Check sites within this 'LTER'
sites %>%
  filter(LTER == "GRO") %>%
  dplyr::select(uniqueID, lat, long)

# GRO Load Raw Watershed Shapefiles ----------------------------------------

# Load in the relevant shapefile
gro_raw <- sf::st_read("watershed-shapefiles/GRO_raw-shapefiles/ArcticGRO_large_watersheds.shp")

# Check Coordinate reference system
sf::st_crs(gro_raw)

# Change to WGS84
gro_actual <- st_transform(x = gro_raw, crs = 4326)

# Check geometry with fixed coordinates
gro_actual$geometry

# GRO Identify Watersheds of Site Coordinates ------------------------------

# Make the relevant subset of the sites object explicitly spatial
gro_spatial <- sites %>%
  filter(LTER == "GRO") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(gro_spatial)
gro_spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Identify watersheds with at least one site coordinate in them
gro_site_ids <- gro_spatial %>%
  dplyr::mutate(
    intersection = as.integer(st_intersects(geometry, gro_actual)),
    name = ifelse(test = !is.na(intersection),
                      yes = gro_actual$name[intersection],
                      no = '') )

# Check that it worked
dplyr::select(gro_site_ids, uniqueID, intersection, name, geometry)

# Make an exploratory plot
plot(gro_actual["name"], main = "Great Rivers Observatory Sites", axes = T, reset = F)
plot(gro_spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# GRO Export Shapefiles ----------------------------------------------------

# Finalize GRO polygons
gro_watersheds <- gro_actual %>%
  # Keep only needed columns
  dplyr::select(name, geometry) %>%
  # Create uniqueID layer
  dplyr::mutate(uniqueID = paste("GRO", name, sep = "_")) %>%
  # Remove name column
  dplyr::select(uniqueID, geometry)

# Check structure
str(gro_watersheds)
## All of them reads as a multipolygon..

# Let's make one to explort
gro_explore <- gro_watersheds %>%
  filter(uniqueID == "GRO_Yukon") %>%
  st_cast("POLYGON") %>%
  dplyr::mutate(rowId = seq_along(uniqueID))
gro_explore
plot(gro_explore["rowId"])

# Save out shapefile
st_write(obj = gro_watersheds, dsn = "watershed-shapefiles/GRO_watersheds.shp", delete_layer = T)
## The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Arctic LTER (ARC) --------------------------------------------------------
## Data link: https://www.uaf.edu/toolik/gis/data/index.php#
### See "Major research watersheds around Toolik Field Station" at the bottom of the list

# Check which stream sites are in this LTER
sites %>%
  filter(LTER == "ARC") %>%
  dplyr::select(uniqueID, lat, long)

# ARC Load Raw Shapefiles --------------------------------------------------

# Read in each watershed's polygon
arc_crump <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Crump_Watershed.shp")
arc_imna <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Imnaviat_Watershed.shp")
arc_kling <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Kling_Watershed.shp")
arc_kupar <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Kuparuk_Watershed.shp")
arc_lost.lake <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Lost_Lake_Watershed.shp")
arc_oksruk <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Oksrukuyik_Watershed.shp")
arc_thermo <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Thermokarst_Watershed.shp")
arc_toolik <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Toolik_inlet_Watershed.shp")

# Simplify them to have just the bare minimum needed stuff
arc_crump_simp <- arc_crump %>%
  dplyr::select(Name, AREA, geometry)
arc_imna_simp <- arc_imna %>%
  dplyr::select(Name, AREA, geometry)
arc_kling_simp <- arc_kling %>%
  dplyr::select(Name, AREA, geometry)
arc_kupar_simp <- arc_kupar %>%
  dplyr::select(Name, AREA, geometry)
arc_lost.lake_simp <- arc_lost.lake %>%
  dplyr::select(Name, AREA, geometry)
arc_oksruk_simp <- arc_oksruk %>%
  dplyr::select(Name, AREA, geometry)
arc_thermo_simp <- arc_thermo %>%
  dplyr::select(Name, AREA, geometry)
arc_toolik_simp <- arc_toolik %>%
  # For some reason this one is missing a name column
  dplyr::mutate(Name = "Toolik Inlet") %>%
  dplyr::select(Name, AREA, geometry) 

# Combine them
arc_all_watersheds <- rbind(arc_crump_simp, arc_imna_simp, arc_kling_simp,
                            arc_kupar_simp, arc_lost.lake_simp, arc_oksruk_simp,
                            arc_thermo_simp, arc_toolik_simp)

# Check Coordinate reference system
sf::st_crs(arc_all_watersheds)

# Change to WGS84
arc_actual <- st_transform(x = arc_all_watersheds, crs = 4326)

# Check geometry with fixed coordinates
arc_actual$geometry

# ARC Identify Watersheds of Site Coordinates ------------------------------

# Make the relevant subset of the sites object explicitly spatial
arc_spatial <- sites %>%
  filter(LTER == "ARC") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(arc_spatial)
arc_spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Identify watersheds with at least one site coordinate in them
arc_site_ids <- arc_spatial %>%
  dplyr::mutate(
    intersection = as.integer(st_intersects(geometry, arc_actual)),
    Name = ifelse(test = !is.na(intersection),
                  yes = arc_actual$Name[intersection],
                  no = '') )

# Check that it worked
dplyr::select(arc_site_ids, uniqueID, intersection, Name)

# Make an exploratory plot
plot(arc_actual["Name"], main = "Arctic LTER Sites", axes = T, reset = F, lab = c(3, 3, 3))
plot(arc_spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# ARC Export Shapefiles ----------------------------------------------------
# Given that the site is in just one of the watersheds, let's snag that shapefile alone

# Check relevant shapefile geometry
arc_imna_simp$geometry

# Prepare final version of shapefile
arc_final <- arc_imna_simp %>%
  # Convert to different CRS
  sf::st_transform(crs = 4326) %>%
  # Create uniqueID column
  dplyr::mutate(uniqueID = "ARC_Imnavait-Weir") %>%
  # Keep only uniqueID and geometry
  dplyr::select(uniqueID, geometry)
  
# Check structure + geometry with fixed coordinates
str(arc_final)
arc_final$geometry

# Save out shapefile
st_write(obj = arc_final, dsn = "watershed-shapefiles/ARC_watersheds.shp", delete_layer = T)
## The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Luquillo LTER (LUQ) ------------------------------------------------------
## Data link: https://data.usgs.gov/datacatalog/data/USGS:596ce15ae4b0d1f9f061686e

# Check which stream sites are in this LTER
sites %>%
  filter(LTER == "LUQ") %>%
  dplyr::select(uniqueID, lat, long)

# LUQ Process Raw Shapefiles ----------------------------------------------

# Read in each raw shapefile
## All downloaded from: https://streamstats.usgs.gov/ss/
luq_mpr <- sf::st_read('watershed-shapefiles/LUQ_raw-shapefiles/LUQ_MPR/globalwatershed.shp')
luq_q1 <- sf::st_read('watershed-shapefiles/LUQ_raw-shapefiles/LUQ_Q1/globalwatershed.shp')
# Note that Q2 and Q3 have identical lat/longs so use the same shapefile
# Until clarification can be gotten from PIs anyway
luq_q2 <- sf::st_read('watershed-shapefiles/LUQ_raw-shapefiles/LUQ_Q2/globalwatershed.shp')
luq_q3 <- sf::st_read('watershed-shapefiles/LUQ_raw-shapefiles/LUQ_Q3/globalwatershed.shp')
luq_qs <- sf::st_read('watershed-shapefiles/LUQ_raw-shapefiles/LUQ_QS/globalwatershed.shp')
luq_ri <- sf::st_read('watershed-shapefiles/LUQ_raw-shapefiles/LUQ_RI/globalwatershed.shp')

# Add the uniqueID the shape corresponds to LUQ remove any unneeded layers in the polygon
luq_mpr_v2 <- luq_mpr %>%
  mutate(uniqueID = "LUQ_MPR") %>%
  dplyr::select(-Name)
luq_q1_v2 <- luq_q1 %>%
  mutate(uniqueID = "LUQ_Q1") %>%
  dplyr::select(-Name)
luq_q2_v2 <- luq_q2 %>%
  mutate(uniqueID = "LUQ_Q2") %>%
  dplyr::select(-Name)
luq_q3_v2 <- luq_q3 %>%
  mutate(uniqueID = "LUQ_Q3") %>%
  dplyr::select(-Name)
luq_qs_v2 <- luq_qs %>%
  mutate(uniqueID = "LUQ_QS") %>%
  dplyr::select(-Name)
luq_ri_v2 <- luq_ri %>%
  mutate(uniqueID = "LUQ_RI") %>%
  dplyr::select(-Name)

# Combine them
luq_all_watersheds <- rbind(luq_mpr_v2, luq_q1_v2, luq_q2_v2,
                            luq_q3_v2, luq_qs_v2, luq_ri_v2)
str(luq_all_watersheds)

# LUQ Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
luq_spatial <- sites %>%
  filter(LTER == "LUQ") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(luq_spatial)
luq_spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Preemptively fix any holes inside of polygons (more of an issue for bigger polygons)
luq_shapes <- luq_all_watersheds %>%
  # Group by uniqueID
  group_by(uniqueID) %>%
  # Close holes
  nngeo::st_remove_holes() %>%
  # Fix geometry naming (st_remove_holes change 'geometry' into 'geom')
  dplyr::rename(geometry = geom) %>%
  # Ungroup
  ungroup()

# Make an exploratory graph to see if everything looks okay
plot(luq_shapes["uniqueID"], main = "Luquillo Sites", axes = T, reset = F)
plot(luq_spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# Check relevant shapefile geometry
luq_shapes$geometry

# Save out shapefile
st_write(obj = luq_shapes, dsn = "watershed-shapefiles/LUQ_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Kissimmee River (KRR) ----------------------------------------------------
## Data link: https://www.lake.wateratlas.usf.edu/watershed/?wshedid=1&wbodyatlas=watershed

# Check which stream sites are in this LTER
sites %>%
  filter(LTER == "KRR") %>%
  dplyr::select(uniqueID, lat, long)

# KRR Load Raw Shapefiles --------------------------------------------------

# Read in the shapefile for southern Florida's drainage basins/watersheds
krr_raw <- sf::st_read("watershed-shapefiles/KRR_raw-shapefiles/KRR_Watersheds.shp")
str(krr_raw)

# Simplify it to have just the bare minimum needed stuff
krr_simp <- krr_raw %>%
  dplyr::select(FID, geometry)

# Check Coordinate reference system
sf::st_crs(krr_simp)
krr_simp$geometry
  ## Already in WGS84 so no conversion is necessary!

# KRR Identify Watersheds of Site Coordinates ------------------------------

# Make the relevant subset of the sites object explicitly spatial
krr_spatial <- sites %>%
  filter(LTER == "KRR") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(krr_spatial)
krr_spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Make an exploratory graph
plot(krr_simp["FID"], main = "Kissimmee Sites", axes = T, reset = F)
plot(krr_spatial["uniqueID"], axes = T, pch = 18, col = 'white', add = T)

# Identify watersheds with at least one site coordinate in them
krr_site_ids <- krr_spatial %>%
  dplyr::mutate(
    ## Same error as LUQ but still important to run this line
    intersection = as.integer(st_intersects(geometry, krr_simp)),
    FID = ifelse(test = !is.na(intersection),
                  yes = krr_simp$FID[intersection],
                  no = '') )

# Diagnose which site(s) have duplicate intersections
krr_spatial %>%
  dplyr::mutate(intersection = st_intersects(geometry, krr_simp))

# Make vector of those numbers
krr.test.vec <- c(2, 4, 25, 28, 41, 45, 59, 77)

# Plot to see what extents these are
plot(subset(krr_simp, FID %in% krr.test.vec), main = "Kissimmee Sites", axes = T, reset = F)
plot(krr_spatial["uniqueID"], axes = T, pch = 18, col = 'white', add = T)

# Plot them one at a time now
  ## 2 appears to be the entire watershed
plot(subset(krr_simp, FID == 2), main = "Kissimmee Sites", axes = T, reset = F)
plot(krr_spatial["uniqueID"], axes = T, pch = 18, col = 'white', add = T)
  ## 4 appears to be the major sub-component of the watershed
plot(subset(krr_simp, FID == 4), main = "Kissimmee Sites", axes = T, reset = F)
plot(krr_spatial["uniqueID"], axes = T, pch = 18, col = 'white', add = T)
  ## the third number then is going to be another division (likely splitting N and S)
plot(subset(krr_simp, FID == 25), main = "Kissimmee Sites", axes = T, reset = F)
plot(krr_spatial["uniqueID"], axes = T, pch = 18, col = 'gray45', add = T)
  ## And I'll leave the fourth number un-plotted but that must be the most specific...
  ## ...and hence is the one we most want

# Try again but this time accounting for the layer nature of 'krr_simp'
krr_site_ids <- krr_spatial %>%
  dplyr::mutate(
    ## Paste together the different IDs
    intersection.list = paste(st_intersects(geometry, krr_simp)),
    ## Snag just the final number (that is the most specific part)
    intersection = as.integer(str_sub(intersection.list,
                           start = nchar(intersection.list) - 3,
                           end = nchar(intersection.list) - 1)),
    FID = ifelse(test = !is.na(intersection),
                 yes = krr_simp$FID[intersection],
                 no = '') ) %>%
  ## And remove intermediary list of intersection points
  dplyr::select(-intersection.list)

# Check that it worked
dplyr::select(krr_site_ids, uniqueID, intersection, FID)

# Finalize the shapefile
krr_final <- krr_simp %>%
  # Strip out the relevant part of the shapefile
  filter(FID %in% krr_site_ids$FID) %>%
  # Bring in the unique ID
  dplyr::mutate(uniqueID = as.character(krr_site_ids$uniqueID[match(FID, krr_site_ids$FID)])) %>%
  # Retain only uniqueID and geometry
  dplyr::select(uniqueID, geometry)

# Check structure
str(krr_final)

# Do a final plot
plot(krr_final["uniqueID"], main = "Kissimmee Sites", axes = T, reset = F, lab = c(3, 2, 3))
plot(krr_spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# KRR Export Shapefiles ----------------------------------------------------

# Check relevant shapefile geometry
krr_final$geometry

# Make it a polygon (rather than multipolygon)
krr_actual <- krr_final %>%
  st_cast("POLYGON")
## This one doesn't duplicate the shapes so the conversion is okay

# Check structure again
str(krr_actual)
plot(krr_actual["uniqueID"], main = "Kissimmee Sites", axes = T, lab = c(3, 2, 3))

# Save out shapefile
st_write(obj = krr_actual, dsn = "watershed-shapefiles/KRR_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# McMurdo LTER (MCM) -------------------------------------------------------
## Data source: waiting for LTER Information Manager to send shapefiles to me

# Check which stream sites are in this LTER
sites %>%
  filter(LTER == "MCM") %>%
  dplyr::select(uniqueID, lat, long)











# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Silica Synthesis - Create Global Shapefile -------------------------------

# We may want a single shapefile of all LTERs' shapefiles
## Could be useful for plotting
## And if land cover/lithology rasters are global...
## ...could gather all associated data in one fell swoop

# Remind yourself of the LTERs at play
sort(unique(sites$LTER))

# Begin by reading in each of the LTER's separate polygons
## Each is standardized before export in this script
and_sheds <- sf::st_read('watershed-shapefiles/AND_watersheds.shp')
arc_sheds <- sf::st_read('watershed-shapefiles/ARC_watersheds.shp')
gro_sheds <- sf::st_read('watershed-shapefiles/GRO_watersheds.shp')
hbr_sheds <- sf::st_read('watershed-shapefiles/HBR_watersheds.shp')
krr_sheds <- sf::st_read('watershed-shapefiles/KRR_watersheds.shp')
lmp_sheds <- sf::st_read('watershed-shapefiles/LMP_watersheds.shp')
luq_sheds <- sf::st_read('watershed-shapefiles/LUQ_watersheds.shp')
# mcm_sheds <- sf::st_read('watershed-shapefiles/MCM_watersheds.shp')
nwt_sheds <- sf::st_read('watershed-shapefiles/NWT_watersheds.shp')
sagehen_sheds <- sf::st_read('watershed-shapefiles/Sagehen_watersheds.shp')
umr_sheds <- sf::st_read('watershed-shapefiles/UMR_watersheds.shp')

# Check what kind of geometry each of them has (it may not matter but good to know)
str(and_sheds)
str(arc_sheds)
str(gro_sheds) # multipolygon
str(hbr_sheds)
str(krr_sheds)
str(lmp_sheds)
str(luq_sheds)
# str(mcm_sheds)
str(nwt_sheds) # multipolygon
str(sagehen_sheds)
str(umr_sheds) # multipolygon

# Combine them into a single shapefile
## Same process as combining separate watersheds within LTER as shown earlier
silica_sheds <- rbind(and_sheds, arc_sheds, gro_sheds, hbr_sheds,
                      krr_sheds, lmp_sheds, luq_sheds, #mcm_sheds,
                      nwt_sheds, sagehen_sheds, umr_sheds)

# Check the structure
str(silica_sheds)

# Check that no duplication of features occurred
length(silica_sheds$uniqueID) == length(unique(silica_sheds$uniqueID))
plyr::count(silica_sheds$uniqueID) %>%
  filter(freq > 1)

# It should include all uniqueIDs
setdiff(unique(sites$uniqueID), unique(silica_sheds$uniqueID))
  ## "character(0)" means all are included

# And should not include any uniqueIDs that aren't in the 'sites' data object
setdiff(unique(silica_sheds$uniqueID), unique(sites$uniqueID))
  ## "character(0)" means no unexpected values/typos

# Do some processing to get this finalized
silica_final <- silica_sheds %>%
  # Create and reposition an LTER column
  dplyr::mutate( LTER = str_sub(uniqueID, 1, 3) ) %>%
  relocate(LTER, .before = everything())

# Check structure
str(silica_final)

# Make an exploratory plot
plot(silica_final["uniqueID"], main = "Silica Synthesis Sites", axes = T)

# Make another grouped by LTER
plot(silica_final["LTER"], main = "Silica Synthesis LTERs", axes = T)

# Check relevant shapefile geometry
silica_final$geometry

# Export the global shapefile
st_write(obj = silica_final, dsn = "watershed-shapefiles/SilicaSynthesis_allWatersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Silica Synthesis - Save Tidy 'Sites' Object -------------------------------

# Read in final silica shapefile to do some final manipulations
silica_final <- sf::st_read("watershed-shapefiles/SilicaSynthesis_allWatersheds.shp")

# Before we are done, let's compute the area of the watersheds
## We can then compare that to the original area (provided by PIs in the raw csv)
area_obj <- silica_final %>%
  # Calculate area (automatically does this within each polygon)
  dplyr::mutate(area_m2 = units::drop_units(st_area(silica_final))) %>%
  #dplyr::mutate(area_m2 = st_area(silica_final)) %>%
  # Convert it to km2
  dplyr::mutate(area_km2 = (area_m2 * 1e-6))

# Check it
head(area_obj)

# Attach it to the site object
sites_final <- sites %>%
  dplyr::mutate(
    drainSqKm_calculated = area_obj$area_km2[match(sites$uniqueID, area_obj$uniqueID)],
    # and calculate the difference
    drainSqKm_diff = round((drainSqKm_original - drainSqKm_calculated), digits = 2),
    # and express that as a percent
    drainSqKm_percDiff = round(((drainSqKm_diff / drainSqKm_original) * 100), digits = 1)
  ) %>%
  # Relocate it to its proper place
  dplyr::relocate(drainSqKm_calculated:drainSqKm_percDiff,
                  .after = drainSqKm_original)

# Check that worked
head(sites_final)

# Let's save this version so we don't need to re-make it in future scripts
write.csv(sites_final, 'tidy_SilicaSites.csv', row.names = F)

# End --------------------------------------------------------------------
