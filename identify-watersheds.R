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
library(tidyverse); library(sf)

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
sites.raw <- read.csv("Longtermsites_DA_Coordinates_2.14.22.csv")

# Do some needed pre-processing
sites <- sites.raw %>%
  dplyr::mutate(
    # Remove spaces from all stream sites' (within LTER) names
    SITE = gsub(' ', '-', SITE),
    # Create unique within LTER site names
    uniqueID = as.factor(paste0(LTER.x, "_", SITE)),
    # Make lat/long numeric
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    # Fix a typo with Sagehen's longitude
    Longitude = ifelse(LTER.x == "Sagehen",
                       yes = (Longitude * -1),
                       no = Longitude),
    # Fix a typo with two sites within the Great Rivers Observatory's longitude
    Longitude = ifelse(uniqueID == "GRO_Mackenzie" | uniqueID == "GRO_Yukon",
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

# Examine output
head(sites)

# Get an explicitly spatial version of the sites dataframe
sites.spatial <- sf::st_as_sf(sites, coords = c("long", "lat"), crs = 4326)
str(sites.spatial)

# Andrews Forest LTER (AND) ----------------------------------------------

# Check what sites are within this LTER
sites %>%
  filter(LTER == "AND") %>%
  dplyr::select(uniqueID, lat, long)

# AND Process Raw Shapefiles ----------------------------------------------

# Read in each raw shapefile
## All downloaded from: https://streamstats.usgs.gov/ss/
and.gsmack <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSMACK/globalwatershed.shp')
and.gsws02 <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSWS02/globalwatershed.shp')
and.gsws06 <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSWS06/globalwatershed.shp')
and.gsws07 <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSWS07/globalwatershed.shp')
and.gsws08 <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSWS08/globalwatershed.shp')
and.gsws09 <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSWS09/globalwatershed.shp')
and.gsws10 <- sf::st_read('watershed-shapefiles/AND_raw-shapefiles/AND_GSWS10/globalwatershed.shp')

# Add the uniqueID the shape corresponds to and remove any unneeded layers in the polygon
and.gsmack.v2 <- and.gsmack %>%
  mutate(uniqueID = "AND_GSMACK") %>%
  dplyr::select(-Name)
and.gsws02.v2 <- and.gsws02 %>%
  mutate(uniqueID = "AND_GSWS02") %>%
  dplyr::select(-Name)
and.gsws06.v2 <- and.gsws06 %>%
  mutate(uniqueID = "AND_GSWS06") %>%
  dplyr::select(-Name)
and.gsws07.v2 <- and.gsws07 %>%
  mutate(uniqueID = "AND_GSWS07") %>%
  dplyr::select(-Name)
and.gsws08.v2 <- and.gsws08 %>%
  mutate(uniqueID = "AND_GSWS08") %>%
  dplyr::select(-Name)
and.gsws09.v2 <- and.gsws09 %>%
  mutate(uniqueID = "AND_GSWS09") %>%
  dplyr::select(-Name)
and.gsws10.v2 <- and.gsws10 %>%
  mutate(uniqueID = "AND_GSWS10") %>%
  dplyr::select(-Name)

# Combine them
and.all.watersheds <- rbind(and.gsmack.v2, and.gsws02.v2, and.gsws02.v2,
                            and.gsws06.v2, and.gsws07.v2, and.gsws08.v2,
                            and.gsws09.v2, and.gsws10.v2)
str(and.all.watersheds)

# AND Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
and.spatial <- sites %>%
  filter(LTER == "AND") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(and.spatial)
and.spatial$geometry

# Make an exploratory graph to see if everything looks okay
plot(and.all.watersheds["uniqueID"], main = "Andrews Forest Sites", axes = T, reset = F, lab = c(2, 2, 3))
plot(and.spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T, las = 2)

# Check relevant shapefile geometry
and.all.watersheds$geometry

# Save out shapefile
st_write(obj = and.all.watersheds, dsn = "watershed-shapefiles/AND_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites.raw', 'sites', 'sites.spatial')))

# Hubbard Brook LTER (HBR) ---------------------------------------------
# Check what sites are within this LTER
sites %>%
  filter(LTER == "HBR") %>%
  dplyr::select(uniqueID, lat, long)

# HBR Process Raw Shapefiles ----------------------------------------------

# Read in each raw shapefile
## All downloaded from: https://streamstats.usgs.gov/ss/
hbr.ws1 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws1/globalwatershed.shp')
hbr.ws2 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws2/globalwatershed.shp')
hbr.ws3 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws3/globalwatershed.shp')
hbr.ws4 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws4/globalwatershed.shp')
hbr.ws5 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws5/globalwatershed.shp')
hbr.ws6 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws6/globalwatershed.shp')
hbr.ws7 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws7/globalwatershed.shp')
hbr.ws8 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws8/globalwatershed.shp')
hbr.ws9 <- sf::st_read('watershed-shapefiles/HBR_raw-shapefiles/HBR_ws9/globalwatershed.shp')

# Add the uniqueID the shape corresponds to and remove any unneeded layers in the polygon
hbr.ws1.v2 <- hbr.ws1 %>%
  mutate(uniqueID = "HBR_ws1") %>%
  dplyr::select(-Name)
hbr.ws2.v2 <- hbr.ws2 %>%
  mutate(uniqueID = "HBR_ws2") %>%
  dplyr::select(-Name)
hbr.ws3.v2 <- hbr.ws3 %>%
  mutate(uniqueID = "HBR_ws3") %>%
  dplyr::select(-Name)
hbr.ws4.v2 <- hbr.ws4 %>%
  mutate(uniqueID = "HBR_ws4") %>%
  dplyr::select(-Name)
hbr.ws5.v2 <- hbr.ws5 %>%
  mutate(uniqueID = "HBR_ws5") %>%
  dplyr::select(-Name)
hbr.ws6.v2 <- hbr.ws6 %>%
  mutate(uniqueID = "HBR_ws6") %>%
  dplyr::select(-Name)
hbr.ws7.v2 <- hbr.ws7 %>%
  mutate(uniqueID = "HBR_ws7") %>%
  dplyr::select(-Name)
hbr.ws8.v2 <- hbr.ws8 %>%
  mutate(uniqueID = "HBR_ws8") %>%
  dplyr::select(-Name)
hbr.ws9.v2 <- hbr.ws9 %>%
  mutate(uniqueID = "HBR_ws9") %>%
  dplyr::select(-Name)

# Combine them
hbr.all.watersheds <- rbind(hbr.ws1.v2, hbr.ws2.v2, hbr.ws3.v2,
                            hbr.ws4.v2, hbr.ws5.v2, hbr.ws6.v2,
                            hbr.ws7.v2, hbr.ws8.v2, hbr.ws9.v2)
str(hbr.all.watersheds)

# HBR Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
hbr.spatial <- sites %>%
  filter(LTER == "HBR") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(hbr.spatial)
hbr.spatial$geometry

# Make an exploratory graph to see if everything looks okay
plot(hbr.all.watersheds["uniqueID"], main = "Hubbard Brook Sites", axes = T, reset = F)
plot(hbr.spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# Check relevant shapefile geometry
hbr.all.watersheds$geometry

# Save out shapefile
st_write(obj = hbr.all.watersheds, dsn = "watershed-shapefiles/HBR_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites.raw', 'sites', 'sites.spatial')))

# Upper Mississippi River (UMR) ---------------------------------------------
# Check what sites are within this LTER
sites %>%
  filter(LTER == "UMR") %>%
  dplyr::select(uniqueID, lat, long)

# UMR Process Raw Shapefiles ----------------------------------------------

# Read in each raw shapefile
## All downloaded from: https://streamstats.usgs.gov/ss/
umr.I080.2M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_I080.2M/globalwatershed.shp')
umr.SG16.2C <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_SG16.2C/globalwatershed.shp')
umr.CH00.1M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_CH00.1M/globalwatershed.shp')
umr.CN00.1M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_CN00.1M/globalwatershed.shp')
umr.CU11.6M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_CU11.6M/globalwatershed.shp')

umr.LM00.5M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_LM00.5M/globalwatershed.shp')
umr.M078.0B <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_M078.0B/globalwatershed.shp')
umr.M556.4A <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_M556.4A/globalwatershed.shp')
umr.M701.1B <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_M701.1B/globalwatershed.shp')
umr.M764.3A <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_M764.3A/globalwatershed.shp')

umr.M786.2C <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_M786.2C/globalwatershed.shp')
umr.MQ02.1M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_MQ02.1M/globalwatershed.shp')
umr.WP02.6M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_WP02.6M/globalwatershed.shp')
umr.BK01.0M <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_BK01.0M/globalwatershed.shp')
umr.M241.4K <- sf::st_read('watershed-shapefiles/UMR_raw-shapefiles/UMR_M241.4K/globalwatershed.shp')

# Add the uniqueID the shape corresponds to and remove any unneeded layers in the polygon
umr.I080.2M.v2 <- umr.I080.2M %>%
  mutate(uniqueID = "UMR_I080.2M") %>%
  dplyr::select(-Name)
umr.SG16.2C.v2 <- umr.SG16.2C %>%
  mutate(uniqueID = "UMR_SG16.2C") %>%
  dplyr::select(-Name)
umr.CH00.1M.v2 <- umr.CH00.1M %>%
  mutate(uniqueID = "UMR_CH00.1M") %>%
  dplyr::select(-Name)
umr.CN00.1M.v2 <- umr.CN00.1M %>%
  mutate(uniqueID = "UMR_CN00.1M") %>%
  dplyr::select(-Name)
umr.CU11.6M.v2 <- umr.CU11.6M %>%
  mutate(uniqueID = "UMR_CU11.6M") %>%
  dplyr::select(-Name)

umr.LM00.5M.v2 <- umr.LM00.5M %>%
  mutate(uniqueID = "UMR_LM00.5M") %>%
  dplyr::select(-Name)
umr.M078.0B.v2 <- umr.M078.0B %>%
  mutate(uniqueID = "UMR_M078.0B") %>%
  dplyr::select(-Name)
umr.M556.4A.v2 <- umr.M556.4A %>%
  mutate(uniqueID = "UMR_M556.4A") %>%
  dplyr::select(-Name)
umr.M701.1B.v2 <- umr.M701.1B %>%
  mutate(uniqueID = "UMR_M701.1B") %>%
  dplyr::select(-Name)
umr.M764.3A.v2 <- umr.M764.3A %>%
  mutate(uniqueID = "UMR_M764.3A") %>%
  dplyr::select(-Name)

umr.M786.2C.v2 <- umr.M786.2C %>%
  mutate(uniqueID = "UMR_M786.2C") %>%
  dplyr::select(-Name)
umr.MQ02.1M.v2 <- umr.MQ02.1M %>%
  mutate(uniqueID = "UMR_MQ02.1M") %>%
  dplyr::select(-Name)
umr.WP02.6M.v2 <- umr.WP02.6M %>%
  mutate(uniqueID = "UMR_WP02.6M") %>%
  dplyr::select(-Name)
umr.BK01.0M.v2 <- umr.BK01.0M %>%
  mutate(uniqueID = "UMR_BK01.0M") %>%
  dplyr::select(-Name)
umr.M241.4K.v2 <- umr.M241.4K %>%
  mutate(uniqueID = "UMR_M241.4K") %>%
  dplyr::select(-Name)

# Combine them
umr.all.watersheds <- rbind(umr.I080.2M.v2, umr.SG16.2C.v2, umr.CH00.1M.v2,
                            umr.CN00.1M.v2, umr.CU11.6M.v2, umr.LM00.5M.v2,
                            umr.M078.0B.v2, umr.M556.4A.v2, umr.M701.1B.v2, 
                            umr.M764.3A.v2, umr.M786.2C.v2, umr.MQ02.1M.v2,
                            umr.WP02.6M.v2, umr.BK01.0M.v2, umr.M241.4K.v2)
str(umr.all.watersheds)

# UMR Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
umr.spatial <- sites %>%
  filter(LTER == "UMR") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(umr.spatial)
umr.spatial$geometry

# Make an exploratory graph to see if everything looks okay
## Takes a little longer to run because of the size of some of them
plot(umr.all.watersheds["uniqueID"], main = "Upper Mississippi River Sites", axes = T, reset = F)
plot(umr.spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# Check relevant shapefile geometry
umr.all.watersheds$geometry

# Save out shapefile
st_write(obj = umr.all.watersheds, dsn = "watershed-shapefiles/UMR_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites.raw', 'sites', 'sites.spatial')))

# Lamprey River (LMP) --------------------------------------------------------
# Check what sites are within this location (technically not an LTER)
sites %>%
  filter(LTER == "LMP") %>%
  dplyr::select(uniqueID, lat, long)

# LMP Process Raw Shapefiles -------------------------------------------------

# Read in needed raw shapefile(s)
## Downloaded from: https://streamstats.usgs.gov/ss/
lmp.73 <- sf::st_read('watershed-shapefiles/LMP_raw-shapefiles/LMP_LMP73/globalwatershed.shp')

# Add the uniqueID the shape corresponds to and remove any unneeded layers in the polygon
lmp.73 <- lmp.73 %>%
  mutate(uniqueID = "LMP_LMP73") %>%
  dplyr::select(-Name)

# Check it
str(lmp.73)

# LMP Final Checks & Export --------------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
lmp.spatial <- sites %>%
  filter(LTER == "LMP") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(lmp.spatial)
lmp.spatial$geometry

# Make an exploratory graph to see if everything looks okay
plot(lmp.73["uniqueID"], main = "Lamprey River Sites", axes = T, reset = F)
plot(lmp.spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# Check relevant shapefile geometry
lmp.73$geometry

# Save out shapefile
st_write(obj = lmp.73, dsn = "watershed-shapefiles/LMP_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites.raw', 'sites', 'sites.spatial')))

# Niwot Ridge LTER (NWT) -----------------------------------------------------
# Check what sites are within this LTER
sites %>%
  filter(LTER == "NWT") %>%
  dplyr::select(uniqueID, lat, long)

# NWT Process Raw Shapefiles ----------------------------------------------

# Read in needed raw shapefile(s)
## Downloaded from: https://streamstats.usgs.gov/ss/
nwt.albion <- sf::st_read('watershed-shapefiles/NWT_raw-shapefiles/NWT_ALBION/globalwatershed.shp')
nwt.martin <- sf::st_read('watershed-shapefiles/NWT_raw-shapefiles/NWT_MARTINELLI/globalwatershed.shp')
nwt.saddle <- sf::st_read('watershed-shapefiles/NWT_raw-shapefiles/NWT_SADDLE-STREAM-007/globalwatershed.shp')

# For some reason the Saddle polygon is reading as a multipolygon
str(nwt.saddle)
nwt.saddle.fix <- nwt.saddle %>%
  sf::st_cast("POLYGON")
str(nwt.saddle.fix)

# Add the uniqueID the shape corresponds to and remove any unneeded layers in the polygon
nwt.albion.v2 <- nwt.albion %>%
  mutate(uniqueID = "NWT_ALBION") %>%
  dplyr::select(-Name)
nwt.martin.v2 <- nwt.martin %>%
  mutate(uniqueID = "NWT_MARTINELLI") %>%
  dplyr::select(-Name)
nwt.saddle.v2 <- nwt.saddle.fix %>%
  mutate(uniqueID = "NWT_SADDLE-STREAM-007") %>%
  dplyr::select(-Name)

# Combine them
nwt.all.watersheds <- rbind(nwt.albion.v2, nwt.martin.v2, nwt.saddle.v2)
str(nwt.all.watersheds)

# NWT Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
nwt.spatial <- sites %>%
  filter(LTER == "NWT") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(nwt.spatial)
nwt.spatial$geometry

# Make an exploratory graph to see if everything looks okay
plot(nwt.all.watersheds["uniqueID"], main = "Niwot Ridge LTER Sites", axes = T, reset = F)
plot(nwt.spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)
## Little jagged ones may be an issue down the line but they look prima facie reasonable

# Check relevant shapefile geometry
nwt.all.watersheds$geometry

# Save out shapefile
st_write(obj = nwt.all.watersheds, dsn = "watershed-shapefiles/NWT_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites.raw', 'sites', 'sites.spatial')))

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
sagehen.v2 <- sagehen %>%
  mutate(uniqueID = "Sagehen") %>%
  dplyr::select(-Name)

# Examine
str(sagehen.v2)

# Sagehen Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
sagehen.spatial <- sites %>%
  filter(LTER == "Sagehen") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(sagehen.spatial)
sagehen.spatial$geometry

# Make an exploratory graph to see if everything looks okay
## Takes a little longer to run because of the size of some of them
plot(sagehen.v2["uniqueID"], main = "Sagehen Sites", axes = T, reset = F)
plot(sagehen.spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# Check relevant shapefile geometry
sagehen.v2$geometry

# Save out shapefile
st_write(obj = sagehen.v2, dsn = "watershed-shapefiles/Sagehen_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites.raw', 'sites', 'sites.spatial')))

# Arctic Great Rivers Observatory (GRO) ------------------------------------
## Data link: https://drive.google.com/drive/folders/1S-MAqA9ahewFhOb8yHEmMc-JC9jX6rwC?usp=sharing

# Check sites within this 'LTER'
sites %>%
  filter(LTER == "GRO") %>%
  dplyr::select(uniqueID, lat, long)

# GRO Load Raw Watershed Shapefiles ----------------------------------------

# Load in the relevant shapefile
gro.raw <- sf::st_read("watershed-shapefiles/GRO_raw-shapefiles/ArcticGRO_large_watersheds.shp")

# Check Coordinate reference system
sf::st_crs(gro.raw)

# Change to WGS84
gro.actual <- st_transform(x = gro.raw, crs = 4326)

# Check geometry with fixed coordinates
gro.actual$geometry

# GRO Identify Watersheds of Site Coordinates ------------------------------

# Make the relevant subset of the sites object explicitly spatial
gro.spatial <- sites %>%
  filter(LTER == "GRO") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(gro.spatial)
gro.spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Identify watersheds with at least one site coordinate in them
gro.site.ids <- gro.spatial %>%
  dplyr::mutate(
    intersection = as.integer(st_intersects(geometry, gro.actual)),
    name = ifelse(test = !is.na(intersection),
                      yes = gro.actual$name[intersection],
                      no = '') )

# Check that it worked
gro.site.ids

# Make an exploratory plot
plot(gro.actual["name"], main = "Great Rivers Observatory Sites", axes = T, reset = F)
plot(gro.spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# GRO Export Shapefiles ----------------------------------------------------

# Finalize GRO polygons
gro.watersheds <- gro.actual %>%
  # Keep only needed columns
  dplyr::select(name, geometry) %>%
  # Create uniqueID layer
  dplyr::mutate(uniqueID = paste("GRO", name, sep = "_")) %>%
  # Remove name column
  dplyr::select(uniqueID, geometry)

# Check structure
str(gro.watersheds)

# Save out shapefile
st_write(obj = gro.watersheds, dsn = "watershed-shapefiles/GRO_watersheds.shp", delete_layer = T)
## The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites.raw', 'sites', 'sites.spatial')))

# Arctic LTER (ARC) --------------------------------------------------------
## Data link: https://www.uaf.edu/toolik/gis/data/index.php#
### See "Major research watersheds around Toolik Field Station" at the bottom of the list

# Check which stream sites are in this LTER
sites %>%
  filter(LTER == "ARC") %>%
  dplyr::select(uniqueID, lat, long)

# ARC Load Raw Shapefiles --------------------------------------------------

# Read in each watershed's polygon
arc.crump <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Crump_Watershed.shp")
arc.imna <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Imnaviat_Watershed.shp")
arc.kling <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Kling_Watershed.shp")
arc.kupar <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Kuparuk_Watershed.shp")
arc.lost.lake <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Lost_Lake_Watershed.shp")
arc.oksruk <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Oksrukuyik_Watershed.shp")
arc.thermo <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Thermokarst_Watershed.shp")
arc.toolik <- sf::st_read("watershed-shapefiles/ARC_raw-shapefiles/Toolik_inlet_Watershed.shp")

# Simplify them to have just the bare minimum needed stuff
arc.crump.simp <- arc.crump %>%
  dplyr::select(Name, AREA, geometry)
arc.imna.simp <- arc.imna %>%
  dplyr::select(Name, AREA, geometry)
arc.kling.simp <- arc.kling %>%
  dplyr::select(Name, AREA, geometry)
arc.kupar.simp <- arc.kupar %>%
  dplyr::select(Name, AREA, geometry)
arc.lost.lake.simp <- arc.lost.lake %>%
  dplyr::select(Name, AREA, geometry)
arc.oksruk.simp <- arc.oksruk %>%
  dplyr::select(Name, AREA, geometry)
arc.thermo.simp <- arc.thermo %>%
  dplyr::select(Name, AREA, geometry)
arc.toolik.simp <- arc.toolik %>%
  # For some reason this one is missing a name column
  dplyr::mutate(Name = "Toolik Inlet") %>%
  dplyr::select(Name, AREA, geometry) 

# Combine them
arc.all.watersheds <- rbind(arc.crump.simp, arc.imna.simp, arc.kling.simp,
                            arc.kupar.simp, arc.lost.lake.simp, arc.oksruk.simp,
                            arc.thermo.simp, arc.toolik.simp)

# Check Coordinate reference system
sf::st_crs(arc.all.watersheds)

# Change to WGS84
arc.actual <- st_transform(x = arc.all.watersheds, crs = 4326)

# Check geometry with fixed coordinates
arc.actual$geometry

# ARC Identify Watersheds of Site Coordinates ------------------------------

# Make the relevant subset of the sites object explicitly spatial
arc.spatial <- sites %>%
  filter(LTER == "ARC") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(arc.spatial)
arc.spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Identify watersheds with at least one site coordinate in them
arc.site.ids <- arc.spatial %>%
  dplyr::mutate(
    intersection = as.integer(st_intersects(geometry, arc.actual)),
    Name = ifelse(test = !is.na(intersection),
                  yes = arc.actual$Name[intersection],
                  no = '') )

# Check that it worked
arc.site.ids

# Make an exploratory plot
plot(arc.actual["Name"], main = "Arctic LTER Sites", axes = T, reset = F)
plot(arc.spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# ARC Export Shapefiles ----------------------------------------------------
# Given that the site is in just one of the watersheds, let's snag that shapefile alone

# Check relevant shapefile geometry
arc.imna.simp$geometry

# Prepare final version of shapefile
arc.final <- arc.imna.simp %>%
  # Convert to different CRS
  sf::st_transform(crs = 4326) %>%
  # Create uniqueID column
  dplyr::mutate(uniqueID = "ARC_Imnavait-Weir") %>%
  # Keep only uniqueID and geometry
  dplyr::select(uniqueID, geometry)
  
# Check structure + geometry with fixed coordinates
str(arc.final)
arc.final$geometry

# Save out shapefile
st_write(obj = arc.final, dsn = "watershed-shapefiles/ARC_watersheds.shp", delete_layer = T)
## The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites.raw', 'sites', 'sites.spatial')))

# Luquillo LTER (LUQ) ------------------------------------------------------
## Data link: https://data.usgs.gov/datacatalog/data/USGS:596ce15ae4b0d1f9f061686e

# Check which stream sites are in this LTER
sites %>%
  filter(LTER == "LUQ") %>%
  dplyr::select(uniqueID, lat, long)

# LUQ Process Raw Shapefiles ----------------------------------------------

# Read in each raw shapefile
## All downloaded from: https://streamstats.usgs.gov/ss/
luq.mpr <- sf::st_read('watershed-shapefiles/LUQ_raw-shapefiles/LUQ_MPR/globalwatershed.shp')
luq.q1 <- sf::st_read('watershed-shapefiles/LUQ_raw-shapefiles/LUQ_Q1/globalwatershed.shp')
# Note that Q2 and Q3 have identical lat/longs so use the same shapefile
# Until clarification can be gotten from PIs anyway
luq.q2 <- sf::st_read('watershed-shapefiles/LUQ_raw-shapefiles/LUQ_Q2/globalwatershed.shp')
luq.q3 <- sf::st_read('watershed-shapefiles/LUQ_raw-shapefiles/LUQ_Q3/globalwatershed.shp')
luq.qs <- sf::st_read('watershed-shapefiles/LUQ_raw-shapefiles/LUQ_QS/globalwatershed.shp')
luq.ri <- sf::st_read('watershed-shapefiles/LUQ_raw-shapefiles/LUQ_RI/globalwatershed.shp')

# Add the uniqueID the shape corresponds to LUQ remove any unneeded layers in the polygon
luq.mpr.v2 <- luq.mpr %>%
  mutate(uniqueID = "LUQ_MPR") %>%
  dplyr::select(-Name)
luq.q1.v2 <- luq.q1 %>%
  mutate(uniqueID = "LUQ_Q1") %>%
  dplyr::select(-Name)
luq.q2.v2 <- luq.q2 %>%
  mutate(uniqueID = "LUQ_Q2") %>%
  dplyr::select(-Name)
luq.q3.v2 <- luq.q3 %>%
  mutate(uniqueID = "LUQ_Q3") %>%
  dplyr::select(-Name)
luq.qs.v2 <- luq.qs %>%
  mutate(uniqueID = "LUQ_QS") %>%
  dplyr::select(-Name)
luq.ri.v2 <- luq.ri %>%
  mutate(uniqueID = "LUQ_RI") %>%
  dplyr::select(-Name)

# Combine them
luq.all.watersheds <- rbind(luq.mpr.v2, luq.q1.v2, luq.q2.v2,
                            luq.q3.v2, luq.qs.v2, luq.ri.v2)
str(luq.all.watersheds)

# LUQ Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
luq.spatial <- sites %>%
  filter(LTER == "LUQ") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(luq.spatial)
luq.spatial$geometry

# Make an exploratory graph to see if everything looks okay
plot(luq.all.watersheds["uniqueID"], main = "Luquillo Sites", axes = T, reset = F)
plot(luq.spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# Check relevant shapefile geometry
luq.all.watersheds$geometry

# Save out shapefile
st_write(obj = luq.all.watersheds, dsn = "watershed-shapefiles/LUQ_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites.raw', 'sites', 'sites.spatial')))

# Kissimmee River (KRR) ----------------------------------------------------
## Data link: https://www.lake.wateratlas.usf.edu/watershed/?wshedid=1&wbodyatlas=watershed

# Check which stream sites are in this LTER
sites %>%
  filter(LTER == "KRR") %>%
  dplyr::select(uniqueID, lat, long)

# KRR Load Raw Shapefiles --------------------------------------------------

# Read in the shapefile for southern Florida's drainage basins/watersheds
krr.raw <- sf::st_read("watershed-shapefiles/KRR_raw-shapefiles/KRR_Watersheds.shp")
str(krr.raw)

# Simplify it to have just the bare minimum needed stuff
krr.simp <- krr.raw %>%
  dplyr::select(FID, geometry)

# Check Coordinate reference system
sf::st_crs(krr.simp)
krr.simp$geometry
  ## Already in WGS84 so no conversion is necessary!

# KRR Identify Watersheds of Site Coordinates ------------------------------

# Make the relevant subset of the sites object explicitly spatial
krr.spatial <- sites %>%
  filter(LTER == "KRR") %>%
  st_as_sf(coords = c('long', 'lat'), crs = 4326)

# Check that it looks right
str(krr.spatial)
krr.spatial$geometry

# Turn off s2 processing to avoid 'invalid spherical geometry' error
sf::sf_use_s2(use_s2 = F)

# Make an exploratory graph
plot(krr.simp["FID"], main = "Kissimmee Sites", axes = T, reset = F)
plot(krr.spatial["uniqueID"], axes = T, pch = 18, col = 'white', add = T)

# Identify watersheds with at least one site coordinate in them
krr.site.ids <- krr.spatial %>%
  dplyr::mutate(
    ## Same error as LUQ but still important to run this line
    intersection = as.integer(st_intersects(geometry, krr.simp)),
    FID = ifelse(test = !is.na(intersection),
                  yes = krr.simp$FID[intersection],
                  no = '') )

# Diagnose which site(s) have duplicate intersections
krr.spatial %>%
  dplyr::mutate(intersection = st_intersects(geometry, krr.simp))

# Make vector of those numbers
krr.test.vec <- c(2, 4, 25, 28, 41, 45, 59, 77)

# Plot to see what extents these are
plot(subset(krr.simp, FID %in% krr.test.vec), main = "Kissimmee Sites", axes = T, reset = F)
plot(krr.spatial["uniqueID"], axes = T, pch = 18, col = 'white', add = T)

# Plot them one at a time now
  ## 2 appears to be the entire watershed
plot(subset(krr.simp, FID == 2), main = "Kissimmee Sites", axes = T, reset = F)
plot(krr.spatial["uniqueID"], axes = T, pch = 18, col = 'white', add = T)
  ## 4 appears to be the major sub-component of the watershed
plot(subset(krr.simp, FID == 4), main = "Kissimmee Sites", axes = T, reset = F)
plot(krr.spatial["uniqueID"], axes = T, pch = 18, col = 'white', add = T)
  ## the third number then is going to be another division (likely splitting N and S)
plot(subset(krr.simp, FID == 25), main = "Kissimmee Sites", axes = T, reset = F)
plot(krr.spatial["uniqueID"], axes = T, pch = 18, col = 'gray45', add = T)
  ## And I'll leave the fourth number un-plotted but that must be the most specific...
  ## ...and hence is the one we most want

# Try again but this time accounting for the layer nature of 'krr.simp'
krr.site.ids <- krr.spatial %>%
  dplyr::mutate(
    ## Paste together the different IDs
    intersection.list = paste(st_intersects(geometry, krr.simp)),
    ## Snag just the final number (that is the most specific part)
    intersection = as.integer(str_sub(intersection.list,
                           start = nchar(intersection.list) - 3,
                           end = nchar(intersection.list) - 1)),
    FID = ifelse(test = !is.na(intersection),
                 yes = krr.simp$FID[intersection],
                 no = '') ) %>%
  ## And remove intermediary list of intersection points
  dplyr::select(-intersection.list)

# Check that it worked
krr.site.ids

# Finalize the shapefile
krr.final <- krr.simp %>%
  # Strip out the relevant part of the shapefile
  filter(FID %in% krr.site.ids$FID) %>%
  # Bring in the unique ID
  dplyr::mutate(uniqueID = as.character(krr.site.ids$uniqueID[match(FID, krr.site.ids$FID)])) %>%
  # Retain only uniqueID and geometry
  dplyr::select(uniqueID, geometry)

# Check structure
str(krr.final)

# Do a final plot
plot(krr.final["uniqueID"], main = "Kissimmee Sites", axes = T, reset = F)
plot(krr.spatial["uniqueID"], axes = T, pch = 18, col = 'black', add = T)

# KRR Export Shapefiles ----------------------------------------------------

# Check relevant shapefile geometry
krr.final$geometry

# Save out shapefile
st_write(obj = krr.final, dsn = "watershed-shapefiles/KRR_watersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites.raw', 'sites', 'sites.spatial')))

# McMurdo LTER (MCM) -------------------------------------------------------
## Data source: waiting for LTER Information Manager to send shapefiles to me

# Check which stream sites are in this LTER
sites %>%
  filter(LTER == "MCM") %>%
  dplyr::select(uniqueID, lat, long)











# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites.raw', 'sites', 'sites.spatial')))

# Silica Synthesis - Create Global Shapefile -------------------------------

# We may want a single shapefile of all LTERs' shapefiles
## Could be useful for plotting
## And if land cover/lithology rasters are global...
## ...could gather all associated data in one fell swoop

# Remind yourself of the LTERs at play
sort(unique(sites$LTER))

# Begin by reading in each of the LTER's separate polygons
## Each is standardized before export in this script
and.sheds <- sf::st_read('watershed-shapefiles/AND_watersheds.shp')
arc.sheds <- sf::st_read('watershed-shapefiles/ARC_watersheds.shp')
gro.sheds <- sf::st_read('watershed-shapefiles/GRO_watersheds.shp')
hbr.sheds <- sf::st_read('watershed-shapefiles/HBR_watersheds.shp')
krr.sheds <- sf::st_read('watershed-shapefiles/KRR_watersheds.shp')
lmp.sheds <- sf::st_read('watershed-shapefiles/LMP_watersheds.shp')
luq.sheds <- sf::st_read('watershed-shapefiles/LUQ_watersheds.shp')
# mcm.sheds <- sf::st_read('watershed-shapefiles/MCM_watersheds.shp')
nwt.sheds <- sf::st_read('watershed-shapefiles/NWT_watersheds.shp')
sagehen.sheds <- sf::st_read('watershed-shapefiles/Sagehen_watersheds.shp')
umr.sheds <- sf::st_read('watershed-shapefiles/UMR_watersheds.shp')

# Check what kind of geometry each of them has (they should all be "POLYGON")
str(and.sheds)
str(arc.sheds)
str(gro.sheds) # multipolygon
str(hbr.sheds)
str(krr.sheds)
str(lmp.sheds)
str(luq.sheds)
# str(mcm.sheds)
str(nwt.sheds)
str(sagehen.sheds)
str(umr.sheds) # multipolygon

# Simplify all non-polygons into simple polygons
gro.sheds.fix <- gro.sheds %>%
  sf::st_cast("POLYGON")
umr.sheds.fix <- umr.sheds %>%
  sf::st_cast("POLYGON")

# Combine them into a single shapefile
## Same process as combining separate watersheds within LTER as shown earlier
silica.sheds <- rbind(and.sheds, arc.sheds, gro.sheds.fix, hbr.sheds,
                      krr.sheds, lmp.sheds, luq.sheds, #mcm.sheds,
                      nwt.sheds, sagehen.sheds, umr.sheds.fix)

# Check the structure
str(silica.sheds)

# It should include all uniqueIDs
setdiff(unique(sites$uniqueID), unique(silica.sheds$uniqueID))
  ## "character(0)" means all are included
# And should not include any uniqueIDs that aren't in the 'sites' data object
setdiff(unique(silica.sheds$uniqueID), unique(sites$uniqueID))
  ## "character(0)" means no unexpected values/typos

# Do some processing to get this finalized
silica.final <- silica.sheds %>%
  # Create and reposition an LTER column
  dplyr::mutate( LTER = str_sub(uniqueID, 1, 3) ) %>%
  relocate(LTER, .before = everything())

# Check structure
str(silica.final)

# Make an exploratory plot
plot(silica.final["uniqueID"], main = "Silica Synthesis Sites", axes = T)

# Make another grouped by LTER
plot(silica.final["LTER"], main = "Silica Synthesis LTERs", axes = T)

# Check relevant shapefile geometry
silica.final$geometry

# Export the global shapefile
st_write(obj = silica.final, dsn = "watershed-shapefiles/SilicaSynthesis_allWatersheds.shp", delete_layer = T)
# The 'delete_layer' argument allows the code to automatically overwrite old versions

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites.raw', 'sites', 'sites.spatial')))

# Silica Synthesis - Save Tidy 'Sites' Object -------------------------------




# If we need a .csv later, let's save this version so we don't need to re-make it
write.csv(sites, 'tidy_SilicaSites.csv', row.names = F)



# End --------------------------------------------------------------------
