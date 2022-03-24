## ----------------------------------------------------------------------- ##
                          # LTER WG: Silica Synthesis
## ----------------------------------------------------------------------- ##
# Written by:
## Nick J Lyon + 

# Purpose:
## Identify watershed boundaries for later use in gathering additional data
## Note that step-by-step comments are only included for processing AND LTER
## Other LTERs follow same process so commenting is somewhat reduced

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
    # And LUQ's Q1 and Q2 have identical lat/longs so we'll need to update that
    ## Correct coords from 3/2/22 email from Joanna Carey to Nick Lyon
    lat = as.numeric(ifelse(uniqueID == "LUQ_Q1",
                            yes = 18.315717280000001, no = lat)),
    long = as.numeric(ifelse(uniqueID == "LUQ_Q1",
                             yes = -65.745437745999993, no = long)),
    lat = as.numeric(ifelse(uniqueID == "LUQ_Q2",
                            yes = 18.314636985000000, no = lat)),
    long = as.numeric(ifelse(uniqueID == "LUQ_Q2",
                             yes = -65.746289848000004, no = long)),
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
sites_spatial <- sf::st_as_sf(sites, coords = c("long", "lat"), crs = 4326)
str(sites_spatial)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites_raw', 'sites', 'sites_spatial')))

# Andrews Forest LTER (AND) ----------------------------------------------

# Check what sites are within this LTER
and_pts <- sites %>%
  filter(LTER == "AND") %>%
  dplyr::select(uniqueID, lat, long)

and_pts

# AND Process Raw Shapefiles ----------------------------------------------

# Create an empty list to store the raw shapefiles in
shed_list <- list()

# And create a counter (we'll need it in the for loop for reading data)
j <- 1

# Read in the shapefiles via for loop
for (stream in and_pts$uniqueID) {
  
  # Grab each shapefile (the `str_sub` bit slices out the LTER's name)
  shed <- sf::st_read(paste0('watershed-shapefiles/', str_sub(stream, 1, 3), '_raw-shapefiles/', stream, '/globalwatershed.shp'))
  
  # For the files from StreamStats this is all the processing that we need to do
  shed_v2 <- shed %>%
    # Add the uniqueID explicitly to its watershed
    mutate(uniqueID = stream) %>%
    # Strip out the default name
    dplyr::select(-Name)
  
  # Add the fixed file into a list at the jth position
  shed_list[[j]] <- shed_v2
  
  # Advance the counter by 1
  j <- j + 1 }

# Collapse the list via rbind
and_all_watersheds <- do.call(rbind, shed_list)

# Check it
str(and_all_watersheds)

# AND Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
and_spatial <- and_pts %>%
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
hbr_pts <- sites %>%
  filter(LTER == "HBR") %>%
  dplyr::select(uniqueID, lat, long)

hbr_pts

# HBR Process Raw Shapefiles ----------------------------------------------

# Create an empty list and a counter for the for loop
shed_list <- list()
j <- 1

# Read in the shapefiles via for loop
for (stream in hbr_pts$uniqueID) {
  
  # Grab each shapefile
  shed <- sf::st_read(paste0('watershed-shapefiles/', str_sub(stream, 1, 3), '_raw-shapefiles/', stream, '/globalwatershed.shp'))
  
  # Ready the raw file for combination
  shed_v2 <- shed %>%
    mutate(uniqueID = stream) %>%
    dplyr::select(-Name)
  
  # Add the fixed file into a list at the jth position
  shed_list[[j]] <- shed_v2
  
  # Advance the counter
  j <- j + 1 }

# Collapse the list
hbr_all_watersheds <- do.call(rbind, shed_list)
str(hbr_all_watersheds)

# HBR Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
hbr_spatial <- hbr_pts %>%
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
umr_pts <- sites %>%
  filter(LTER == "UMR") %>%
  dplyr::select(uniqueID, lat, long)
umr_pts

# UMR Process Raw Shapefiles ----------------------------------------------

# Create an empty list and a counter for the for loop
shed_list <- list()
j <- 1

# Read in the shapefiles via for loop
for (stream in umr_pts$uniqueID) {
  
  # Grab each shapefile
  shed <- sf::st_read(paste0('watershed-shapefiles/', str_sub(stream, 1, 3), '_raw-shapefiles/', stream, '/globalwatershed.shp'))
  
  # Ready the raw file for combination
  shed_v2 <- shed %>%
    mutate(uniqueID = stream) %>%
    dplyr::select(-Name)
  
  # Add the fixed file into a list at the jth position
  shed_list[[j]] <- shed_v2
  
  # Advance the counter
  j <- j + 1 }

# Collapse the list
umr_all_watersheds <- do.call(rbind, shed_list)
str(umr_all_watersheds)

# UMR Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
umr_spatial <- umr_pts %>%
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
lmp_pts <- sites %>%
  filter(LTER == "LMP") %>%
  dplyr::select(uniqueID, lat, long)
lmp_pts

# LMP Process Raw Shapefiles -------------------------------------------------

# Create an empty list and a counter for the for loop
shed_list <- list()
j <- 1

# Read in the shapefiles via for loop
for (stream in lmp_pts$uniqueID) {
  
  # Grab each shapefile
  shed <- sf::st_read(paste0('watershed-shapefiles/', str_sub(stream, 1, 3), '_raw-shapefiles/', stream, '/globalwatershed.shp'))
  
  # Ready the raw file for combination
  shed_v2 <- shed %>%
    mutate(uniqueID = stream) %>%
    dplyr::select(-Name)
  
  # Add the fixed file into a list at the jth position
  shed_list[[j]] <- shed_v2
  
  # Advance the counter
  j <- j + 1 }

# Collapse the list
lmp_all_watersheds <- do.call(rbind, shed_list)
str(lmp_all_watersheds)
## This LTER has only a single stream so the for loop is maybe overkill but still better to have a standard mode of reading in objects

# LMP Final Checks & Export --------------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
lmp_spatial <- lmp_pts %>%
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
nwt_pts <- sites %>%
  filter(LTER == "NWT") %>%
  dplyr::select(uniqueID, lat, long)
nwt_pts

# NWT Process Raw Shapefiles ----------------------------------------------

# Create an empty list and a counter for the for loop
shed_list <- list()
j <- 1

# Read in the shapefiles via for loop
for (stream in nwt_pts$uniqueID) {
  
  # Grab each shapefile
  shed <- sf::st_read(paste0('watershed-shapefiles/', str_sub(stream, 1, 3), '_raw-shapefiles/', stream, '/globalwatershed.shp'))
  
  # Ready the raw file for combination
  shed_v2 <- shed %>%
    mutate(uniqueID = stream) %>%
    dplyr::select(-Name)
  
  # Add the fixed file into a list at the jth position
  shed_list[[j]] <- shed_v2
  
  # Advance the counter
  j <- j + 1 }

# Collapse the list
nwt_all_watersheds <- do.call(rbind, shed_list)
str(nwt_all_watersheds)

# NWT Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
nwt_spatial <- nwt_pts %>%
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
  # Ungroup
  ungroup()

# Make an exploratory graph to see if everything looks okay
plot(nwt_shapes["uniqueID"], main = "Niwot Ridge LTER Sites", axes = T, reset = F, lab = c(3, 1, 3), col = rgb(1,0,0, 0.5))
  ## Had to sacrifice unique colors to get transparency (one of the little sheds printed "beneath" the large one and was invisible)
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
sagehen_pts <- sites %>%
  filter(LTER == "Sagehen") %>%
  dplyr::select(uniqueID, lat, long)
sagehen_pts

# Sagehen Process Raw Shapefiles ----------------------------------------------

# Create an empty list and a counter for the for loop
shed_list <- list()
j <- 1

# Read in the shapefiles via for loop
for (stream in sagehen_pts$uniqueID) {
  
  # Grab each shapefile
  shed <- sf::st_read(paste0('watershed-shapefiles/', stream, '_raw-shapefiles/', stream, '/globalwatershed.shp'))
  
  # Ready the raw file for combination
  shed_v2 <- shed %>%
    mutate(uniqueID = stream) %>%
    dplyr::select(-Name)
  
  # Add the fixed file into a list at the jth position
  shed_list[[j]] <- shed_v2
  
  # Advance the counter
  j <- j + 1 }

# Collapse the list
sagehen_all_watersheds <- do.call(rbind, shed_list)
str(sagehen_all_watersheds)

# Sagehen Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
sagehen_spatial <- sagehen_pts %>%
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
luq_pts <- sites %>%
  filter(LTER == "LUQ") %>%
  dplyr::select(uniqueID, lat, long)
luq_pts

# LUQ Process Raw Shapefiles ----------------------------------------------
# Create an empty list and a counter for the for loop
shed_list <- list()
j <- 1

# Read in the shapefiles via for loop
for (stream in luq_pts$uniqueID) {
  
  # Grab each shapefile
  shed <- sf::st_read(paste0('watershed-shapefiles/', str_sub(stream, 1, 3), '_raw-shapefiles/', stream, '/globalwatershed.shp'))
  
  # Ready the raw file for combination
  shed_v2 <- shed %>%
    mutate(uniqueID = stream) %>%
    dplyr::select(-Name)
  
  # Add the fixed file into a list at the jth position
  shed_list[[j]] <- shed_v2
  
  # Advance the counter
  j <- j + 1 }

# Collapse the list
luq_all_watersheds <- do.call(rbind, shed_list)
str(luq_all_watersheds)

# LUQ Final Checks & Export ----------------------------------------------

# Make the relevant subset of the sites object explicitly spatial
luq_spatial <- luq_pts %>%
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
  dplyr::mutate( LTER = ifelse(uniqueID == "Sagehen",
                               yes = "Sagehen",
                               no = str_sub(uniqueID, 1, 3)) ) %>%
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
