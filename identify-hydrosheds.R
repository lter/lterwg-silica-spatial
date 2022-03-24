## ----------------------------------------------------------------------- ##
                          # LTER WG: Silica Synthesis
## ----------------------------------------------------------------------- ##
# Written by:
## Nick J Lyon + 

# Purpose:
## Create/find shapefiles of watershed boundaries around site lat/long points
## These polygons can be used later as 'cookie cutters' to extract...
## ...the relevant portion of global climate data rasters

# Housekeeping ----------------------------------------------------------

# Read needed libraries
library(tidyverse); library(sf); library(terra); library(nngeo)

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
sites_full <- read.csv("tidy_SilicaSites.csv")

# Do some needed pre-processing
sites <- sites_full %>%
  # Keep only needed columns
  select(LTER, uniqueID, drainSqKm_original, lat, long) %>%
  # Make uniqueID and LTER factors
  mutate(across(LTER:uniqueID, factor)) %>%
  # Drop McMurdo (Antarctica isn't included in Hydrosheds)
  # Also drop GRO (its shapefiles take too long to process in this path so we can just use their special ones used in the 'extract-watershed-info.R' script)
  filter(LTER != "MCM" & LTER != "GRO")

# Check structure
str(sites)
head(sites)

# Get an explicitly spatial version
sites_spatial <- sf::st_as_sf(sites, coords = c("long", "lat"), crs = 4326)

# Check it out
str(sites_spatial)

# Load in HydroSHEDS basin delineations ----------------------------------

# See HydroSHEDS website (link below) for download links & tech documentation
## https://www.hydrosheds.org/page/hydrobasins

# Load in relevant files
arctic <- sf::st_read("hydrosheds-raw/hybas_ar_lev00_v1c.shp")
# xmin: -180       ymin: 51.20833   xmax: -61.09936   ymax: 83.21723
asia <- sf::st_read("hydrosheds-raw/hybas_as_lev00_v1c.shp")
# xmin: 57.60833   ymin: 1.166667   xmax: 150.9215    ymax: 55.9375
oceania <- sf::st_read("hydrosheds-raw/hybas_au_lev00_v1c.shp")
# xmin: 94.97022   ymin: -55.11667  xmax: 180.0006    ymax: 24.30053
greenland <- sf::st_read("hydrosheds-raw/hybas_gr_lev00_v1c.shp")
# xmin: -73.00067  ymin: 59.74167   xmax: -11.34932   ymax: 83.62564
north_am <- sf::st_read("hydrosheds-raw/hybas_na_lev00_v1c.shp")
# xmin: -137.9625  ymin: 5.495833   xmax: -52.61605   ymax: 62.74232
south_am <- sf::st_read("hydrosheds-raw/hybas_sa_lev00_v1c.shp")
# xmin: -92.00068  ymin: -55.9875   xmax: -32.37453   ymax: 14.88273
siberia <- sf::st_read("hydrosheds-raw/hybas_si_lev00_v1c.shp")
# xmin: 58.95833   ymin: 45.5625    xmax: 180         ymax: 81.26735

# Antarctica is not supported by this product so we just want everything else
# (the minimum latitude is -55.2° for any of the slices)

# Examine structure of one for greater detail
str(arctic)
# page 6 of the technical documentation contains an attribute table that defines these fields
# PFAF_[number] refers to the level of specificity in the basin delineation
## PFAF_1 = separates continents from one another
## PFAF_# + N = progressively finer separation

# Bind our files into a single (admittedly giant) object
all_basins <- rbind(arctic, asia, oceania, greenland, north_am, south_am, siberia)

# For ease of manipulation get just the HYBAS_ID
# These uniquely identify the most specific level so they work upstream too (no pun intended)
basin_simp <- all_basins %>%
  dplyr::select(HYBAS_ID, NEXT_DOWN, NEXT_SINK, SUB_AREA)

# Re-check structure
str(basin_simp)

# Extract IDs at site points --------------------------------------------

# Pre-emptively resolve an error with 'invalid spherical geometry'
sf::sf_use_s2(F)
  ## s2 processing assumes that two points lie on a sphere
  ## earlier form of processing assumes two points lie on a plane

# Pull out HYBAS_IDs at site coordinates
sites_actual <- sites_spatial %>%
  dplyr::mutate(
    ixn = as.integer(st_intersects(geometry, basin_simp)),
    HYBAS_ID = ifelse(test = !is.na(ixn),
                      yes = basin_simp$HYBAS_ID[ixn],
                      no = '') )

# Check it out
sites_actual

# And to make our lives easier, check out which continents we actually need
sort(unique(stringr::str_sub(sites_actual$HYBAS_ID, 1, 1)))
# 1 = Africa; 2 = Europe; 3 = Siberia; 4 = Asia; 5 = Australia; 6 = South America; 7 = North America; 8 = Arctic (North America); 9 = Greenland 

# Prepare only needed HydroSheds 'continents'
basin_needs <- rbind(north_am, arctic)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sites_actual', 'basin_needs')))

# Extract PFAF codes from key polygons  --------------------------------------

# Bring each PFAF code into the sites_actual object by matching with HYBAS_ID
for(i in 1:12) {
  sites_actual[[paste0("PFAF_", i)]] <- basin_needs[[paste0("PFAF_", i)]][match(sites_actual$HYBAS_ID, basin_needs$HYBAS_ID)]
}

# Also grab area
sites_actual$SUB_AREA <- basin_needs$SUB_AREA[match(sites_actual$HYBAS_ID, basin_needs$HYBAS_ID)]

# Check the object again
str(sites_actual)
# This object has polygons defined at the finest possible level
# We may want to visualize aggregated basins so let's go that direction now

# Get custom Hydrosheds functions ------------------------------------------

# These are modified from someone's GitHub functions to accept non-S4 objects
# Link to originals here: https://rdrr.io/github/ECCC-MSC/Basin-Delineation/

# First function finds just the next upstream polygon(s)
find_next_up <- function(HYBAS, HYBAS.ID, ignore.endorheic = F){
  
  # Process sf object into a regular dataframe
  HYBAS_df <- HYBAS %>%
    sf::st_drop_geometry()
  
  # Find next upstream polygon(s) as character
  upstream.ab <- HYBAS_df[HYBAS_df$NEXT_DOWN == HYBAS.ID, c("HYBAS_ID", "ENDO")]
  
  if (ignore.endorheic){
    upstream.ab <- upstream.ab[upstream.ab$ENDO != 2, ]
  }
  return(as.character(upstream.ab$HYBAS_ID))
}

# Second function iteratively runs through the first one to find all of the upstream polygons
find_all_up <- function(HYBAS, HYBAS.ID, ignore.endorheic = F, split = F){
  
  # make containers
  HYBAS.ID <- as.character(HYBAS.ID)
  HYBAS.ID.master <- list()
  
  #Get the possible upstream 'branches'
  direct.upstream <- find_next_up(HYBAS = HYBAS, HYBAS.ID = HYBAS.ID,
                                  ignore.endorheic = ignore.endorheic)
  
  # for each branch iterate upstream until only returning empty results
  for (i in direct.upstream){
    run <- T
    HYBAS.ID.list <- i
    sub.basins <- i # this is the object that gets passed to find_next_up in each iteration
    while (run){
      result.i <- unlist(lapply(sub.basins, find_next_up,
                                HYBAS = HYBAS, ignore.endorheic = ignore.endorheic))
      
      if (length(result.i) == 0){ run <- F } # Stopping criterion
      HYBAS.ID.list <- c(HYBAS.ID.list, result.i)
      sub.basins <- result.i
    }
    HYBAS.ID.master[[i]] <- HYBAS.ID.list
  }
  
  if (!split){ HYBAS.ID.master <- as.character(unlist(HYBAS.ID.master)) }
  
  return(HYBAS.ID.master)
}

# Identify all upstream polygon(s) -----------------------------------------

# For every uniqueID, find all of the upstream polygons
for (stream_id in unique(sites_actual$uniqueID)) {
  
  # If the file already exists, skip the processing step
  if (fs::file_exists(paste0('hydrosheds-basin-ids/', stream_id, '_HYBAS_ID.csv'))) {
    print(paste0("HydroSheds IDs for '", stream_id, "' already identified."))
    
    # If the file doesn't yet exist, get it
  } else {
    
  # Identify the focal polygon HYBAS_ID that corresponds to this uniqueID
  focal_poly <- as.character(sites_actual$HYBAS_ID[sites_actual$uniqueID == stream_id])

  # Print start-up message
  print(paste0( "Processing for '", stream_id, "' begun at ", Sys.time()))
  
  # Identify all upstream shapes
  fxn_out <- find_all_up(HYBAS = basin_needs, HYBAS.ID = focal_poly)
  
  # Make a dataframe of this
  hydro_df <- data.frame(uniqueID = rep(stream_id, (length(fxn_out) + 1)),
                         hybas_id = c(focal_poly, fxn_out))
  
  # Save this out
   write.csv(x = hydro_df,
             file = paste0('hydrosheds-basin-ids/', stream_id, '_HYBAS_ID.csv'),
             na = '', row.names = F)
   
  # Print success message
  print(paste0( "Processing complete for '", stream_id, "' at ", Sys.time())) } }

# Make an empty list and counter set to 1
id_list <- list()
n <- 1

# Now that we have individual .csvs for every watershed, let's read them back in
for (stream_id in unique(sites_actual$uniqueID)) {
  
  # If the file doesn't yet exist, warn the user
  if (fs::file_exists(paste0('hydrosheds-basin-ids/', stream_id, '_HYBAS_ID.csv')) == F) {
    print(paste0("HydroSheds IDs for '", stream_id, "' NOT identified."))
    
    # If the file does exist, get it
  } else {
    
    # Read in the csv being considered
    ids <- read.csv(paste0('hydrosheds-basin-ids/', stream_id, '_HYBAS_ID.csv'))
    
    # Add it to the list at the nth position
    id_list[[n]] <- ids
    
    # Advance the counter by 1
    n <- n + 1 } }

# Go from a list of 2-column dataframes to a single long 2-col dataframe
hydro_out <- do.call(rbind, id_list)
str(hydro_out)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sites_actual',
                          'basin_needs', 'hydro_out')))

# Subset basin object to only needed polygons ---------------------------------

# Pre-emptively resolve an error with 'invalid spherical geometry'
sf::sf_use_s2(F)
  ## s2 processing assumes that two points lie on a sphere
  ## earlier form of processing assumes two points lie on a plane

# Strip the polygons that correspond to those IDs
hydro_poly <- hydro_out %>%
  # Make the HydroBasins ID column have an identical name between the df and sf objects
  rename(HYBAS_ID = hybas_id) %>%
  # Attach everything in the polygon variant
  ## Necessary because of some polygons are found in >1 uniqueID
  left_join(basin_needs, by = 'HYBAS_ID') %>%
  # Within uniqueID...
  group_by(uniqueID) %>%
  # ...sum sub-polygon areas and combine sub-polygon geometries
  summarise(
    drainSqKm = sum(SUB_AREA),
    geometry = sf::st_union(geometry)
  ) %>%
  # Need to make the class officially sf again before continuing
  st_as_sf() %>%
  # Then eliminate any small gaps within those shapes
  nngeo::st_remove_holes() %>%
  # Retrieve the LTER and stream names
  separate(col = uniqueID, into = c("LTER", "stream"), sep = "_", remove = F)
  ## ignore "expected 2 pieces" warning because it refers to "Sagehen"

# Check structure
str(hydro_poly)

# Experimentally plot subsets of this larger sf object
hydro_poly2 <- hydro_poly %>%
  select(-drainSqKm)

# Plot this object
plot(hydro_poly2["uniqueID"], reset = F, axes = T, lab = c(2, 2, 2))
plot(sites_actual["uniqueID"], add = T, pch = 15, col = 'gray45')

# Plot all of the watersheds
plot(hydro_poly["uniqueID"], axes = T, lab = c(2, 2, 2))

# Add on GRO watersheds' files ---------------------------------------------

# Grab all of our special single-origin shapefiles
artisanal_sheds <- sf::st_read('watershed-shapefiles/SilicaSynthesis_allWatersheds.shp')

# Get just the GRO polygons out of that
gro_sheds <- artisanal_sheds %>%
  filter(LTER == "GRO") %>%
  # Reformat to match structure of HydroSheds object
  ## Move uniqueID column
  relocate(uniqueID, .before = LTER) %>%
  # Add blank columns that the hydro_poly has that this one doesn't
  mutate(stream = NA,
         drainSqKm = NA,
         .after = LTER)

# Check structure
str(gro_sheds)
str(hydro_poly)

# Now let's attach the GRO shapefiles we got elsewhere to the HydroSheds polygons
poly_actual <- rbind(hydro_poly, gro_sheds)

# Make a plot to make sure it worked
plot(poly_actual["uniqueID"], axes = T, lab = c(2, 2, 2)) # yep!

# And check structure
str(poly_actual)

# Save out shapefile of all the HydroSheds polygons and the GRO polygons
st_write(obj = poly_actual, dsn = "hydrosheds-shapefiles/hydrosheds_watersheds.shp", delete_layer = T)

# End ----------------------------------------------------------------
