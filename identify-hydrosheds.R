## ------------------------------------------------------- ##
                 # LTER WG: Silica Synthesis
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon + 

# Purpose:
## Create/find shapefiles of watershed boundaries around site lat/long points. These polygons can be used later as 'cookie cutters' to extract the relevant portion of global climate data rasters

# Housekeeping ----------------------------------------------------------

# Read needed libraries
library(tidyverse); library(sf); library(terra); library(nngeo)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
path <- file.path('/', "home", "shares", "lter-si", "si-watershed-extract")
path

# Site coordinate retrieval and preparation --------------------

# Load in site names with lat/longs
sites_old <- read.csv(file.path(path, "tidy_SilicaSites.csv"))
sites_new <- read.csv(file.path(path, "NewSitesLatLong_8.2.2022.csv"))
sites_finn <- read.csv(file.path(path, "FinnData_LatLongs_8.3.22.csv"))
  
# Do some pre-processing of the new data
sites_v0 <- sites_new %>%
  # Swap spaces/underscores for hyphens in stream site names and domains
  dplyr::mutate(stream = gsub(pattern = " |_", replacement = "-", x = site_fullname),
                domain = gsub(pattern = " |_", replacement = "-", x = domain)) %>%
    # Make a 'uniqueID' column
  dplyr::mutate(uniqueID = paste(domain, stream, sep = "_")) %>%
  # Simplify Sagehen's entry
  dplyr::mutate(uniqueID = ifelse(test = uniqueID == "Sagehen_Sagehen",
                                  yes = "Sagehen",
                                  no = uniqueID)) %>%
  # Pare down to only needed columns
  dplyr::select(domain, stream, uniqueID, latitude, longitude) %>%
  # As a quick check, remove any rows that lack coordinates
  dplyr::filter(!is.na(latitude) & !is.na(longitude)) %>%
  # Drop McMurdo (Antarctica isn't included in Hydrosheds)
  # Also drop GRO (its shapefiles take too long to process in this path so we can just use their special ones used in the 'extract-watershed-info.R' script)
  dplyr::filter(domain != "MCM" & domain != "GRO") %>%
  # Make all character columns into factors
  dplyr::mutate(dplyr::across(domain:uniqueID, factor)) %>%
  # Rename lat/long more simply
  dplyr::rename(long = longitude, lat = latitude)

# Check to see if the new dataframe is missing any sites that the old CSV had
setdiff(x = sites_old$uniqueID, y = sites_v0$uniqueID)
## Any 'GRO_' and 'MCM_' sites *should* be missing because we removed them above

# Want to see which sites are new?
setdiff(x = sites_v0$uniqueID, y = sites_old$uniqueID)

# Standardize Finnish sites' dataframe
sites_finn_rev <- sites_finn %>%
  # Remove double spaces from station names
  dplyr::mutate(Station.name = gsub(pattern = "      |  ", replacement = " ",
                                    x = Station.name)) %>%
  # Fix column names
  dplyr::rename(domain = Station.name, stream = Id, lat = Latitude, long = Longitude) %>%
  # Swap spaces/underscores for hyphens in stream site names and domains
  dplyr::mutate(stream = gsub(pattern = " |_", replacement = "-", x = stream),
                domain = gsub(pattern = " |_", replacement = "-", x = domain)) %>%
  # Make a 'uniqueID' column
  dplyr::mutate(uniqueID = paste(domain, stream, sep = "_"), .before = lat) %>%
  # As a quick check, remove any rows that lack coordinates
  dplyr::filter(!is.na(lat) & !is.na(long)) %>%
  # Make all character columns into factors
  dplyr::mutate(dplyr::across(domain:uniqueID, factor))

# Check structure of both
str(sites_finn_rev)
str(sites_v0)

# Combine them
sites <- sites_v0 %>%
  dplyr::bind_rows(sites_finn_rev)

# Check structure
str(sites)
range(sites$lat)
range(sites$long)

# Get an explicitly spatial version
sites_spatial <- sf::st_as_sf(sites, coords = c("long", "lat"), crs = 4326)

# Check it out
str(sites_spatial)

# Load in HydroSHEDS basin delineations ----------------------------------

# See HydroSHEDS website (link below) for download links & tech documentation
## https://www.hydrosheds.org/page/hydrobasins

# Load in relevant files
arctic <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_ar_lev00_v1c.shp"))
# xmin: -180       ymin: 51.20833   xmax: -61.09936   ymax: 83.21723
asia <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_as_lev00_v1c.shp"))
# xmin: 57.60833   ymin: 1.166667   xmax: 150.9215    ymax: 55.9375
oceania <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_au_lev00_v1c.shp"))
# xmin: 94.97022   ymin: -55.11667  xmax: 180.0006    ymax: 24.30053
greenland <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_gr_lev00_v1c.shp"))
# xmin: -73.00067  ymin: 59.74167   xmax: -11.34932   ymax: 83.62564
north_am <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_na_lev00_v1c.shp"))
# xmin: -137.9625  ymin: 5.495833   xmax: -52.61605   ymax: 62.74232
south_am <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_sa_lev00_v1c.shp"))
# xmin: -92.00068  ymin: -55.9875   xmax: -32.37453   ymax: 14.88273
siberia <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_si_lev00_v1c.shp"))
# xmin: 58.95833   ymin: 45.5625    xmax: 180         ymax: 81.26735
africa <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_af_lev00_v1c.shp"))
# xmin: -18.1631   ymin: 54.5381    xmax: -34.8370    ymax: 37.5631
europe <- sf::st_read(file.path(path, "hydrosheds-raw", "hybas_eu_lev00_v1c.shp"))
# xmin: -24.5423   ymin: 69.5545    xmax: 12.5913     ymax: 81.8589

# Antarctica is not supported by this product so we just want everything else
# (the minimum latitude is -55.2° for any of the slices)

# Examine structure of one for greater detail
str(arctic)
# page 6 of the technical documentation contains an attribute table that defines these fields
# PFAF_[number] refers to the level of specificity in the basin delineation
## PFAF_1 = separates continents from one another
## PFAF_# + N = progressively finer separation

# Bind our files into a single (admittedly giant) object
all_basins <- rbind(arctic, asia, oceania, greenland, north_am,
                    south_am, siberia, africa, europe)

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
    # Find the interaction points as integers
    ixn = as.integer(st_intersects(geometry, basin_simp)),
    # If a given interaction is not NA (meaning it does overlap)...
    HYBAS_ID = ifelse(test = !is.na(ixn),
                      # ...retain the HYBAS_ID of that interaction...
                      yes = basin_simp$HYBAS_ID[ixn],
                      #...if not, retain nothing
                      no = '') )

# Check it out
sites_actual

# And to make our lives easier, check out which continents we actually need
sort(unique(stringr::str_sub(sites_actual$HYBAS_ID, 1, 1)))
# 1 = Africa; 2 = Europe; 3 = Siberia; 4 = Asia; 5 = Australia; 6 = South America; 7 = North America; 8 = Arctic (North America); 9 = Greenland 

# Prepare only needed HydroSheds 'continents'
basin_needs <- rbind(europe, north_am, arctic)

# Clean up environment to have less data stored as we move forward
rm(list = setdiff(ls(), c('path', 'sites', 'sites_actual', 'basin_needs')))

# Extract PFAF codes from key polygons  --------------------------------------

# Bring each PFAF code into the sites_actual object by matching with HYBAS_ID
for(i in 1:12) {
  message("Processing Pfafstetter code level ", i)
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
# for (stream_id in "BNZ_C3"){ # Test "loop"
  
  # If the file already exists, skip the processing step
  if (fs::file_exists(file.path(path, 'hydrosheds-basin-ids',
                                paste0(stream_id, '_HYBAS_ID.csv')))) {
    message("HydroSheds IDs for '", stream_id, "' already identified.")
    
    # If the file doesn't yet exist, get it
  } else {
    
  # Identify the focal polygon HYBAS_ID that corresponds to this uniqueID
  focal_poly <- as.character(sites_actual$HYBAS_ID[sites_actual$uniqueID == stream_id])

  # Print start-up message
  message( "Processing for '", stream_id, "' begun at ", Sys.time())
  
  # Identify all upstream shapes
  fxn_out <- find_all_up(HYBAS = basin_needs, HYBAS.ID = focal_poly)
  
  # Make a dataframe of this
  hydro_df <- data.frame(uniqueID = rep(stream_id, (length(fxn_out) + 1)),
                         hybas_id = c(focal_poly, fxn_out))
  
  # Save this out
  write.csv(x = hydro_df,
            file = file.path(path, 'hydrosheds-basin-ids',
                             paste0(stream_id, '_HYBAS_ID.csv')),
            na = '', row.names = F)
   
  # Print success message
  message( "Processing complete for '", stream_id, "' at ", Sys.time()) } }

# Make an empty list and counter set to 1
id_list <- list()
n <- 1

# Now that we have individual .csvs for every watershed, let's read them back in
for (stream_id in unique(sites_actual$uniqueID)) {
# for (stream_id in "BNZ_C3"){ # Test "loop"
    
  # If the file doesn't yet exist, warn the user
  if (fs::file_exists(file.path(path, 'hydrosheds-basin-ids',
                                paste0(stream_id, '_HYBAS_ID.csv'))) == F) {
    message("HydroSheds IDs for '", stream_id, "' NOT identified.")
    
    # If the file *does* exist, get it
  } else {
    
    # Read in the csv being considered
    ids <- read.csv(file.path(path, 'hydrosheds-basin-ids',
                              paste0(stream_id, '_HYBAS_ID.csv')))
    
    # Add it to the list at the nth position
    id_list[[n]] <- ids
    
    # Advance the counter by 1
    n <- n + 1 } }

# Unlist the list
hydro_out <- id_list %>%
  purrr::map_dfr(dplyr::select, dplyr::everything())

# Check the structure
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
  dplyr::rename(HYBAS_ID = hybas_id) %>%
  # Attach everything in the polygon variant
  ## Necessary because of some polygons are found in >1 uniqueID
  dplyr::left_join(basin_needs, by = 'HYBAS_ID') %>%
  # Within uniqueID...
  dplyr::group_by(uniqueID) %>%
  # ...sum sub-polygon areas and combine sub-polygon geometries
  dplyr::summarise(drainSqKm = sum(SUB_AREA, na.rm = TRUE),
                   geometry = sf::st_union(geometry)) %>%
  # Need to make the class officially sf again before continuing
  sf::st_as_sf() %>%
  # Then eliminate any small gaps within those shapes
  # nngeo::st_remove_holes() %>%
  ## Throws error: `Error in tmp[j][[1]] : subscript out of bounds`
  # Retrieve the domain and stream names
  tidyr::separate(col = uniqueID, into = c("domain", "stream"), sep = "_",
                  remove = F, fill = "right")

# Check structure
str(hydro_poly)

# Write this out for later use (though as a dataframe)
hydro_poly_df <- sf::st_drop_geometry(x = hydro_poly)

# Check it again
str(hydro_poly_df)

# Export this for later use as a CSV
write.csv(hydro_poly_df, file = file.path(path, 'watershed_areas.csv'),
          row.names = F, na = "")

# Now upload this as well to the GoogleDrive
# googledrive::drive_upload(media = file.path(path, 'watershed_areas.csv'),
#                           path = as_id("https://drive.google.com/drive/folders/1HQtpWYoq_YQwj_bDNNbv8D-0swi00o_s"))

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
artisanal_sheds <- sf::st_read(file.path(path, 'watershed-shapefiles',
                                         'SilicaSynthesis_allWatersheds.shp'))

# Get just the GRO polygons out of that
gro_sheds <- artisanal_sheds %>%
  dplyr::filter(LTER == "GRO") %>%
  # Rename the LTER column to match the phrasing of the newer sites CSV
  dplyr::rename(domain = LTER)

# Check structure
str(gro_sheds)
str(hydro_poly)

# Now let's attach the GRO shapefiles we got elsewhere to the HydroSheds polygons
poly_actual <- dplyr::bind_rows(hydro_poly, gro_sheds)

# Make a plot to make sure it worked
plot(poly_actual["uniqueID"], axes = T, lab = c(2, 2, 2)) # yep!

# And check structure
str(poly_actual)

# Save out shapefile of all the HydroSheds polygons and the GRO polygons
st_write(obj = poly_actual, dsn = file.path(path, "hydrosheds-shapefiles",
                                            "hydrosheds_watersheds.shp"),
         delete_layer = T)

# End ----
