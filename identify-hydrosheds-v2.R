## ------------------------------------------------------- ##
                 # LTER WG: Silica Synthesis
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Create/find shapefiles of watershed boundaries around site lat/long points. These polygons can be used later as 'cookie cutters' to extract the relevant portion of global climate data rasters

## ------------------------------------------------------- ##
                     # Housekeeping -----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, googledrive, sf, terra, nngeo, NCEAS/scicomptools)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

## ------------------------------------------------------- ##
            # Site Coordinate Acquisition ----
## ------------------------------------------------------- ##

# Grab ID of the GoogleSheet with site coordinates
ref_id <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/1/folders/1HQtpWYoq_YQwj_bDNNbv8D-0swi00o_s")) %>%
  dplyr::filter(name == "WRTDS_Reference_Table")

# Download ref table (overwriting previous downloads)
googledrive::drive_download(file = googledrive::as_id(ref_id), overwrite = T,
                            path = file.path(path, "site-coordinates",
                                             "Silica_Coordinates.xlsx"))

# Read it in
coord_df <- readxl::read_excel(path = file.path(path, "site-coordinates", "Silica_Coordinates.xlsx"))

# Glimpse this
dplyr::glimpse(coord_df)

# Do some necessary processing
sites <- coord_df %>%
  # Remove some unneeded columns
  dplyr::select(LTER, Stream_Name, Discharge_File_Name, Latitude, Longitude) %>%
  # Drop missing coordinates
  dplyr::filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  # Remove McMurdo (Antarctica isn't included in Hydrosheds) and GRO (shapefiles too large)
  dplyr::filter(!LTER %in% c("GRO", "MCM")) %>%
  # Rename lat/long more simply
  dplyr::rename(long = Longitude, lat = Latitude) %>%
  # Make all character columns into factors
  dplyr::mutate(dplyr::across(LTER:Discharge_File_Name, factor))
  
# Glimpse it
dplyr::glimpse(sites)

# Check coordinates
range(sites$lat)
range(sites$long)

# Get an explicitly spatial version
sites_spatial <- sf::st_as_sf(sites, coords = c("long", "lat"), crs = 4326)

# Check it out
str(sites_spatial)

## ------------------------------------------------------- ##
          # Load HydroSHEDS Basin Delineations ----
## ------------------------------------------------------- ##

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

## ------------------------------------------------------- ##
      # Identify Site x HydroSHEDS Intersections ----
## ------------------------------------------------------- ##

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
                      no = '') ) %>%
  # Manually address sites that apparently are missing but we are confident in
  dplyr::mutate(HYBAS_ID = dplyr::case_when(
    # This ID is from a *very* close neighboring site (Kiiminkij 13010 4-tie)
    LTER == "Finnish Environmental Institute" & 
      Stream_Name == "Site 28208" ~ "2000029960",
    # This one is slightly further away (Kiiminkij 13010 4-tie)
    LTER == "Finnish Environmental Institute" & 
      Stream_Name == "Oulujoki 13000" ~ "2000029960",
    # Otherwise, keep what we already had
    TRUE ~ HYBAS_ID))
  
# Check it out
dplyr::glimpse(sites_actual)

# Check any sites missing intersections
sites_actual %>%
  dplyr::filter(nchar(stringr::str_sub(HYBAS_ID, 1, 1)) == 0) %>%
  dplyr::select(LTER, Stream_Name)

# And to make our lives easier, check out which continents we actually need
sort(unique(stringr::str_sub(sites_actual$HYBAS_ID, 1, 1)))
# 1 = Africa; 2 = Europe; 3 = Siberia; 4 = Asia;
# 5 = Australia; 6 = South America; 7 = North America
# 8 = Arctic (North America); 9 = Greenland 

# Prepare only needed HydroSheds 'continents'
basin_needs <- rbind(europe, north_am, arctic)

# Bring each PFAF code into the sites_actual object by matching with HYBAS_ID
for(i in 1:12) {
  message("Processing Pfafstetter code level ", i)
  sites_actual[[paste0("PFAF_", i)]] <- basin_needs[[paste0("PFAF_", i)]][match(sites_actual$HYBAS_ID, basin_needs$HYBAS_ID)]
}

# Also grab area
sites_actual$SUB_AREA <- basin_needs$SUB_AREA[match(sites_actual$HYBAS_ID, basin_needs$HYBAS_ID)]

# Check the object again
dplyr::glimpse(sites_actual)
# This object has polygons defined at the finest possible level
# We may want to visualize aggregated basins so let's go that direction now

# Clean up environment to have less data stored as we move forward
rm(list = setdiff(ls(), c('path', 'sites', 'sites_actual', 'basin_needs')))

## ------------------------------------------------------- ##
              # Identify Drainage Basins ----
## ------------------------------------------------------- ##

# We can identify a site's drainage basin by:
## 1) Identify focal HydroSHEDS polygon (done already)
## 2) Identifying all HydroSHEDS polygons upstream of that polygon
## 3) Merging the focal polygon and all upstream polygons into a single spatial object

# Load custom functions needed for this operation
source("hydrosheds_custom_fxns.R")

# Create an empty list
id_list <- list()

# For each focal polygon, identify all upstream polygons
## This is preferable to 'for each stream' because some streams share a focal polygon
## So looping across focal polygons avoids redundancy!
for(focal_poly in unique(sites_actual$HYBAS_ID)){
# for(focal_poly in "7000073120"){
  
  # Create/identify name and path of file
  poly_file <- file.path(path, 'hydrosheds-basin-ids',
                         paste0(focal_poly, '_Upstream_IDs.csv'))
  
  # If we've already found this polygon's upstream polygons:
  if (fs::file_exists(poly_file) == TRUE) {
    
    # Read the CSV
    hydro_df <- read.csv(file = poly_file)
    
    # Add to the list
    id_list[[focal_poly]] <- hydro_df
    
    # Message outcome
    message("Upstream HydroSheds IDs for HYBAS ID '", focal_poly, "' already identified.")
    
    # If we *don't* have the polygon, continue!
  } else {
    
    # Print start-up message
    message( "Processing for HYBAS ID '", focal_poly, "' begun at ", Sys.time())
    
    # Identify all upstream shapes
    fxn_out <- find_all_up(HYBAS = basin_needs, HYBAS.ID = focal_poly)
    
    # Make a dataframe of this
    hydro_df <- data.frame(focal_poly = rep(focal_poly, (length(fxn_out) + 1)),
                           hybas_id = c(focal_poly, fxn_out))
    
    # Save this out
    write.csv(x = hydro_df, file = poly_file, na = '', row.names = F)
    
    # And add it to the list
    id_list[[focal_poly]] <- hydro_df
    
    # Print finishing message
    message( "Processing for HYBAS ID '", focal_poly, "' finished at ", Sys.time())
    
  } # Close `else` clause
} # Close main loop

# Unlist the list
hydro_out <- id_list %>%
  purrr::map_dfr(dplyr::select, dplyr::everything())

# Check the structure
dplyr::glimpse(hydro_out)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sites_actual', 'basin_needs', 'hydro_out')))

## ------------------------------------------------------- ##
            # Wrangle Drainage Basin Object ----
## ------------------------------------------------------- ##

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
  dplyr::group_by(focal_poly) %>%
  # ...sum sub-polygon areas and combine sub-polygon geometries
  dplyr::summarise(drainSqKm = sum(SUB_AREA, na.rm = TRUE),
                   geometry = sf::st_union(geometry)) %>%
  # Need to make the class officially sf again before continuing
  sf::st_as_sf() %>%
  # Then eliminate any small gaps within those shapes
  nngeo::st_remove_holes()
  ## Throws error: `Error in tmp[j][[1]] : subscript out of bounds`

# Check structure
str(hydro_poly)

# Get a dataframe version of this + site information for later use
hydro_poly_df <- sites_actual %>%
  # Drop geometry
  sf::st_drop_geometry() %>%
  # Get a column to join by
  dplyr::mutate(focal_poly = as.numeric(HYBAS_ID)) %>%
  # Bind in a geometry-less version of the focal polygon thing
  dplyr::left_join(y = sf::st_drop_geometry(x = hydro_poly), by = "focal_poly") %>%
  # Drop some unneeded columns
  dplyr::select(-ixn, -SUB_AREA, -focal_poly) %>%
  # Relocate area
  dplyr::relocate(drainSqKm, .before = HYBAS_ID)

# Check it again
dplyr::glimpse(hydro_poly_df)

# Export this for later use as a CSV
write.csv(hydro_poly_df, row.names = F, na = "",
          file = file.path(path, "site-coordinates", 'Silica_Basin_Areas.csv') )

# Experimentally plot subsets of this larger sf object
hydro_poly2 <- hydro_poly %>%
  select(-drainSqKm)

# Plot this object
plot(hydro_poly2["focal_poly"], reset = F, axes = T, lab = c(2, 2, 2))
plot(sites_actual["Stream_Name"], add = T, pch = 15, col = 'gray45')

# Plot all of the watersheds
plot(hydro_poly["focal_poly"], axes = T, lab = c(2, 2, 2))

## ------------------------------------------------------- ##
              # Add GRO Streams' Shapefiles ----
## ------------------------------------------------------- ##

# Grab all of our special single-origin shapefiles
artisanal_sheds <- sf::st_read(file.path(path, 'watershed-shapefiles',
                                         'SilicaSynthesis_allWatersheds.shp'))

# Get just the GRO polygons out of that
gro_sheds <- artisanal_sheds %>%
  dplyr::filter(LTER == "GRO") %>%
  # Drop LTER column
  dplyr::select(-LTER) %>%
  # Rename stream name to match other data object
  dplyr::rename(focal_poly = uniqueID)

# Check structure
dplyr::glimpse(gro_sheds)
dplyr::glimpse(hydro_poly)

# Now let's attach the GRO shapefiles we got elsewhere to the HydroSheds polygons
poly_actual <- hydro_poly %>%
  # First make "focal_poly" into a character
  dplyr::mutate(focal_poly = as.character(focal_poly)) %>%
  # Now attach the GRO shapefiles
  dplyr::bind_rows(gro_sheds)

# Make a plot to make sure it worked
plot(poly_actual["focal_poly"], axes = T, lab = c(2, 2, 2)) # yep!

# And check structure
dplyr::glimpse(poly_actual)

# Save out shapefile of all the HydroSheds polygons and the GRO polygons
st_write(obj = poly_actual, dsn = file.path(path, "site-hydrosheds-shapefiles",
                                            "hydrosheds_watersheds.shp"),
         delete_layer = T)

# End ----
