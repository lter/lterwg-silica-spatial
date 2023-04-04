## ------------------------------------------------------- ##
        # Silica WG - Identify Watershed Shapefiles
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Create/find shapefiles of watershed boundaries around site lat/long points.
## These polygons can be used later as 'cookie cutters' to extract the relevant portion of global climate data rasters

## ------------------------------------------------------- ##
                     # Housekeeping -----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, magrittr, googledrive, sf, terra, nngeo, NCEAS/scicomptools)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

## ------------------------------------------------------- ##
            # Site Coordinate Acquisition ----
## ------------------------------------------------------- ##

# Grab ID of the GoogleSheet with site coordinates
ref_id <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AIPkWhVuXjqFUk9PVA")) %>%
  dplyr::filter(name == "WRTDS_Reference_Table")

# Download ref table (overwriting previous downloads)
googledrive::drive_download(file = googledrive::as_id(ref_id), overwrite = T,
                            path = file.path(path, "site-coordinates",
                                             "silica-coords_RAW.xlsx"))

# Read it in
coord_df <- readxl::read_excel(path = file.path(path, "site-coordinates", "silica-coords_RAW.xlsx"))

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
sites_actual <- sites %>%
  # Quickly address any coordinates that don't intersect with *any* HydroSHEDS (but should)
  ## Only affects one site and that one is very near the ocean in reality
  ## Longitude
  dplyr::mutate(long = dplyr::case_when(
    # Bump a Finnish site a little further inland
    LTER == "Finnish Environmental Institute" &
      Stream_Name %in% c("Site 28208", "Oulujoki 13000") &
      long == 25.4685 ~ 25.5,
    # Otherwise retain old coordinate
    TRUE ~ long)) %>%
  # Make this explicitly spatial
  sf::st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  # Identify HydroSHEDS polygons that intersect with those coordinates
  dplyr::mutate(
    # Find the interaction points as integers
    ixn = as.integer(sf::st_intersects(geometry, basin_simp)),
    # If a given interaction is not NA (meaning it does overlap)...
    HYBAS_ID = ifelse(test = !is.na(ixn),
                      # ...retain the HYBAS_ID of that interaction...
                      yes = basin_simp$HYBAS_ID[ixn],
                      #...if not, retain nothing
                      no = '') )
  
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

# Attach the twelfth Pfafstetter code by matching with HYBAS_ID
sites_actual$PFAF_12 <- basin_needs$PFAF_12[match(sites_actual$HYBAS_ID, 
                                                  basin_needs$HYBAS_ID)]

# Also grab area
sites_actual$SUB_AREA <- basin_needs$SUB_AREA[match(sites_actual$HYBAS_ID,
                                                    basin_needs$HYBAS_ID)]

# Check the object again
dplyr::glimpse(sites_actual)
# This object has polygons defined at the finest possible level
# We may want to visualize aggregated basins so let's go that direction now

# Clean up environment to have less data stored as we move forward
rm(list = setdiff(ls(), c('path', 'sites', 'sites_actual', 'basin_needs', 'coord_df')))

## ------------------------------------------------------- ##
              # Identify Drainage Basins ----
## ------------------------------------------------------- ##

# We can identify a site's drainage basin by:
## 1) Identify focal HydroSHEDS polygon (done already)
## 2) Identifying all HydroSHEDS polygons upstream of that polygon
## 3) Merging the focal polygon and all upstream polygons into a single spatial object

# Load custom functions needed for this operation
source("hydrosheds_custom_fxns.R")

# Identify focal polygons where we've already identified upstream polygon IDs
found_polys <- dir(path = file.path(path, 'hydrosheds-basin-ids'),
                   pattern = "_Upstream_IDs.csv")

# Simplify names
found_ids <- gsub(pattern = "_Upstream_IDs.csv", replacement = "", x = found_polys)

# Create an empty list
id_list <- list()

# Loop across found polygons to read their CSVs
for(focal_poly in found_ids){
  
  # Read CSV
  id_df <- read.csv(file = file.path(path, "hydrosheds-basin-ids",
                                     paste0(focal_poly, "_Upstream_IDs.csv")))
  
  # Add to list
  id_list[[focal_poly]] <- id_df
  
  # Completion message
  message("HYBAS ID '", focal_poly, "' retrieved") }

# Now, we also need to identify upstream polygons for any where we hadn't yet done that

# Identify to-be-identified ones
unk_polys <- setdiff(x = unique(sites_actual$HYBAS_ID),
                     y = as.numeric(found_ids))

# If any are unknown, get them!
if(length(unk_polys) != 0){
  for(focal_poly in unk_polys){
    
    # Print start-up message
    message( "Processing for HYBAS ID '", focal_poly, "' begun at ", Sys.time())
    
    # Assemble file name
    poly_file <- file.path(path, 'hydrosheds-basin-ids',
                           paste0(focal_poly, '_Upstream_IDs.csv'))
    
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
  } 
} else { message("No unidentified upstream polygons.") }

# Unlist the list
hydro_out <- id_list %>%
  purrr::list_rbind()

# Check the structure
dplyr::glimpse(hydro_out)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sites_actual', 'basin_needs', 'hydro_out', 'coord_df')))

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

# Check structure
str(hydro_poly)

# Quick diagnostic check: identify number of HydroSHEDS polygons that get fused into each river's watershed shapefile
poly_ct <- hydro_out %>%
  # Group by focal polygon and count number of unique HYBAS IDs
  dplyr::group_by(focal_poly) %>%
  dplyr::summarize(hydrosheds_polygon_ct = dplyr::n()) %>%
  dplyr::ungroup() %>%
  # Attach drainage area sums from the hydro_poly object and rename that column descriptively
  dplyr::left_join(y = sf::st_drop_geometry(hydro_poly), by = "focal_poly") %>%
  dplyr::rename(hydrosheds_area_km2 = drainSqKm) %>%
  # Rename "focal_poly"
  dplyr::rename(HYBAS_ID = focal_poly) %>%
  # Attach other site information (like more descriptive names)
  dplyr::left_join(y = sites_actual, by = "HYBAS_ID") %>%
  # Pare down columns and reorder remaining ones
  dplyr::select(LTER:Discharge_File_Name, hydrosheds_polygon_ct, hydrosheds_area_km2)

# Glimpse that
dplyr::glimpse(poly_ct)

# Export as a CSV
write.csv(x = poly_ct, file = file.path(path, "hydrosheds_polygon_count.csv"),
          row.names = F, na = '')

# Upload to Drive too
googledrive::drive_upload(media = file.path(path, "hydrosheds_polygon_count.csv"), path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1hrVN2qTzhxbcxe-XZNdRORkPfu4TQaO7"), overwrite = T)

# Get a dataframe version of this + site information for later use
hydro_poly_df <- sites_actual %>%
  # Drop geometry
  sf::st_drop_geometry() %>%
  # Get a column to join by
  dplyr::mutate(focal_poly = as.numeric(HYBAS_ID)) %>%
  # Bind in a geometry-less version of the focal polygon thing
  dplyr::left_join(y = sf::st_drop_geometry(x = hydro_poly), by = "focal_poly") %>%
  # Drop some unneeded columns
  dplyr::select(-ixn, -SUB_AREA, -focal_poly, -dplyr::starts_with("PFAF_")) %>%
  # Relocate area
  dplyr::relocate(drainSqKm, .before = HYBAS_ID) %>%
  # Attach original lat/long coordinates in case needed later
  dplyr::left_join(y = sites, by = c("LTER", "Stream_Name", "Discharge_File_Name"))

# Check it again
dplyr::glimpse(hydro_poly_df)

# Export this for later use as a CSV
write.csv(hydro_poly_df, row.names = F, na = "",
          file = file.path(path, "site-coordinates", 'silica-coords_HydroSHEDS.csv') )

# Plot all of the watersheds
plot(hydro_poly["focal_poly"], axes = T, lab = c(2, 2, 2))

## ------------------------------------------------------- ##
              # Add GRO Streams' Shapefiles ----
## ------------------------------------------------------- ##

# Grab the GRO watershed polygons
gro_sheds <- sf::st_read(file.path(path, 'GRO-shapefiles', 'GRO_watersheds.shp')) %>%
  # Rename the unique ID column to match the HydroSHEDS polygons
  dplyr::rename(focal_poly = uniqueID)

# Need to also rename the geometry attribute
sf::st_geometry(gro_sheds) <- "geom"

# Check structure
dplyr::glimpse(gro_sheds)
dplyr::glimpse(hydro_poly)

# Now let's attach the GRO shapefiles we got elsewhere to the HydroSheds polygons
poly_actual <- hydro_poly %>%
  # First make "focal_poly" into a character
  dplyr::mutate(focal_poly = as.character(focal_poly)) %>%
  # Drop the drainage area
  dplyr::select(-drainSqKm) %>%
  # Now attach the GRO shapefiles
  dplyr::bind_rows(gro_sheds) %>%
  # Let's rename the 'focal_poly' column more informatively
  dplyr::rename(river_id = focal_poly)

# And check structure
dplyr::glimpse(poly_actual)

# Make a plot to make sure it worked
plot(poly_actual["river_id"], axes = T, lab = c(2, 2, 2)) # yep!

# Save out shapefile of all the HydroSheds polygons and the GRO polygons
st_write(obj = poly_actual, delete_layer = T,
         dsn = file.path(path, "site-coordinates", "silica-watersheds.shp"))

# Strip GRO information from coordinate dataframe
gro_coords <- coord_df %>%
  # Filter to only GRO
  dplyr::filter(LTER == "GRO") %>%
  # Pare down to only desired columns
  dplyr::select(LTER, Stream_Name, Discharge_File_Name, drainSqKm,
                lat = Latitude, long = Longitude) %>%
  # Make a "HYBAS_ID" stand-in column
  dplyr::mutate(HYBAS_ID = paste(LTER, Stream_Name, sep = "_"),
                .after = drainSqKm)

# Combine this information with the HydroSHEDS site info already gathered
sites_final <- hydro_poly_df %>%
  # Make HYBAS_ID a character
  dplyr::mutate(HYBAS_ID = as.character(HYBAS_ID)) %>%
  # Bind on GRO coordinates
  dplyr::bind_rows(gro_coords) %>%
  # Rename ID column more informatively
  dplyr::rename(river_id = HYBAS_ID)

# Glimpse it
dplyr::glimpse(sites_final)

# Export for later
write.csv(sites_final, row.names = F, na = "",
          file = file.path(path, "site-coordinates", 'silica-coords_ACTUAL.csv') )

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sites_actual', 'basin_needs', 'hydro_out', 'coord_df', 'poly_actual', 'sites_final')))

## ------------------------------------------------------- ##
                # Crop to Bounding Box ----
## ------------------------------------------------------- ##
# We may want to crop drainage basins to their expert-defined lat/long maxima

# Attach bounding box values to final sites object
bbox_coords <- sites_final %>%
  dplyr::left_join(y = coord_df, by = c("LTER", "Stream_Name", "Discharge_File_Name")) %>%
  # Pare down columns
  dplyr::select(river_id, Min_Long, Max_Long, Min_Lat, Max_Lat) %>%
  # Filter to only rivers with bounding boxes
  dplyr::filter(!is.na(Min_Long) & !is.na(Max_Long) & !is.na(Min_Lat) & !is.na(Max_Lat)) %>%
  # Keep only one row per river_id
  dplyr::distinct()

# Check that out
dplyr::glimpse(bbox_coords)

# Make an empty list
crop_list <- list()

# For each polygon
for(river in unique(bbox_coords$river_id)){
  
  # Filter the bbox_coords object to just one river
  river_bbox_coords <- dplyr::filter(bbox_coords, river_id == river)
  
  # If we have a complete bounding box...
  if(!is.na(river_bbox_coords$Min_Long) & !is.na(river_bbox_coords$Max_Long) & 
     !is.na(river_bbox_coords$Min_Lat) & !is.na(river_bbox_coords$Max_Lat)){
    
    # Strip out the coordinates of the bounding box
    bbox_vec = c("xmin" = river_bbox_coords$Min_Long,
                 "xmax" = river_bbox_coords$Max_Long,
                 "ymin" = river_bbox_coords$Min_Lat,
                 "ymax" = river_bbox_coords$Max_Lat)
    
    # Filter the polygon `sf` object to just this river
    poly_sub <- poly_actual %>%
      dplyr::filter(river_id == river) %>%
      # Now crop using the named vector
      sf::st_crop(y = bbox_vec)
    
    # Add this to the list
    crop_list[[river]] <- poly_sub
    
    # Print success message
    message("River ID '", river, "' cropped to bounding box") 
    
    # If we *do not* have the bounding box...
  } else {
    # If any part of the bounding box is unknown, skip cropping of this watershed
    message("Bounding box for river ID '", river, "' incomplete or entirely missing") 
  } 
}

# Remove all cropped rivers from the uncropped river object
uncropped <- poly_actual %>%
  dplyr::filter(!river_id %in% names(crop_list))

# Exploratory plot to confirm
## Pre-cropping polygon (for the Andrews rivers)
plot(dplyr::filter(poly_actual, river_id == "7000435790")["river_id"], axes = T)
plot(crop_list[["7000435790"]]["river_id"], axes = T)

# Duplicate the uncropped object
final_sf <- uncropped

# For each cropped river:
for(crop_river in names(crop_list)){

  # Attach to larger object containing other rivers
  final_sf %<>%
    dplyr::bind_rows(crop_list[[crop_river]])
  
  # Success message
  message("Added cropped river ", crop_river, " to all site sf object") }

# Exploratory final plot
plot(final_sf, axes = T)

# Save out this object as a shapefile
st_write(obj = poly_actual, delete_layer = T,
         dsn = file.path(path, "site-coordinates", "CROPPED-silica-watersheds.shp"))

## ------------------------------------------------------- ##
                # Assess Shapefile Area ----
## ------------------------------------------------------- ##

# Triple check no rivers are lost
supportR::diff_check(old = unique(poly_actual$river_id),
                     new = unique(final_sf$river_id))

# Empty list
area_list <- list()

# For each river:
for(area_river in unique(poly_actual$river_id)){
  
  # Subset both the uncropped and cropped areas to this river
  sub_uncrop_sf <- dplyr::filter(poly_actual, river_id == area_river)
  sub_crop_sf <- dplyr::filter(final_sf, river_id == area_river)
  
  # Calculate shapefile area (in km2) for both
  sub_uncrop_area <- sub_uncrop_sf %>%
    sf::st_area() %>%
    units::set_units(x = ., km^2)
  sub_crop_area <- sub_crop_sf %>%
    sf::st_area() %>%
    units::set_units(x = ., km^2)

  # Assemble dataframe
  sub_area_df <- data.frame("river_id" = as.character(area_river), 
                            "hydrosheds_area_km2" = as.numeric(sub_uncrop_area),
                            "cropped_area_km2" = as.numeric(sub_crop_area))
  
  # Add to list
  area_list[[area_river]] <- sub_area_df
  
  # Message
  message("Area calculated for river ", area_river) }

# Unlist output
area_actual <- area_list %>%
  purrr::list_rbind(x = .)

# Glimpse it
dplyr::glimpse(area_actual)

# Wrangle a final area checking object for ease of evaluation of this method
glimpse(coord_df)

# Begin with raw reference table object
area_export <- coord_df %>%
  # Drop any rivers that lack coordinates
  dplyr::filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  # Also remove McMurdo because HydroSHEDS doesn't work for those sites
  dplyr::filter(LTER != "MCM") %>%
  # Pare down to only needed columns and rename one more informatively
  dplyr::select(LTER, Stream_Name, Discharge_File_Name, expert_area_km2 = drainSqKm) %>%
  # Bind on the final sites information so we can get the HydroSHEDS river IDs
  dplyr::left_join(sites_final, c("LTER", "Stream_Name", "Discharge_File_Name")) %>%
  # Pare down the columns again
  dplyr::select(LTER, Stream_Name, Discharge_File_Name, river_id, expert_area_km2) %>%
  # Drop duplicate rows
  dplyr::distinct() %>%
  # Attach area information
  dplyr::left_join(area_actual, by = "river_id") %>%
  # Rename a column more intuitively
  dplyr::rename(hydrosheds_id = river_id) %>%
  # Drop duplicate rows (again)
  dplyr::distinct() %>%
  # Collapse duplicate 'chemistry steam names' within 'discharge stream names'
  dplyr::group_by(LTER, Discharge_File_Name, hydrosheds_id, 
                  expert_area_km2, hydrosheds_area_km2, cropped_area_km2) %>%
  dplyr::summarize(Stream_Name = paste(Stream_Name, collapse = " / ")) %>%
  dplyr::ungroup() %>%
  # Move stream name back to where it belongs
  dplyr::relocate(Stream_Name, .after = Discharge_File_Name)

# Check this out
dplyr::glimpse(area_export)

# Export locally
write.csv(area_export, file = file.path(path, "hydrosheds_shape_area_check.csv"),
          row.names = F, na = '')

# Upload to Drive
googledrive::drive_upload(media = file.path(path, "hydrosheds_shape_area_check.csv"), path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1hrVN2qTzhxbcxe-XZNdRORkPfu4TQaO7"), overwrite = T)

# End ----
