## ------------------------------------------------------- ##
                # Hydrosheds  ----
## ------------------------------------------------------- ##

# This script is currently for extracting watersheds for Canada and Murray Darling for Data Release 2
# It can be adapted to add more regions by changing code in line 114 and 174
                
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
          # Reference Table Acquisition ----
## ------------------------------------------------------- ##

# Grab ID of the GoogleSheet with site coordinates
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AIPkWhVuXjqFUk9PVA")) %>%
  dplyr::filter(name == "Site_Reference_Table") %>%
  googledrive::drive_download(file = ., overwrite = T,
                              path = file.path(path, "site-coordinates",
                                               "silica-coords_RAW.xlsx"))

## ------------------------------------------------------- ##
# Site Coordinate Acquisition ----
## ------------------------------------------------------- ##

# Read in site coordinates (i.e., ref table)
coord_df <- readxl::read_excel(path = file.path(path, "site-coordinates",
                                                "silica-coords_RAW.xlsx")) %>%
  ## Pare down to minimum needed columns
  dplyr::select(LTER, Shapefile_Name, Discharge_File_Name, Stream_Name, drainSqKm, Latitude, Longitude, Shapefile_CRS_EPSG) %>%
  ## Drop duplicate rows (if any)
  dplyr::distinct() %>%
  ## Rename some columns
  dplyr::rename(expert_area_km2 = drainSqKm,
                crs_code = Shapefile_CRS_EPSG) |>
  filter(is.na(Shapefile_Name)!=T|
           is.na(Shapefile_Name)==T& !LTER %in% c("ARC","BcCZO", "BNZ", "Cameroon", "Catalina Jemez", "Coal Creek11", "Finnish Environmental Institute", 
                                                  "HYBAM", "KNZ", "KRR", "LMP", "LUQ", "MCM", "PIE", "Tanguro(Jankowski)", "USGS")
         & (!is.na(Latitude)&!is.na(Longitude)))

# Glimpse this
dplyr::glimpse(coord_df)
coord_df|> filter(is.na(Shapefile_Name)) |> pull(LTER) |> unique()

# Do some necessary processing
good_sheds2 <- coord_df %>%
  # Filter to only shapefiles in ref. table & on Aurora
  dplyr::filter(is.na(Shapefile_Name)) %>%
  # Drop any non-unique rows (shouldn't be any but good to double check)
  dplyr::distinct() %>%
  # Condense what remains to ensure no duplicates
  dplyr::group_by(LTER, Stream_Name, Discharge_File_Name, Latitude, Longitude) %>%
  dplyr::summarize(expert_area_km2 = mean(expert_area_km2, na.rm = T),
                   Latitude = dplyr::first(Latitude),
                   Longitude = dplyr::first(Longitude)) %>%
  dplyr::ungroup()

# Check that out
dplyr::glimpse(good_sheds2)

# Check coordinates
# range(good_sheds2$Latitude)
# range(good_sheds2$Longitude)

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
all_basins <- rbind(africa, arctic, north_am, oceania, south_am)

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
sites_actual <- good_sheds2 %>%
  # Quickly address any coordinates that don't intersect with *any* HydroSHEDS (but should)
  ## Only affects one site and that one is very near the ocean in reality
  ## Longitude
  dplyr::mutate(Longitude = dplyr::case_when(
    # Bump a Finnish site a little further inland
    LTER == "Finnish Environmental Institute" &
      Stream_Name %in% c("Site 28208", "Oulujoki 13000") &
      Longitude == 25.4685 ~ 25.5,
    # Otherwise retain old coordinate
    TRUE ~ Longitude)) %>%
  # Make this explicitly spatial
  sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
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
  dplyr::select(LTER, Stream_Name, Discharge_File_Name)

# And to make our lives easier, check out which continents we actually need
sort(unique(stringr::str_sub(sites_actual$HYBAS_ID, 1, 1)))
# 1 = Africa; 2 = Europe; 3 = Siberia; 4 = Asia;
# 5 = Australia; 6 = South America; 7 = North America
# 8 = Arctic (North America); 9 = Greenland 

# Prepare only needed HydroSheds 'continents'
## Update this for canada/murray darling
basin_needs <- rbind(oceania, north_am, arctic)

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
rm(list = setdiff(ls(), c('path', 'good_sheds2', 'sites_actual', 'basin_needs', 'coord_df')))

## ------------------------------------------------------- ##
# Identify Drainage Basins ----
## ------------------------------------------------------- ##

# We can identify a site's drainage basin by:
## 1) Identify focal HydroSHEDS polygon (done already)
## 2) Identifying all HydroSHEDS polygons upstream of that polygon
## 3) Merging the focal polygon and all upstream polygons into a single spatial object

# Load custom functions needed for this operation
# source("hydrosheds_custom_fxns.R")
source(file.path("tools", "hydrosheds_custom_fxns.R"))


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
rm(list = setdiff(ls(), c('path', 'good_sheds2', 'sites_actual', 'basin_needs', 'hydro_out', 'coord_df')))

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
  dplyr::ungroup() %>%
  # Need to make the class officially sf again before continuing
  sf::st_as_sf()
  
# Check structure
str(hydro_poly)
dplyr::glimpse(hydro_poly)


## Filling of polygon gaps code should go here

## ------------------------------------------------------- ##
# FINAL WRANGLING ----
## ------------------------------------------------------- ##

## Need hydrosheds to have unique shapefile names (shp_nm)
sites_actual <- cbind(sites_actual, "hydrosheds"=1:nrow(sites_actual)) 
sites_actual$shp_nm <-paste0(sites_actual$LTER, sites_actual$hydrosheds)

poly_actual <- sites_actual %>%
  sf::st_drop_geometry() %>%
  rename(focal_poly = HYBAS_ID) %>%
  select(-PFAF_12, -SUB_AREA, -ixn) %>%
  left_join(hydro_poly, by = "focal_poly") %>%
  select(-focal_poly) %>%
  dplyr::rename(exp_area = expert_area_km2,
                real_area = drainSqKm) 
  #mutate(shp_nm = "hydrosheds", .before = dplyr:: everything())

dplyr::glimpse(poly_actual)

# Export the combine shapefile for all rivers
sf::st_write(obj = poly_actual, delete_layer = T,
             dsn = file.path(path, "site-coordinates", "silica-watersheds_hydrosheds.shp"))

## Want to see that shapefile: 
# Path to the shapefile you exported
shapefile_path <- file.path(path, "site-coordinates", "silica-watersheds_hydrosheds.shp")

# Read the shapefile
poly_actual <- sf::st_read(shapefile_path)

# Plot the shapefile
ggplot(data = poly_actual) +
  geom_sf(aes(fill = real_ar)) +  # Color polygons by the real_area column
  scale_fill_viridis_c() +         # Use a color scale for better visualization
  theme_minimal() +                # Clean plot appearance
  labs(title = "HydroSHEDS Watersheds",
       fill = "Drainage Area (sq km)")

## This shapefile is only the Canada and Australia sites. We want ALL sites: 
## ------------------------------------------------------- ##
# Acquire Shapefiles ----
## ------------------------------------------------------- ##
watersheds <- sf::st_read(file.path(path, "site-coordinates", "silica-watersheds.shp"))
hydrosheds <- sf::st_read(file.path(path, "site-coordinates", "silica-watersheds_hydrosheds.shp"))

all_shps <- dplyr::bind_rows(watersheds, hydrosheds)

# Plot the shapefile
ggplot(data = all_shps) +
  geom_sf(aes(fill = real_ar)) +  # Color polygons by the real_area column
  scale_fill_viridis_c() +         # Use a color scale for better visualization
  theme_minimal() +                # Clean plot appearance
  labs(title = "HydroSHEDS Watersheds",
       fill = "Drainage Area (sq km)")


# Export the combine shapefile for all rivers
sf::st_write(obj = all_shps, delete_layer = T,
             dsn = file.path(path, "site-coordinates", "silica-watersheds_hydrosheds_DR_2.shp"))

# Tidy up environment
rm(list = ls()); gc()

# End ----


