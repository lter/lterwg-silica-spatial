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

# Shared key normalization helpers (includes Congo-basin legacy aliases)
source(file = "site-subset-helpers.R")
subset_targets <- load_site_subset()
forced_hydrosheds_targets <- load_forced_hydrosheds_targets(subset_targets)

excluded_missing_shp_lter <- c(
  "ARC", "BcCZO", "BNZ", "Congo Basin", "Catalina Jemez", "Coal Creek11",
  "Finnish Environmental Institute", "HYBAM", "KNZ", "KRR", "LMP", "LUQ",
  "MCM", "PIE", "Tanguro(Jankowski)", "USGS"
)
skip_drive_auth <- tolower(Sys.getenv("SILICA_SKIP_DRIVE_AUTH", "false")) == "true"
                
## ------------------------------------------------------- ##
          # Reference Table Acquisition ----
## ------------------------------------------------------- ##

if (skip_drive_auth) {
  message("Skipping Drive download of site reference table because SILICA_SKIP_DRIVE_AUTH=TRUE.")
} else {
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AIPkWhVuXjqFUk9PVA")) %>%
    dplyr::filter(name == "Site_Reference_Table") %>%
    googledrive::drive_download(file = ., overwrite = T,
                                path = file.path(path, "site-coordinates",
                                                 "silica-coords_RAW.xlsx"))
}

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
                crs_code = Shapefile_CRS_EPSG) %>%
  filter_to_target_records(subset_targets = subset_targets) %>%
  dplyr::mutate(.LTER_KEY = normalize_lter_key(LTER)) |>
  filter(is.na(Shapefile_Name)!=T|
           is.na(Shapefile_Name)==T & (!.LTER_KEY %in% normalize_lter_key(excluded_missing_shp_lter) |
                                         !is.null(subset_targets))
         & (!is.na(Latitude)&!is.na(Longitude))) %>%
  dplyr::select(-.LTER_KEY)

# Glimpse this
dplyr::glimpse(coord_df)
coord_df|> filter(is.na(Shapefile_Name)) |> pull(LTER) |> unique()

# Identify shapefile names that already have artisanal polygons
artisanal_shp_keys <- character(0)
artisanal_path <- file.path(path, "site-coordinates", "silica-watersheds_artisanal.shp")
if (file.exists(artisanal_path)) {
  artisanal <- sf::st_read(artisanal_path, quiet = TRUE)
  if ("shp_nm" %in% names(artisanal)) {
    names(artisanal)[names(artisanal) == "shp_nm"] <- "Shapefile_Name"
  }
  artisanal_shp_keys <- artisanal %>%
    sf::st_drop_geometry() %>%
    dplyr::transmute(.SHP_KEY = normalize_site_key(Shapefile_Name)) %>%
    dplyr::filter(!is.na(.SHP_KEY)) %>%
    dplyr::distinct() %>%
    dplyr::pull(.SHP_KEY)
}

if (!is.null(forced_hydrosheds_targets) && ".SHP_KEY" %in% names(forced_hydrosheds_targets)) {
  forced_shp_keys <- forced_hydrosheds_targets %>%
    dplyr::filter(!is.na(.SHP_KEY)) %>%
    dplyr::pull(.SHP_KEY) %>%
    unique()
  artisanal_shp_keys <- setdiff(artisanal_shp_keys, forced_shp_keys)
}

# Do some necessary processing
good_sheds2 <- coord_df %>%
  # Keep rows with coordinates and without an existing artisanal polygon.
  # This includes rows with missing Shapefile_Name and rows with named shapefiles
  # that are absent from the artisanal watershed layer.
  dplyr::mutate(.SHP_KEY = normalize_site_key(Shapefile_Name)) %>%
  dplyr::filter(!is.na(Latitude), !is.na(Longitude)) %>%
  dplyr::filter(is.na(.SHP_KEY) | !.SHP_KEY %in% artisanal_shp_keys) %>%
  # Drop any non-unique rows (shouldn't be any but good to double check)
  dplyr::distinct() %>%
  # Condense what remains to ensure no duplicates
  dplyr::group_by(LTER, Shapefile_Name, Stream_Name, Discharge_File_Name, Latitude, Longitude) %>%
  dplyr::summarize(expert_area_km2 = mean(expert_area_km2, na.rm = T),
                   Latitude = dplyr::first(Latitude),
                   Longitude = dplyr::first(Longitude),
                   .groups = "drop")

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

basin_lookup <- list(
  "1" = africa,
  "2" = europe,
  "3" = siberia,
  "4" = asia,
  "5" = oceania,
  "6" = south_am,
  "7" = north_am,
  "8" = arctic,
  "9" = greenland
)

# Bind our files into a single (admittedly giant) object when targeted mode is active.
# Otherwise preserve the narrower historical default to avoid widening old runs.
all_basins <- if (is.null(subset_targets)) {
  rbind(africa, arctic, north_am, oceania, south_am)
} else {
  dplyr::bind_rows(basin_lookup)
}

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
needed_codes <- sort(unique(stringr::str_sub(sites_actual$HYBAS_ID, 1, 1)))
needed_codes <- needed_codes[needed_codes %in% names(basin_lookup)]
basin_needs <- dplyr::bind_rows(basin_lookup[needed_codes])

if (nrow(basin_needs) == 0) {
  stop("No HydroSHEDS basin slices were selected for the target sites.", call. = FALSE)
}

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
sites_actual <- cbind(sites_actual, "hydrosheds" = seq_len(nrow(sites_actual)))
sites_actual$shp_nm <- dplyr::if_else(
  !is.na(sites_actual$Shapefile_Name) & nchar(trimws(sites_actual$Shapefile_Name)) > 0,
  sites_actual$Shapefile_Name,
  paste0(sites_actual$LTER, "_hydrosheds_", sites_actual$hydrosheds)
)

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

# Export the combined shapefile for all rivers
if (!exists("subset_targets", inherits = FALSE)) {
  subset_targets <- load_site_subset()
}

subset_hydro_mode <- !is.null(subset_targets) &&
  tolower(Sys.getenv("SILICA_MERGE_SUBSET_OUTPUTS", "false")) == "true"

hydro_out_path <- if (subset_hydro_mode) {
  file.path(path, "site-coordinates", "silica-watersheds_hydrosheds_subset.shp")
} else {
  file.path(path, "site-coordinates", "silica-watersheds_hydrosheds.shp")
}

if (subset_hydro_mode) {
  message("Writing subset-only hydrosheds shapefile: ", hydro_out_path)
} else if (file.exists(hydro_out_path)) {
  existing_hydro <- sf::st_read(hydro_out_path, quiet = TRUE)
  poly_actual <- merge_subset_sf(existing_hydro, poly_actual, key_cols = c("LTER", "shp_nm"))
  message("Merged subset hydrosheds polygons into existing silica-watersheds_hydrosheds.shp")
}

sf::st_write(obj = poly_actual, delete_layer = T,
             dsn = hydro_out_path)

# Tidy up environment
rm(list = ls()); gc()

# End ----
