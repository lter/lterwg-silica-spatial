## ------------------------------------------------------- ##
                # Hydrosheds  ----
## ------------------------------------------------------- ##

# Build HydroBASINS watersheds for any approved site subset.
#
# Site-specific outlet adjustments are read from an editable TSV; they are not
# embedded in this script.
                
## ------------------------------------------------------- ##
                # Housekeeping -----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, magrittr, googledrive, sf, terra, nngeo)

# Do not clear the session/environment here. This script may be sourced by the
# workflow.

source(file = file.path(getwd(), "tools", "workflow_paths.R"))
source(file = file.path(getwd(), "tools", "subset_and_output_helpers.R"))
# Identify path to location of shared data
(path <- resolve_silica_data_root())

resolve_shared_sitecoord_file <- function(root_path, stem, ext = "shp") {
  candidates <- c(
    file.path(root_path, "site-coordinates"),
    file.path(root_path, "silica-shapefiles", "site-coordinates")
  )
  candidates <- candidates[dir.exists(candidates)]
  if (!length(candidates)) {
    stop("Could not locate shared site-coordinates directory under data root.", call. = FALSE)
  }

  for (dir_path in candidates) {
    hit <- silica_find_existing_output(dir_path, stem, ext)
    if (file.exists(hit)) {
      return(hit)
    }
  }

  stop("Could not locate shared site-coordinate file for stem: ", stem, call. = FALSE)
}

# Shared identifier and subset helpers.
subset_targets <- load_site_subset()
site_coord_dir <- silica_site_coordinates_dir(path)

missing_shapefile_exclusions_path <- Sys.getenv(
  "SILICA_MISSING_SHAPEFILE_LTER_EXCLUSIONS",
  unset = file.path(
    "02_watershed_delineation",
    "config",
    "missing_shapefile_lter_exclusions.tsv"
  )
)
excluded_missing_shp_lter <- load_lter_exclusion_list(
  missing_shapefile_exclusions_path
)
skip_drive_auth <- tolower(Sys.getenv("SILICA_SKIP_DRIVE_AUTH", "false")) == "true"
site_reference_drive_folder_id <- Sys.getenv(
  "SILICA_SITE_REFERENCE_DRIVE_FOLDER_ID",
  unset = ""
)
if (!skip_drive_auth && !nzchar(site_reference_drive_folder_id)) {
  stop(
    "Set SILICA_SITE_REFERENCE_DRIVE_FOLDER_ID or set ",
    "SILICA_SKIP_DRIVE_AUTH=TRUE.",
    call. = FALSE
  )
}
                
## ------------------------------------------------------- ##
          # Reference Table Acquisition ----
## ------------------------------------------------------- ##

if (skip_drive_auth) {
  message("Skipping Drive download of site reference table because SILICA_SKIP_DRIVE_AUTH=TRUE.")
} else {
  googledrive::drive_ls(
    googledrive::as_id(site_reference_drive_folder_id)
  ) %>%
                                dplyr::filter(name == "Site_Reference_Table") %>%
    googledrive::drive_download(file = ., overwrite = T,
                                path = file.path(site_coord_dir,
                                                 "silica-coords_RAW.xlsx"))
}

## ------------------------------------------------------- ##
# Site Coordinate Acquisition ----
## ------------------------------------------------------- ##

# Read in site coordinates (i.e., ref table)
coord_df <- read_silica_site_reference(site_coord_dir) %>%
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

outlet_override_path <- Sys.getenv(
  "SILICA_OUTLET_COORDINATE_OVERRIDES",
  unset = file.path(
    "02_watershed_delineation",
    "config",
    "outlet_coordinate_overrides.tsv"
  )
)
if (file.exists(outlet_override_path)) {
  outlet_overrides <- read.delim(
    outlet_override_path,
    sep = "\t",
    quote = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  assert_required_columns(
    outlet_overrides,
    c("LTER", "Stream_Name", "Longitude", "Latitude", "Reason"),
    label = outlet_override_path
  )
  outlet_overrides <- outlet_overrides %>%
    dplyr::transmute(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_stream_key(Stream_Name),
      .LONGITUDE_OVERRIDE = suppressWarnings(as.numeric(Longitude)),
      .LATITUDE_OVERRIDE = suppressWarnings(as.numeric(Latitude))
    )
  if (anyDuplicated(outlet_overrides[, c(".LTER_KEY", ".STREAM_KEY")])) {
    stop("Outlet override table contains duplicate site keys.", call. = FALSE)
  }
  coord_df <- coord_df %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_stream_key(Stream_Name)
    ) %>%
    dplyr::left_join(
      outlet_overrides,
      by = c(".LTER_KEY", ".STREAM_KEY")
    ) %>%
    dplyr::mutate(
      Longitude = dplyr::coalesce(.LONGITUDE_OVERRIDE, Longitude),
      Latitude = dplyr::coalesce(.LATITUDE_OVERRIDE, Latitude)
    ) %>%
    dplyr::select(
      -.LTER_KEY,
      -.STREAM_KEY,
      -.LONGITUDE_OVERRIDE,
      -.LATITUDE_OVERRIDE
    )
}

if (!is.null(subset_targets) && nrow(subset_targets) > 0) {
  force_vals <- if ("Force_HydroSHEDS" %in% names(subset_targets)) {
    normalize_site_key(subset_targets$Force_HydroSHEDS)
  } else {
    rep(NA_character_, nrow(subset_targets))
  }
  source_vals <- if ("Watershed_Source" %in% names(subset_targets)) {
    normalize_site_key(subset_targets$Watershed_Source)
  } else {
    rep(NA_character_, nrow(subset_targets))
  }

  forced_name_overrides <- subset_targets %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_stream_key(Stream_Name),
      .SUBSET_SHP_OVERRIDE = dplyr::na_if(trimws(as.character(Shapefile_Name)), ""),
      .FORCE_HYDROSHEDS = (!is.na(source_vals) & source_vals == "hydrosheds") |
        (!is.na(force_vals) & force_vals %in% c("true", "t", "1", "yes", "y")),
      .HAS_SUBSET_OVERRIDE = TRUE
    ) %>%
    dplyr::filter(.FORCE_HYDROSHEDS) %>%
    dplyr::select(.LTER_KEY, .STREAM_KEY, .SUBSET_SHP_OVERRIDE, .HAS_SUBSET_OVERRIDE) %>%
    dplyr::distinct(.LTER_KEY, .STREAM_KEY, .keep_all = TRUE)

  if (nrow(forced_name_overrides) > 0) {
    coord_df <- coord_df %>%
      dplyr::mutate(
        .LTER_KEY = normalize_lter_key(LTER),
        .STREAM_KEY = normalize_stream_key(Stream_Name)
      ) %>%
      dplyr::left_join(forced_name_overrides, by = c(".LTER_KEY", ".STREAM_KEY")) %>%
      dplyr::mutate(
        Shapefile_Name = dplyr::if_else(
          !is.na(.HAS_SUBSET_OVERRIDE),
          .SUBSET_SHP_OVERRIDE,
          as.character(Shapefile_Name)
        )
      ) %>%
      dplyr::select(-.LTER_KEY, -.STREAM_KEY, -.SUBSET_SHP_OVERRIDE, -.HAS_SUBSET_OVERRIDE)
  }
}

hydrosheds_targets <- build_hydrosheds_target_keys(
  subset_targets = subset_targets,
  coord_df = coord_df
)

# Glimpse this
dplyr::glimpse(coord_df)
coord_df|> filter(is.na(Shapefile_Name)) |> pull(LTER) |> unique()

# Identify shapefile names that already have artisanal polygons
artisanal_shp_keys <- character(0)
artisanal_path <- silica_sitecoord_existing_file(path, "silica-watersheds_artisanal", "shp")
if (!file.exists(artisanal_path)) {
  artisanal_path <- resolve_shared_sitecoord_file(path, "silica-watersheds_artisanal", "shp")
}
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

if (!is.null(hydrosheds_targets) && ".SHP_KEY" %in% names(hydrosheds_targets)) {
  forced_shp_keys <- hydrosheds_targets %>%
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

resolve_hydrosheds_raw_dir <- function(root_path) {
  env_dir <- Sys.getenv("SILICA_HYDROSHEDS_RAW_DIR", unset = NA_character_)
  candidates <- c(
    env_dir,
    file.path(root_path, "hydrosheds-raw"),
    file.path(root_path, "silica-shapefiles", "hydrosheds-raw"),
    file.path("/", "home", "shares", "lter-si", "si-watershed-extract", "hydrosheds-raw"),
    file.path("/", "home", "shares", "lter-si", "si-watershed-extract", "silica-shapefiles", "hydrosheds-raw")
  )
  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])

  required_files <- c(
    "hybas_ar_lev00_v1c.shp",
    "hybas_as_lev00_v1c.shp",
    "hybas_au_lev00_v1c.shp",
    "hybas_gr_lev00_v1c.shp",
    "hybas_na_lev00_v1c.shp",
    "hybas_sa_lev00_v1c.shp",
    "hybas_si_lev00_v1c.shp",
    "hybas_af_lev00_v1c.shp",
    "hybas_eu_lev00_v1c.shp"
  )

  for (cand in candidates) {
    if (dir.exists(cand) && all(file.exists(file.path(cand, required_files)))) {
      return(normalizePath(cand, mustWork = TRUE))
    }
  }

  stop(
    paste0(
      "Could not locate HydroSHEDS raw shapefiles. Set SILICA_HYDROSHEDS_RAW_DIR to a directory containing:\n- ",
      paste(required_files, collapse = "\n- "),
      "\nChecked:\n- ",
      paste(candidates, collapse = "\n- ")
    ),
    call. = FALSE
  )
}

hydrosheds_raw_dir <- resolve_hydrosheds_raw_dir(path)

# Load in relevant files
arctic <- sf::st_read(file.path(hydrosheds_raw_dir, "hybas_ar_lev00_v1c.shp"))
# xmin: -180       ymin: 51.20833   xmax: -61.09936   ymax: 83.21723
asia <- sf::st_read(file.path(hydrosheds_raw_dir, "hybas_as_lev00_v1c.shp"))
# xmin: 57.60833   ymin: 1.166667   xmax: 150.9215    ymax: 55.9375
oceania <- sf::st_read(file.path(hydrosheds_raw_dir, "hybas_au_lev00_v1c.shp"))
# xmin: 94.97022   ymin: -55.11667  xmax: 180.0006    ymax: 24.30053
greenland <- sf::st_read(file.path(hydrosheds_raw_dir, "hybas_gr_lev00_v1c.shp"))
# xmin: -73.00067  ymin: 59.74167   xmax: -11.34932   ymax: 83.62564
north_am <- sf::st_read(file.path(hydrosheds_raw_dir, "hybas_na_lev00_v1c.shp"))
# xmin: -137.9625  ymin: 5.495833   xmax: -52.61605   ymax: 62.74232
south_am <- sf::st_read(file.path(hydrosheds_raw_dir, "hybas_sa_lev00_v1c.shp"))
# xmin: -92.00068  ymin: -55.9875   xmax: -32.37453   ymax: 14.88273
siberia <- sf::st_read(file.path(hydrosheds_raw_dir, "hybas_si_lev00_v1c.shp"))
# xmin: 58.95833   ymin: 45.5625    xmax: 180         ymax: 81.26735
africa <- sf::st_read(file.path(hydrosheds_raw_dir, "hybas_af_lev00_v1c.shp"))
# xmin: -18.1631   ymin: 54.5381    xmax: -34.8370    ymax: 37.5631
europe <- sf::st_read(file.path(hydrosheds_raw_dir, "hybas_eu_lev00_v1c.shp"))
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

# Keep helper/path functions alive for later subset-aware export logic.

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

normalize_upstream_id_df <- function(df, source_label = "unknown") {
  required_cols <- c("focal_poly", "hybas_id")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "Upstream HydroSHEDS ID table is missing required columns (",
      paste(missing_cols, collapse = ", "),
      ") in ",
      source_label,
      call. = FALSE
    )
  }

  out <- data.frame(
    focal_poly = suppressWarnings(as.numeric(df$focal_poly)),
    hybas_id = suppressWarnings(as.numeric(df$hybas_id))
  )

  bad_focal <- !is.na(df$focal_poly) & trimws(as.character(df$focal_poly)) != "" & is.na(out$focal_poly)
  bad_hybas <- !is.na(df$hybas_id) & trimws(as.character(df$hybas_id)) != "" & is.na(out$hybas_id)

  if (any(bad_focal) || any(bad_hybas)) {
    stop(
      "Failed to coerce HydroSHEDS upstream IDs to numeric in ",
      source_label,
      call. = FALSE
    )
  }

  out
}


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
                                     paste0(focal_poly, "_Upstream_IDs.csv")),
                    stringsAsFactors = FALSE,
                    check.names = FALSE)
  id_df <- normalize_upstream_id_df(
    id_df,
    source_label = paste0("cached upstream ID file for focal poly ", focal_poly)
  )
  
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
    hydro_df <- normalize_upstream_id_df(
      hydro_df,
      source_label = paste0("generated upstream IDs for focal poly ", focal_poly)
    )
    
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

# Keep helper/path functions alive for later subset-aware export logic.

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

## Need hydrosheds to have unique shapefile names (shp_nm). Keep existing names
## from the reference table; otherwise derive stable names from site identity.
make_hydrosheds_slug <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("(^_+|_+$)", "", x)
}

hydrosheds_id <- paste0(
  make_hydrosheds_slug(sites_actual$LTER),
  "_hydrosheds_",
  make_hydrosheds_slug(sites_actual$Stream_Name)
)
duplicate_hydrosheds_id <- duplicated(hydrosheds_id) | duplicated(hydrosheds_id, fromLast = TRUE)
if (any(duplicate_hydrosheds_id, na.rm = TRUE)) {
  hydrosheds_id[duplicate_hydrosheds_id] <- paste0(
    hydrosheds_id[duplicate_hydrosheds_id],
    "_",
    ave(
      seq_along(hydrosheds_id[duplicate_hydrosheds_id]),
      hydrosheds_id[duplicate_hydrosheds_id],
      FUN = seq_along
    )
  )
}

sites_actual <- cbind(sites_actual, "hydrosheds" = hydrosheds_id)
reference_shp_nm <- trimws(as.character(sites_actual$Shapefile_Name))
# Replace legacy numeric labels with stable site-specific HydroSHEDS names.
# MD is the Murray-Darling network.
stale_generated_shp_nm <- grepl(
  "^[A-Za-z0-9]+_hydrosheds_[0-9]+$|^Canada[1-6]$|^MD([7-9]|1[0-8])$",
  reference_shp_nm
)
sites_actual$shp_nm <- dplyr::if_else(
  !is.na(reference_shp_nm) & nchar(reference_shp_nm) > 0 & !stale_generated_shp_nm,
  reference_shp_nm,
  sites_actual$hydrosheds
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
  silica_sitecoord_output_file(path, "silica-watersheds_hydrosheds_subset", "shp")
} else {
  silica_sitecoord_output_file(path, "silica-watersheds_hydrosheds", "shp")
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

# Avoid clearing the caller environment when this script is sourced from
# 03_combine-artisanal-hydrosheds.R.
invisible(gc())

# End ----
