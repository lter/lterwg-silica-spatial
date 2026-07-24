# Combine accepted watershed sources into one extraction-ready layer.
#
# Release-aware runs validate and select the watershed version recorded in the
# site-reference table before the sources are combined.

# Read needed libraries
librarian::shelf(tidyverse, magrittr, googledrive, sf, supportR, readxl)

# Do not clear the session/environment here. This script may be sourced by the
# workflow

source(file = file.path(getwd(), "tools", "workflow_paths.R"))
source(file = file.path(getwd(), "tools", "subset_and_output_helpers.R"))
# Identify path to location of shared data
(path <- resolve_silica_data_root())

subset_targets <- load_site_subset()
site_coord_dir <- silica_site_coordinates_dir(path)
canonical_release_mode <- silica_use_canonical_release_library()

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

# Optional rebuilds before combining:
#   SILICA_REBUILD_ARTISANAL=TRUE to regenerate silica-watersheds_artisanal.shp
#   SILICA_REBUILD_HYDROSHEDS=TRUE to regenerate silica-watersheds_hydrosheds.shp
if (toupper(Sys.getenv("SILICA_REBUILD_ARTISANAL", unset = "FALSE")) == "TRUE") {
  source(file.path(getwd(), "02_watershed_delineation", "01_wrangle-artisanal-watersheds.R"))
}

if (!canonical_release_mode &&
    toupper(Sys.getenv("SILICA_REBUILD_HYDROSHEDS", unset = "FALSE")) == "TRUE") {
  source(file.path(getwd(), "02_watershed_delineation", "02_wrangle-hydrosheds.R"))
} else if (canonical_release_mode) {
  message(
    "Canonical release mode uses the validated one-site-per-folder library; ",
    "skipping legacy HydroSHEDS reconstruction."
  )
}

## ------------------------------------------------------- ##
                  # Acquire Shapefiles ----
## ------------------------------------------------------- ##
artisan_full_path <- silica_sitecoord_existing_file(path, "silica-watersheds_artisanal", "shp")
if (!file.exists(artisan_full_path)) {
  artisan_full_path <- resolve_shared_sitecoord_file(path, "silica-watersheds_artisanal", "shp")
}

artisan_subset_path <- silica_sitecoord_existing_file(path, "silica-watersheds_artisanal_subset", "shp")
if (!file.exists(artisan_subset_path)) {
  artisan_subset_path <- NA_character_
}
hydro_full_path <- silica_sitecoord_existing_file(path, "silica-watersheds_hydrosheds", "shp")
hydro_subset_path <- silica_sitecoord_existing_file(path, "silica-watersheds_hydrosheds_subset", "shp")

artisan_path <- if (!is.null(subset_targets) && !is.na(artisan_subset_path) && file.exists(artisan_subset_path)) {
  message("Using subset artisanal shapefile: ", artisan_subset_path)
  artisan_subset_path
} else {
  artisan_full_path
}

artisan <- sf::st_read(artisan_path)
if (!canonical_release_mode) {
  hydro_path <- if (!is.null(subset_targets) && file.exists(hydro_subset_path)) {
    message("Using subset hydrosheds shapefile: ", hydro_subset_path)
    hydro_subset_path
  } else {
    hydro_full_path
  }
  hydro <- sf::st_read(hydro_path)
}

clean_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | is.na(x)] <- NA_character_
  x
}

add_missing_metadata_cols <- function(x) {
  add_chr_col <- function(df, col) {
    if (!col %in% names(df)) {
      df[[col]] <- rep(NA_character_, nrow(df))
    }
    df
  }
  x <- add_chr_col(x, "LTER")
  x <- add_chr_col(x, "shp_nm")
  x <- add_chr_col(x, "Strm_Nm")
  x <- add_chr_col(x, "Dsc_F_N")
  x
}

site_metadata <- read_silica_site_reference(site_coord_dir) %>%
  dplyr::transmute(
    LTER = clean_chr(LTER),
    shp_nm = clean_chr(Shapefile_Name),
    .REF_STRM_NM = clean_chr(Stream_Name),
    .REF_DSC_F_N = clean_chr(Discharge_File_Name)
  ) %>%
  dplyr::filter(!is.na(LTER), !is.na(shp_nm)) %>%
  dplyr::distinct(LTER, shp_nm, .keep_all = TRUE)

input_watersheds <- if (canonical_release_mode) {
  list(add_missing_metadata_cols(artisan))
} else {
  list(
    add_missing_metadata_cols(artisan),
    add_missing_metadata_cols(hydro)
  )
}

all_shps <- dplyr::bind_rows(input_watersheds) %>%
  dplyr::mutate(
    LTER = clean_chr(LTER),
    shp_nm = clean_chr(shp_nm),
    Strm_Nm = clean_chr(Strm_Nm),
    Dsc_F_N = clean_chr(Dsc_F_N)
  ) %>%
  dplyr::left_join(site_metadata, by = c("LTER", "shp_nm")) %>%
  dplyr::mutate(
    Strm_Nm = dplyr::coalesce(Strm_Nm, .REF_STRM_NM),
    Dsc_F_N = dplyr::coalesce(Dsc_F_N, .REF_DSC_F_N)
  ) %>%
  dplyr::select(-.REF_STRM_NM, -.REF_DSC_F_N)
dplyr::glimpse(all_shps)

## ------------------------------------------------------- ##
                    # Export Results ----
## ------------------------------------------------------- ##

# Export the combine shapefile for all rivers
sf::st_write(obj = all_shps, delete_layer = T,
             dsn = silica_sitecoord_output_file(path, "silica-watersheds", "shp"))

# Avoid clearing the caller environment when this script is sourced from
# 03_spatial_extraction/wrappers/run-targeted-subset-workflow.R
invisible(gc())

# End ----
