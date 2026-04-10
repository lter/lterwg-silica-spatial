## ------------------------------------------------------- ##
        # Silica WG - Wrangle Watershed Shapefiles
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon
# Edited by Sidney Bush -- for data release 2
## LATER: will need to add HydroSHEDS sites for: Murray-Darling (Australia) and Canada sites


# Purpose:
## Wrangle "artisanal" watershed shapefiles into a single file to extract driver data
## Artisanal = mixed provenance / provided by WG participants

## ------------------------------------------------------- ##
                      # Housekeeping -----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, magrittr, googledrive, sf, supportR, NCEAS/scicomptools, readxl)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

source(file = "site-subset-helpers.R")
subset_targets <- load_site_subset()

# Optional rebuilds before combining:
#   SILICA_REBUILD_ARTISANAL=TRUE to regenerate silica-watersheds_artisanal.shp
#   SILICA_REBUILD_HYDROSHEDS=TRUE to regenerate silica-watersheds_hydrosheds.shp
if (toupper(Sys.getenv("SILICA_REBUILD_ARTISANAL", unset = "FALSE")) == "TRUE") {
  source("wrangle-artisanal-watersheds.R")
}

if (toupper(Sys.getenv("SILICA_REBUILD_HYDROSHEDS", unset = "FALSE")) == "TRUE") {
  source("wrangle-hydrosheds.R")
}

## ------------------------------------------------------- ##
                  # Acquire Shapefiles ----
## ------------------------------------------------------- ##
artisan_path <- file.path(path, "site-coordinates", "silica-watersheds_artisanal.shp")
hydro_full_path <- file.path(path, "site-coordinates", "silica-watersheds_hydrosheds.shp")
hydro_subset_path <- file.path(path, "site-coordinates", "silica-watersheds_hydrosheds_subset.shp")

hydro_path <- if (!is.null(subset_targets) && file.exists(hydro_subset_path)) {
  message("Using subset hydrosheds shapefile: ", hydro_subset_path)
  hydro_subset_path
} else {
  hydro_full_path
}

artisan <- sf::st_read(artisan_path)
hydro <- sf::st_read(hydro_path)

all_shps <- dplyr::bind_rows(artisan, hydro)
dplyr::glimpse(all_shps)

## ------------------------------------------------------- ##
                    # Export Results ----
## ------------------------------------------------------- ##

# Export the combine shapefile for all rivers
sf::st_write(obj = all_shps, delete_layer = T,
             dsn = file.path(path, "site-coordinates", "silica-watersheds.shp"))

# Tidy up environment
rm(list = ls()); gc()

# End ----
