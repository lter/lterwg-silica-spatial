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

# source("wrangle-artisanal-watersheds.R")
# source("wrangle-hydrosheds.R")

## ------------------------------------------------------- ##
                  # Acquire Shapefiles ----
## ------------------------------------------------------- ##
artisan <- sf::st_read(file.path(path, "site-coordinates", "silica-watersheds_artisanal.shp"))
hydro <- sf::st_read(file.path(path, "site-coordinates", "silica-watersheds_hydrosheds.shp"))

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
