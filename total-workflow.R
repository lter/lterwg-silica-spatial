## ------------------------------------------------------- ##
              # Silica WG - Extract Drivers
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon
## Edited by Sidney A Bush 04/2024

# Revisions as of 4/2024: 
## Added additional regions for each extract- file (evapo, greenup, npp, snowfrac)

# Purpose:
## Re-wrangle the 'artisanal' watersheds and re-extract all drivers
## Useful as a one-stop shop for updating driver data

## ------------------------------------------------------- ##
                    # Housekeeping -----
## ------------------------------------------------------- ##

# Make sure googledrive library is loaded
library(googledrive)

# Authorize Google Drive
## Without this done first `source`ing any of these scripts will fail
googledrive::drive_auth()

## ------------------------------------------------------- ##
                  # Wrangle Watersheds -----
## ------------------------------------------------------- ##
# NOTE: Watershed shapefiles must be manually downloaded from Drive and uploaded to Aurora
## No automated way of accomplishing this task (unfortunately)

# Reupdate the single shapefile with all watersheds
source(file = "wrangle-watersheds.R", echo = T)

## ------------------------------------------------------- ##
                   # Extract Drivers ----
## ------------------------------------------------------- ##
# NOTE: Drivers with "_partial-extracted" folders must have those CSVs deleted
## This script handles that automatically just note that they are deleted

# NOTE no. 2: Arranged from least to most computing time


## ------------------------- ##
          # Soil -----
## ------------------------- ##

# Re-extract (global, static)
source(file = "extract-soil.R", echo = T)

# Do garbage collection to free up memory
gc()

## ------------------------- ##
        # Land Cover -----
## ------------------------- ##

# Re-extract (global, static)
source(file = "extract-landcover.R", echo = T)

# Garbage collection
gc()

## ------------------------- ##
        # Lithology -----
## ------------------------- ##

# Re-extract (global, static)
source(file = "extract-lithology.R", echo = T)

# Garbage collection
gc()

## ------------------------- ##
        # Elevation -----
## ------------------------- ##

# Re-extract (global, static)
source(file = "extract-elevation.R", echo = T)

# Garbage collection
gc()

## ------------------------- ##
        # Permafrost -----
## ------------------------- ##

# Re-extract (global, static(?))
source(file = "extract-permafrost.R", echo = T)

# Garbage collection
gc()

## ------------------------- ##
     # Precipitation -----
## ------------------------- ##

# Re-extract (global, monthly [x43 years])
source(file = "extract-precip.R", echo = T)

# Garbage collection
gc()

## ------------------------- ##
    # Air Temperature -----
## ------------------------- ##

# Re-extract (global, monthly [x74 years])
source(file = "extract-airtemp.R", echo = T)

# Garbage collection
gc()

## ------------------------- ##
          # NPP -----
## ------------------------- ##
# Net Primary Productivity (bounding boxes [x8], annual [x20 years])

# Focal driver *folder* name
focal <- "raw-npp-v061"

# Define folder path
path <- file.path('/', "home", "shares", "lter-si", "si-watershed-extract", 
                  "raw-driver-data", focal, "_partial-extracted")

# Identify partially extracted CSVs from previous run
partials <- dir(path = path)

# Delete them
for(file in sort(unique(partials))){
  
  # Actual deletion step
  unlink(x = file.path(path, file))
  
  # Message
  message("File '", file, "' deleted.") }

# Re-extract from clean slate
source(file = "extract-npp.R", echo = T)

# Garbage collection
gc()

## ------------------------- ##
      # Green-Up Day -----
## ------------------------- ##
# (bounding boxes [x8], annual [x20 years], x2 "cycles" of green up)

# Focal driver *folder* name
focal <- "raw-greenup-v061"

# Define folder path
path <- file.path('/', "home", "shares", "lter-si", "si-watershed-extract", 
                  "raw-driver-data", focal, "_partial-extracted")

# Identify partially extracted CSVs from previous run
partials <- dir(path = path)

# Delete them
for(file in sort(unique(partials))){
  
  # Actual deletion step
  unlink(x = file.path(path, file))
  
  # Message
  message("File '", file, "' deleted.") }

# Re-extract from clean slate
source(file = "extract-greenup.R", echo = T)

# Garbage collection
gc()

## ------------------------- ##
          # ET -----
## ------------------------- ##
# Evapotranspiration (bounding boxes [x8], 8-day [x20 years])

# Focal driver *folder* name
focal <- "raw-evapo-v061"

# Define folder path
path <- file.path('/', "home", "shares", "lter-si", "si-watershed-extract", 
                  "raw-driver-data", focal, "_partial-extracted")

# Identify partially extracted CSVs from previous run
partials <- dir(path = path)

# Delete them
for(file in sort(unique(partials))){
  
  # Actual deletion step
  unlink(x = file.path(path, file))
  
  # Message
  message("File '", file, "' deleted.") }

# Re-extract from clean slate
source(file = "extract-evapo.R", echo = T)

# Garbage collection
gc()

## ------------------------- ##
          # Snow -----
## ------------------------- ##
# Snow Fraction (bounding boxes [x8], 8-day [x20 years])

# Focal driver *folder* name
focal <- "raw-snow-v061"

# Define folder path
path <- file.path('/', "home", "shares", "lter-si", "si-watershed-extract", 
                  "raw-driver-data", focal, "_partial-extracted")

# Identify partially extracted CSVs from previous run
partials <- dir(path = path)

# Delete them
for(file in sort(unique(partials))){
  
  # Actual deletion step
  unlink(x = file.path(path, file))
  
  # Message
  message("File '", file, "' deleted.") }

# Re-extract from clean slate
source(file = "extract-snowfrac.R", echo = T)

# Garbage collection
gc()

## ------------------------------------------------------- ##
                  # Combine Drivers ----
## ------------------------------------------------------- ##

# Combine all extracted drivers
source(file = "combine-drivers.R", echo = T)

# Do garbage collection & clear environment
gc()
rm(list = ls())

# End ----

