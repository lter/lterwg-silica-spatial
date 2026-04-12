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

# Optional no-drive mode for non-interactive runs on Aurora.
skip_drive_auth <- tolower(Sys.getenv("SILICA_SKIP_DRIVE_AUTH", "false")) == "true"
if (skip_drive_auth) {
  message("Skipping drive_auth because SILICA_SKIP_DRIVE_AUTH=TRUE.")
} else {
  # Authorize Google Drive
  ## Without this done first `source`ing any of these scripts will fail
  googledrive::drive_auth()
}

# Optional upload skip for headless runs.
skip_drive_upload <- tolower(Sys.getenv("SILICA_SKIP_DRIVE_UPLOAD", "false")) == "true"
if (skip_drive_upload) {
  assignInNamespace(
    x = "drive_upload",
    value = function(...) {
      message("Skipping drive_upload because SILICA_SKIP_DRIVE_UPLOAD=TRUE.")
      invisible(NULL)
    },
    ns = "googledrive"
  )
}

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

# Legacy land cover is deprecated; enable only when explicitly requested
run_legacy_landcover <- tolower(Sys.getenv("SILICA_RUN_LEGACY_LANDCOVER", "false")) == "true"
if (run_legacy_landcover) {
  source(file = "extract-landcover.R", echo = T)
} else {
  message("Skipping legacy land cover extraction.")
}

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
use_legacy_combine <- tolower(Sys.getenv("SILICA_USE_LEGACY_COMBINE", "false")) == "true"
if (use_legacy_combine) {
  source(file = "combine-drivers.R", echo = T)
} else {
  source(file = file.path("tools", "combine_from_site_ref_local.R"), echo = T)
}

# Do garbage collection & clear environment
gc()
rm(list = ls())

# End ----

