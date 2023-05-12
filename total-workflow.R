## ------------------------------------------------------- ##
              # Silica WG - Extract Drivers
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Re-wrangle the 'artisanal' watersheds and re-extract all drivers
## Useful as a one-stop shop for updating driver data

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
# NOTE: Drivers with "_partial-extracted" folders must have those CSVs deleted manually
## Otherwise won't actually re-extract those drivers
## This is a feature (not a bug) for when R crashes during extraction

# NOTE no. 2: Arranged from least to most computing time

# Soil
## Global, static
source(file = "extract-soil.R", echo = T)

# Land Cover
## Global, static
source(file = "extract-landcover.R", echo = T)

# Lithology (I.e., rocks)
## Global, static
source(file = "extract-lithology.R", echo = T)

# Elevation
## Global, static
source(file = "extract-elevation.R", echo = T)

# Do garbage collection to free up memory
gc()

# Precipitation (I.e., rainfall)
## Global, monthly (x 43 years)
source(file = "extract-precip.R", echo = T)

# Garbage collection
gc()

# Air Temperature
## Global, monthly (x 74 years)
source(file = "extract-airtemp.R", echo = T)

# Garbage collection
gc()

# Net Primary Productivity
## Bounding boxes (x8), annual (x20 years)
source(file = "extract-npp.R", echo = T)
## Need to delete '_partial-extracted' CSVs!

# Garbage collection
gc()

# Green Up Day
## Bounding boxes (x8), annual (x20 years), x2 "cycles" of green up
source(file = "extract-greenup.R", echo = T)
## Need to delete '_partial-extracted' CSVs!

# Garbage collection
gc()

# Evapotranspiration
## Bounding boxes (x8), 8-day (x20 years)
source(file = "extract-evapo.R", echo = T)
## Need to delete '_partial-extracted' CSVs!

# Garbage collection
gc()

# Snow Fraction
## Bounding boxes (x8), 8-day (x20 years)
source(file = "extract-snowfrac.R", echo = T)
## Need to delete '_partial-extracted' CSVs!

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

