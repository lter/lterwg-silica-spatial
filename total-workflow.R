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

# Soil
source(file = "extract-soil.R", echo = T)

# Land Cover
source(file = "extract-landcover.R", echo = T)

# Lithology (I.e., rocks)
source(file = "extract-lithology.R", echo = T)

# Elevation
source(file = "extract-elevation.R", echo = T)

# Air Temperature
source(file = "extract-airtemp.R", echo = T)

# Precipitation (I.e., rainfall)
source(file = "extract-precip.R", echo = T)

# Net Primary Productivity
## Need to delete '_partial-extracted' CSVs!
source(file = "extract-npp.R", echo = T)

# Green Up Day
## Need to delete '_partial-extracted' CSVs!
source(file = "extract-greenup.R", echo = T)

# Evapotranspiration
## Need to delete '_partial-extracted' CSVs!
source(file = "extract-evapo.R", echo = T)

# Snow Fraction
## Need to delete '_partial-extracted' CSVs!
source(file = "extract-snowfrac.R", echo = T)

## ------------------------------------------------------- ##
                  # Combine Drivers ----
## ------------------------------------------------------- ##

# Combine all extracted drivers
source(file = "combine-drivers.R", echo = T)

# End ----

