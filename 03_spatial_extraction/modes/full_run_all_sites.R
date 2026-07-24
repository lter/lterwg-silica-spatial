# Rebuild the configured watershed layer and extract all spatial drivers.
#
# This is the publication-facing full-run mode. Use the targeted modes for
# reviewed subsets or year extensions.

# Make sure googledrive library is loaded
library(googledrive)

canonical_release_mode <- tolower(Sys.getenv(
  "SILICA_USE_CANONICAL_RELEASE_LIBRARY",
  unset = "TRUE"
)) %in% c("1", "true", "yes", "y")

Sys.setenv(
  SILICA_USE_CANONICAL_RELEASE_LIBRARY = if (canonical_release_mode) "TRUE" else "FALSE",
  SILICA_REFERENCE_RELEASE = Sys.getenv("SILICA_REFERENCE_RELEASE", unset = "3"),
  SILICA_REBUILD_ARTISANAL = Sys.getenv("SILICA_REBUILD_ARTISANAL", unset = "TRUE"),
  SILICA_REBUILD_HYDROSHEDS = if (canonical_release_mode) {
    "FALSE"
  } else {
    Sys.getenv("SILICA_REBUILD_HYDROSHEDS", unset = "TRUE")
  }
)

# Optional no-drive mode for non-interactive runs on Aurora.
skip_drive_auth <- canonical_release_mode ||
  tolower(Sys.getenv("SILICA_SKIP_DRIVE_AUTH", "false")) == "true"
if (skip_drive_auth) {
  message(
    "Skipping Drive reference-table download; using the frozen canonical ",
    "release reference."
  )
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

# Fail before any extraction if the selected frozen reference table and
# release-specific watershed bundles disagree.
if (canonical_release_mode) {
  status <- system2(
    "Rscript",
    file.path("tools", "validate_release_reference_library.R")
  )
  if (!identical(status, 0L)) {
    stop("Canonical release-library preflight failed.", call. = FALSE)
  }
}

# Rebuild the combined extraction watershed layer before extracting drivers.
source(file = file.path("02_watershed_delineation", "03_combine-artisanal-hydrosheds.R"), echo = T)

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
source(file = file.path("03_spatial_extraction", "extraction_scripts", "extract-soil.R"), echo = T)

# Do garbage collection to free up memory
gc()

## ------------------------- ##
        # Lithology -----
## ------------------------- ##

# Re-extract (global, static)
source(file = file.path("03_spatial_extraction", "extraction_scripts", "extract-lithology.R"), echo = T)

# Garbage collection
gc()

## ------------------------- ##
        # Elevation -----
## ------------------------- ##

# Re-extract (global, static)
source(file = file.path("03_spatial_extraction", "extraction_scripts", "extract-elevation.R"), echo = T)

# Garbage collection
gc()

## ------------------------- ##
        # Permafrost -----
## ------------------------- ##

# Re-extract (global, static(?))
source(file = file.path("03_spatial_extraction", "extraction_scripts", "extract-permafrost.R"), echo = T)

# Garbage collection
gc()

## ------------------------- ##
     # Precipitation -----
## ------------------------- ##

# Re-extract (global, monthly [x43 years])
source(file = file.path("03_spatial_extraction", "extraction_scripts", "extract-precip.R"), echo = T)

# Garbage collection
gc()

## ------------------------- ##
    # Air Temperature -----
## ------------------------- ##

# Re-extract (global, monthly [x74 years])
source(file = file.path("03_spatial_extraction", "extraction_scripts", "extract-airtemp.R"), echo = T)

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
source(file = file.path("03_spatial_extraction", "extraction_scripts", "extract-npp.R"), echo = T)

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
source(file = file.path("03_spatial_extraction", "extraction_scripts", "extract-greenup.R"), echo = T)

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
source(file = file.path("03_spatial_extraction", "extraction_scripts", "extract-evapo.R"), echo = T)

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
source(file = file.path("03_spatial_extraction", "extraction_scripts", "extract-snowfrac.R"), echo = T)

# Garbage collection
gc()

## ------------------------------------------------------- ##
                  # Combine Drivers ----
## ------------------------------------------------------- ##

# Combine all extracted drivers
combine_full <- tolower(Sys.getenv("SILICA_COMBINE_FULL", "false")) == "true"
if (combine_full) {
  source(file = file.path("04_combine_qaqc", "combine-spatial-data.R"), echo = T)
} else {
  message("Skipping combine step because SILICA_COMBINE_FULL=FALSE.")
}

# Do garbage collection & clear environment
gc()
gc()

# End ----
