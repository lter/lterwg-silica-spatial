## ------------------------------------------------------- ##
      # Silica WG - Extract Spatial Data - Permafrost
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Uses the watershed shapefiles built by "02_watershed_delineation/03_combine-artisanal-hydrosheds.R"
## Extract the following data: PERMAFROST

## ------------------------------------------------------- ##
                      # Housekeeping ----
## ------------------------------------------------------- ##

# Read needed libraries

# Do not clear the session/environment here. This script may be sourced by the
# workflow.

source(file.path(getwd(), "tools", "workflow_paths.R"))
load_workflow_packages(c("tidyverse", "sf", "stars", "terra", "exactextractr", "googledrive", "readxl"))

# Silence dplyr grouping message
options(dplyr.summarise.inform = F)

# Identify path to location of shared data
(path <- resolve_silica_data_root())
site_coord_dir <- silica_site_coordinates_dir(path)
raw_driver_dir <- silica_raw_driver_data_dir(path)

# Load in site names with lat/longs
sites <- read_silica_site_reference(site_coord_dir) %>%
  ## Pare down to minimum needed columns
  dplyr::select(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name) %>%
  ## Drop duplicate rows (if any)
  dplyr::distinct() 
# Remove any watersheds without a shapefile
# dplyr::filter(!is.na(Shapefile_Name) &
#                 nchar(Shapefile_Name) != 0 &
#                 !Shapefile_Name %in% c("?", "MISSING"))

# Check it out
dplyr::glimpse(sites)

# Grab the shapefiles the previous script (see PURPOSE section) created
sheds <- sf::st_read(dsn = silica_watershed_file(path)) %>%
  # Expand names to what they were before
  dplyr::rename(Shapefile_Name = shp_nm,
                Stream_Name = Strm_Nm,
                expert_area_km2 = exp_area,
                shape_area_km2 = real_area)

## combine sites and sheds to get ALL sheds (including hydrosheds) and their metadata (from the sites dataframe)
sheds <- sheds %>%
  dplyr::left_join(y = sites, by = c("LTER", "Shapefile_Name"))

sheds$Stream_Name <- ifelse(!is.na(sheds$Stream_Name.x), sheds$Stream_Name.x, sheds$Stream_Name.y)
sheds$Discharge_File_Name <- ifelse(!is.na(sheds$Dsc_F_N), sheds$Dsc_F_N, sheds$Discharge_File_Name)
sheds <- sheds %>%
  dplyr::select(-dplyr::any_of(c(
    "Stream_Name.x", "Stream_Name.y", "expert_area_km2", "shape_area_km2",
    "exp_are", "hydrshd", "real_ar", "Dsc_F_N"
  )))

# Check that out
dplyr::glimpse(sheds)

# Optionally filter to a target site subset (set SILICA_SITE_SUBSET_FILE env var)
source(file = file.path(getwd(), "tools", "subset_and_output_helpers.R"))
subset_targets <- load_site_subset()
subset_data <- filter_to_target_sites(sites = sites, sheds = sheds, subset_targets = subset_targets)
sites <- subset_data$sites
sheds <- subset_data$sheds
merge_subset_outputs <- !is.null(subset_targets) &&
  tolower(Sys.getenv("SILICA_MERGE_SUBSET_OUTPUTS", "false")) == "true"


# Clean up environment
# rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

## ------------------------------------------------------- ##
                # Permafrost - Extract ----
## ------------------------------------------------------- ##

# Read in dataset
pf_raw <- terra::rast(x = file.path(raw_driver_dir, "raw-permafrost",
                                    "perprob.tif"))

# # Exploratory plot for overlap
# plot(pf_raw, axes = T, reset = F)
# plot(sheds, axes = T, add = T)

# Strip out land cover for our polygons
pf_out <- exactextractr::exact_extract(x = pf_raw, y = sheds,
                                         include_cols = c("LTER", "Shapefile_Name")) %>%
  # Above returns a list so switch it to a dataframe
  purrr::map_dfr(dplyr::select, dplyr::everything()) %>%
  # Filter out NAs
  dplyr::filter(!is.na(value))

# Check that dataframe
dplyr::glimpse(pf_out)

## ------------------------------------------------------- ##
                # Permafrost - Summarize ----
## ------------------------------------------------------- ##

# Wrangle extracted data
pf_actual <- pf_out %>%
  # Summarize elevation within river ID
  dplyr::group_by(LTER, Shapefile_Name) %>%
  dplyr::summarize(permafrost_median_m = stats::median(value, na.rm = T),
                   permafrost_mean_m = mean(value, na.rm = T),
                   permafrost_min_m = min(value, na.rm = T),
                   permafrost_max_m = max(value, na.rm = T)) %>%
  dplyr::ungroup()

# Glimpse this
dplyr::glimpse(pf_actual)

## ------------------------------------------------------- ##
                # Permafrost - Export ----
## ------------------------------------------------------- ##
# Let's get ready to export
pf_export <- sheds %>%
  dplyr::left_join(y = pf_actual, by = c("LTER", "Shapefile_Name"))%>%
  # this drops the geometry column, which causes issues on export
  sf::st_drop_geometry()  

# Check it out
dplyr::glimpse(pf_export)

# Create folder to export to
permafrost_out_file <- silica_driver_output_file(path, "si-extract_permafrost")

# Export the summarized elevation and slope data
write_subset_csv(
  df = pf_export,
  output_path = permafrost_out_file,
  key_cols = c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name"),
  subset_targets = subset_targets,
  na = ""
)

# Upload to GoogleDrive
googledrive::drive_upload(media = permafrost_out_file,
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld"))

# End ----
