## ------------------------------------------------------- ##
      # Silica WG - Extract Spatial Data - Lithology
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Using the watershed shapefiles created in "wrangle-watersheds.R"
## Extract the following data: LITHOLOGY

## ------------------------------------------------------- ##
                      # Housekeeping ----
## ------------------------------------------------------- ##

# Load needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, stars, terra, exactextractr, NCEAS/scicomptools, 
                 googledrive, readxl)

# Clear environment
rm(list = ls())

# Silence `summarize`
options(dplyr.summarise.inform = F)

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

# Load in site names with lat/longs
sites <- readxl::read_excel(path = file.path(path, "site-coordinates",
                                             "silica-coords_RAW.xlsx")) %>%
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
sheds <- sf::st_read(dsn = file.path(path, "site-coordinates", "silica-watersheds.shp")) %>%
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
sheds <- sheds %>% select (-c(Stream_Name.x, Stream_Name.y, expert_area_km2, shape_area_km2, exp_are, hydrshd, real_ar, 
                              Dsc_F_N))

# Check that out
dplyr::glimpse(sheds)


# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

## ------------------------------------------------------- ##
                # Lithology - Extract ----
## ------------------------------------------------------- ##

# Pull in the raw lithology data
rocks_raw <- terra::rast(x = file.path(path, "raw-driver-data", "raw-lithology-data",
                                       "glim_wgs84_0point5deg.txt.asc"))

# Check CRS
crs(rocks_raw)
## Looks good

# Experimental plotting
# plot(rocks_raw, reset = F, axes = T)
# plot(sheds, add = T, axes = T)

# Strip out rocks from our polygons
rocks_out <- exactextractr::exact_extract(x = rocks_raw, y = sheds,
                                           include_cols = c("LTER", "Shapefile_Name")) %>%
  # Above returns a list so switch it to a dataframe
  purrr::map_dfr(dplyr::select, c(LTER, Shapefile_Name, value)) %>%
  # Filter out NAs
  dplyr::filter(!is.na(value)) %>%
  # Count pixels within each river
  dplyr::group_by(LTER, Shapefile_Name, value) %>%
  dplyr::summarise(pixel_ct = dplyr::n()) %>%
  dplyr::ungroup()

# Check that dataframe
dplyr::glimpse(rocks_out)

## ------------------------------------------------------- ##
                # Lithology - Index Prep ----
## ------------------------------------------------------- ##
# Bring in the index tying rock code integers with rock abbreviations
rock_index_raw <- read.table(file = file.path(path, "raw-driver-data", 
                                              "raw-lithology-data", "Classnames.txt"),
                             header = T, sep = ';')

# Fix this index to make it more usable
rock_index <- rock_index_raw %>%
  # Rename the most important columns
  dplyr::rename(rock_code = OBJECTID,
                rock_abbrev = xx) %>%
  # And get a more descriptive version of each of the rock types
  dplyr::mutate(
    rock_type = dplyr::case_when(
      # Abbreviations found here:
      # https://www.clisap.de/fileadmin/B-Research/IA/IA5/LITHOMAP/
      rock_abbrev == 'su' ~ 'unconsolidated_sediments',
      rock_abbrev == 'ss' ~ 'siliciclastic_sedimentary_rocks',
      rock_abbrev == 'sm' ~ 'mixed_sedimentary_rocks',
      rock_abbrev == 'py' ~ 'pyroclastic',
      rock_abbrev == 'sc' ~ 'carbonate_sedimentary_rocks',
      rock_abbrev == 'ev' ~ 'evaporites',
      rock_abbrev == 'mt' ~ 'metamorphic_rocks',
      rock_abbrev == 'pa' ~ 'acid_plutonic_rocks',
      rock_abbrev == 'pi' ~ 'intermediate_plutonic_rocks',
      rock_abbrev == 'pb' ~ 'basic_plutonic_rocks',
      rock_abbrev == 'va' ~ 'acid_volcanic_rocks',
      rock_abbrev == 'vi' ~ 'intermediate_volcanic_rocks',
      rock_abbrev == 'vb' ~ 'basic_volcanic_rocks',
      rock_abbrev == 'ig' ~ 'ice_and_glaciers',
      rock_abbrev == 'wb' ~ 'water_bodies',
      rock_abbrev == 'nd' ~ 'no_data',
      TRUE ~ as.character(rock_abbrev) ) ) %>%
  # Remove unneeded columns
  dplyr::select(value = rock_code, rock_type)

# Check that worked
dplyr::glimpse(rock_index)

## ------------------------------------------------------- ##
                # Lithology - Summarize ----
## ------------------------------------------------------- ##

# Process the extracted lithology information into a dataframe
rock_v2 <- rocks_out %>%
  # Bring over the rock names from the index
  dplyr::left_join(y = rock_index, by = "value") %>%
  # Remove the now-unneeded code column
  dplyr::select(-value) %>%
  # Simplify the imported categories
  dplyr::mutate(
    rock_type = dplyr::case_when(
      rock_type == 'unconsolidated_sediments' ~ 'sedimentary',
      rock_type == 'siliciclastic_sedimentary_rocks' ~ 'sedimentary',
      rock_type == 'mixed_sedimentary_rocks' ~ 'sedimentary',
      rock_type == 'pyroclastic' ~ 'volcanic',
      rock_type == 'carbonate_sedimentary_rocks' ~ 'carbonate_evaporite',
      rock_type == 'evaporites' ~ 'carbonate_evaporite',
      rock_type == 'metamorphic_rocks' ~ 'metamorphic',
      rock_type == 'acid_plutonic_rocks' ~ 'plutonic',
      rock_type == 'intermediate_plutonic_rocks' ~ 'plutonic',
      rock_type == 'basic_plutonic_rocks' ~ 'plutonic',
      rock_type == 'acid_volcanic_rocks' ~ 'volcanic',
      rock_type == 'intermediate_volcanic_rocks' ~ 'volcanic',
      rock_type == 'basic_volcanic_rocks' ~ 'volcanic',
      T ~ as.character(rock_type) ) ) %>%
  # Remove unwanted data values
  dplyr::filter(!rock_type %in% c("no_data", "water_bodies", "ice_and_glaciers")) %>%
  # Bin rock categories and re-summarise within consolidated categories
  dplyr::group_by(LTER, Shapefile_Name, rock_type) %>%
  dplyr::summarize(pixel_ct_v2 = sum(pixel_ct, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # Now count total pixels per watershed
  dplyr::group_by(LTER, Shapefile_Name) %>%
  dplyr::mutate(total_shed_pixels = sum(pixel_ct_v2, na.rm = T)) %>%
  dplyr::ungroup() %>%
  # And calculate the percent of total for each row
  dplyr::mutate(perc_total = ((pixel_ct_v2 / total_shed_pixels) * 100)) %>%
  # Remove the two pixel count columns
  dplyr::select(-pixel_ct_v2, -total_shed_pixels)

# Get wide format version where categories are columns
rock_wide <- rock_v2 %>%
  # Add "rocks" to each category
  dplyr::mutate(rock_type = paste0("rocks_", rock_type)) %>%
  # Now pivot
  tidyr::pivot_wider(names_from = rock_type,
                     values_from = perc_total)

# Now identify "major" (i.e., dominant) rocks
rock_major <- rock_v2 %>%
  # Filter to only max of each rock type per uniqueID & LTER
  dplyr::group_by(LTER, Shapefile_Name) %>%
  dplyr::filter(perc_total == max(perc_total)) %>%
  dplyr::ungroup() %>%
  # Remove the percent total
  dplyr::select(-perc_total) %>%
  # Pivot back to wide format
  tidyr::pivot_wider(names_from = rock_type,
                     values_from = rock_type) %>%
  # Paste all the non-NAs (i.e., the dominant rocks) into a single column
  tidyr::unite(col = major_rock, -LTER, -Shapefile_Name, na.rm = T, sep = "; ")

# Now attach the major rocks to the wide format one
rock_actual <- rock_wide %>%
  dplyr::left_join(y = rock_major, by = c("LTER", "Shapefile_Name")) %>%
  dplyr::relocate(major_rock, .after = Shapefile_Name)

# Examine
dplyr::glimpse(rock_actual)

## ------------------------------------------------------- ##
                  # Lithology - Export ----
## ------------------------------------------------------- ##
# Let's get ready to export
rock_export <- sheds %>%
  # Join the rock data
  dplyr::left_join(y = rock_actual, by = c("LTER", "Shapefile_Name"))%>%
  # this drops the geometry column, which causes issues on export
  sf::st_drop_geometry()  

# Check it out
dplyr::glimpse(rock_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized lithology data
write.csv(x = rock_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_lithology_2.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_lithology_2.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1FBq2-FW6JikgIuGVMX5eyFRB6Axe2Hld"))

# End ----
