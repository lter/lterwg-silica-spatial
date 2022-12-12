## ------------------------------------------------------- ##
      # Silica WG - Extract Spatial Data - Lithology
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Using the watershed shapefiles created in "id-watershed-polygons.R"
## Extract the following data: LITHOLOGY

## ------------------------------------------------------- ##
                      # Housekeeping ----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, stars, terra, exactextractr, NCEAS/scicomptools, googledrive)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

# Load in site names with lat/longs
sites <- read.csv(file = file.path(path, "site-coordinates", 'silica-coords_ACTUAL.csv'))

# Check it out
dplyr::glimpse(sites)

# Grab the shapefiles the previous script (see PURPOSE section) created
sheds <- sf::st_read(dsn = file.path(path, "site-coordinates", "silica-watersheds.shp"))

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
plot(rocks_raw, reset = F, axes = T)
plot(sheds, add = T, axes = T)

# Strip out rocks from our polygons
rocks_out <- exactextractr::exact_extract(x = rocks_raw, y = sheds,
                                           include_cols = c("river_id")) %>%
  # Above returns a list so switch it to a dataframe
  purrr::map_dfr(dplyr::select, c(river_id, value)) %>%
  # Filter out NAs
  dplyr::filter(!is.na(value))

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
  dplyr::select(rock_code, rock_type)

# Check that worked
dplyr::glimpse(rock_index)

## ------------------------------------------------------- ##
                # Lithology - Summarize ----
## ------------------------------------------------------- ##

# Process the extracted lithology information into a dataframe
rock_v2 <- rocks_out %>%
  # Get value column named more informatively
  dplyr::rename(rock_code = value) %>%
  # Bring over the rock names from the index
  dplyr::left_join(y = rock_index, by = "rock_code") %>%
  # Remove the now-unneeded code column
  dplyr::select(-rock_code) %>%
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
  # Group by focal polygon and rock type
  dplyr::group_by(river_id, rock_type) %>%
  # Count the instances within each rock type
  ## 0.5° degree pixels within the watershed that contain this rock type
  dplyr::summarise(rock_totals = dplyr::n()) %>%
  # Ungroup
  dplyr::ungroup() %>%
  # Remove unwanted data values
  dplyr::filter(!rock_type %in% c("no_data", "water_bodies", "ice_and_glaciers")) %>%
  # Bin rock categories and re-summarise within consolidated categories
  # Group by LTER and uniqueID
  dplyr::group_by(river_id) %>%
  # We'll want the totals as a percent (total pixels is not very intuitive)
  dplyr::mutate(total_shed_pixels = sum(rock_totals, na.rm = T)) %>%
  # Now ungroup
  dplyr::ungroup() %>%
  # And calculate the percent of total for each row
  dplyr::mutate(perc_total = ((rock_totals / total_shed_pixels) * 100)) %>%
  # Remove the two pixel count columns
  dplyr::select(-rock_totals, -total_shed_pixels)

# Now we want to split into two directions
## First: get a version where each rock type is its own column
rock_data_wide <- rock_v2 %>%
  # Pivot to wide format
  tidyr::pivot_wider(names_from = rock_type,
                     values_from = perc_total)

## Second: get the *majority* rock for each watershed
rock_data_major <- rock_v2 %>%
  # Filter to only max of each rock type per uniqueID & LTER
  dplyr::group_by(river_id) %>%
  filter(perc_total == max(perc_total)) %>%
  dplyr::ungroup() %>%
  # Remove the percent total
  dplyr::select(-perc_total) %>%
  # Get the columns into wide format where the column name and value are both whatever the dominant rock was
  tidyr::pivot_wider(names_from = rock_type,
                     values_from = rock_type) %>%
  # Paste all the non-NAs (i.e., the dominant rocks) into a single column
  tidyr::unite(col = major_rock, -river_id, na.rm = T, sep = "; ")

# Now attach the major rocks to the wide format one
rock_data_actual <- rock_data_wide %>%
  dplyr::left_join(y = rock_data_major, by = c("river_id")) %>%
  dplyr::relocate(major_rock, .after = river_id)

# Examine
dplyr::glimpse(rock_data_actual)

## ------------------------------------------------------- ##
                  # Lithology - Export ----
## ------------------------------------------------------- ##

# Let's get ready to export
rock_export <- sites %>%
  # Join the rock data
  dplyr::left_join(y = rock_data_actual, by = c("river_id"))

# Check it out
dplyr::glimpse(rock_export)

# Create folder to export to
dir.create(path = file.path(path, "extracted-data"), showWarnings = F)

# Export the summarized lithology data
write.csv(x = rock_export, na = '', row.names = F,
          file = file.path(path, "extracted-data", "si-extract_lithology.csv"))

# Upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "si-extract_lithology.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1-X0WjsBg-BTS_ows1jj6n_UehSVE9zwU"))

## ------------------------------------------------------- ##
              # Combine Extracted Data ----
## ------------------------------------------------------- ##
# Clear environment
rm(list = setdiff(ls(), c('path', 'sites')))

# List current extracted data
extracted_data <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1-X0WjsBg-BTS_ows1jj6n_UehSVE9zwU")) %>%
  dplyr::filter(name != "all-data_si-extract.csv")

# Make an empty list
data_list <- list()

# Download these files
for(file_name in extracted_data$name){
  
  # Filter to desired filed
  wanted <- extracted_data %>%
    dplyr::filter(name == file_name)
  
  # Download
  googledrive::drive_download(file = googledrive::as_id(wanted$id),
                              path = file.path(path, "extracted-data", file_name),
                              overwrite = T)
  
  # Read the CSV and add to the list
  data_list[[file_name]] <- read.csv(file = file.path(path, "extracted-data", file_name))
  
} # End loop

# Get a duplicate of the 'sites' object
out_df <- sites

# Now loop across remaining elements and left join each
for(k in 1:length(data_list)){
  
  # Add each bit to the existing dataframe
  out_df <- out_df %>%
    # Left join by all non-data columns
    dplyr::left_join(y = data_list[[k]],
                     by = c("LTER", "Stream_Name", "Discharge_File_Name", "drainSqKm", 
                            "river_id", "lat", "long")) %>%
    # Drop duplicated columns
    unique()
}

# Check for dropped rivers (i.e., rows)
## Stream names (chemistry river names)
setdiff(x = unique(sites$Stream_Name), y = unique(out_df$Stream_Name))
setdiff(y = unique(sites$Stream_Name), x = unique(out_df$Stream_Name))
## Discharge file names (discharge river names)
setdiff(x = unique(sites$Discharge_File_Name), y = unique(out_df$Discharge_File_Name))
setdiff(y = unique(sites$Discharge_File_Name), x = unique(out_df$Discharge_File_Name))

# Take a look
dplyr::glimpse(out_df)

# Export this
write.csv(x = out_df, na = '', row.names = F,
          file = file.path(path, "extracted-data", "all-data_si-extract.csv"))

# And upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "all-data_si-extract.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1-X0WjsBg-BTS_ows1jj6n_UehSVE9zwU"))

# End ----
