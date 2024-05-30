## ------------------------------------------------------- ##
# Hydrosheds  ----
## ------------------------------------------------------- ##
good_sheds2 <- coord_df %>%
  # Filter to only shapefiles in ref. table & on Aurora
  dplyr::filter(is.na(Shapefile_Name)) %>%
  # Drop any non-unique rows (shouldn't be any but good to double check)
  dplyr::distinct() %>%
  # Condense what remains to ensure no duplicates
  dplyr::group_by(LTER, Stream_Name, Latitude, Longitude) %>%
  dplyr::summarize(expert_area_km2 = mean(expert_area_km2, na.rm = T),
                   Latitude = dplyr::first(Latitude),
                   Longitude = dplyr::first(Longitude)) %>%
  dplyr::ungroup()

# Check that out
dplyr::glimpse(good_sheds2)

source(file.path("deprecated", "hydrosheds_custom_fxns.R"))
