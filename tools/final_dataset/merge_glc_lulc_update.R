librarian::shelf(dplyr, tidyr, readr, stringr)

args <- commandArgs(trailingOnly = TRUE)

# This script needs exactly three file paths: old master, new GLC input, and output.
if (length(args) != 3) {
  stop(
    "Usage: Rscript tools/final_dataset/merge_glc_lulc_update.R <master_lulc_csv> <new_glc_csv> <output_csv>",
    call. = FALSE
  )
}

master_path <- args[[1]]
new_glc_path <- args[[2]]
output_path <- args[[3]]

simple_classes <- c(
  "Bare",
  "Cropland",
  "Filled_Value",
  "Forest",
  "Grassland_Shrubland",
  "Ice_Snow",
  "Impervious",
  "Salt_Water",
  "Tidal_Wetland",
  "Open_water",
  "Wetland_Marsh"
)

glc_class_crosswalk <- tribble(
  ~LC_ID, ~LandClass, ~Simple_Class,
  "0", "Filled_value", "Filled_Value",
  "10", "Rainfed_cropland", "Cropland",
  "11", "Herbaceous_cover_cropland", "Cropland",
  "12", "Tree_or_shrub_cover_cropland", "Cropland",
  "20", "Irrigated_cropland", "Cropland",
  "50", "Evergreen_broadleaved_forest", "Forest",
  "51", "Open_evergreen_broadleaved_forest", "Forest",
  "52", "Closed_evergreen_broadleaved_forest", "Forest",
  "61", "Open_deciduous_broadleaved_forest", "Forest",
  "62", "Closed_deciduous_broadleaved_forest", "Forest",
  "71", "Open_evergreen_needle_leaved_forest", "Forest",
  "72", "Closed_evergreen_needle_leaved_forest", "Forest",
  "81", "Open_deciduous_needle_leaved_forest", "Forest",
  "82", "Closed_deciduous_needle_leaved_forest", "Forest",
  "91", "Open_mixed_leaf_forest", "Forest",
  "92", "Closed_mixed_leaf_forest", "Forest",
  "120", "Shrubland", "Grassland_Shrubland",
  "121", "Evergreen_shrubland", "Grassland_Shrubland",
  "122", "Deciduous_shrubland", "Grassland_Shrubland",
  "130", "Grassland", "Grassland_Shrubland",
  "140", "Lichens_and_mosses", "Bare",
  "150", "Sparse_vegetation", "Grassland_Shrubland",
  "152", "Sparse_shrubland", "Grassland_Shrubland",
  "153", "Sparse_herbaceous", "Grassland_Shrubland",
  "181", "Swamp", "Wetland_Marsh",
  "182", "Marsh", "Wetland_Marsh",
  "183", "Flooded_flat", "Wetland_Marsh",
  "184", "Saline", "Salt_Water",
  "185", "Mangrove", "Tidal_Wetland",
  "186", "Salt_marsh", "Tidal_Wetland",
  "187", "Tidal_flat", "Tidal_Wetland",
  "190", "Impervious_surfaces", "Impervious",
  "200", "Bare_areas", "Bare",
  "201", "Consolidated_bare_areas", "Bare",
  "202", "Unconsolidated_bare_areas", "Bare",
  "210", "Water_body", "Open_water",
  "220", "Permanent_ice_and_snow", "Ice_Snow"
)

landclass_to_simple <- setNames(glc_class_crosswalk$Simple_Class, glc_class_crosswalk$LandClass)
lc_id_to_landclass <- setNames(glc_class_crosswalk$LandClass, glc_class_crosswalk$LC_ID)
lc_id_to_simple <- setNames(glc_class_crosswalk$Simple_Class, glc_class_crosswalk$LC_ID)

# Fill yearly land-cover values between the GLC anchor years.
interp_series <- function(years, values) {
  stopifnot(length(years) == length(values))
  years <- as.integer(years)

  # The early GLC years are five-year anchors; all three are needed to interpolate.
  if (!all(c(1985, 1990, 1995) %in% years)) {
    stop("Expected five-year anchors 1985, 1990, and 1995 in new GLC input.")
  }

  annual_years <- sort(unique(years[years >= 2000]))

  # From 2000 onward, the GLC product should already have every year.
  if (!identical(annual_years, 2000:2022)) {
    stop("Expected annual GLC years 2000:2022 in new GLC input.")
  }

  approx(
    x = years,
    y = values,
    xout = 1985:2022,
    method = "linear",
    rule = 2
  )$y
}

master_lulc <- read_csv(master_path, show_col_types = FALSE) %>%
  select(any_of(c("X", "Stream_Name", "Year", "Simple_Class", "LandClass_sum")))

new_glc <- read_csv(new_glc_path, show_col_types = FALSE)

required_cols <- c("Stream_Name", "Year", "LandClass", "Area_m2")
missing_cols <- setdiff(required_cols, names(new_glc))

# Stop early if the new GLC file is missing any columns used below.
if (length(missing_cols) > 0) {
  stop("Missing required columns in new GLC file: ", paste(missing_cols, collapse = ", "))
}

updated_sites <- new_glc %>%
  distinct(Stream_Name) %>%
  arrange(Stream_Name) %>%
  pull(Stream_Name)

message("Updating ", length(updated_sites), " Stream_Name values.")

source_years <- new_glc %>%
  mutate(
    # Some files carry LC_ID instead of a filled LandClass name, so use LC_ID when needed.
    LC_ID = if ("LC_ID" %in% names(.)) as.character(LC_ID) else NA_character_,
    LandClass = if_else(
      (is.na(LandClass) | LandClass == "") & !is.na(LC_ID),
      unname(lc_id_to_landclass[LC_ID]),
      LandClass
    ),
    Simple_Class = coalesce(
      unname(landclass_to_simple[LandClass]),
      unname(lc_id_to_simple[LC_ID])
    )
  )

unmapped_classes <- source_years %>%
  filter(is.na(Simple_Class)) %>%
  distinct(LC_ID, LandClass) %>%
  arrange(LC_ID, LandClass)

# Every class must map to a simple class before areas can be combined.
if (nrow(unmapped_classes) > 0) {
  stop(
    "Unmapped GLC classes remain after applying the full-to-simple crosswalk: ",
    paste(format(as.data.frame(unmapped_classes)), collapse = " "),
    call. = FALSE
  )
}

source_years <- source_years %>%
  filter(!is.na(Simple_Class)) %>%
  group_by(Stream_Name, Year, Simple_Class) %>%
  summarise(Area_m2 = sum(Area_m2, na.rm = TRUE), .groups = "drop") %>%
  group_by(Stream_Name, Year) %>%
  mutate(
    total_area_m2 = sum(Area_m2, na.rm = TRUE),

    # Convert class areas to proportions; zero-area rows stay at zero.
    LandClass_sum = if_else(total_area_m2 > 0, Area_m2 / total_area_m2, 0)
  ) %>%
  ungroup() %>%
  select(Stream_Name, Year, Simple_Class, LandClass_sum) %>%
  complete(
    Stream_Name,
    Year = c(1985, 1990, 1995, 2000:2022),
    Simple_Class = simple_classes,
    fill = list(LandClass_sum = 0)
  )

interpolated_1985_2022 <- source_years %>%
  arrange(Stream_Name, Simple_Class, Year) %>%
  group_by(Stream_Name, Simple_Class) %>%

  # Interpolate each site/class pair separately so proportions stay grouped correctly.
  group_modify(~ tibble(
    Year = 1985:2022,
    LandClass_sum = interp_series(.x$Year, .x$LandClass_sum)
  )) %>%
  ungroup()

pre_1985 <- interpolated_1985_2022 %>%
  filter(Year == 1985) %>%
  select(-Year) %>%
  crossing(Year = 1900:1984) %>%
  select(Stream_Name, Year, Simple_Class, LandClass_sum)

updated_lulc <- bind_rows(pre_1985, interpolated_1985_2022) %>%
  arrange(Stream_Name, Year, Simple_Class)

combined_lulc <- master_lulc %>%
  filter(!Stream_Name %in% updated_sites) %>%
  select(-any_of("X")) %>%
  bind_rows(updated_lulc) %>%
  arrange(Stream_Name, Year, Simple_Class) %>%
  mutate(X = row_number()) %>%
  select(X, Stream_Name, Year, Simple_Class, LandClass_sum)

write_csv(combined_lulc, output_path)

crosswalk_output <- file.path(dirname(output_path), "GLC_FCS30D_full_to_simple_class_translation.csv")
write_csv(glc_class_crosswalk, crosswalk_output)

message("Wrote: ", output_path)
message("Wrote: ", crosswalk_output)
