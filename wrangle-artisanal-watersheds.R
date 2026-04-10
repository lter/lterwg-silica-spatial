## ------------------------------------------------------- ##
# Silica WG - Wrangle Watershed Shapefiles
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon
# Edited by Sidney Bush -- for data release 2
## LATER: will need to add HydroSHEDS sites for: Murray-Darling (Australia) and Canada sites


# Purpose:
## Wrangle "artisanal" watershed shapefiles into a single file to extract driver data
## Artisanal = mixed provenance / provided by WG participants

## ------------------------------------------------------- ##
# Housekeeping -----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, magrittr, googledrive, sf, supportR, NCEAS/scicomptools, readxl)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

# Shared key normalization helpers (includes Congo-basin legacy aliases)
source(file = "site-subset-helpers.R")
subset_targets <- load_site_subset()

excluded_missing_shp_lter <- c(
  "ARC", "BcCZO", "BNZ", "Congo Basin", "Catalina Jemez", "Coal Creek11",
  "Finnish Environmental Institute", "HYBAM", "KNZ", "KRR", "LMP", "LUQ",
  "MCM", "PIE", "Tanguro(Jankowski)", "USGS"
)

# Define the Drive folder for exporting checks / diagnostics to
check_folder <- googledrive::as_id("https://drive.google.com/drive/u/1/folders/1-IawEkFjfkrzAlgolvS1KTTm0pEW9K9h")
skip_drive_auth <- tolower(Sys.getenv("SILICA_SKIP_DRIVE_AUTH", "false")) == "true"
skip_drive_upload <- tolower(Sys.getenv("SILICA_SKIP_DRIVE_UPLOAD", "false")) == "true"


## ------------------------------------------------------- ##
# Reference Table Acquisition ----
## ------------------------------------------------------- ##

if (skip_drive_auth) {
  message("Skipping Drive download of site reference table because SILICA_SKIP_DRIVE_AUTH=TRUE.")
} else {
  googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AIPkWhVuXjqFUk9PVA")) %>%
    dplyr::filter(name == "Site_Reference_Table") %>%
    googledrive::drive_download(file = ., overwrite = T,
                                path = file.path(path, "site-coordinates",
                                                 "silica-coords_RAW.xlsx"))
}

# Read in site coordinates (i.e., ref table)
coord_df <- readxl::read_excel(path = file.path(path, "site-coordinates",
                                                "silica-coords_RAW.xlsx")) %>%
  ## Pare down to minimum needed columns
  dplyr::select(LTER, Shapefile_Name, Stream_Name, drainSqKm, Latitude, Longitude, Shapefile_CRS_EPSG) %>%
  ## Drop duplicate rows (if any)
  dplyr::distinct() %>%
  ## Rename some columns
  dplyr::rename(expert_area_km2 = drainSqKm,
                crs_code = Shapefile_CRS_EPSG) %>%
  dplyr::mutate(.LTER_KEY = normalize_lter_key(LTER)) |>
  filter(is.na(Shapefile_Name)!=T|
           is.na(Shapefile_Name)==T& !.LTER_KEY %in% normalize_lter_key(excluded_missing_shp_lter)
         & (!is.na(Latitude)&!is.na(Longitude))) %>%
  dplyr::select(-.LTER_KEY)

coord_df <- filter_to_target_records(coord_df, subset_targets = subset_targets)

# Glimpse this
dplyr::glimpse(coord_df)
coord_df|> filter(is.na(Shapefile_Name)) |> pull(LTER) |> unique()

## ------------------------------------------------------- ##
# Acquire Shapefiles ----
## ------------------------------------------------------- ##

# Identify all shapefiles currently in Aurora
server_files <- data.frame("files" = dir(path = file.path(path, 'artisanal-shapefiles-2'))) %>%
  # Split off file type
  dplyr::mutate(file_type = stringr::str_sub(string = files, start = nchar(files) - 3,
                                             nchar(files)))

# Check that out
dplyr::glimpse(server_files)

# What file types are included?
sort(unique(server_files$file_type))

# Normally, `googledrive::drive_ls()` is the way we recommend to download files
# Unfortunately, it doesn't work super well for >500 files
# So, I've downloaded the watersheds manually and re-uploaded to Aurora
# This will need to be re-done if any files change / more shapefiles are added

# See Drive folder to download here:
## https://drive.google.com/drive/folders/1TLEFKLWUpTKwxKiZv9nqhdwccflPuF9g

## ------------------------------------------------------- ##
# Combine Shapefiles ----
## ------------------------------------------------------- ##

# Identify just the .shp files and build a lowercase key for robust matching
raw_sheds <- server_files %>%
  dplyr::filter(file_type == ".shp") %>%
  dplyr::transmute(
    server_shp_name = gsub(pattern = "\\.shp", replacement = "", x = files),
    shp_key = tolower(server_shp_name)
  ) %>%
  dplyr::distinct()

# Warn if multiple on-disk files collapse to the same lowercase key
dup_raw <- raw_sheds %>%
  dplyr::count(shp_key) %>%
  dplyr::filter(n > 1)
if (nrow(dup_raw) > 0) {
  warning(
    "Some shapefiles share the same lowercase filename key. ",
    "Please de-duplicate on-disk names before running."
  )
}

# Build a matching key in the reference table
coord_df <- coord_df %>%
  dplyr::mutate(shp_key = tolower(Shapefile_Name))

# Compare shapefiles we have with those that are named in the reference table
supportR::diff_check(old = unique(coord_df$shp_key), 
                     new = unique(raw_sheds$shp_key))
## Any 'in old but not new' = shapefiles named in reference table but not on Aurora
## Any 'in new but not old' = shapefiles in Aurora that aren't in the reference table

# Wrangle river coordinates
good_sheds <- coord_df %>%
  # Attach matching on-disk shapefile name (case-insensitive)
  dplyr::left_join(raw_sheds, by = "shp_key") %>%
  # Keep only shapefiles in ref table and on Aurora
  dplyr::filter(!is.na(server_shp_name)) %>%
  # Drop any non-unique rows (shouldn't be any but good to double check)
  dplyr::distinct() %>%
  # Condense what remains to ensure no duplicates
  dplyr::group_by(LTER, Shapefile_Name, server_shp_name, crs_code) %>%
  dplyr::summarize(expert_area_km2 = mean(expert_area_km2, na.rm = T),
                   Latitude = dplyr::first(Latitude),
                   Longitude = dplyr::first(Longitude)) %>%
  dplyr::ungroup()

# Check that out
dplyr::glimpse(good_sheds)

# Double check that there are no duplicated shapefile names
good_sheds %>%
  dplyr::group_by(Shapefile_Name) %>%
  dplyr::summarize(name_ct = dplyr::n()) %>%
  dplyr::filter(name_ct != 1)
# Kemijoen vesistöalue was duplicated four times but I think that's not a mistake

# Create an empty object to store combined shapefiles
all_shps <- NULL

known_crs_overrides <- tibble::tribble(
  ~shp_key, ~crs_override,
  "amazon_manacapuru", "ESRI:54012",
  "amazon_santoantonio", "ESRI:54012",
  "amazon_vergemgrande", "ESRI:54012",
  "atalaya_aval", "ESRI:54012",
  "borja", "ESRI:54012",
  "caracarai", "ESRI:54012",
  "cuidad_bolivar", "ESRI:54012",
  "franscisco", "ESRI:54012",
  "itaituba", "ESRI:54012",
  "itapeua", "ESRI:54012",
  "langa_tabiki", "ESRI:54012",
  "manacapuru", "ESRI:54012",
  "nazareth", "ESRI:54012",
  "porto_velho", "ESRI:54012",
  "rio_ica", "ESRI:54012",
  "rio_japura", "ESRI:54012",
  "rio_jurua", "ESRI:54012",
  "rio_jutai", "ESRI:54012",
  "rio_madeira", "ESRI:54012",
  "rio_negro", "ESRI:54012",
  "rio_purus", "ESRI:54012",
  "rurrenabaque", "ESRI:54012",
  "saut_maripa", "ESRI:54012",
  "congo_brazzaville", "ESRI:54012",
  "niger_bamako", "ESRI:54012",
  "elberiver", "ESRI:54012",
  "awout_messam", "EPSG:32632",
  "nyong_ayos", "EPSG:32632",
  "nyong_mbalmayo", "EPSG:32632",
  "nyong_olama", "EPSG:32632",
  "soo_pontsoo", "EPSG:32632",
  "vilajoen_vesistoalue", "EPSG:3067"
)

# Turn off spherical processing
sf::sf_use_s2(F)

# For each on-disk shapefile we have:
for(focal_name in sort(unique(good_sheds$server_shp_name))){
  
  # Identify table metadata for this shapefile
  focal_info <- good_sheds %>%
    dplyr::filter(server_shp_name == focal_name)

  focal_override <- known_crs_overrides %>%
    dplyr::filter(shp_key == tolower(focal_name)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(crs_override)

  # Read in the shapefile
  focal_shp_raw <- sf::st_read(file.path(path, "artisanal-shapefiles-2",
                                         paste0(focal_name, ".shp")), quiet = T)
  
  
  # If CRS is missing (for some reason), manually set what the CRS *should* be
  if(is.na(sf::st_crs(focal_shp_raw))){
    crs_guess <- focal_info$crs_code[!is.na(focal_info$crs_code)][1]

    if (!is.na(focal_override) && nzchar(focal_override)) {
      sf::st_crs(focal_shp_raw) <- sf::st_crs(focal_override)
      message("Assigned known CRS override ", focal_override, " to shapefile: ", focal_name)
    } else if (!is.na(crs_guess)) {
      sf::st_crs(focal_shp_raw) <- crs_guess
      message("Assigned reference-table CRS ", crs_guess, " to shapefile: ", focal_name)
    } else {
      stop("Missing CRS for shapefile '", focal_name, "' and no override was available.", call. = FALSE)
    }
  }
  
  # Make sure CRS is WGS84 (EPSG code 4326)
  focal_shp_wgs84 <- focal_shp_raw %>%
    sf::st_transform(crs = 4326)
  
  # Make sure the polygon geometry is consistently named
  sf::st_geometry(obj = focal_shp_wgs84) <- "geom"
  
  # Calculate shapefile area
  focal_area <- focal_shp_wgs84 %>%
    sf::st_area(x = .) %>%
    units::set_units(x = ., km^2)
  
  # Wrangle raw shapfiles as needed
  focal_shp <- focal_shp_wgs84 %>%
    ## Attach relevant information into this object
    dplyr::mutate(LTER = focal_info$LTER,
                  Shapefile_Name = focal_info$Shapefile_Name,
                  expert_area_km2 = focal_info$expert_area_km2,
                  shape_area_km2 = as.numeric(focal_area)) %>%
    ## Pare down to only this information (+ geometry)!
    ### This approach flexibly ignores whatever idiosyncratic columns may be in the raw object
    dplyr::select(LTER, Shapefile_Name, expert_area_km2, shape_area_km2, geom) %>%
    ## Keep only unique rows
    dplyr::distinct() %>%
    ## Filter to only the largest sub-shape (if there are more than one)
    dplyr::filter(shape_area_km2 == max(shape_area_km2)) %>%
    ## Drop Z/M axes (if they are included)
    sf::st_zm(drop = TRUE, what = "ZM") %>%
    ## Make sure this is a polygon
    sf::st_cast("MULTIPOLYGON")
  
  # Attach this shape to the rest of them
  all_shps %<>%
    dplyr::bind_rows(focal_shp)
  
  # Loop end message
  message("Completed processing for shapefile: ", focal_name, " (", focal_shp$LTER, ")") }

# Exploratory plot
plot(all_shps["Shapefile_Name"], axes = T, main = NULL)

# Tidy up environment
rm(list = setdiff(ls(), c("path", "coord_df", "all_shps", "check_folder")))


## ------------------------------------------------------- ##
# Export Results ----
## ------------------------------------------------------- ##

# Long column names get coerced into abbreviations if left alone
final_shps <- all_shps %>%
  # Rename the columns more succinctly
  dplyr::rename(shp_nm = Shapefile_Name,
                exp_area = expert_area_km2,
                real_area = shape_area_km2)

# Take one last look
dplyr::glimpse(final_shps)

if (!exists("subset_targets", inherits = FALSE)) {
  subset_targets <- load_site_subset()
}

merge_subset_shapes <- !is.null(subset_targets) &&
  tolower(Sys.getenv("SILICA_MERGE_SUBSET_OUTPUTS", "false")) == "true" &&
  file.exists(file.path(path, "site-coordinates", "silica-watersheds_artisanal.shp"))

if (merge_subset_shapes) {
  existing_artisanal <- sf::st_read(
    file.path(path, "site-coordinates", "silica-watersheds_artisanal.shp"),
    quiet = TRUE
  )
  final_shps <- merge_subset_sf(existing_artisanal, final_shps, key_cols = c("LTER", "shp_nm"))
  message("Merged subset artisanal shapefiles into existing silica-watersheds_artisanal.shp")
}

# Export the combine shapefile for all rivers
sf::st_write(obj = final_shps, delete_layer = T,
             dsn = file.path(path, "site-coordinates", "silica-watersheds_artisanal.shp"))

# Process the shapefile object a bit to make a flat table variant
shps_df <- all_shps %>%
  # Drop geometry
  sf::st_drop_geometry(x = .) %>%
  # Calculate some area difference metrics
  dplyr::mutate(
    area_diff_km2 = round((shape_area_km2 - expert_area_km2), digits = 2),
    area_diff_perc = round((area_diff_km2 / expert_area_km2) * 100, digits = 2),
    area_diff_direction = dplyr::case_when(
      abs(area_diff_perc) < 5 ~ "under 5% mismatch",
      abs(area_diff_perc) >= 5 & expert_area_km2 <= 5 ~ "mismatch but watershed tiny (<5km2)",
      area_diff_perc <= -5 & expert_area_km2 > 5 ~ "underestimated",
      area_diff_perc >= 5 & expert_area_km2 > 5 ~ "overestimated"))

# Glimpse it
dplyr::glimpse(shps_df)

# How many shapefiles where area has >5% mismatch with expert know-how?
shps_df %>%
  dplyr::group_by(area_diff_direction) %>%
  dplyr::summarize(file_ct = dplyr::n())

# Create a checks folder if it doesn't exist yet
dir.create(path = file.path(path, "shape_checks"), showWarnings = F)

# Export locally
write_subset_csv(
  df = shps_df,
  output_path = file.path(path, "shape_checks", "artisanal_shape_area_check_2.csv"),
  key_cols = c("LTER", "Shapefile_Name"),
  subset_targets = subset_targets,
  na = ""
)

# Upload to Drive
if (skip_drive_upload) {
  message("Skipping drive_upload for artisanal shape checks because SILICA_SKIP_DRIVE_UPLOAD=TRUE.")
} else {
  googledrive::drive_upload(media = file.path(path, "shape_checks", "artisanal_shape_area_check_2.csv"), 
                            overwrite = T, 
                            path = check_folder)
}

# Tidy up environment
rm(list = ls()); gc()

# End ----
