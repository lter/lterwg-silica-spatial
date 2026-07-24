# Prepare accepted non-HydroSHEDS watershed files for spatial extraction.
#
# Source provenance belongs in the site-reference table. Release-aware runs
# select the exact accepted watershed version for each row.

# Read needed libraries
librarian::shelf(tidyverse, magrittr, googledrive, sf, supportR, readxl)

# Do not clear the session/environment here. This script may be sourced by the
# workflow.

source(file = file.path(getwd(), "tools", "workflow_paths.R"))
source(file = file.path(getwd(), "tools", "subset_and_output_helpers.R"))
# Identify path to location of shared data
(path <- resolve_silica_data_root())

# Shared identifier and subset helpers.
subset_targets <- load_site_subset()
site_coord_dir <- silica_site_coordinates_dir(path)

missing_shapefile_exclusions_path <- Sys.getenv(
  "SILICA_MISSING_SHAPEFILE_LTER_EXCLUSIONS",
  unset = file.path(
    "02_watershed_delineation",
    "config",
    "missing_shapefile_lter_exclusions.tsv"
  )
)
excluded_missing_shp_lter <- load_lter_exclusion_list(
  missing_shapefile_exclusions_path
)

skip_drive_auth <- tolower(Sys.getenv("SILICA_SKIP_DRIVE_AUTH", "false")) ==
  "true"
skip_drive_upload <- tolower(Sys.getenv("SILICA_SKIP_DRIVE_UPLOAD", "false")) ==
  "true"
site_reference_drive_folder_id <- Sys.getenv(
  "SILICA_SITE_REFERENCE_DRIVE_FOLDER_ID",
  unset = ""
)
qa_drive_folder_id <- Sys.getenv("SILICA_QA_DRIVE_FOLDER_ID", unset = "")
if (!skip_drive_auth && !nzchar(site_reference_drive_folder_id)) {
  stop(
    "Set SILICA_SITE_REFERENCE_DRIVE_FOLDER_ID or set ",
    "SILICA_SKIP_DRIVE_AUTH=TRUE.",
    call. = FALSE
  )
}
if (!skip_drive_upload && !nzchar(qa_drive_folder_id)) {
  stop(
    "Set SILICA_QA_DRIVE_FOLDER_ID or set SILICA_SKIP_DRIVE_UPLOAD=TRUE.",
    call. = FALSE
  )
}


## ------------------------------------------------------- ##
# Reference Table Acquisition ----
## ------------------------------------------------------- ##

if (skip_drive_auth) {
  message(
    "Skipping Drive download of site reference table because SILICA_SKIP_DRIVE_AUTH=TRUE."
  )
} else {
  googledrive::drive_ls(
    googledrive::as_id(site_reference_drive_folder_id)
  ) %>%
    dplyr::filter(name == "Site_Reference_Table") %>%
    googledrive::drive_download(
      file = .,
      overwrite = T,
      path = file.path(site_coord_dir, "silica-coords_RAW.xlsx")
    )
}

canonical_release_mode <- silica_use_canonical_release_library()
reference_raw <- read_silica_site_reference(site_coord_dir)
if (canonical_release_mode &&
    !"Spatial_Data_Version" %in% names(reference_raw)) {
  stop(
    "Canonical release mode requires Spatial_Data_Version ",
    "in the site-reference table.",
    call. = FALSE
  )
}

# Read in site coordinates (i.e., reference table). In canonical mode, retain
# the per-row spatial release so a same-named watershed can never be selected
# from the wrong data_release_N directory.
coord_df <- reference_raw %>%
  dplyr::transmute(
    LTER = LTER,
    Shapefile_Name = Shapefile_Name,
    Stream_Name = Stream_Name,
    expert_area_km2 = drainSqKm,
    Latitude = Latitude,
    Longitude = Longitude,
    crs_code = Shapefile_CRS_EPSG,
    spatial_release = if (canonical_release_mode) {
      suppressWarnings(as.integer(.data[["Spatial_Data_Version"]]))
    } else {
      0L
    }
  ) %>%
  ## Drop duplicate rows (if any)
  dplyr::distinct() %>%
  dplyr::mutate(.LTER_KEY = normalize_lter_key(LTER)) |>
  filter(
    is.na(Shapefile_Name) != T |
      is.na(Shapefile_Name) == T &
        !.LTER_KEY %in% normalize_lter_key(excluded_missing_shp_lter) &
        (!is.na(Latitude) & !is.na(Longitude))
  ) %>%
  dplyr::select(-.LTER_KEY)

coord_df <- filter_to_target_records(coord_df, subset_targets = subset_targets)

if (canonical_release_mode) {
  invalid_release <- !is.na(coord_df$Shapefile_Name) &
    (is.na(coord_df$spatial_release) | !coord_df$spatial_release %in% 1:3)
  if (any(invalid_release)) {
    bad <- unique(paste(
      coord_df$LTER[invalid_release],
      coord_df$Stream_Name[invalid_release],
      coord_df$Shapefile_Name[invalid_release],
      sep = " / "
    ))
    stop(
      "Reference rows with Shapefile_Name must declare spatial release 1, 2, ",
      "or 3 in canonical mode:\n- ",
      paste(bad, collapse = "\n- "),
      call. = FALSE
    )
  }
}

hydrosheds_targets <- if (canonical_release_mode) {
  NULL
} else {
  build_hydrosheds_target_keys(
    subset_targets = subset_targets,
    coord_df = coord_df
  )
}

if (!is.null(hydrosheds_targets)) {
  coord_df <- coord_df %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_site_key(Stream_Name),
      .SHP_KEY = normalize_site_key(Shapefile_Name)
    ) %>%
    dplyr::anti_join(
      hydrosheds_targets,
      by = c(".LTER_KEY", ".STREAM_KEY", ".SHP_KEY")
    ) %>%
    dplyr::select(-.LTER_KEY, -.STREAM_KEY, -.SHP_KEY)
}

# Glimpse this
dplyr::glimpse(coord_df)
coord_df |> filter(is.na(Shapefile_Name)) |> pull(LTER) |> unique()

## ------------------------------------------------------- ##
# Acquire Shapefiles ----
## ------------------------------------------------------- ##

# Identify all shapefiles currently available. Canonical mode reads the
# one-site-per-folder release library recursively; legacy mode retains the
# former flat artisanal-shapefiles-2 behavior.
if (canonical_release_mode) {
  release_dirs <- silica_release_shapefile_dirs(path)
  release_shp_paths <- lapply(names(release_dirs), function(release) {
    files <- list.files(
      release_dirs[[release]],
      pattern = "[.]shp$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    data.frame(
      files = files,
      spatial_release = as.integer(release),
      stringsAsFactors = FALSE
    )
  })
  server_files <- dplyr::bind_rows(release_shp_paths) %>%
    dplyr::mutate(file_type = ".shp")
} else {
  server_files <- data.frame(
    "files" = dir(path = file.path(path, "artisanal-shapefiles-2")),
    spatial_release = 0L
  ) %>%
    # Split off file type
    dplyr::mutate(
      file_type = stringr::str_sub(
        string = files,
        start = nchar(files) - 3,
        nchar(files)
      ),
      files = file.path(path, "artisanal-shapefiles-2", files)
    )
}

# Check that out
dplyr::glimpse(server_files)

# What file types are included?
sort(unique(server_files$file_type))

# Normally, `googledrive::drive_ls()` is the way we recommend to download files
# Unfortunately, it doesn't work super well for >500 files
# So, I've downloaded the watersheds manually and re-uploaded to Aurora
# This will need to be re-done if any files change / more shapefiles are added

# The canonical source location is external. Set SILICA_DATA_ROOT to the
# materialized library; do not embed a contributor-specific Drive link here.

## ------------------------------------------------------- ##
# Combine Shapefiles ----
## ------------------------------------------------------- ##

# Identify just the .shp files and build a lowercase key for robust matching
raw_sheds <- server_files %>%
  dplyr::filter(file_type == ".shp") %>%
  dplyr::transmute(
    shp_path = files,
    server_shp_name = tools::file_path_sans_ext(basename(files)),
    shp_key = tolower(server_shp_name),
    spatial_release = spatial_release
  ) %>%
  dplyr::distinct()

# Stop if multiple on-disk files within one release collapse to the same key.
# The same Shapefile_Name may legitimately exist in multiple releases.
dup_raw <- raw_sheds %>%
  dplyr::count(spatial_release, shp_key) %>%
  dplyr::filter(n > 1)
if (nrow(dup_raw) > 0) {
  stop(
    "Some shapefiles within the same release share a lowercase filename key. ",
    "De-duplicate the canonical release folder before running.",
    call. = FALSE
  )
}

# Build a matching key in the reference table
coord_df <- coord_df %>%
  dplyr::mutate(
    shp_key = tolower(Shapefile_Name),
    library_key = paste(spatial_release, shp_key, sep = "::")
  )
raw_sheds <- raw_sheds %>%
  dplyr::mutate(library_key = paste(spatial_release, shp_key, sep = "::"))

# Compare shapefiles we have with those that are named in the reference table
supportR::diff_check(
  old = unique(coord_df$library_key),
  new = unique(raw_sheds$library_key)
)
## Any 'in old but not new' = shapefiles named in reference table but not on Aurora
## Any 'in new but not old' = shapefiles in Aurora that aren't in the reference table

# Match river coordinates to release-specific bundles.
matched_sheds <- coord_df %>%
  # Attach the exact release-specific on-disk bundle.
  dplyr::left_join(
    raw_sheds,
    by = c("library_key", "spatial_release", "shp_key")
  )

if (canonical_release_mode) {
  missing_bundle <- !is.na(matched_sheds$Shapefile_Name) &
    is.na(matched_sheds$server_shp_name)
  if (any(missing_bundle)) {
    bad <- unique(paste(
      matched_sheds$spatial_release[missing_bundle],
      matched_sheds$Shapefile_Name[missing_bundle],
      matched_sheds$LTER[missing_bundle],
      matched_sheds$Stream_Name[missing_bundle],
      sep = " / "
    ))
    stop(
      "The reference table names canonical watershed bundles that are absent ",
      "from their declared release folder:\n- ",
      paste(bad, collapse = "\n- "),
      call. = FALSE
    )
  }
}

# Wrangle river coordinates
good_sheds <- matched_sheds %>%
  # Keep only shapefiles in ref table and on Aurora
  dplyr::filter(!is.na(server_shp_name)) %>%
  # Drop any non-unique rows (shouldn't be any but good to double check)
  dplyr::distinct() %>%
  # Condense what remains to ensure no duplicates
  dplyr::group_by(
    LTER,
    Shapefile_Name,
    server_shp_name,
    shp_path,
    spatial_release,
    crs_code
  ) %>%
  dplyr::summarize(
    expert_area_km2 = mean(expert_area_km2, na.rm = T),
    Latitude = dplyr::first(Latitude),
    Longitude = dplyr::first(Longitude)
  ) %>%
  dplyr::ungroup()

# Check that out
dplyr::glimpse(good_sheds)

# Double check that there are no duplicated shapefile names
good_sheds %>%
  dplyr::group_by(Shapefile_Name) %>%
  dplyr::summarize(name_ct = dplyr::n()) %>%
  dplyr::filter(name_ct != 1)
# Kemijoen vesistöalue was duplicated four times but I think that's not a mistake

# Create an empty sf object to store combined shapefiles.
# This lets HydroSHEDS-only subsets pass through cleanly when there are zero
# artisanal sites in the requested subset.
all_shps <- sf::st_sf(
  LTER = character(),
  Shapefile_Name = character(),
  expert_area_km2 = double(),
  shape_area_km2 = double(),
  geom = sf::st_sfc(crs = 4326)
)

crs_override_path <- Sys.getenv(
  "SILICA_ARTISANAL_CRS_OVERRIDES",
  unset = file.path(
    "02_watershed_delineation",
    "config",
    "artisanal_crs_overrides.tsv"
  )
)
known_crs_overrides <- read.delim(
  crs_override_path,
  sep = "\t",
  quote = "",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
assert_required_columns(
  known_crs_overrides,
  c("Shapefile_Key", "CRS"),
  "artisanal CRS override table"
)
known_crs_overrides <- known_crs_overrides %>%
  dplyr::transmute(
    shp_key = tolower(trimws(Shapefile_Key)),
    crs_override = trimws(CRS)
  )
if (anyDuplicated(known_crs_overrides$shp_key)) {
  stop("Artisanal CRS override table has duplicate keys.", call. = FALSE)
}

# Turn off spherical processing
sf::sf_use_s2(F)

# For each exact release-specific on-disk shapefile we have:
for (focal_path in sort(unique(good_sheds$shp_path))) {
  # Identify table metadata for this shapefile
  focal_info <- good_sheds %>%
    dplyr::filter(shp_path == focal_path)
  focal_name <- focal_info$server_shp_name[[1]]

  focal_override <- known_crs_overrides %>%
    dplyr::filter(shp_key == tolower(focal_name)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::pull(crs_override)

  # Read in the shapefile
  focal_shp_raw <- sf::st_read(
    focal_path,
    quiet = T
  )

  # If CRS is missing (for some reason), manually set what the CRS *should* be
  if (is.na(sf::st_crs(focal_shp_raw))) {
    crs_guess <- focal_info$crs_code[!is.na(focal_info$crs_code)][1]

    if (!is.na(focal_override) && nzchar(focal_override)) {
      sf::st_crs(focal_shp_raw) <- sf::st_crs(focal_override)
      message(
        "Assigned known CRS override ",
        focal_override,
        " to shapefile: ",
        focal_name
      )
    } else if (!is.na(crs_guess)) {
      sf::st_crs(focal_shp_raw) <- crs_guess
      message(
        "Assigned reference-table CRS ",
        crs_guess,
        " to shapefile: ",
        focal_name
      )
    } else {
      stop(
        "Missing CRS for shapefile '",
        focal_name,
        "' and no override was available.",
        call. = FALSE
      )
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
    dplyr::mutate(
      LTER = focal_info$LTER,
      Shapefile_Name = focal_info$Shapefile_Name,
      expert_area_km2 = focal_info$expert_area_km2,
      shape_area_km2 = as.numeric(focal_area)
    ) %>%
    ## Pare down to only this information (+ geometry)!
    ### This approach flexibly ignores whatever idiosyncratic columns may be in the raw object
    dplyr::select(
      LTER,
      Shapefile_Name,
      expert_area_km2,
      shape_area_km2,
      geom
    ) %>%
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
  message(
    "Completed processing for shapefile: ",
    focal_name,
    ", release ",
    focal_info$spatial_release[[1]],
    " (",
    focal_shp$LTER,
    ")"
  )
}

# Exploratory plot
plot_artisanal_checks <- tolower(Sys.getenv(
  "SILICA_PLOT_ARTISANAL_CHECKS",
  "false"
)) ==
  "true"
if (plot_artisanal_checks && nrow(all_shps) > 0) {
  non_empty_geom <- !sf::st_is_empty(all_shps)
  if (any(non_empty_geom)) {
    plot(all_shps[non_empty_geom, "Shapefile_Name"], axes = TRUE, main = NULL)
  } else {
    message(
      "Skipping artisanal exploratory plot because all geometries are empty."
    )
  }
} else {
  message("Skipping artisanal exploratory plot.")
}

# Do not drop helper/path functions mid-script. They are needed below for
# subset-aware exports and merge logic.

## ------------------------------------------------------- ##
# Export Results ----
## ------------------------------------------------------- ##

# Long column names get coerced into abbreviations if left alone
final_shps <- all_shps %>%
  # Rename the columns more succinctly
  dplyr::rename(
    shp_nm = Shapefile_Name,
    exp_area = expert_area_km2,
    real_area = shape_area_km2
  )

# Take one last look
dplyr::glimpse(final_shps)

if (!exists("subset_targets", inherits = FALSE)) {
  subset_targets <- load_site_subset()
}

subset_artisanal_mode <- !is.null(subset_targets) &&
  tolower(Sys.getenv("SILICA_MERGE_SUBSET_OUTPUTS", "false")) == "true"

artisanal_out_path <- if (subset_artisanal_mode) {
  silica_sitecoord_output_file(
    path,
    "silica-watersheds_artisanal_subset",
    "shp"
  )
} else {
  silica_sitecoord_output_file(path, "silica-watersheds_artisanal", "shp")
}

if (subset_artisanal_mode) {
  message("Writing subset-only artisanal shapefile: ", artisanal_out_path)
}

# Export the combined shapefile for all rivers
sf::st_write(obj = final_shps, delete_layer = T, dsn = artisanal_out_path)

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
      abs(area_diff_perc) >= 5 &
        expert_area_km2 <= 5 ~ "mismatch but watershed tiny (<5km2)",
      area_diff_perc <= -5 & expert_area_km2 > 5 ~ "underestimated",
      area_diff_perc >= 5 & expert_area_km2 > 5 ~ "overestimated"
    )
  )

# Glimpse it
dplyr::glimpse(shps_df)

# How many shapefiles where area has >5% mismatch with expert know-how?
shps_df %>%
  dplyr::group_by(area_diff_direction) %>%
  dplyr::summarize(file_ct = dplyr::n())

# Create a checks folder if it doesn't exist yet
dir.create(path = file.path(path, "shape_checks"), showWarnings = F)

# Export locally
shape_check_file <- file.path(
  path,
  "shape_checks",
  paste0("artisanal_shape_area_check_", silica_output_date(), ".csv")
)
write_subset_csv(
  df = shps_df,
  output_path = shape_check_file,
  key_cols = c("LTER", "Shapefile_Name"),
  subset_targets = subset_targets,
  na = ""
)

# Upload to Drive
if (skip_drive_upload) {
  message(
    "Skipping drive_upload for artisanal shape checks because SILICA_SKIP_DRIVE_UPLOAD=TRUE."
  )
} else {
  googledrive::drive_upload(
    media = shape_check_file,
    overwrite = T,
    path = googledrive::as_id(qa_drive_folder_id)
  )
}

# Avoid clearing the caller environment when this script is sourced from
# 03_combine-artisanal-hydrosheds.R.
invisible(gc())

# End ----
