## ------------------------------------------------------- ##
# Silica WG - Optional Site Subset Helpers
## ------------------------------------------------------- ##

# Normalize strings for robust matching across case/spacing differences
normalize_site_key <- function(x) {
  x <- trimws(as.character(x))
  x <- tolower(x)
  x[x %in% c("", "na")] <- NA_character_
  x
}

# Canonicalize LTER labels used by subset files and helper maps.
# Legacy local-country labels are mapped to a broader Congo-basin grouping.
normalize_lter_key <- function(x) {
  x <- normalize_site_key(x)
  x[x %in% c("cameroon", "cameroon site", "cameroon sites", "congo", "congo basin")] <- "congo-basin"
  x
}

# Normalize region labels from subset files / helper maps
normalize_region_key <- function(x) {
  x <- normalize_site_key(x)
  x <- gsub("[[:space:]_]+", "-", x)
  x
}

# Load optional target-site subset file from environment
load_site_subset <- function() {
  subset_path <- Sys.getenv("SILICA_SITE_SUBSET_FILE", unset = "")

  if (identical(subset_path, "")) {
    return(NULL)
  }

  if (!file.exists(subset_path)) {
    stop(
      "SILICA_SITE_SUBSET_FILE is set but file does not exist: ",
      subset_path,
      call. = FALSE
    )
  }

  subset_targets <- read.csv(subset_path, stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      LTER = trimws(as.character(LTER)),
      Stream_Name = trimws(as.character(Stream_Name)),
      Shapefile_Name = if ("Shapefile_Name" %in% names(.)) {
        trimws(as.character(Shapefile_Name))
      } else {
        NA_character_
      },
      Region = if ("Region" %in% names(.)) {
        trimws(as.character(Region))
      } else {
        NA_character_
      }
    ) %>%
    dplyr::filter(!is.na(LTER), LTER != "", !is.na(Stream_Name), Stream_Name != "") %>%
    dplyr::distinct()

  if (!all(c("LTER", "Stream_Name") %in% names(subset_targets))) {
    stop(
      "Subset file must include columns: LTER, Stream_Name",
      call. = FALSE
    )
  }

  message(
    "Loaded target subset from ", subset_path,
    " (", nrow(subset_targets), " rows)"
  )

  subset_targets
}

# Identify subset rows that should bypass artisanal polygons and use HydroSHEDS.
# Supported flags in the subset CSV:
#   Force_HydroSHEDS = TRUE/FALSE
#   Watershed_Source = "hydrosheds" / "artisanal"
load_forced_hydrosheds_targets <- function(subset_targets = NULL) {
  if (is.null(subset_targets) || nrow(subset_targets) == 0) {
    return(NULL)
  }

  src_vals <- if ("Watershed_Source" %in% names(subset_targets)) {
    normalize_site_key(subset_targets$Watershed_Source)
  } else {
    rep(NA_character_, nrow(subset_targets))
  }

  force_vals <- if ("Force_HydroSHEDS" %in% names(subset_targets)) {
    normalize_site_key(subset_targets$Force_HydroSHEDS)
  } else {
    rep(NA_character_, nrow(subset_targets))
  }

  keep <- (!is.na(src_vals) & src_vals == "hydrosheds") |
    (!is.na(force_vals) & force_vals %in% c("true", "t", "1", "yes", "y"))

  forced <- subset_targets[keep, , drop = FALSE]
  if (nrow(forced) == 0) {
    return(NULL)
  }

  forced %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_site_key(Stream_Name),
      .SHP_KEY = if ("Shapefile_Name" %in% names(.)) normalize_site_key(Shapefile_Name) else NA_character_
    ) %>%
    dplyr::distinct(.LTER_KEY, .STREAM_KEY, .SHP_KEY)
}

silica_output_date <- function() {
  stamp <- Sys.getenv("SILICA_OUTPUT_DATE", unset = format(Sys.Date(), "%Y%m%d"))
  stamp <- gsub("[^0-9]", "", stamp)
  if (!grepl("^[0-9]{8}$", stamp)) {
    stop("SILICA_OUTPUT_DATE must resolve to YYYYMMDD, got: ", stamp, call. = FALSE)
  }
  stamp
}

silica_driver_output_file <- function(root_path, stem, ext = "csv") {
  file.path(root_path, "extracted-data", paste0(stem, "_", silica_output_date(), ".", ext))
}

# Filter a generic site-like dataframe to subset targets
filter_to_target_records <- function(df, subset_targets = NULL,
                                     lter_col = "LTER",
                                     stream_col = "Stream_Name",
                                     shp_col = "Shapefile_Name") {
  if (is.null(subset_targets)) {
    return(df)
  }

  if (!all(c(lter_col, stream_col) %in% names(df))) {
    stop(
      "Target record filtering requires columns '", lter_col, "' and '",
      stream_col, "' in the input data.",
      call. = FALSE
    )
  }

  shp_vals <- if (shp_col %in% names(df)) df[[shp_col]] else NA_character_

  df_aug <- df %>%
    dplyr::mutate(
      .ROW_ID = dplyr::row_number(),
      .LTER_KEY = normalize_lter_key(.data[[lter_col]]),
      .STREAM_KEY = normalize_site_key(.data[[stream_col]]),
      .SHP_KEY = normalize_site_key(shp_vals)
    )

  target_stream <- subset_targets %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_site_key(Stream_Name)
    ) %>%
    dplyr::filter(!is.na(.LTER_KEY), !is.na(.STREAM_KEY)) %>%
    dplyr::distinct(.LTER_KEY, .STREAM_KEY)

  target_shp <- subset_targets %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .SHP_KEY = normalize_site_key(Shapefile_Name)
    ) %>%
    dplyr::filter(!is.na(.LTER_KEY), !is.na(.SHP_KEY)) %>%
    dplyr::distinct(.LTER_KEY, .SHP_KEY)

  keep_df <- dplyr::semi_join(df_aug, target_stream, by = c(".LTER_KEY", ".STREAM_KEY"))

  if (nrow(target_shp) > 0) {
    keep_df <- dplyr::bind_rows(
      keep_df,
      dplyr::semi_join(df_aug, target_shp, by = c(".LTER_KEY", ".SHP_KEY"))
    )
  }

  keep_df <- keep_df %>%
    dplyr::distinct(.ROW_ID, .keep_all = TRUE) %>%
    dplyr::arrange(.ROW_ID)

  unmatched <- target_stream %>%
    dplyr::anti_join(
      df_aug %>% dplyr::distinct(.LTER_KEY, .STREAM_KEY),
      by = c(".LTER_KEY", ".STREAM_KEY")
    )

  if (nrow(unmatched) > 0) {
    warning(
      nrow(unmatched),
      " target rows in SILICA_SITE_SUBSET_FILE did not match the input record table. ",
      "Check spelling/case for LTER + Stream_Name.",
      call. = FALSE
    )
  }

  keep_df %>%
    dplyr::select(-.ROW_ID, -.LTER_KEY, -.STREAM_KEY, -.SHP_KEY)
}

# Normalize merge key values for row replacement
normalize_merge_key <- function(x, col_name) {
  if (tolower(col_name) == "lter") {
    return(normalize_lter_key(x))
  }
  normalize_site_key(x)
}

fill_missing_columns <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  for (nm in missing_cols) {
    df[[nm]] <- NA
  }
  df[, cols, drop = FALSE]
}

harmonize_merge_column_types <- function(existing_df, patch_df, cols) {
  for (nm in cols) {
    ptype <- tryCatch(
      vctrs::vec_ptype2(existing_df[[nm]], patch_df[[nm]]),
      error = function(e) NULL
    )

    # Existing CSV outputs often round-trip dates/times as character. When a
    # fresh subset patch carries Date/POSIX columns, coerce both sides to
    # character before row-binding so subset merges remain append-safe.
    if (is.null(ptype) || inherits(ptype, "Date") || inherits(ptype, "POSIXt")) {
      existing_df[[nm]] <- as.character(existing_df[[nm]])
      patch_df[[nm]] <- as.character(patch_df[[nm]])
    }
  }

  list(existing_df = existing_df, patch_df = patch_df)
}

# Merge a subset patch into an existing data.frame using key columns
merge_subset_rows <- function(existing_df, patch_df, key_cols) {
  if (nrow(patch_df) == 0) {
    return(existing_df)
  }

  if (!all(key_cols %in% names(existing_df)) || !all(key_cols %in% names(patch_df))) {
    stop("All key columns must exist in both existing_df and patch_df.", call. = FALSE)
  }

  all_cols <- union(names(existing_df), names(patch_df))
  existing_df <- fill_missing_columns(existing_df, all_cols)
  patch_df <- fill_missing_columns(patch_df, all_cols)

  harmonized <- harmonize_merge_column_types(existing_df, patch_df, all_cols)
  existing_df <- harmonized$existing_df
  patch_df <- harmonized$patch_df

  existing_keyed <- existing_df
  patch_keyed <- patch_df
  for (nm in key_cols) {
    existing_keyed[[nm]] <- normalize_merge_key(existing_keyed[[nm]], nm)
    patch_keyed[[nm]] <- normalize_merge_key(patch_keyed[[nm]], nm)
  }

  keep_existing <- dplyr::anti_join(
    existing_keyed,
    patch_keyed[, key_cols, drop = FALSE],
    by = key_cols
  )

  keep_existing %>%
    dplyr::bind_rows(patch_keyed) %>%
    fill_missing_columns(all_cols)
}

# Merge a subset patch into an existing sf object using key columns
merge_subset_sf <- function(existing_sf, patch_sf, key_cols) {
  if (nrow(patch_sf) == 0) {
    return(existing_sf)
  }

  if (!inherits(existing_sf, "sf") || !inherits(patch_sf, "sf")) {
    stop("merge_subset_sf requires both inputs to be sf objects.", call. = FALSE)
  }

  if (!all(key_cols %in% names(existing_sf)) || !all(key_cols %in% names(patch_sf))) {
    stop("All key columns must exist in both existing_sf and patch_sf.", call. = FALSE)
  }

  all_cols <- union(names(existing_sf), names(patch_sf))
  existing_sf <- fill_missing_columns(existing_sf, all_cols)
  patch_sf <- fill_missing_columns(patch_sf, all_cols)

  existing_crs <- sf::st_crs(existing_sf)
  patch_crs <- sf::st_crs(patch_sf)

  if (!is.na(existing_crs) && !is.na(patch_crs) &&
      !identical(existing_crs$wkt, patch_crs$wkt)) {
    patch_sf <- sf::st_transform(patch_sf, existing_crs)
  } else if (!is.na(existing_crs) && is.na(patch_crs)) {
    sf::st_crs(patch_sf) <- existing_crs
  } else if (is.na(existing_crs) && !is.na(patch_crs)) {
    sf::st_crs(existing_sf) <- patch_crs
  }

  existing_key <- sf::st_drop_geometry(existing_sf)
  patch_key <- sf::st_drop_geometry(patch_sf)
  for (nm in key_cols) {
    existing_key[[nm]] <- normalize_merge_key(existing_key[[nm]], nm)
    patch_key[[nm]] <- normalize_merge_key(patch_key[[nm]], nm)
  }

  keep_rows <- dplyr::anti_join(
    dplyr::mutate(existing_key, .ROW_ID = dplyr::row_number()),
    patch_key[, key_cols, drop = FALSE],
    by = key_cols
  )$.ROW_ID

  dplyr::bind_rows(
    existing_sf[keep_rows, , drop = FALSE],
    patch_sf
  )
}

# Write a CSV, merging subset rows into an existing file when requested
write_subset_csv <- function(df, output_path, key_cols, subset_targets = NULL, na = "") {
  merge_subset <- !is.null(subset_targets) &&
    tolower(Sys.getenv("SILICA_MERGE_SUBSET_OUTPUTS", "false")) == "true" &&
    file.exists(output_path)

  if (merge_subset) {
    existing_df <- read.csv(output_path, stringsAsFactors = FALSE, check.names = FALSE)
    df <- merge_subset_rows(existing_df, df, key_cols = key_cols)
  }

  write.csv(df, file = output_path, row.names = FALSE, na = na)
}

# Filter sites/sheds to only target records
filter_to_target_sites <- function(sites, sheds, subset_targets = NULL) {
  if (is.null(subset_targets)) {
    return(list(sites = sites, sheds = sheds))
  }

  sites_aug <- sites %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_site_key(Stream_Name),
      .SHP_KEY = normalize_site_key(Shapefile_Name)
    )

  sheds_aug <- sheds %>%
    dplyr::mutate(
      .ROW_ID = dplyr::row_number(),
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_site_key(Stream_Name),
      .SHP_KEY = normalize_site_key(Shapefile_Name)
    )

  target_stream <- subset_targets %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_site_key(Stream_Name)
    ) %>%
    dplyr::filter(!is.na(.LTER_KEY), !is.na(.STREAM_KEY)) %>%
    dplyr::distinct(.LTER_KEY, .STREAM_KEY)

  target_shp <- subset_targets %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .SHP_KEY = normalize_site_key(Shapefile_Name)
    ) %>%
    dplyr::filter(!is.na(.LTER_KEY), !is.na(.SHP_KEY)) %>%
    dplyr::distinct(.LTER_KEY, .SHP_KEY)

  sites_keep <- dplyr::semi_join(sites_aug, target_stream, by = c(".LTER_KEY", ".STREAM_KEY"))
  sheds_keep <- dplyr::semi_join(sheds_aug, target_stream, by = c(".LTER_KEY", ".STREAM_KEY"))

  if (nrow(target_shp) > 0) {
    sites_keep <- dplyr::bind_rows(
      sites_keep,
      dplyr::semi_join(sites_aug, target_shp, by = c(".LTER_KEY", ".SHP_KEY"))
    )
    sheds_keep <- dplyr::bind_rows(
      sheds_keep,
      dplyr::semi_join(sheds_aug, target_shp, by = c(".LTER_KEY", ".SHP_KEY"))
    )
  }

  sites_keep <- sites_keep %>%
    dplyr::distinct() %>%
    dplyr::select(-.LTER_KEY, -.STREAM_KEY, -.SHP_KEY)

  sheds_keep <- sheds_keep %>%
    # Avoid `distinct()` on geometry columns; de-duplicate using original row id.
    dplyr::distinct(.ROW_ID, .keep_all = TRUE) %>%
    dplyr::select(-.ROW_ID, -.LTER_KEY, -.STREAM_KEY, -.SHP_KEY)

  unmatched <- target_stream %>%
    dplyr::anti_join(
      sites_aug %>% dplyr::distinct(.LTER_KEY, .STREAM_KEY),
      by = c(".LTER_KEY", ".STREAM_KEY")
    )

  if (nrow(unmatched) > 0) {
    warning(
      nrow(unmatched),
      " target rows in SILICA_SITE_SUBSET_FILE did not match the site table. ",
      "Check spelling/case for LTER + Stream_Name.",
      call. = FALSE
    )
  }

  if (nrow(sheds_keep) == 0) {
    stop(
      "Target subset retained zero watersheds. Check SILICA_SITE_SUBSET_FILE values.",
      call. = FALSE
    )
  }

  message(
    "Subset active: retained ", nrow(sites_keep), " site rows and ",
    nrow(sheds_keep), " watershed polygons"
  )

  list(sites = sites_keep, sheds = sheds_keep)
}

# Restrict dynamic-driver region loops when a subset is active
resolve_target_regions <- function(subset_targets = NULL, default_regions) {
  if (is.null(subset_targets)) {
    return(default_regions)
  }

  if ("Region" %in% names(subset_targets)) {
    explicit_regions <- subset_targets %>%
      dplyr::transmute(region = normalize_region_key(Region)) %>%
      dplyr::filter(!is.na(region), region != "") %>%
      dplyr::distinct() %>%
      dplyr::pull(region)

    explicit_regions <- intersect(default_regions, explicit_regions)

    if (length(explicit_regions) > 0) {
      message("Dynamic regions restricted via subset Region column to: ",
              paste(sort(explicit_regions), collapse = ", "))
      return(sort(explicit_regions))
    }
  }

  region_map <- data.frame(
    LTER_KEY = c("arc", "congo-basin", "gro", "guadeloupe", "hybam", "md", "niva", "seine", "uk", "usgs", "westernaustralia"),
    region = c(
      "north-america-arctic",
      "congo",
      "amazon",
      "puerto-rico",
      "amazon",
      "australia",
      "scandinavia",
      "germany",
      "united-kingdom",
      "north-america-usa",
      "australia"
    ),
    stringsAsFactors = FALSE
  )

  region_targets <- subset_targets %>%
    dplyr::distinct(LTER) %>%
    dplyr::mutate(LTER_KEY = normalize_lter_key(LTER)) %>%
    dplyr::left_join(region_map, by = "LTER_KEY")

  unknown_lter <- region_targets %>%
    dplyr::filter(is.na(region)) %>%
    dplyr::pull(LTER)

  if (length(unknown_lter) > 0) {
    warning(
      "Could not map LTER to region for: ",
      paste(unique(unknown_lter), collapse = ", "),
      ". Keeping full default region set.",
      call. = FALSE
    )
    return(default_regions)
  }

  target_regions <- sort(unique(region_targets$region))
  target_regions <- intersect(default_regions, target_regions)

  if (length(target_regions) == 0) {
    warning(
      "Subset did not map to any known dynamic-driver regions. Keeping full default region set.",
      call. = FALSE
    )
    return(default_regions)
  }

  message("Dynamic regions restricted to: ", paste(target_regions, collapse = ", "))
  target_regions
}
