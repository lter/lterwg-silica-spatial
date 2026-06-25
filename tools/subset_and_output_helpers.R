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

clean_lter_label <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\\s*\\([^)]*\\)", "", x)
  x <- trimws(x)
  x[x %in% c("", "na")] <- NA_character_
  x
}

clean_lter_column <- function(df, drop_duplicates = TRUE) {
  if ("LTER" %in% names(df)) {
    df$LTER <- clean_lter_label(df$LTER)
  }

  if (drop_duplicates) {
    df <- df[!duplicated(df), , drop = FALSE]
  }

  df
}

# Clean LTER labels used by subset files and helper maps.
# This is also where we keep known reference-table name changes across releases,
# for example V2 -> V3 label changes such as:
# - Swedish Goverment -> Sweden
# - Carey -> PIE
# Legacy local-country labels are also mapped to broader workflow groupings.
normalize_lter_key <- function(x) {
  x <- normalize_site_key(clean_lter_label(x))
  x[x %in% c("walkerbranch")] <- "walker branch"
  x[x %in% c("elbe")] <- "germany"
  x[x %in% c("swedish goverment", "swedish government")] <- "sweden"
  x[x %in% c("carey")] <- "pie"
  x[x %in% c("cameroon", "cameroon site", "cameroon sites", "congo", "congo basin")] <- "congo-basin"
  x
}

# Normalize region labels from subset files / helper maps
normalize_region_key <- function(x) {
  x <- normalize_site_key(x)
  x <- gsub("[[:space:]_]+", "-", x)
  x
}

# Clean stream-name variants that appear across discharge, reference, and spatial
# outputs. This helper is for matching keys, so everything stays in a normalized
# lowercase form.
normalize_stream_key <- function(x) {
  x <- normalize_site_key(x)
  x[x %in% c("mg_weir", "mgweir")] <- "mgweir"
  x[x %in% c("or_low", "orlow")] <- "orlow"
  x[x %in% c("sopchoppy river", "sopchoppy river ")] <- "sopchoppy river"
  x[x %in% c("east fork")] <- "east fork"
  x[x %in% c("west fork")] <- "west fork"
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
      .STREAM_KEY = normalize_stream_key(Stream_Name),
      .SHP_KEY = if ("Shapefile_Name" %in% names(.)) normalize_site_key(Shapefile_Name) else NA_character_
    ) %>%
    dplyr::distinct(.LTER_KEY, .STREAM_KEY, .SHP_KEY)
}

silica_hydrosheds_area_threshold_km2 <- function() {
  raw_val <- Sys.getenv("SILICA_HYDROSHEDS_AREA_THRESHOLD_KM2", unset = "100")
  threshold <- suppressWarnings(as.numeric(raw_val))

  if (is.na(threshold) || threshold < 0) {
    stop(
      "SILICA_HYDROSHEDS_AREA_THRESHOLD_KM2 must be a non-negative number. Got: ",
      raw_val,
      call. = FALSE
    )
  }

  threshold
}

# Build the final list of subset rows that should go through HydroSHEDS.
# Two routes are supported:
# 1. Explicit flags in the subset CSV
# 2. Automatic routing for subset rows that are missing a shapefile name and
#    have a drainage area above the HydroSHEDS threshold
build_hydrosheds_target_keys <- function(subset_targets = NULL, coord_df = NULL) {
  explicit_targets <- load_forced_hydrosheds_targets(subset_targets)

  auto_targets <- NULL
  if (!is.null(subset_targets) && !is.null(coord_df) && nrow(coord_df) > 0) {
    if (!all(c("LTER", "Stream_Name", "expert_area_km2") %in% names(coord_df))) {
      stop(
        "coord_df must include LTER, Stream_Name, and expert_area_km2 to build HydroSHEDS targets.",
        call. = FALSE
      )
    }

    threshold_km2 <- silica_hydrosheds_area_threshold_km2()

    auto_targets <- coord_df %>%
      dplyr::mutate(
        .LTER_KEY = normalize_lter_key(LTER),
        .STREAM_KEY = normalize_stream_key(Stream_Name),
        .SHP_KEY = if ("Shapefile_Name" %in% names(.)) normalize_site_key(Shapefile_Name) else NA_character_
      ) %>%
      dplyr::filter(
        is.na(.SHP_KEY),
        !is.na(expert_area_km2),
        expert_area_km2 > threshold_km2
      ) %>%
      dplyr::distinct(.LTER_KEY, .STREAM_KEY, .SHP_KEY)

    if (nrow(auto_targets) > 0) {
      message(
        "Automatically routing ", nrow(auto_targets),
        " subset row(s) to HydroSHEDS because shapefile name is missing and drainage area is > ",
        threshold_km2, " km2."
      )
    }
  }

  all_targets <- dplyr::bind_rows(explicit_targets, auto_targets)
  required_cols <- c(".LTER_KEY", ".STREAM_KEY", ".SHP_KEY")
  if (nrow(all_targets) == 0 || !all(required_cols %in% names(all_targets))) {
    return(NULL)
  }

  dplyr::distinct(all_targets, .LTER_KEY, .STREAM_KEY, .SHP_KEY)
}

silica_output_date <- function() {
  stamp <- Sys.getenv("SILICA_OUTPUT_DATE", unset = format(Sys.Date(), "%Y%m%d"))
  stamp <- gsub("[^0-9]", "", stamp)
  if (!grepl("^[0-9]{8}$", stamp)) {
    stop("SILICA_OUTPUT_DATE must resolve to YYYYMMDD, got: ", stamp, call. = FALSE)
  }
  stamp
}

silica_run_label <- function(default = "") {
  label <- Sys.getenv("SILICA_RUN_LABEL", unset = default)
  label <- trimws(as.character(label))
  if (!nzchar(label)) {
    return("")
  }
  label <- tolower(label)
  label <- gsub("[^a-z0-9]+", "-", label)
  label <- gsub("(^-+|-+$)", "", label)
  label
}

silica_output_tag <- function() {
  label <- silica_run_label()
  date_tag <- silica_output_date()
  if (!nzchar(label)) {
    return(date_tag)
  }
  paste(date_tag, label, sep = "_")
}

silica_allow_overwrite <- function() {
  tolower(Sys.getenv("SILICA_ALLOW_OVERWRITE", unset = "false")) %in%
    c("true", "t", "1", "yes", "y")
}

silica_update_years_active <- function() {
  tolower(Sys.getenv("SILICA_RUN_MODE", unset = "")) == "update_years"
}

silica_subset_run_active <- function() {
  nzchar(Sys.getenv("SILICA_SITE_SUBSET_FILE", unset = ""))
}

silica_partial_extract_dir <- function(raw_driver_dir, focal_driver, subset_targets = NULL) {
  base_dir <- file.path(raw_driver_dir, focal_driver, "_partial-extracted")

  isolate_partial_cache <- !is.null(subset_targets) ||
    silica_subset_run_active() ||
    silica_update_years_active()

  if (isolate_partial_cache) {
    dir <- file.path(base_dir, silica_output_tag())
  } else {
    dir <- base_dir
  }

  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  dir
}

silica_target_years <- function() {
  raw_years <- trimws(Sys.getenv("SILICA_TARGET_YEARS", unset = ""))
  start_year <- suppressWarnings(as.integer(Sys.getenv("SILICA_TARGET_YEAR_START", unset = "")))
  end_year <- suppressWarnings(as.integer(Sys.getenv("SILICA_TARGET_YEAR_END", unset = "")))

  years <- integer(0)

  if (nzchar(raw_years)) {
    parts <- unlist(strsplit(raw_years, ","))
    parts <- trimws(parts)
    years <- suppressWarnings(as.integer(parts))
    years <- years[!is.na(years)]
  } else if (!is.na(start_year) && !is.na(end_year)) {
    years <- seq.int(min(start_year, end_year), max(start_year, end_year))
  } else if (!is.na(start_year)) {
    years <- start_year
  } else if (!is.na(end_year)) {
    years <- end_year
  }

  sort(unique(years))
}

silica_full_record_start_year <- function() {
  # Use the first full year shared by the legacy dynamic drivers in this workflow.
  # Snow can retain a partial 2001 year, but 2002 is the first full legacy year.
  2002L
}

silica_full_record_end_year <- function(default_end_year = 2024L) {
  target_years <- silica_target_years()
  if (length(target_years)) {
    return(as.integer(max(target_years)))
  }
  as.integer(default_end_year)
}

silica_full_record_label <- function(default_end_year = 2024L) {
  paste0(
    "full_record_",
    silica_full_record_start_year(),
    "_through_",
    silica_full_record_end_year(default_end_year)
  )
}

filter_target_year_rows <- function(df, year_col = "year") {
  if (!year_col %in% names(df)) {
    return(df)
  }

  target_years <- silica_target_years()
  if (!length(target_years)) {
    return(df)
  }

  year_vals <- suppressWarnings(as.integer(as.character(df[[year_col]])))
  df[!is.na(year_vals) & year_vals %in% target_years, , drop = FALSE]
}

filter_target_region_year_rows <- function(df, driver, region_col = "region", year_col = "year") {
  target_file <- trimws(Sys.getenv("SILICA_TARGET_REGION_YEAR_FILE", unset = ""))
  if (!nzchar(target_file)) {
    return(df)
  }
  if (!file.exists(target_file)) {
    stop("SILICA_TARGET_REGION_YEAR_FILE does not exist: ", target_file, call. = FALSE)
  }
  if (!all(c(region_col, year_col) %in% names(df))) {
    return(df)
  }

  target_list <- read.csv(target_file, stringsAsFactors = FALSE, check.names = FALSE)
  required <- c("driver", "region", "year")
  missing_cols <- setdiff(required, names(target_list))
  if (length(missing_cols)) {
    stop(
      "Target region-year file is missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  target_list <- target_list %>%
    dplyr::transmute(
      driver = normalize_site_key(driver),
      region = normalize_region_key(region),
      year = suppressWarnings(as.integer(year))
    ) %>%
    dplyr::filter(
      driver == normalize_site_key(.env$driver),
      !is.na(region),
      nzchar(region),
      !is.na(year)
    ) %>%
    dplyr::distinct()

  if (nrow(target_list) == 0) {
    warning("No target region-year rows for driver ", driver, ". Keeping zero rows.", call. = FALSE)
    return(df[0, , drop = FALSE])
  }

  df_keyed <- df %>%
    dplyr::mutate(
      .target_region = normalize_region_key(.data[[region_col]]),
      .target_year = suppressWarnings(as.integer(as.character(.data[[year_col]])))
    ) %>%
    dplyr::inner_join(
      target_list,
      by = c(".target_region" = "region", ".target_year" = "year")
    ) %>%
    dplyr::select(-dplyr::any_of(c(".target_region", ".target_year", "driver")))

  message(
    "Dynamic files restricted by region-year target file for ",
    driver,
    ": ",
    nrow(df_keyed),
    " file rows"
  )
  df_keyed
}

silica_should_merge_output <- function(subset_targets = NULL, output_path = NULL) {
  merge_subset <- !is.null(subset_targets) &&
    tolower(Sys.getenv("SILICA_MERGE_SUBSET_OUTPUTS", "false")) == "true"

  merge_updates <- tolower(Sys.getenv("SILICA_MERGE_OUTPUTS", "false")) == "true"

  should_merge <- (merge_subset || merge_updates)
  if (!should_merge) {
    return(FALSE)
  }

  if (is.null(output_path)) {
    return(TRUE)
  }

  file.exists(output_path)
}

silica_assert_new_output <- function(path) {
  if (silica_allow_overwrite()) {
    return(path)
  }

  ext <- tolower(tools::file_ext(path))
  exists_now <- if (ext == "shp") {
    stem <- sub("\\.[^.]+$", "", path)
    any(file.exists(paste0(stem, c(".shp", ".shx", ".dbf", ".prj", ".cpg"))))
  } else {
    file.exists(path)
  }

  if (exists_now) {
    stop(
      "Refusing to overwrite existing output: ", path,
      ". Change SILICA_RUN_LABEL or SILICA_OUTPUT_DATE, or set SILICA_ALLOW_OVERWRITE=TRUE.",
      call. = FALSE
    )
  }

  path
}

silica_tagged_output_file <- function(output_dir, stem, ext = "csv") {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  silica_assert_new_output(
    file.path(output_dir, paste0(stem, "_", silica_output_tag(), ".", ext))
  )
}

silica_find_existing_output <- function(output_dir, stem, ext = "csv") {
  tagged_now <- file.path(output_dir, paste0(stem, "_", silica_output_tag(), ".", ext))
  if (file.exists(tagged_now)) {
    return(tagged_now)
  }

  fixed_old <- file.path(output_dir, paste0(stem, ".", ext))
  if (file.exists(fixed_old)) {
    return(fixed_old)
  }

  pattern <- paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", stem), "_[0-9]{8}(?:_[a-z0-9-]+)?\\.", ext, "$")
  candidates <- list.files(output_dir, pattern = pattern, full.names = TRUE)
  if (length(candidates)) {
    return(candidates[which.max(file.info(candidates)$mtime)])
  }

  tagged_now
}

silica_output_dir_from_root <- function(root_path, env_var, path_candidates) {
  env_dir <- Sys.getenv(env_var, unset = NA_character_)
  candidates <- c(env_dir, path_candidates)
  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])
  hit <- candidates[dir.exists(candidates)][1]
  if (is.na(hit) || !nzchar(hit)) {
    return(candidates[[1]])
  }
  hit
}

silica_review_root <- function(root_path) {
  env_dir <- Sys.getenv("SILICA_QA_ROOT", unset = "")
  if (nzchar(env_dir)) {
    return(env_dir)
  }
  file.path(root_path, "review")
}

silica_driver_output_file <- function(root_path, stem, ext = "csv") {
  extracted_dir <- silica_output_dir_from_root(
    root_path = root_path,
    env_var = "SILICA_EXTRACTED_DIR",
    path_candidates = c(
      file.path(root_path, "extracted-data"),
      file.path(root_path, "silica-shapefiles", "extracted-data")
    )
  )
  silica_tagged_output_file(extracted_dir, stem, ext)
}

silica_sitecoord_output_file <- function(root_path, stem, ext = "shp") {
  site_coord_dir <- silica_output_dir_from_root(
    root_path = root_path,
    env_var = "SILICA_SITE_COORD_DIR",
    path_candidates = c(
      file.path(root_path, "site-coordinates"),
      file.path(root_path, "silica-shapefiles", "site-coordinates")
    )
  )
  silica_tagged_output_file(site_coord_dir, stem, ext)
}

silica_sitecoord_existing_file <- function(root_path, stem, ext = "shp") {
  site_coord_dir <- silica_output_dir_from_root(
    root_path = root_path,
    env_var = "SILICA_SITE_COORD_DIR",
    path_candidates = c(
      file.path(root_path, "site-coordinates"),
      file.path(root_path, "silica-shapefiles", "site-coordinates")
    )
  )
  silica_find_existing_output(site_coord_dir, stem, ext)
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
      .STREAM_KEY = normalize_stream_key(.data[[stream_col]]),
      .SHP_KEY = normalize_site_key(shp_vals)
    )

  target_stream <- subset_targets %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_stream_key(Stream_Name)
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

    # Existing CSV outputs often round-trip dates/times or all-NA columns into
    # looser types than a fresh subset patch. When we can't derive a stable
    # common prototype, fall back to character so row-binding remains safe.
    if (is.null(ptype) || inherits(ptype, "Date") || inherits(ptype, "POSIXt")) {
      existing_df[[nm]] <- as.character(existing_df[[nm]])
      patch_df[[nm]] <- as.character(patch_df[[nm]])
      next
    }

    cast_ok <- TRUE
    existing_cast <- tryCatch(
      vctrs::vec_cast(existing_df[[nm]], ptype),
      error = function(e) {
        cast_ok <<- FALSE
        NULL
      }
    )
    patch_cast <- tryCatch(
      vctrs::vec_cast(patch_df[[nm]], ptype),
      error = function(e) {
        cast_ok <<- FALSE
        NULL
      }
    )

    if (!cast_ok) {
      existing_df[[nm]] <- as.character(existing_df[[nm]])
      patch_df[[nm]] <- as.character(patch_df[[nm]])
      next
    }

    existing_df[[nm]] <- existing_cast
    patch_df[[nm]] <- patch_cast
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
  df <- clean_lter_column(df)

  merge_subset <- silica_should_merge_output(
    subset_targets = subset_targets,
    output_path = output_path
  )

  if (merge_subset) {
    existing_df <- read.csv(output_path, stringsAsFactors = FALSE, check.names = FALSE)
    existing_df <- clean_lter_column(existing_df)
    df <- merge_subset_rows(existing_df, df, key_cols = key_cols)
    df <- clean_lter_column(df)
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
      .STREAM_KEY = normalize_stream_key(Stream_Name),
      .SHP_KEY = normalize_site_key(Shapefile_Name)
    )

  sheds_aug <- sheds %>%
    dplyr::mutate(
      .ROW_ID = dplyr::row_number(),
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_stream_key(Stream_Name),
      .SHP_KEY = normalize_site_key(Shapefile_Name)
    )

  target_stream <- subset_targets %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_stream_key(Stream_Name)
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

  if (inherits(sheds_keep, "sf")) {
    empty_geom <- sf::st_is_empty(sheds_keep)
    empty_geom[is.na(empty_geom)] <- TRUE

    if (any(empty_geom)) {
      empty_counts <- sort(table(sheds_keep$LTER[empty_geom]), decreasing = TRUE)
      warning(
        "Dropping ", sum(empty_geom), " empty watershed geometries from the target subset: ",
        paste(paste0(names(empty_counts), "=", as.integer(empty_counts)), collapse = ", "),
        call. = FALSE
      )
      sheds_keep <- sheds_keep[!empty_geom, , drop = FALSE]
    }
  }

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

# Lookup coordinates for subset targets from the cleaned site table when
# callers did not include Latitude/Longitude directly in the subset CSV.
silica_lookup_subset_coords <- function(subset_targets) {
  if (is.null(subset_targets) || nrow(subset_targets) == 0) {
    return(subset_targets)
  }

  has_coords <- all(c("Latitude", "Longitude") %in% names(subset_targets))
  if (has_coords) {
    return(subset_targets)
  }

  if (!exists("resolve_silica_data_root", mode = "function") ||
      !exists("silica_site_coordinates_dir", mode = "function")) {
    return(subset_targets)
  }

  site_coord_dir <- silica_site_coordinates_dir(resolve_silica_data_root())
  if (!exists("read_silica_site_reference", mode = "function") &&
      !file.exists(file.path(site_coord_dir, "silica-coords_RAW.xlsx"))) {
    return(subset_targets)
  }

  ref_coords <- tryCatch(
    {
      if (exists("read_silica_site_reference", mode = "function")) {
        read_silica_site_reference(site_coord_dir)
      } else {
        readxl::read_excel(file.path(site_coord_dir, "silica-coords_RAW.xlsx"))
      }
    } %>%
      dplyr::transmute(
        .LTER_KEY = normalize_lter_key(LTER),
        .STREAM_KEY = normalize_stream_key(Stream_Name),
        Latitude = suppressWarnings(as.numeric(Latitude)),
        Longitude = suppressWarnings(as.numeric(Longitude))
      ) %>%
      dplyr::filter(!is.na(.LTER_KEY), !is.na(.STREAM_KEY)) %>%
      dplyr::distinct(.LTER_KEY, .STREAM_KEY, .keep_all = TRUE),
    error = function(e) NULL
  )

  if (is.null(ref_coords) || nrow(ref_coords) == 0) {
    return(subset_targets)
  }

  subset_targets %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_stream_key(Stream_Name)
    ) %>%
    dplyr::left_join(ref_coords, by = c(".LTER_KEY", ".STREAM_KEY")) %>%
    dplyr::select(-.LTER_KEY, -.STREAM_KEY)
}

infer_dynamic_region_from_coords <- function(latitude, longitude) {
  lat <- suppressWarnings(as.numeric(latitude))
  lon <- suppressWarnings(as.numeric(longitude))

  out <- rep(NA_character_, length(lat))

  out[!is.na(lat) & !is.na(lon) &
        lon >= -82 & lon <= -45 &
        lat >= -20 & lat <= 12] <- "amazon"

  out[!is.na(lat) & !is.na(lon) &
        lon >= -130 & lon <= -60 &
        lat >= 15 & lat <= 55] <- "north-america-usa"

  out[!is.na(lat) & !is.na(lon) &
        lon >= 110 & lon <= 160 &
        lat >= -45 & lat <= -10] <- "australia"

  out[!is.na(lat) & !is.na(lon) &
        lon >= -68 & lon <= -64 &
        lat >= 17 & lat <= 19] <- "puerto-rico"

  out[!is.na(lat) & !is.na(lon) &
        lon >= 8 & lon <= 32 &
        lat >= -12 & lat <= 8] <- "congo"

  out
}

# Restrict dynamic-driver region loops when a subset is active
resolve_target_regions <- function(subset_targets = NULL, default_regions) {
  forced_regions <- trimws(Sys.getenv("SILICA_FORCE_TARGET_REGIONS", unset = ""))
  if (nzchar(forced_regions)) {
    target_regions <- unlist(strsplit(forced_regions, ",", fixed = TRUE), use.names = FALSE)
    target_regions <- normalize_region_key(target_regions)
    target_regions <- sort(unique(target_regions[!is.na(target_regions) & nzchar(target_regions)]))
    target_regions <- intersect(default_regions, target_regions)

    if (!length(target_regions)) {
      stop(
        "SILICA_FORCE_TARGET_REGIONS did not match any known dynamic-driver regions. Got: ",
        forced_regions,
        call. = FALSE
      )
    }

    message("Dynamic regions forced via SILICA_FORCE_TARGET_REGIONS to: ",
            paste(target_regions, collapse = ", "))
    return(target_regions)
  }

  if (is.null(subset_targets)) {
    return(default_regions)
  }

  subset_targets <- silica_lookup_subset_coords(subset_targets)

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

  stream_region_overrides <- data.frame(
    .LTER_KEY = c("lmp", "krr"),
    .STREAM_KEY = c("nor27", "s65c"),
    region = c("north-america-usa", "north-america-usa"),
    stringsAsFactors = FALSE
  )

  region_map <- data.frame(
    LTER_KEY = c(
      "amazon",
      "and",
      "arc",
      "australia",
      "bcczo",
      "canada",
      "coloradoalpine",
      "congo-basin",
      "east riversfa",
      "finnish environmental institute",
      "germany",
      "gro",
      "guadeloupe",
      "hbr",
      "hybam",
      "knz",
      "krr",
      "lmp",
      "luq",
      "mali",
      "md",
      "niva",
      "nwt",
      "pie",
      "seine",
      "sweden",
      "uk",
      "umr",
      "usgs",
      "walker branch",
      "westernaustralia"
    ),
    region = c(
      "amazon",
      "north-america-usa",
      "north-america-arctic",
      "australia",
      "north-america-usa",
      "canada",
      "north-america-usa",
      "congo",
      "north-america-usa",
      "scandinavia",
      "germany",
      "amazon",
      "puerto-rico",
      "north-america-usa",
      "amazon",
      "north-america-usa",
      "north-america-usa",
      "north-america-usa",
      "puerto-rico",
      "mali",
      "australia",
      "scandinavia",
      "north-america-usa",
      "north-america-usa",
      "germany",
      "scandinavia",
      "united-kingdom",
      "north-america-usa",
      "north-america-usa",
      "north-america-usa",
      "australia"
    ),
    stringsAsFactors = FALSE
  )

  region_targets <- subset_targets %>%
    dplyr::mutate(
      .LTER_KEY = normalize_lter_key(LTER),
      .STREAM_KEY = normalize_stream_key(Stream_Name),
      coord_region = infer_dynamic_region_from_coords(
        latitude = if ("Latitude" %in% names(.)) Latitude else NA_real_,
        longitude = if ("Longitude" %in% names(.)) Longitude else NA_real_
      )
    ) %>%
    dplyr::left_join(stream_region_overrides, by = c(".LTER_KEY", ".STREAM_KEY")) %>%
    dplyr::left_join(region_map, by = c(".LTER_KEY" = "LTER_KEY")) %>%
    dplyr::transmute(
      LTER,
      region = dplyr::coalesce(region.x, coord_region, region.y)
    ) %>%
    dplyr::distinct()

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
