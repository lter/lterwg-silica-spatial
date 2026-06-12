#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
})

date_tag <- Sys.getenv("SILICA_FINAL_HARMONIZED_DATE", unset = "20260608")
year_min <- as.integer(Sys.getenv("SILICA_FINAL_YEAR_MIN", unset = "2002"))
year_max <- as.integer(Sys.getenv("SILICA_FINAL_YEAR_MAX", unset = "2025"))
excluded_model_columns <- c("NOx", "P")
dsi_output_columns <- c("FNConc", "FNYield", "GenConc", "GenYield")
silicon_molar_mass_kg_per_kmol <- 28.0855

box_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn"
data_root <- file.path(box_root, "spatial-data-extractions")

march_harmonized_file <- file.path(
  box_root,
  "Spatial_controls_GRL",
  "GRL_Materials",
  "GRL_submission_v1",
  "code_inputs",
  "AllDrivers_Harmonized_Yearly_filtered_5_years.csv"
)
current_site_file <- file.path(
  data_root,
  "review",
  "harmonization",
  "harmonized-spatial-drivers_20260607.csv"
)
current_annual_file <- file.path(
  data_root,
  "review",
  "harmonization",
  "harmonized-spatial-drivers-annual_20260607.csv"
)
wrtds_annual_file <- file.path(data_root, "master", "Full_Results_WRTDS_kalman_annual.csv")
raw_chem_file <- file.path(data_root, "master", "20260105_masterdata_chem.csv")
site_ref_file <- file.path(data_root, "master", "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv")
esom_sites_file <- file.path(box_root, "ESOM", "spatial-data", "ESOM_Sites.csv")
silica_shapefile_root <- file.path(data_root, "silica-shapefiles")

full_out_dir <- file.path(data_root, "final-data", "full-dataset")
esom_out_dir <- file.path(data_root, "final-data", "esom")
audit_dir <- file.path(data_root, "final-data", "audit-summaries")
dir.create(full_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(esom_out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

read_csv_clean <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  blank_names <- is.na(names(x)) | names(x) == ""
  names(x)[blank_names] <- paste0("source_col_", seq_len(sum(blank_names)))
  names(x) <- make.unique(names(x))
  x
}

norm_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  gsub("_+", "_", x)
}

read_lter_aliases <- function() {
  path <- "tools/lter_name_aliases.csv"
  if (!file.exists(path)) {
    return(data.frame(source = character(), target = character()))
  }
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  x <- x[x$field == "LTER", , drop = FALSE]
  data.frame(
    source = norm_key(x$source_value),
    target = norm_key(x$target_value),
    stringsAsFactors = FALSE
  )
}

read_stream_aliases <- function() {
  path <- "tools/stream_name_aliases.csv"
  if (!file.exists(path)) {
    return(data.frame(lter = character(), source = character(), target = character()))
  }
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  data.frame(
    lter = norm_key(x$LTER),
    source = norm_key(x$alias_stream_name),
    target = norm_key(x$canonical_stream_name),
    stringsAsFactors = FALSE
  )
}

lter_aliases <- read_lter_aliases()
stream_aliases <- read_stream_aliases()

canonical_lter_key <- function(lter) {
  out <- norm_key(lter)
  hit <- match(out, lter_aliases$source)
  out[!is.na(hit)] <- lter_aliases$target[hit[!is.na(hit)]]
  out
}

canonical_stream_key <- function(lter_key, stream_name) {
  out <- norm_key(stream_name)
  if (!nrow(stream_aliases)) {
    return(out)
  }

  for (i in seq_len(nrow(stream_aliases))) {
    hit <- lter_key == stream_aliases$lter[[i]] &
      (out == stream_aliases$source[[i]] | out == stream_aliases$target[[i]])
    out[hit] <- stream_aliases$target[[i]]
  }
  out
}

split_stream_id <- function(stream_id) {
  stream_id <- as.character(stream_id)
  has_sep <- grepl("__", stream_id, fixed = TRUE)
  lter <- ifelse(has_sep, sub("__.*$", "", stream_id), NA_character_)
  stream <- ifelse(has_sep, sub("^[^_]*__", "", stream_id), stream_id)
  data.frame(LTER = lter, Stream_Name = stream, stringsAsFactors = FALSE)
}

site_key_from_parts <- function(lter, stream_name) {
  lter_key <- canonical_lter_key(lter)
  stream_key <- canonical_stream_key(lter_key, stream_name)
  paste(lter_key, stream_key, sep = "||")
}

site_key_from_stream_id <- function(stream_id) {
  parts <- split_stream_id(stream_id)
  site_key_from_parts(parts$LTER, parts$Stream_Name)
}

coerce_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

is_present <- function(x) {
  if (is.character(x)) {
    return(!is.na(x) & trimws(x) != "")
  }
  !is.na(x)
}

first_present <- function(x) {
  v <- x[is_present(x)]
  if (length(v)) v[[1]] else NA
}

first_present_numeric <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  v <- x[!is.na(x)]
  if (length(v)) v[[1]] else NA_real_
}

prefer_old <- function(old, new) {
  out <- old
  use_new <- !is_present(out) & is_present(new)
  out[use_new] <- new[use_new]
  out
}

forbidden_column_summary <- function(x) {
  bad <- intersect(excluded_model_columns, names(x))
  if (length(bad)) {
    return(paste(bad, collapse = ";"))
  }
  "NONE"
}

shape_area_km2_from_file <- function(path) {
  tryCatch({
    x <- sf::st_read(path, quiet = TRUE)
    if (is.na(sf::st_crs(x))) {
      return(NA_real_)
    }
    bbox <- sf::st_bbox(x)
    if (isTRUE(sf::st_is_longlat(x)) &&
      (bbox[["xmin"]] < -180 || bbox[["xmax"]] > 180 || bbox[["ymin"]] < -90 || bbox[["ymax"]] > 90)) {
      return(NA_real_)
    }
    x <- sf::st_make_valid(x)
    x <- sf::st_transform(x, 6933)
    area_km2 <- sum(as.numeric(sf::st_area(x)), na.rm = TRUE) / 1e6
    if (!is.finite(area_km2) || area_km2 <= 0 || area_km2 > 1e7) {
      return(NA_real_)
    }
    area_km2
  }, error = function(e) {
    NA_real_
  })
}

read_combined_watershed_area <- function(path, priority) {
  if (!file.exists(path)) {
    return(data.frame())
  }
  x <- sf::st_drop_geometry(sf::st_read(path, quiet = TRUE))
  shp <- if ("shp_nm" %in% names(x)) x$shp_nm else rep(NA_character_, nrow(x))
  real_area <- if ("real_area" %in% names(x)) x$real_area else rep(NA_real_, nrow(x))
  real_ar <- if ("real_ar" %in% names(x)) x$real_ar else rep(NA_real_, nrow(x))
  exp_area <- if ("exp_area" %in% names(x)) x$exp_area else rep(NA_real_, nrow(x))
  exp_are <- if ("exp_are" %in% names(x)) x$exp_are else rep(NA_real_, nrow(x))

  data.frame(
    shp_key = norm_key(shp),
    shapefile_name = shp,
    drainage_area_shape = ifelse(
      is.na(suppressWarnings(as.numeric(real_area))),
      suppressWarnings(as.numeric(real_ar)),
      suppressWarnings(as.numeric(real_area))
    ),
    drainage_area_expected = ifelse(
      is.na(suppressWarnings(as.numeric(exp_area))),
      suppressWarnings(as.numeric(exp_are)),
      suppressWarnings(as.numeric(exp_area))
    ),
    drainage_area_shape_source = basename(path),
    drainage_area_shape_source_type = "combined_watershed_polygon",
    drainage_area_shape_priority = priority,
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(shp_key), shp_key != "", !is.na(drainage_area_shape))
}

build_watershed_area_lookup <- function(site_rows) {
  combined_paths <- c(
    file.path(silica_shapefile_root, "site-coordinates", "silica-watersheds_hydrosheds_DR_2.shp"),
    file.path(silica_shapefile_root, "site-coordinates", "silica-watersheds.shp"),
    file.path(silica_shapefile_root, "site-coordinates", "silica-watersheds_20260512.shp"),
    file.path(silica_shapefile_root, "site-coordinates", "silica-watersheds_hydrosheds.shp")
  )

  combined <- do.call(rbind, Map(read_combined_watershed_area, combined_paths, seq_along(combined_paths)))
  if (!nrow(combined)) {
    combined <- data.frame(
      shp_key = character(),
      shapefile_name = character(),
      drainage_area_shape = numeric(),
      drainage_area_expected = numeric(),
      drainage_area_shape_source = character(),
      drainage_area_shape_source_type = character(),
      drainage_area_shape_priority = integer(),
      stringsAsFactors = FALSE
    )
  }

  needed <- site_rows %>%
    transmute(shp_key = norm_key(Shapefile_Name), shapefile_name = Shapefile_Name) %>%
    filter(!is.na(shp_key), shp_key != "") %>%
    distinct(shp_key, .keep_all = TRUE)

  shp_files <- list.files(silica_shapefile_root, pattern = "[.]shp$", recursive = TRUE, full.names = TRUE)
  individual_index <- data.frame(
    individual_path = shp_files,
    shp_key = norm_key(tools::file_path_sans_ext(basename(shp_files))),
    stringsAsFactors = FALSE
  ) %>%
    filter(!grepl("/site-coordinates/", individual_path, fixed = TRUE)) %>%
    arrange(shp_key, individual_path) %>%
    distinct(shp_key, .keep_all = TRUE)

  individual_targets <- needed %>%
    left_join(individual_index, by = "shp_key") %>%
    filter(!is.na(individual_path))

  individual <- data.frame()
  if (nrow(individual_targets)) {
    individual <- individual_targets %>%
      rowwise() %>%
      mutate(drainage_area_shape = shape_area_km2_from_file(individual_path)) %>%
      ungroup() %>%
      transmute(
        shp_key,
        shapefile_name,
        drainage_area_shape,
        drainage_area_expected = NA_real_,
        drainage_area_shape_source = individual_path,
        drainage_area_shape_source_type = "individual_shapefile_polygon",
        drainage_area_shape_priority = 100L
      ) %>%
      filter(!is.na(drainage_area_shape))
  }

  lookup <- bind_rows(combined, individual) %>%
    arrange(shp_key, drainage_area_shape_priority) %>%
    group_by(shp_key) %>%
    summarise(
      shapefile_name = first_present(shapefile_name),
      drainage_area_shape = first_present_numeric(drainage_area_shape),
      drainage_area_expected = first_present_numeric(drainage_area_expected),
      drainage_area_shape_source = first_present(drainage_area_shape_source),
      drainage_area_shape_source_type = first_present(drainage_area_shape_source_type),
      .groups = "drop"
    )

  site_rows %>%
    transmute(.site_key, shp_key = norm_key(Shapefile_Name), Shapefile_Name) %>%
    left_join(lookup, by = "shp_key") %>%
    select(
      .site_key,
      Shapefile_Name,
      drainage_area_shape,
      drainage_area_expected,
      drainage_area_shape_source,
      drainage_area_shape_source_type
    ) %>%
    distinct(.site_key, .keep_all = TRUE)
}

land_classes <- c(
  "Bare",
  "Cropland",
  "Forest",
  "Grassland_Shrubland",
  "Ice_Snow",
  "Impervious",
  "Salt_Water",
  "Tidal_Wetland",
  "Water",
  "Wetland_Marsh"
)

snow_months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
monthly_snow_cols <- as.vector(rbind(
  paste0("snow_", snow_months, "_avg_prop_area"),
  paste0("snow_", snow_months, "_num_days")
))

required <- c(
  current_site_file,
  current_annual_file,
  wrtds_annual_file,
  raw_chem_file,
  site_ref_file,
  esom_sites_file,
  march_harmonized_file
)
missing <- required[!file.exists(required)]
if (length(missing)) {
  stop("Missing required files:\n- ", paste(missing, collapse = "\n- "), call. = FALSE)
}

current_site <- read_csv_clean(current_site_file)
current_annual <- read_csv_clean(current_annual_file)
wrtds_annual <- read_csv_clean(wrtds_annual_file)
raw_chem <- read_csv_clean(raw_chem_file)
site_ref <- read_csv_clean(site_ref_file)
march <- read_csv_clean(march_harmonized_file)

current_site$.site_key <- site_key_from_stream_id(current_site$Stream_ID)
current_annual$.site_key <- site_key_from_stream_id(current_annual$Stream_ID)
site_ref$.site_key <- site_key_from_parts(site_ref$LTER, site_ref$Stream_Name)
march$.site_key <- site_key_from_stream_id(march$Stream_ID)

current_site_one <- current_site %>%
  filter(!is.na(.site_key), .site_key != "||") %>%
  arrange(.site_key) %>%
  distinct(.site_key, .keep_all = TRUE)

watershed_area_one <- build_watershed_area_lookup(current_site_one)

site_ref_one <- site_ref %>%
  filter(!is.na(.site_key), .site_key != "||") %>%
  transmute(.site_key, drainage_area_ref = coerce_num(drainSqKm)) %>%
  group_by(.site_key) %>%
  summarise(drainage_area_ref = first_present(drainage_area_ref), .groups = "drop")

site_overlay <- current_site_one %>%
  transmute(
    .site_key,
    elevation = elevation_mean_m,
    RBI = RBI,
    RCS = if ("RCS" %in% names(current_site_one)) RCS else recession_slope,
    basin_slope = basin_slope_mean_degree,
    permafrost = pmin(pmax(coerce_num(permafrost_mean_m), 0), 1) * 100,
    major_rock = major_rock,
    rocks_volcanic = rocks_volcanic,
    rocks_sedimentary = rocks_sedimentary,
    rocks_carbonate_evaporite = rocks_carbonate_evaporite,
    rocks_metamorphic = rocks_metamorphic,
    rocks_plutonic = rocks_plutonic
  )

annual_base <- current_annual %>%
  filter(Year >= year_min, Year <= year_max) %>%
  transmute(
    .site_key,
    Stream_ID,
    Year = as.integer(Year),
    precip = precip_mm_per_day,
    Q = Q,
    temp = temp_degC,
    snow_cover = snow_max_prop_area,
    snow_num_days = snow_num_days,
    npp = npp_kgC_m2_year,
    evapotrans = evapotrans_kg_m2,
    greenup_day = greenup_cycle0_doy
  ) %>%
  filter(!is.na(.site_key), .site_key != "||", !is.na(Year)) %>%
  arrange(.site_key, Year) %>%
  group_by(.site_key, Year) %>%
  summarise(
    Stream_ID = first_present(Stream_ID),
    across(c(precip, Q, temp, snow_cover, snow_num_days, npp, evapotrans, greenup_day), first_present),
    .groups = "drop"
  )

march_wrtDS <- march %>%
  transmute(
    .site_key,
    Year = as.integer(Year),
    FNConc_march = FNConc,
    FNYield_march = FNYield,
    GenConc_march = GenConc,
    GenYield_march = GenYield
  ) %>%
  filter(!is.na(.site_key), .site_key != "||", !is.na(Year)) %>%
  group_by(.site_key, Year) %>%
  summarise(across(everything(), first_present), .groups = "drop")

wrtds_dsi <- wrtds_annual %>%
  filter(chemical == "DSi") %>%
  mutate(.site_key = site_key_from_parts(LTER, Stream_Name)) %>%
  transmute(
    .site_key,
    Year = as.integer(Year),
    drainage_area_wrtds = coerce_num(drainSqKm),
    FNConc_wrtds = coerce_num(FNConc_mgL),
    FNYield_wrtds = coerce_num(FNYield_10_6kmol_yr_km2) * 1e6 * silicon_molar_mass_kg_per_kmol,
    GenConc_wrtds = coerce_num(GenConc_mgL),
    GenYield_wrtds = coerce_num(GenYield_10_6kmol_yr_km2) * 1e6 * silicon_molar_mass_kg_per_kmol
  ) %>%
  filter(!is.na(.site_key), .site_key != "||", !is.na(Year)) %>%
  group_by(.site_key, Year) %>%
  summarise(across(everything(), first_present), .groups = "drop")

out <- annual_base %>%
  left_join(site_overlay, by = ".site_key") %>%
  left_join(site_ref_one, by = ".site_key") %>%
  left_join(watershed_area_one, by = ".site_key") %>%
  left_join(wrtds_dsi, by = c(".site_key", "Year")) %>%
  left_join(march_wrtDS, by = c(".site_key", "Year")) %>%
  mutate(
    drainage_area = case_when(
      is_present(drainage_area_ref) ~ drainage_area_ref,
      is_present(drainage_area_shape) ~ drainage_area_shape,
      TRUE ~ drainage_area_wrtds
    ),
    drainage_area_source = case_when(
      is_present(drainage_area_ref) ~ "site_reference_drainSqKm",
      is_present(drainage_area_shape) ~ drainage_area_shape_source_type,
      is_present(drainage_area_wrtds) ~ "wrtds_annual_drainSqKm",
      TRUE ~ "missing"
    ),
    FNConc = prefer_old(FNConc_march, FNConc_wrtds),
    FNYield = prefer_old(FNYield_march, FNYield_wrtds),
    GenConc = prefer_old(GenConc_march, GenConc_wrtds),
    GenYield = prefer_old(GenYield_march, GenYield_wrtds)
  )

land_years <- sort(unique(as.integer(sub(
  "^gee_glc_([0-9]{4})_.*$", "\\1",
  grep("^gee_glc_[0-9]{4}_", names(current_site_one), value = TRUE)
))))
land_years <- land_years[!is.na(land_years)]

for (class_name in land_classes) {
  target_col <- paste0("land_", class_name)
  out[[target_col]] <- NA_real_
  if (!length(land_years)) {
    next
  }

  for (year in sort(unique(out$Year))) {
    source_year <- max(land_years[land_years <= year], na.rm = TRUE)
    if (!is.finite(source_year)) {
      next
    }
    source_col <- paste0("gee_glc_", source_year, "_", class_name)
    if (!source_col %in% names(current_site_one)) {
      next
    }
    year_rows <- which(out$Year == year)
    site_match <- match(out$.site_key[year_rows], current_site_one$.site_key)
    has_site <- !is.na(site_match)
    if (any(has_site)) {
      out[[target_col]][year_rows[has_site]] <- coerce_num(current_site_one[[source_col]][site_match[has_site]]) * 100
    }
  }
}

land_cols <- paste0("land_", land_classes)
land_matrix <- as.data.frame(lapply(out[land_cols], coerce_num))
out$major_land <- apply(land_matrix, 1, function(x) {
  if (all(is.na(x))) {
    return(NA_character_)
  }
  sub("^land_", "", names(x)[which.max(replace(x, is.na(x), -Inf))])
})

base_cols <- c(
  "FNConc", "Stream_ID", "Year", "drainage_area", "precip", "Q", "temp",
  "snow_cover", "snow_num_days", "npp", "evapotrans", "greenup_day",
  "permafrost", "elevation", "RBI", "RCS", "basin_slope",
  "FNYield", "GenConc", "GenYield", "major_rock", "major_land",
  "rocks_volcanic", "rocks_sedimentary", "rocks_carbonate_evaporite",
  "rocks_metamorphic", "rocks_plutonic", land_cols
)
base_cols <- setdiff(base_cols, excluded_model_columns)
out_base_with_source <- out[, c(".site_key", base_cols, "drainage_area_source"), drop = FALSE]
out_base <- out_base_with_source[, c(".site_key", base_cols), drop = FALSE]

add_monthly_snow <- function(x) {
  snow_cols <- intersect(monthly_snow_cols, names(current_site_one))
  if (!length(snow_cols)) {
    return(x)
  }
  snow_overlay <- current_site_one[, c(".site_key", snow_cols), drop = FALSE]
  x %>%
    left_join(snow_overlay, by = ".site_key")
}

raw_dsi <- raw_chem %>%
  filter(variable == "DSi") %>%
  mutate(
    .site_key = site_key_from_parts(LTER, Stream_Name),
    Year = as.integer(format(as.Date(date), "%Y")),
    value = coerce_num(value)
  ) %>%
  filter(!is.na(.site_key), .site_key != "||", !is.na(Year), !is.na(value)) %>%
  group_by(.site_key, Year) %>%
  summarise(
    raw_DSi_n = dplyr::n(),
    raw_DSi_mean_uM = mean(value, na.rm = TRUE),
    raw_DSi_median_uM = median(value, na.rm = TRUE),
    raw_DSi_sd_uM = if (dplyr::n() > 1L) sd(value, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )

full_base <- out_base %>% select(-.site_key)
full_expanded_snow <- add_monthly_snow(out_base) %>% select(-.site_key)
full_raw_dsi <- out_base %>%
  left_join(raw_dsi, by = c(".site_key", "Year")) %>%
  select(-.site_key)
full_spatial_only <- add_monthly_snow(out_base_with_source) %>%
  select(-.site_key) %>%
  select(-any_of(dsi_output_columns))

esom_sites <- read_csv_clean(esom_sites_file)
esom_sites$.site_key <- site_key_from_parts(esom_sites$LTER, esom_sites$Stream_Name)
esom_unique <- esom_sites %>%
  filter(!is.na(.site_key), .site_key != "||") %>%
  distinct(.site_key, .keep_all = TRUE)
esom_duplicates <- esom_sites %>%
  filter(duplicated(.site_key) | duplicated(.site_key, fromLast = TRUE)) %>%
  arrange(.site_key)
matched_esom_keys <- intersect(unique(out_base$.site_key), esom_unique$.site_key)
esom_missing <- esom_unique %>%
  filter(!.site_key %in% matched_esom_keys) %>%
  select(-.site_key)
esom_base <- out_base %>%
  filter(.site_key %in% esom_unique$.site_key) %>%
  select(-.site_key)
esom_expanded_snow <- add_monthly_snow(out_base) %>%
  filter(.site_key %in% esom_unique$.site_key) %>%
  select(-.site_key)
esom_spatial_only <- add_monthly_snow(out_base_with_source) %>%
  filter(.site_key %in% esom_unique$.site_key) %>%
  select(-.site_key) %>%
  select(-any_of(dsi_output_columns))

full_out_file <- file.path(full_out_dir, paste0("final_full_harmonized_annual_", date_tag, ".csv"))
full_expanded_snow_file <- file.path(full_out_dir, paste0("final_full_harmonized_annual_expanded_snow_", date_tag, ".csv"))
full_raw_dsi_file <- file.path(full_out_dir, paste0("final_full_harmonized_annual_raw_DSi_", date_tag, ".csv"))
full_spatial_file <- file.path(full_out_dir, paste0("final_full_spatial_drivers_annual_", date_tag, ".csv"))
esom_out_file <- file.path(esom_out_dir, paste0("ESOM_final_harmonized_annual_", date_tag, ".csv"))
esom_expanded_snow_file <- file.path(esom_out_dir, paste0("ESOM_final_harmonized_annual_expanded_snow_", date_tag, ".csv"))
esom_spatial_file <- file.path(esom_out_dir, paste0("ESOM_spatial_drivers_annual_", date_tag, ".csv"))
esom_missing_file <- file.path(esom_out_dir, paste0("ESOM_missing_from_final_harmonized_annual_", date_tag, ".csv"))
esom_duplicate_file <- file.path(esom_out_dir, paste0("ESOM_duplicate_site_keys_final_harmonized_annual_", date_tag, ".csv"))

write.csv(full_base, full_out_file, row.names = FALSE, na = "")
write.csv(full_expanded_snow, full_expanded_snow_file, row.names = FALSE, na = "")
write.csv(full_raw_dsi, full_raw_dsi_file, row.names = FALSE, na = "")
write.csv(full_spatial_only, full_spatial_file, row.names = FALSE, na = "")
write.csv(esom_base, esom_out_file, row.names = FALSE, na = "")
write.csv(esom_expanded_snow, esom_expanded_snow_file, row.names = FALSE, na = "")
write.csv(esom_spatial_only, esom_spatial_file, row.names = FALSE, na = "")
write.csv(esom_missing, esom_missing_file, row.names = FALSE, na = "")
write.csv(esom_duplicates, esom_duplicate_file, row.names = FALSE, na = "")

variable_coverage <- function(x) {
  vars <- setdiff(names(x), c("Stream_ID", "Year"))
  do.call(rbind, lapply(vars, function(v) {
    nonmissing <- !is.na(x[[v]]) & !(is.character(x[[v]]) & trimws(x[[v]]) == "")
    data.frame(
      variable = v,
      nonmissing_rows = sum(nonmissing),
      first_year = if (any(nonmissing)) min(x$Year[nonmissing]) else NA_integer_,
      last_year = if (any(nonmissing)) max(x$Year[nonmissing]) else NA_integer_,
      stringsAsFactors = FALSE
    )
  }))
}

full_summary <- data.frame(
  rows = nrow(full_base),
  cols = ncol(full_base),
  sites = length(unique(full_base$Stream_ID)),
  first_year = min(full_base$Year),
  last_year = max(full_base$Year),
  wrtds_rows = sum(!is.na(full_base$FNConc)),
  wrtds_last_year = max(full_base$Year[!is.na(full_base$FNConc)], na.rm = TRUE),
  raw_DSi_rows = sum(!is.na(full_raw_dsi$raw_DSi_n)),
  raw_DSi_last_year = max(full_raw_dsi$Year[!is.na(full_raw_dsi$raw_DSi_n)], na.rm = TRUE),
  forbidden_columns = forbidden_column_summary(full_base),
  stringsAsFactors = FALSE
)
esom_summary <- data.frame(
  esom_site_rows = nrow(esom_sites),
  esom_unique_site_keys = nrow(esom_unique),
  esom_matched_site_keys = length(matched_esom_keys),
  esom_missing_site_keys = nrow(esom_missing),
  esom_output_rows = nrow(esom_base),
  esom_output_cols = ncol(esom_base),
  duplicate_esom_rows = nrow(esom_duplicates),
  first_year = min(esom_base$Year),
  last_year = max(esom_base$Year),
  forbidden_columns = forbidden_column_summary(esom_base),
  stringsAsFactors = FALSE
)
expanded_snow_summary <- data.frame(
  dataset = c("full", "ESOM"),
  rows = c(nrow(full_expanded_snow), nrow(esom_expanded_snow)),
  cols = c(ncol(full_expanded_snow), ncol(esom_expanded_snow)),
  snow_columns = c(sum(grepl("^snow", names(full_expanded_snow))), sum(grepl("^snow", names(esom_expanded_snow)))),
  monthly_snow_columns = c(
    sum(names(full_expanded_snow) %in% monthly_snow_cols),
    sum(names(esom_expanded_snow) %in% monthly_snow_cols)
  ),
  snow_num_days_missing = c(sum(is.na(full_expanded_snow$snow_num_days)), sum(is.na(esom_expanded_snow$snow_num_days))),
  forbidden_columns = c(forbidden_column_summary(full_expanded_snow), forbidden_column_summary(esom_expanded_snow)),
  stringsAsFactors = FALSE
)

drainage_area_site_audit <- out %>%
  distinct(
    .site_key,
    Stream_ID,
    drainage_area,
    drainage_area_source,
    Shapefile_Name,
    drainage_area_shape,
    drainage_area_expected,
    drainage_area_shape_source,
    drainage_area_shape_source_type
  ) %>%
  mutate(
    drainage_area_source = ifelse(is.na(drainage_area_source), "missing", drainage_area_source)
  ) %>%
  arrange(drainage_area_source, Stream_ID)

drainage_area_source_summary <- drainage_area_site_audit %>%
  count(drainage_area_source, name = "sites") %>%
  mutate(
    rows = sites * length(unique(out$Year))
  ) %>%
  arrange(drainage_area_source)

write.csv(
  full_summary,
  file.path(audit_dir, paste0("final_full_harmonized_annual_summary_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)
write.csv(
  variable_coverage(full_base),
  file.path(audit_dir, paste0("final_full_harmonized_annual_variable_coverage_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)
write.csv(
  variable_coverage(full_raw_dsi),
  file.path(audit_dir, paste0("final_full_harmonized_annual_raw_DSi_variable_coverage_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)
write.csv(
  data.frame(
    rows = nrow(full_raw_dsi),
    cols = ncol(full_raw_dsi),
    raw_DSi_rows = sum(!is.na(full_raw_dsi$raw_DSi_n)),
    raw_DSi_missing_rows = sum(is.na(full_raw_dsi$raw_DSi_n)),
    raw_DSi_last_year = max(full_raw_dsi$Year[!is.na(full_raw_dsi$raw_DSi_n)], na.rm = TRUE),
    source_file = raw_chem_file,
    forbidden_columns = forbidden_column_summary(full_raw_dsi),
    stringsAsFactors = FALSE
  ),
  file.path(audit_dir, paste0("final_full_harmonized_annual_raw_DSi_summary_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)
write.csv(
  expanded_snow_summary[expanded_snow_summary$dataset == "full", setdiff(names(expanded_snow_summary), "dataset"), drop = FALSE],
  file.path(audit_dir, paste0("final_full_harmonized_annual_expanded_snow_summary_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)
write.csv(
  drainage_area_site_audit,
  file.path(audit_dir, paste0("final_full_spatial_drivers_drainage_area_by_site_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)
write.csv(
  drainage_area_source_summary,
  file.path(audit_dir, paste0("final_full_spatial_drivers_drainage_area_source_summary_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)
write.csv(
  data.frame(
    rows = nrow(full_spatial_only),
    cols = ncol(full_spatial_only),
    sites = length(unique(full_spatial_only$Stream_ID)),
    first_year = min(full_spatial_only$Year),
    last_year = max(full_spatial_only$Year),
    snow_columns = sum(grepl("^snow", names(full_spatial_only))),
    monthly_snow_columns = sum(names(full_spatial_only) %in% monthly_snow_cols),
    drainage_area_nonmissing_sites = length(unique(full_spatial_only$Stream_ID[!is.na(full_spatial_only$drainage_area)])),
    drainage_area_missing_sites = length(unique(full_spatial_only$Stream_ID[is.na(full_spatial_only$drainage_area)])),
    dropped_DSi_columns = paste(intersect(dsi_output_columns, names(full_base)), collapse = ";"),
    forbidden_columns = forbidden_column_summary(full_spatial_only),
    stringsAsFactors = FALSE
  ),
  file.path(audit_dir, paste0("final_full_spatial_drivers_annual_summary_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)
write.csv(
  esom_summary,
  file.path(esom_out_dir, paste0("ESOM_final_harmonized_annual_summary_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)
write.csv(
  expanded_snow_summary[expanded_snow_summary$dataset == "ESOM", setdiff(names(expanded_snow_summary), "dataset"), drop = FALSE],
  file.path(esom_out_dir, paste0("ESOM_final_harmonized_annual_expanded_snow_summary_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)
write.csv(
  data.frame(
    rows = nrow(esom_spatial_only),
    cols = ncol(esom_spatial_only),
    sites = length(unique(esom_spatial_only$Stream_ID)),
    first_year = min(esom_spatial_only$Year),
    last_year = max(esom_spatial_only$Year),
    snow_columns = sum(grepl("^snow", names(esom_spatial_only))),
    monthly_snow_columns = sum(names(esom_spatial_only) %in% monthly_snow_cols),
    drainage_area_nonmissing_sites = length(unique(esom_spatial_only$Stream_ID[!is.na(esom_spatial_only$drainage_area)])),
    drainage_area_missing_sites = length(unique(esom_spatial_only$Stream_ID[is.na(esom_spatial_only$drainage_area)])),
    dropped_DSi_columns = paste(intersect(dsi_output_columns, names(esom_base)), collapse = ";"),
    forbidden_columns = forbidden_column_summary(esom_spatial_only),
    stringsAsFactors = FALSE
  ),
  file.path(esom_out_dir, paste0("ESOM_spatial_drivers_annual_summary_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)

cat("WROTE:", full_out_file, "\n", sep = "")
cat("WROTE:", full_expanded_snow_file, "\n", sep = "")
cat("WROTE:", full_raw_dsi_file, "\n", sep = "")
cat("WROTE:", full_spatial_file, "\n", sep = "")
cat("WROTE:", esom_out_file, "\n", sep = "")
cat("WROTE:", esom_expanded_snow_file, "\n", sep = "")
cat("WROTE:", esom_spatial_file, "\n", sep = "")
print(full_summary, row.names = FALSE)
print(esom_summary, row.names = FALSE)
