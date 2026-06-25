librarian::shelf(dplyr, sf)

date_tag <- Sys.getenv("SILICA_FINAL_HARMONIZED_DATE", unset = "20260608")
year_min <- as.integer(Sys.getenv("SILICA_FINAL_YEAR_MIN", unset = "2002"))
year_max <- as.integer(Sys.getenv("SILICA_FINAL_YEAR_MAX", unset = "2025"))
write_audit_outputs <- toupper(Sys.getenv("SILICA_WRITE_FINAL_AUDITS", unset = "FALSE")) == "TRUE"
write_data_check_export <- toupper(Sys.getenv("SILICA_WRITE_DATA_CHECK_EXPORT", unset = "FALSE")) == "TRUE"
excluded_model_columns <- c("NOx", "P")
dsi_output_columns <- c("FNConc", "FNYield", "GenConc", "GenYield")
silicon_molar_mass_kg_per_kmol <- 28.0855

box_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn"
data_root <- file.path(box_root, "spatial-data-extractions")
harmonization_dir <- file.path(data_root, "review", "harmonization")

latest_required_file <- function(pattern, label) {
  hits <- Sys.glob(pattern)
  hits <- hits[file.exists(hits)]
  if (!length(hits)) {
    stop(
      "Missing ", label, ". Run 05_harmonization/01_build-harmonized-drivers.R first or set the matching input path explicitly.",
      call. = FALSE
    )
  }
  hits[which.max(file.info(hits)$mtime)]
}

env_or_latest <- function(env_name, pattern, label) {
  env_value <- trimws(Sys.getenv(env_name, unset = ""))
  if (nzchar(env_value)) {
    return(env_value)
  }
  latest_required_file(pattern, label)
}

march_harmonized_file <- file.path(
  box_root,
  "Spatial_controls_GRL",
  "GRL_Materials",
  "GRL_submission_v1",
  "code_inputs",
  "AllDrivers_Harmonized_Yearly_filtered_5_years.csv"
)
current_site_file <- file.path(
  if (write_data_check_export) {
    ""
  } else {
    env_or_latest(
      "SILICA_FINAL_CURRENT_SITE_FILE",
      file.path(harmonization_dir, "harmonized-spatial-drivers_*.csv"),
      "harmonized site driver file"
    )
  }
)
current_annual_file <- if (write_data_check_export) {
  ""
} else {
  env_or_latest(
    "SILICA_FINAL_CURRENT_ANNUAL_FILE",
    file.path(harmonization_dir, "harmonized-spatial-drivers-annual_*.csv"),
    "harmonized annual driver file"
  )
}
wrtds_annual_file <- file.path(data_root, "master-datasets", "Full_Results_WRTDS_kalman_annual.csv")
raw_chem_file <- file.path(data_root, "master-datasets", "20260105_masterdata_chem.csv")
site_ref_file <- file.path(data_root, "master-datasets", "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv")
esom_sites_file <- file.path(box_root, "esom", "spatial-data", "ESOM_Sites.csv")
silica_shapefile_root <- file.path(data_root, "silica-shapefiles")
lulc_patch_file <- file.path(
  data_root,
  "gee-glc-lulc-outputs",
  "merged-master-checkpoints",
  "DSi_LULC_filled_interpolated_Simple_06252026_V2.csv"
)

esom_out_dir <- file.path(box_root, "esom", "spatial-data")
audit_dir <- file.path(data_root, "audit-summaries")
dir.create(esom_out_dir, recursive = TRUE, showWarnings = FALSE)
if (write_audit_outputs) {
  dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
}

read_csv_clean <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  blank_names <- is.na(names(x)) | names(x) == ""
  names(x)[blank_names] <- paste0("source_col_", seq_len(sum(blank_names)))
  names(x) <- make.unique(names(x))
  x
}

source(file.path("tools", "name_keys.R"))

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
  hydrosheds_id <- if ("hydrshd" %in% names(x)) as.character(x$hydrshd) else rep(NA_character_, nrow(x))
  hydrosheds_id[!is_present(hydrosheds_id)] <- NA_character_

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
    hydrosheds_id = hydrosheds_id,
    hydrosheds_used = is_present(hydrosheds_id) | grepl("hydroshed", shp, ignore.case = TRUE),
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(shp_key), shp_key != "", !is.na(drainage_area_shape))
}

build_watershed_area_lookup <- function(site_rows) {
  combined_paths <- c(
    file.path(silica_shapefile_root, "site-coordinates", "silica-watersheds_hydrosheds_DR_2.shp"),
    file.path(silica_shapefile_root, "site-coordinates", "silica-watersheds.shp"),
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
      hydrosheds_id = character(),
      hydrosheds_used = logical(),
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
    filter(!shp_key %in% unique(combined$shp_key)) %>%
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
        drainage_area_shape_priority = 100L,
        hydrosheds_id = NA_character_,
        hydrosheds_used = grepl("hydroshed", shapefile_name, ignore.case = TRUE)
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
      hydrosheds_id = first_present(hydrosheds_id),
      hydrosheds_used = any(hydrosheds_used, na.rm = TRUE),
      .groups = "drop"
    )

  site_rows %>%
    transmute(.site_key, shp_key = norm_key(Shapefile_Name), Shapefile_Name) %>%
    left_join(lookup, by = "shp_key") %>%
    mutate(
      hydrosheds_id = ifelse(is_present(hydrosheds_id), hydrosheds_id, NA_character_),
      hydrosheds_used = ifelse(is.na(hydrosheds_used), FALSE, hydrosheds_used) |
        grepl("hydroshed", Shapefile_Name, ignore.case = TRUE)
    ) %>%
    group_by(.site_key) %>%
    summarise(
      Shapefile_Name = {
        hydrosheds_name <- first_present(Shapefile_Name[hydrosheds_used])
        if (is_present(hydrosheds_name)) hydrosheds_name else first_present(Shapefile_Name)
      },
      drainage_area_shape = {
        hydrosheds_area <- first_present_numeric(drainage_area_shape[hydrosheds_used])
        if (!is.na(hydrosheds_area)) hydrosheds_area else first_present_numeric(drainage_area_shape)
      },
      drainage_area_expected = {
        hydrosheds_expected <- first_present_numeric(drainage_area_expected[hydrosheds_used])
        if (!is.na(hydrosheds_expected)) hydrosheds_expected else first_present_numeric(drainage_area_expected)
      },
      drainage_area_shape_source = {
        hydrosheds_source <- first_present(drainage_area_shape_source[hydrosheds_used])
        if (is_present(hydrosheds_source)) hydrosheds_source else first_present(drainage_area_shape_source)
      },
      drainage_area_shape_source_type = {
        hydrosheds_source_type <- first_present(drainage_area_shape_source_type[hydrosheds_used])
        if (is_present(hydrosheds_source_type)) hydrosheds_source_type else first_present(drainage_area_shape_source_type)
      },
      hydrosheds_used = any(hydrosheds_used, na.rm = TRUE),
      hydrosheds_id = first_present(hydrosheds_id),
      .groups = "drop"
    )
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

lulc_stream_key <- function(x) {
  gsub("[^a-z0-9]+", "", tolower(x))
}

apply_lulc_patch <- function(df, patch_path) {
  patch <- read_csv_clean(patch_path) %>%
    transmute(
      .lulc_key = lulc_stream_key(Stream_Name),
      Year = suppressWarnings(as.integer(Year)),
      Simple_Class = gsub("^_+|_+$", "", gsub("[^A-Za-z0-9]+", "_", Simple_Class)),
      LandClass_sum = coerce_num(LandClass_sum)
    ) %>%
    filter(
      !is.na(.lulc_key),
      .lulc_key != "",
      !is.na(Year),
      Simple_Class %in% land_classes
    )

  patch_years <- sort(unique(patch$Year))
  source_year <- vapply(df$Year, function(year) {
    available <- patch_years[patch_years <= year]
    if (length(available)) max(available) else NA_integer_
  }, integer(1))
  df_lookup <- paste(lulc_stream_key(sub("^[^_]+__", "", df$Stream_ID)), source_year)

  for (class_name in land_classes) {
    target_col <- paste0("land_", class_name)
    patch_values <- patch %>%
      filter(Simple_Class == class_name) %>%
      group_by(.lulc_key, Year) %>%
      summarise(LandClass_sum = mean(LandClass_sum, na.rm = TRUE), .groups = "drop")

    lookup_values <- patch_values$LandClass_sum
    names(lookup_values) <- paste(patch_values$.lulc_key, patch_values$Year)
    match_id <- match(df_lookup, names(lookup_values))
    use_patch <- !is.na(match_id)
    df[[target_col]][use_patch] <- lookup_values[match_id[use_patch]] * 100
  }

  land_cols <- paste0("land_", land_classes)
  land_matrix <- as.data.frame(lapply(df[land_cols], coerce_num))
  df$major_land <- apply(land_matrix, 1, function(x) {
    if (all(is.na(x))) {
      return(NA_character_)
    }
    sub("^land_", "", names(x)[which.max(replace(x, is.na(x), -Inf))])
  })

  df
}

snow_months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
monthly_snow_cols <- as.vector(rbind(
  paste0("snow_", snow_months, "_avg_prop_area"),
  paste0("snow_", snow_months, "_num_days")
))

if (write_data_check_export) {
  final_file <- file.path(data_root, paste0("final_annual_dataset_", date_tag, ".csv"))
  data_check_file <- file.path(
    data_root,
    paste0("all_data_extractions_data_check_GEE_GLC_V2_", format(Sys.Date(), "%Y%m%d"), ".csv")
  )

  data_check <- read_csv_clean(final_file)
  data_check <- apply_lulc_patch(data_check, lulc_patch_file)
  data_check <- data_check[, setdiff(names(data_check), dsi_output_columns), drop = FALSE]

  spatial_review_cols <- c(
    "precip", "temp", "npp", "evapotrans", "greenup_day", "elevation", "basin_slope",
    grep("^rocks_|^land_", names(data_check), value = TRUE)
  )
  has_spatial_data <- rowSums(as.data.frame(lapply(data_check[spatial_review_cols], function(x) {
    if (is.character(x)) {
      return(!is.na(x) & trimws(x) != "")
    }
    !is.na(x) & x != 0
  }))) > 0
  data_check <- data_check[has_spatial_data, , drop = FALSE]

  land_cols <- paste0("land_", land_classes)
  land_matrix <- as.data.frame(lapply(data_check[land_cols], coerce_num))
  land_sum <- rowSums(land_matrix, na.rm = TRUE)
  canada_md <- grepl("^Canada__|^MD__", data_check$Stream_ID)
  land_sum_flag <- abs(land_sum - 100) > 1

  write.csv(data_check, data_check_file, row.names = FALSE, na = "")
  cat("WROTE:", data_check_file, "\n", sep = "")
  cat("rows=", nrow(data_check), "\n", sep = "")
  cat("cols=", ncol(data_check), "\n", sep = "")
  cat("sites=", length(unique(data_check$Stream_ID)), "\n", sep = "")
  cat("canada_md_land_sum_flags=", sum(land_sum_flag & canada_md, na.rm = TRUE), "\n", sep = "")
  quit(save = "no", status = 0)
}

required <- c(
  current_site_file,
  current_annual_file,
  wrtds_annual_file,
  raw_chem_file,
  site_ref_file,
  esom_sites_file,
  march_harmonized_file,
  lulc_patch_file
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

current_site_keyed <- current_site %>%
  filter(!is.na(.site_key), .site_key != "||") %>%
  arrange(.site_key)

current_site_one <- current_site_keyed %>%
  distinct(.site_key, .keep_all = TRUE)

watershed_area_one <- build_watershed_area_lookup(current_site_keyed)

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

out <- apply_lulc_patch(out, lulc_patch_file)

base_cols <- c(
  "FNConc", "Stream_ID", "Year", "drainage_area", "hydrosheds_used", "precip", "Q", "temp",
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

final_annual <- full_spatial_only %>%
  left_join(
    full_base %>% select(Stream_ID, Year, all_of(dsi_output_columns)),
    by = c("Stream_ID", "Year")
  ) %>%
  select(
    Stream_ID,
    Year,
    drainage_area,
    hydrosheds_used,
    precip,
    Q,
    all_of(dsi_output_columns),
    temp,
    snow_cover,
    snow_num_days,
    npp,
    evapotrans,
    greenup_day,
    permafrost,
    elevation,
    RBI,
    RCS,
    basin_slope,
    everything()
  )

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

final_out_file <- file.path(data_root, paste0("final_annual_dataset_", date_tag, ".csv"))
esom_out_file <- file.path(esom_out_dir, paste0("ESOM_final_harmonized_annual_", date_tag, ".csv"))
esom_expanded_snow_file <- file.path(esom_out_dir, paste0("ESOM_final_harmonized_annual_expanded_snow_", date_tag, ".csv"))
esom_spatial_file <- file.path(esom_out_dir, paste0("ESOM_spatial_drivers_annual_", date_tag, ".csv"))
esom_missing_file <- file.path(esom_out_dir, paste0("ESOM_missing_from_final_harmonized_annual_", date_tag, ".csv"))
esom_duplicate_file <- file.path(esom_out_dir, paste0("ESOM_duplicate_site_keys_final_harmonized_annual_", date_tag, ".csv"))

write.csv(final_annual, final_out_file, row.names = FALSE, na = "")
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
  rows = nrow(final_annual),
  cols = ncol(final_annual),
  sites = length(unique(final_annual$Stream_ID)),
  first_year = min(final_annual$Year),
  last_year = max(final_annual$Year),
  wrtds_rows = sum(!is.na(final_annual$FNConc)),
  wrtds_last_year = max(final_annual$Year[!is.na(final_annual$FNConc)], na.rm = TRUE),
  raw_DSi_rows = sum(!is.na(full_raw_dsi$raw_DSi_n)),
  raw_DSi_last_year = max(full_raw_dsi$Year[!is.na(full_raw_dsi$raw_DSi_n)], na.rm = TRUE),
  forbidden_columns = forbidden_column_summary(final_annual),
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
    drainage_area_shape_source_type,
    hydrosheds_used,
    hydrosheds_id
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

if (write_audit_outputs) {
  write.csv(
    full_summary,
    file.path(audit_dir, paste0("final_annual_dataset_summary_", date_tag, ".csv")),
    row.names = FALSE,
    na = ""
  )
  write.csv(
    variable_coverage(final_annual),
    file.path(audit_dir, paste0("final_annual_dataset_variable_coverage_", date_tag, ".csv")),
    row.names = FALSE,
    na = ""
  )
  write.csv(
    variable_coverage(full_raw_dsi),
    file.path(audit_dir, paste0("final_annual_dataset_raw_DSi_variable_coverage_", date_tag, ".csv")),
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
    file.path(audit_dir, paste0("final_annual_dataset_raw_DSi_summary_", date_tag, ".csv")),
    row.names = FALSE,
    na = ""
  )
  write.csv(
    drainage_area_site_audit,
    file.path(audit_dir, paste0("final_annual_dataset_drainage_area_by_site_", date_tag, ".csv")),
    row.names = FALSE,
    na = ""
  )
  write.csv(
    drainage_area_source_summary,
    file.path(audit_dir, paste0("final_annual_dataset_drainage_area_source_summary_", date_tag, ".csv")),
    row.names = FALSE,
    na = ""
  )
}
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

cat("WROTE:", final_out_file, "\n", sep = "")
cat("WROTE:", esom_out_file, "\n", sep = "")
cat("WROTE:", esom_expanded_snow_file, "\n", sep = "")
cat("WROTE:", esom_spatial_file, "\n", sep = "")
