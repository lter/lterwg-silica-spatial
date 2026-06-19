librarian::shelf(dplyr)

env_or_default <- function(env_name, default_value) {
  value <- trimws(Sys.getenv(env_name, unset = ""))
  if (nzchar(value)) value else default_value
}

norm_blank <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

has_any_values <- function(df, cols) {
  if (!length(cols)) {
    return(rep(FALSE, nrow(df)))
  }
  vals <- lapply(df[, cols, drop = FALSE], function(x) {
    x <- norm_blank(x)
    !is.na(x) & x != ""
  })
  Reduce(`|`, vals)
}

as_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

row_summary <- function(df, cols, fun) {
  if (!length(cols)) {
    return(rep(NA_real_, nrow(df)))
  }
  vals <- as.data.frame(lapply(df[, cols, drop = FALSE], as_num))
  out <- apply(vals, 1, function(z) {
    z <- z[!is.na(z)]
    if (!length(z)) {
      return(NA_real_)
    }
    fun(z)
  })
  as.numeric(out)
}

row_any_positive <- function(df, cols) {
  if (!length(cols)) {
    return(rep(FALSE, nrow(df)))
  }
  vals <- as.data.frame(lapply(df[, cols, drop = FALSE], as_num))
  rowSums(vals > 0, na.rm = TRUE) > 0
}

env_list <- function(env_name, default_values) {
  raw <- trimws(Sys.getenv(env_name, unset = ""))
  if (!nzchar(raw)) {
    return(default_values)
  }
  out <- trimws(unlist(strsplit(raw, ","), use.names = FALSE))
  out[nzchar(out)]
}

first_reason <- function(...) {
  reasons <- list(...)
  out <- rep(NA_character_, length(reasons[[1]]))
  for (reason_name in names(reasons)) {
    idx <- is.na(out) & reasons[[reason_name]]
    out[idx] <- reason_name
  }
  out
}

make_fix_log <- function(df, rows, fix_family, reason) {
  if (!length(rows)) {
    return(data.frame())
  }
  pick <- function(col) {
    if (col %in% names(df)) {
      return(df[[col]][rows])
    }
    rep(NA, length(rows))
  }
  data.frame(
    LTER = df$LTER[rows],
    Stream_Name = df$Stream_Name[rows],
    Discharge_File_Name = df$Discharge_File_Name[rows],
    Shapefile_Name = df$Shapefile_Name[rows],
    fix_family = fix_family,
    reason = reason,
    latitude = pick(".domain_latitude"),
    mean_temp_degC = pick(".domain_mean_temp_degC"),
    coldest_month_temp_degC = pick(".domain_min_month_temp_degC"),
    elevation_max_m = pick(".domain_elevation_max_m"),
    stringsAsFactors = FALSE
  )
}

centroid_latitude_from_watersheds <- function(df, watershed_file) {
  if (!nzchar(watershed_file) || !file.exists(watershed_file)) {
    return(rep(NA_real_, nrow(df)))
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    warning("sf is not installed; skipping watershed latitude lookup")
    return(rep(NA_real_, nrow(df)))
  }

  g <- sf::st_read(watershed_file, quiet = TRUE)
  if (!"LTER" %in% names(g)) {
    return(rep(NA_real_, nrow(df)))
  }
  shape_col <- intersect(c("Shapefile_Name", "shp_nm", "Shpfl_N"), names(g))
  if (!length(shape_col)) {
    return(rep(NA_real_, nrow(df)))
  }
  shape_col <- shape_col[[1]]

  pts <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(g)))
  coords <- sf::st_coordinates(pts)
  g_key <- paste(norm_blank(g$LTER), norm_blank(g[[shape_col]]), sep = "||")
  x_key <- paste(norm_blank(df$LTER), norm_blank(df$Shapefile_Name), sep = "||")
  coords[, "Y"][match(x_key, g_key)]
}

data_root <- env_or_default(
  "SILICA_QAQC_DATA_ROOT",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions"
)

source_file <- env_or_default(
  "SILICA_DOMAIN_FIX_SOURCE_FILE",
  file.path(
    data_root,
    "si-extracted-data",
    "all_data_extractions",
    "all-data_si-extract_4_20260522_final-extract-merge-spatial-data-extractions.csv"
  )
)

out_file <- env_or_default(
  "SILICA_DOMAIN_FIX_OUTPUT_FILE",
  file.path(
    data_root,
    "si-extracted-data",
    "all_data_extractions",
    "all-data_si-extract_4_20260522_final-extract-merge-v4-spatial-data-extractions.csv"
  )
)

review_dir <- env_or_default(
  "SILICA_DOMAIN_FIX_REVIEW_DIR",
  file.path(data_root, "review", "final_20260522_final-extract-merge-v4")
)

run_label <- env_or_default("SILICA_DOMAIN_FIX_LABEL", "final-extract-merge-v4")
run_date <- env_or_default("SILICA_DOMAIN_FIX_DATE", "20260522")
watershed_file <- env_or_default("SILICA_DOMAIN_FIX_WATERSHED_FILE", "")

if (!file.exists(source_file)) {
  stop("Missing source file: ", source_file, call. = FALSE)
}

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)

x <- read.csv(source_file, stringsAsFactors = FALSE, check.names = FALSE)
for (col in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) {
  if (!col %in% names(x)) {
    x[[col]] <- NA_character_
  }
}

family_patterns <- c(
  evapo = "^evapotrans_",
  greenup = "^greenup_",
  precip = "^precip_",
  airtemp = "^temp_",
  snow = "^snow_",
  npp = "^npp_",
  elev = "^elevation_|^basin_slope_",
  lith = "^major_rock$|^rocks_",
  permafrost = "^permafrost_",
  soil = "^major_soil$|^soil_"
)

spatial_cols <- grep(paste(family_patterns, collapse = "|"), names(x), value = TRUE)
actionable_cols <- grep(
  paste(family_patterns[setdiff(names(family_patterns), "permafrost")], collapse = "|"),
  names(x),
  value = TRUE
)
snow_cols <- grep("^snow_", names(x), value = TRUE)
permafrost_cols <- grep("^permafrost_", names(x), value = TRUE)

annual_temp_cols <- grep("^temp_(19|20)[0-9]{2}_degC$", names(x), value = TRUE)
monthly_temp_cols <- grep(
  "^temp_(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)_degC$",
  names(x),
  value = TRUE
)
x$.domain_mean_temp_degC <- row_summary(x, annual_temp_cols, mean)
x$.domain_min_month_temp_degC <- row_summary(x, monthly_temp_cols, min)
x$.domain_elevation_max_m <- if ("elevation_max_m" %in% names(x)) as_num(x$elevation_max_m) else NA_real_
x$.domain_latitude <- if ("Latitude" %in% names(x)) as_num(x$Latitude) else NA_real_

missing_lat <- is.na(x$.domain_latitude)
if (any(missing_lat)) {
  watershed_lat <- centroid_latitude_from_watersheds(x, watershed_file)
  x$.domain_latitude[missing_lat] <- watershed_lat[missing_lat]
}
x$.domain_abs_latitude <- abs(x$.domain_latitude)

fix_log <- list()
has_actionable <- has_any_values(x, actionable_cols)

# Same shapefile, same spatial data. Fill missing spatial fields from another
# row with the same Shapefile_Name when the source value is unambiguous.
if (length(spatial_cols)) {
  shp <- norm_blank(x$Shapefile_Name)
  has_spatial <- has_any_values(x, spatial_cols)
  needs_spatial_fill <- !Reduce(`&`, lapply(x[, spatial_cols, drop = FALSE], function(z) {
    z <- norm_blank(z)
    !is.na(z) & z != ""
  }))
  duplicate_shape_rows <- which(needs_spatial_fill & !is.na(shp))

  copied_rows <- integer()
  for (i in duplicate_shape_rows) {
    source_rows <- which(
      seq_len(nrow(x)) != i &
        shp == shp[i] &
        has_spatial
    )
    if (!length(source_rows)) {
      next
    }
    row_copied <- FALSE
    for (col in spatial_cols) {
      old_value <- norm_blank(x[[col]][i])
      source_values <- unique(norm_blank(x[[col]][source_rows]))
      source_values <- source_values[!is.na(source_values)]
      if (length(source_values) != 1) {
        next
      }
      new_value <- source_values[[1]]
      if (is.na(old_value) && !is.na(new_value)) {
        x[[col]][i] <- new_value
        row_copied <- TRUE
      }
    }
    if (row_copied) {
      copied_rows <- c(copied_rows, i)
    }
  }

  fix_log[["duplicate_shapefile_copy"]] <- make_fix_log(
    x,
    unique(copied_rows),
    "duplicate_shapefile_copy",
    "same Shapefile_Name as a row with spatial values; copied missing spatial fields from that row"
  )
}

# These rows have enough climate or location evidence to treat missing
# permafrost as no mapped permafrost. Cold/high-latitude rows stay visible
# rather than being silently filled.
if (length(permafrost_cols)) {
  has_permafrost <- has_any_values(x, permafrost_cols)
  has_shape <- !is.na(norm_blank(x$Shapefile_Name))
  no_permafrost_lters <- env_list(
    "SILICA_ZERO_PERMAFROST_LTERS",
    c(
      "Amazon",
      "Cameroon",
      "Congo Basin",
      "Guadeloupe",
      "HYBAM",
      "KRR",
      "LUQ",
      "Mali"
    )
  )
  permafrost_candidates <- has_actionable &
    !has_permafrost &
    has_shape &
    norm_blank(x$LTER) != "MCM"

  permafrost_reason <- first_reason(
    "listed non-permafrost domain for this data product" =
      norm_blank(x$LTER) %in% no_permafrost_lters,
    "mean annual temperature above permafrost threshold" =
      !is.na(x$.domain_mean_temp_degC) & x$.domain_mean_temp_degC >= 2,
    "all monthly mean temperatures above freezing" =
      !is.na(x$.domain_min_month_temp_degC) & x$.domain_min_month_temp_degC > 0,
    "low latitude and not a high alpine basin" =
      !is.na(x$.domain_abs_latitude) & x$.domain_abs_latitude < 45 &
        (is.na(x$.domain_elevation_max_m) | x$.domain_elevation_max_m < 2500)
  )
  permafrost_rows <- which(
    permafrost_candidates &
      !is.na(permafrost_reason)
  )
  permafrost_unresolved_rows <- which(
    permafrost_candidates &
      is.na(permafrost_reason)
  )

  if (length(permafrost_rows)) {
    for (col in permafrost_cols) {
      x[[col]][permafrost_rows] <- 0
    }
  }
  fix_log[["permafrost_zero"]] <- make_fix_log(
    x,
    permafrost_rows,
    "permafrost_zero",
    permafrost_reason[permafrost_rows]
  )
  fix_log[["permafrost_unresolved"]] <- make_fix_log(
    x,
    permafrost_unresolved_rows,
    "permafrost_unresolved",
    "not zeroed: cold/high-latitude basin or not enough domain evidence"
  )
}

# MODIS snow can return cloud/snow artifacts in warm places. Zeroing is
# applied only to listed no-snow regions or rows supported by warm, lowland
# climate/location checks.
if (length(snow_cols)) {
  has_shape <- !is.na(norm_blank(x$Shapefile_Name))
  zero_snow_lters <- env_list(
    "SILICA_ZERO_SNOW_LTERS",
    c(
      "Amazon",
      "Cameroon",
      "Congo Basin",
      "Guadeloupe",
      "HYBAM",
      "KRR",
      "LUQ",
      "Mali"
    )
  )
  snow_reason <- first_reason(
    "listed no-snow domain for this data product" =
      norm_blank(x$LTER) %in% zero_snow_lters,
    "tropical lowland basin with warm annual temperature" =
      !is.na(x$.domain_abs_latitude) & x$.domain_abs_latitude < 23.5 &
        !is.na(x$.domain_elevation_max_m) & x$.domain_elevation_max_m < 1500 &
        !is.na(x$.domain_mean_temp_degC) & x$.domain_mean_temp_degC >= 10,
    "frost-free warm lowland basin" =
      !is.na(x$.domain_abs_latitude) & x$.domain_abs_latitude < 35 &
        !is.na(x$.domain_elevation_max_m) & x$.domain_elevation_max_m < 800 &
        !is.na(x$.domain_mean_temp_degC) & x$.domain_mean_temp_degC >= 18 &
        !is.na(x$.domain_min_month_temp_degC) & x$.domain_min_month_temp_degC >= 10,
    "all monthly mean temperatures warm and basin is low elevation" =
      !is.na(x$.domain_min_month_temp_degC) & x$.domain_min_month_temp_degC >= 12 &
        !is.na(x$.domain_mean_temp_degC) & x$.domain_mean_temp_degC >= 20 &
        (is.na(x$.domain_elevation_max_m) | x$.domain_elevation_max_m < 1000)
  )
  snow_rows <- which(
    has_actionable &
      has_shape &
      !is.na(snow_reason)
  )
  snow_positive_not_zeroed <- which(
    has_shape &
      is.na(snow_reason) &
      row_any_positive(x, snow_cols)
  )

  if (length(snow_rows)) {
    for (col in snow_cols) {
      x[[col]][snow_rows] <- 0
    }
  }
  fix_log[["snow_zero"]] <- make_fix_log(
    x,
    snow_rows,
    "snow_zero",
    snow_reason[snow_rows]
  )
  fix_log[["snow_positive_not_zeroed"]] <- make_fix_log(
    x,
    snow_positive_not_zeroed,
    "snow_positive_not_zeroed",
    "snow retained: no no-snow rule matched this row"
  )
}

fix_log <- bind_rows(fix_log) %>%
  distinct() %>%
  arrange(fix_family, LTER, Stream_Name, Shapefile_Name)

domain_cols <- grep("^\\.domain_", names(x), value = TRUE)
x_out <- x %>% select(-all_of(domain_cols))
write.csv(x_out, out_file, row.names = FALSE, na = "")

fix_log_file <- file.path(review_dir, paste0(run_label, "_domain_fixes_", run_date, ".csv"))
write.csv(fix_log, fix_log_file, row.names = FALSE, na = "")

summary_file <- file.path(review_dir, paste0(run_label, "_domain_fix_summary_", run_date, ".csv"))
fix_summary <- fix_log %>%
  count(fix_family, reason, name = "n_sites") %>%
  arrange(fix_family)
write.csv(fix_summary, summary_file, row.names = FALSE, na = "")

cat("WROTE:", out_file, "\n", sep = "")
cat("WROTE:", fix_log_file, "\n", sep = "")
cat("WROTE:", summary_file, "\n", sep = "")
