#!/usr/bin/env Rscript

source("tools/build_final_harmonized_through_2025.R")
quit(save = "no", status = 0)

suppressPackageStartupMessages({
  library(dplyr)
})

date_tag <- Sys.getenv("SILICA_FINAL_HARMONIZED_DATE", unset = "20260608")
excluded_model_columns <- c("NOx", "P")

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
raw_chem_file <- file.path(data_root, "master", "20260105_masterdata_chem.csv")
esom_sites_file <- file.path(box_root, "ESOM", "spatial-data", "ESOM_Sites.csv")

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

overlay_vector <- function(old, new) {
  use_new <- is_present(new)
  old[use_new] <- new[use_new]
  old
}

forbidden_column_summary <- function(x) {
  bad <- intersect(excluded_model_columns, names(x))
  if (length(bad)) {
    return(paste(bad, collapse = ";"))
  }
  "NONE"
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

insert_after <- function(cols, add, after) {
  add <- add[!add %in% cols]
  if (!length(add)) {
    return(cols)
  }
  if (!after %in% cols) {
    return(c(cols, add))
  }
  i <- match(after, cols)
  if (i == length(cols)) {
    return(c(cols, add))
  }
  c(cols[seq_len(i)], add, cols[(i + 1L):length(cols)])
}

required <- c(march_harmonized_file, current_site_file, current_annual_file, esom_sites_file)
missing <- required[!file.exists(required)]
if (length(missing)) {
  stop("Missing required files:\n- ", paste(missing, collapse = "\n- "), call. = FALSE)
}

march <- read_csv_clean(march_harmonized_file)
current_site <- read_csv_clean(current_site_file)
current_annual <- read_csv_clean(current_annual_file)

march$.site_key <- site_key_from_stream_id(march$Stream_ID)
march$.row_id <- seq_len(nrow(march))
current_site$.site_key <- site_key_from_stream_id(current_site$Stream_ID)
current_annual$.site_key <- site_key_from_stream_id(current_annual$Stream_ID)

current_site_one <- current_site %>%
  filter(!is.na(.site_key), .site_key != "||") %>%
  arrange(.site_key) %>%
  distinct(.site_key, .keep_all = TRUE)

annual_overlay <- current_annual %>%
  transmute(
    .site_key,
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
  group_by(.site_key, Year) %>%
  summarise(
    across(everything(), ~ {
      v <- .[is_present(.)]
      if (length(v)) v[[1]] else NA
    }),
    .groups = "drop"
  )

site_overlay <- current_site_one %>%
  transmute(
    .site_key,
    elevation = elevation_mean_m,
    RBI = RBI,
    recession_slope = recession_slope,
    basin_slope = basin_slope_mean_degree,
    permafrost = pmin(pmax(coerce_num(permafrost_mean_m), 0), 1) * 100,
    major_rock = major_rock,
    rocks_volcanic = rocks_volcanic,
    rocks_sedimentary = rocks_sedimentary,
    rocks_carbonate_evaporite = rocks_carbonate_evaporite,
    rocks_metamorphic = rocks_metamorphic,
    rocks_plutonic = rocks_plutonic
  )

out <- march %>%
  left_join(annual_overlay, by = c(".site_key", "Year"), suffix = c("", ".new")) %>%
  left_join(site_overlay, by = ".site_key", suffix = c("", ".new"))

overlay_cols <- c(
  "precip", "Q", "temp", "snow_cover", "snow_num_days", "npp", "evapotrans", "greenup_day",
  "elevation", "RBI", "recession_slope", "basin_slope", "permafrost",
  "major_rock", "rocks_volcanic", "rocks_sedimentary", "rocks_carbonate_evaporite",
  "rocks_metamorphic", "rocks_plutonic"
)

change_log <- list()
for (col in overlay_cols) {
  new_col <- paste0(col, ".new")
  if (!new_col %in% names(out) || !col %in% names(out)) {
    next
  }

  old_value <- out[[col]]
  new_value <- out[[new_col]]
  use_new <- is_present(new_value)

  changed <- use_new & (
    (is.na(old_value) & !is.na(new_value)) |
      (!is.na(old_value) & is.na(new_value)) |
      (as.character(old_value) != as.character(new_value))
  )

  if (any(changed, na.rm = TRUE)) {
    change_log[[length(change_log) + 1L]] <- data.frame(
      variable = col,
      n_overlaid = sum(changed, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  out[[col]] <- overlay_vector(out[[col]], new_value)
}

for (col in paste0(overlay_cols, ".new")) {
  if (col %in% names(out)) {
    out[[col]] <- NULL
  }
}

land_update_rows <- integer(0)
for (class_name in land_classes) {
  target_col <- paste0("land_", class_name)
  if (!target_col %in% names(out)) {
    next
  }
  gee_values <- rep(NA_real_, nrow(out))
  for (year in sort(unique(out$Year))) {
    source_col <- paste0("gee_glc_", year, "_", class_name)
    if (!source_col %in% names(current_site_one)) {
      next
    }
    year_rows <- which(out$Year == year)
    site_match <- match(out$.site_key[year_rows], current_site_one$.site_key)
    has_site <- !is.na(site_match)
    if (any(has_site)) {
      gee_values[year_rows[has_site]] <- coerce_num(current_site_one[[source_col]][site_match[has_site]]) * 100
    }
  }
  use_gee <- !is.na(gee_values)
  land_update_rows <- union(land_update_rows, which(use_gee))
  out[[target_col]][use_gee] <- gee_values[use_gee]
}

land_cols <- paste0("land_", land_classes)
land_cols <- intersect(land_cols, names(out))
if (length(land_cols) && "major_land" %in% names(out) && length(land_update_rows)) {
  land_matrix <- as.data.frame(lapply(out[land_update_rows, land_cols, drop = FALSE], coerce_num))
  row_has_land <- rowSums(!is.na(land_matrix)) > 0
  major_land_new <- apply(land_matrix, 1, function(x) {
    if (all(is.na(x))) {
      return(NA_character_)
    }
    sub("^land_", "", names(x)[which.max(replace(x, is.na(x), -Inf))])
  })
  target_rows <- land_update_rows[row_has_land]
  out$major_land[target_rows] <- major_land_new[row_has_land]
}

if (length(land_update_rows)) {
  change_log[[length(change_log) + 1L]] <- data.frame(
    variable = "land_* and major_land",
    n_overlaid = length(unique(land_update_rows)),
    stringsAsFactors = FALSE
  )
}

matched_current_site <- out$.site_key %in% current_site_one$.site_key
matched_current_annual <- paste(out$.site_key, out$Year, sep = "||") %in%
  paste(annual_overlay$.site_key, annual_overlay$Year, sep = "||")

output_rows <- nrow(out)

unmatched_march_sites <- out %>%
  filter(!matched_current_site) %>%
  distinct(Stream_ID) %>%
  arrange(Stream_ID)

out$.site_key <- NULL
out$.row_id <- NULL
base_output_columns <- setdiff(
  names(march)[!names(march) %in% c(".site_key", ".row_id")],
  excluded_model_columns
)
final_output_columns <- insert_after(
  base_output_columns,
  intersect(c("snow_num_days"), names(out)),
  "snow_cover"
)
out <- out[, final_output_columns, drop = FALSE]
names(out)[names(out) == "recession_slope"] <- "RCS"

add_monthly_snow <- function(x) {
  snow_cols <- intersect(monthly_snow_cols, names(current_site_one))
  if (!length(snow_cols)) {
    return(x)
  }

  keyed <- x
  keyed$.site_key <- site_key_from_stream_id(keyed$Stream_ID)
  snow_overlay <- current_site_one[, c(".site_key", snow_cols), drop = FALSE]
  expanded <- keyed %>%
    left_join(snow_overlay, by = ".site_key") %>%
    select(-.site_key)

  expanded
}

build_raw_dsi_annual <- function(x) {
  if (!file.exists(raw_chem_file)) {
    warning("Raw chemistry file not found, skipping raw DSi output: ", raw_chem_file)
    return(NULL)
  }

  raw_chem <- read_csv_clean(raw_chem_file)
  required_raw <- c("LTER", "Stream_Name", "date", "variable", "units", "value")
  missing_raw <- setdiff(required_raw, names(raw_chem))
  if (length(missing_raw)) {
    stop("Raw chemistry file is missing columns: ", paste(missing_raw, collapse = ", "), call. = FALSE)
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

  keyed <- x
  keyed$.site_key <- site_key_from_stream_id(keyed$Stream_ID)
  keyed %>%
    left_join(raw_dsi, by = c(".site_key", "Year")) %>%
    select(-.site_key)
}

if (length(change_log)) {
  change_log <- lapply(change_log, function(x) {
    x$variable[x$variable == "recession_slope"] <- "RCS"
    x
  })
}

audit_summary <- data.frame(
  march_rows = nrow(march),
  march_sites = length(unique(march$Stream_ID)),
  matched_current_sites = length(unique(march$Stream_ID[matched_current_site])),
  unmatched_current_sites = length(unique(march$Stream_ID[!matched_current_site])),
  matched_site_year_rows = sum(matched_current_annual),
  unmatched_site_year_rows = sum(!matched_current_annual),
  output_rows = output_rows,
  output_cols = ncol(out),
  stringsAsFactors = FALSE
)

full_out_file <- file.path(full_out_dir, paste0("final_full_harmonized_annual_", date_tag, ".csv"))
full_expanded_snow_file <- file.path(full_out_dir, paste0("final_full_harmonized_annual_expanded_snow_", date_tag, ".csv"))
full_raw_dsi_file <- file.path(full_out_dir, paste0("final_full_harmonized_annual_raw_DSi_", date_tag, ".csv"))
full_expanded_snow_summary_file <- file.path(audit_dir, paste0("final_full_harmonized_annual_expanded_snow_summary_", date_tag, ".csv"))
full_raw_dsi_summary_file <- file.path(audit_dir, paste0("final_full_harmonized_annual_raw_DSi_summary_", date_tag, ".csv"))
write.csv(out, full_out_file, row.names = FALSE, na = "")
full_expanded_snow_out <- add_monthly_snow(out)
write.csv(full_expanded_snow_out, full_expanded_snow_file, row.names = FALSE, na = "")
raw_dsi_out <- build_raw_dsi_annual(out)
if (!is.null(raw_dsi_out)) {
  write.csv(raw_dsi_out, full_raw_dsi_file, row.names = FALSE, na = "")
}
write.csv(data.frame(
  rows = nrow(full_expanded_snow_out),
  cols = ncol(full_expanded_snow_out),
  snow_columns = sum(grepl("^snow", names(full_expanded_snow_out))),
  monthly_snow_columns = sum(names(full_expanded_snow_out) %in% monthly_snow_cols),
  snow_num_days_missing = sum(is.na(full_expanded_snow_out$snow_num_days)),
  forbidden_columns = forbidden_column_summary(full_expanded_snow_out),
  stringsAsFactors = FALSE
), full_expanded_snow_summary_file, row.names = FALSE, na = "")
if (!is.null(raw_dsi_out)) {
  write.csv(data.frame(
    rows = nrow(raw_dsi_out),
    cols = ncol(raw_dsi_out),
    raw_DSi_rows = sum(!is.na(raw_dsi_out$raw_DSi_n)),
    raw_DSi_missing_rows = sum(is.na(raw_dsi_out$raw_DSi_n)),
    source_file = raw_chem_file,
    forbidden_columns = forbidden_column_summary(raw_dsi_out),
    stringsAsFactors = FALSE
  ), full_raw_dsi_summary_file, row.names = FALSE, na = "")
}

audit_summary_file <- file.path(audit_dir, paste0("final_full_harmonized_annual_summary_", date_tag, ".csv"))
audit_changes_file <- file.path(audit_dir, paste0("final_full_harmonized_annual_overlay_counts_", date_tag, ".csv"))
unmatched_file <- file.path(audit_dir, paste0("final_full_harmonized_annual_unmatched_march_sites_", date_tag, ".csv"))
write.csv(audit_summary, audit_summary_file, row.names = FALSE, na = "")
write.csv(if (length(change_log)) do.call(rbind, change_log) else data.frame(), audit_changes_file, row.names = FALSE, na = "")
write.csv(unmatched_march_sites, unmatched_file, row.names = FALSE, na = "")

esom_sites <- read_csv_clean(esom_sites_file)
esom_sites$.site_key <- site_key_from_parts(esom_sites$LTER, esom_sites$Stream_Name)
esom_unique <- esom_sites %>%
  filter(!is.na(.site_key), .site_key != "||") %>%
  distinct(.site_key, .keep_all = TRUE)
esom_duplicates <- esom_sites %>%
  filter(duplicated(.site_key) | duplicated(.site_key, fromLast = TRUE)) %>%
  arrange(.site_key)

out_for_esom <- out
out_for_esom$.site_key <- site_key_from_stream_id(out_for_esom$Stream_ID)
esom_out <- out_for_esom %>%
  filter(.site_key %in% esom_unique$.site_key) %>%
  select(-.site_key)
matched_esom_keys <- unique(out_for_esom$.site_key[out_for_esom$.site_key %in% esom_unique$.site_key])
esom_missing <- esom_unique %>%
  filter(!.site_key %in% matched_esom_keys) %>%
  select(-.site_key)

esom_out_file <- file.path(esom_out_dir, paste0("ESOM_final_harmonized_annual_", date_tag, ".csv"))
esom_expanded_snow_file <- file.path(esom_out_dir, paste0("ESOM_final_harmonized_annual_expanded_snow_", date_tag, ".csv"))
esom_expanded_snow_summary_file <- file.path(esom_out_dir, paste0("ESOM_final_harmonized_annual_expanded_snow_summary_", date_tag, ".csv"))
esom_missing_file <- file.path(esom_out_dir, paste0("ESOM_missing_from_final_harmonized_annual_", date_tag, ".csv"))
esom_duplicate_file <- file.path(esom_out_dir, paste0("ESOM_duplicate_site_keys_final_harmonized_annual_", date_tag, ".csv"))
esom_summary_file <- file.path(esom_out_dir, paste0("ESOM_final_harmonized_annual_summary_", date_tag, ".csv"))

write.csv(esom_out, esom_out_file, row.names = FALSE, na = "")
esom_expanded_snow_out <- add_monthly_snow(esom_out)
write.csv(esom_expanded_snow_out, esom_expanded_snow_file, row.names = FALSE, na = "")
write.csv(data.frame(
  rows = nrow(esom_expanded_snow_out),
  cols = ncol(esom_expanded_snow_out),
  snow_columns = sum(grepl("^snow", names(esom_expanded_snow_out))),
  monthly_snow_columns = sum(names(esom_expanded_snow_out) %in% monthly_snow_cols),
  snow_num_days_missing = sum(is.na(esom_expanded_snow_out$snow_num_days)),
  forbidden_columns = forbidden_column_summary(esom_expanded_snow_out),
  stringsAsFactors = FALSE
), esom_expanded_snow_summary_file, row.names = FALSE, na = "")
write.csv(esom_missing, esom_missing_file, row.names = FALSE, na = "")
write.csv(esom_duplicates, esom_duplicate_file, row.names = FALSE, na = "")

esom_summary <- data.frame(
  esom_site_rows = nrow(esom_sites),
  esom_unique_site_keys = nrow(esom_unique),
  esom_matched_site_keys = length(matched_esom_keys),
  esom_missing_site_keys = nrow(esom_missing),
  esom_output_rows = nrow(esom_out),
  esom_output_cols = ncol(esom_out),
  duplicate_esom_rows = nrow(esom_duplicates),
  stringsAsFactors = FALSE
)
write.csv(esom_summary, esom_summary_file, row.names = FALSE, na = "")

cat("WROTE:", full_out_file, "\n", sep = "")
cat("WROTE:", full_expanded_snow_file, "\n", sep = "")
if (!is.null(raw_dsi_out)) {
  cat("WROTE:", full_raw_dsi_file, "\n", sep = "")
}
cat("WROTE:", esom_out_file, "\n", sep = "")
cat("WROTE:", esom_expanded_snow_file, "\n", sep = "")
cat("WROTE:", audit_summary_file, "\n", sep = "")
cat("WROTE:", audit_changes_file, "\n", sep = "")
cat("WROTE:", unmatched_file, "\n", sep = "")
cat("WROTE:", full_expanded_snow_summary_file, "\n", sep = "")
if (!is.null(raw_dsi_out)) {
  cat("WROTE:", full_raw_dsi_summary_file, "\n", sep = "")
}
cat("WROTE:", esom_missing_file, "\n", sep = "")
cat("WROTE:", esom_duplicate_file, "\n", sep = "")
cat("WROTE:", esom_summary_file, "\n", sep = "")
cat("WROTE:", esom_expanded_snow_summary_file, "\n", sep = "")
print(audit_summary, row.names = FALSE)
print(esom_summary, row.names = FALSE)
