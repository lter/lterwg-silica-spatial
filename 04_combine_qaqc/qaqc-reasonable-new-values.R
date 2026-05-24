suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(ggplot2)
})

source(file.path("04_combine_qaqc", "00_qaqc_config.R"))
source(file.path("04_combine_qaqc", "00_qaqc_functions.R"))

# Keep output names stable across manual runs and subset runs
driver_specs <- tibble::tribble(
  ~driver, ~regex,
  "evapo", "^evapotrans_([0-9]{4})_kg_m2$",
  "greenup", "^greenup_cycle[01]_([0-9]{4})MMDD$",
  "precip", "^precip_([0-9]{4})_mm_per_day$",
  "airtemp", "^temp_([0-9]{4})_degC$",
  "snow", "^snow_([0-9]{4})_num_days$",
  "npp", "^npp_([0-9]{4})_kgC_m2_year$"
)

# Use robust spread metrics so a few extreme values do not dominate
safe_mad <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) return(NA_real_)
  stats::mad(x, constant = 1, na.rm = TRUE)
}

safe_min <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  min(x)
}

safe_max <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  max(x)
}

out_dir <- year_extension_dir
detail_file <- file.path(out_dir, review_file_name("new-years-plausibility-review", date_tag))
summary_file <- file.path(out_dir, review_file_name("new-years-plausibility-summary", date_tag))

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

old <- read_combined_table(old_file)
new <- read_combined_table(new_file, sanitize_new = TRUE)
ref <- prepare_combined_table(read_excel(ref_file))

# Reshape annual driver columns so each row is one site-driver-year value
old_long <- extract_driver_long(old, driver_specs, "old_value")
new_long <- extract_driver_long(new, driver_specs, "new_value")

coord_ref <- ref %>%
  transmute(
    key,
    Latitude = suppressWarnings(as.numeric(Latitude)),
    Longitude = suppressWarnings(as.numeric(Longitude))
  ) %>%
  distinct(key, .keep_all = TRUE)

elevation_ref <- new %>%
  transmute(
    key,
    elevation_m = suppressWarnings(as.numeric(elevation_mean_m))
  ) %>%
  distinct(key, .keep_all = TRUE)

haversine_km <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  lat1 <- lat1 * rad
  lon1 <- lon1 * rad
  lat2 <- lat2 * rad
  lon2 <- lon2 * rad
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  6371 * 2 * atan2(sqrt(a), sqrt(1 - a))
}

driver_year_limits <- bind_rows(lapply(seq_len(nrow(driver_specs)), function(i) {
  cols <- grep(driver_specs$regex[i], names(old), value = TRUE)
  years <- as.integer(str_match(cols, driver_specs$regex[i])[, 2])
  data.frame(
    driver = driver_specs$driver[i],
    legacy_last_year = max_year_from_names(names(old), driver_specs$regex[i]),
    stringsAsFactors = FALSE
  )
}))

# Limit the review to years that extend beyond the vetted legacy record
new_year_rows <- new_long %>%
  left_join(driver_year_limits, by = "driver") %>%
  filter(!is.na(new_value), !is.na(legacy_last_year), year > legacy_last_year)

site_baseline <- new_long %>%
  left_join(driver_year_limits, by = "driver") %>%
  filter(!is.na(new_value), !is.na(legacy_last_year), year <= legacy_last_year) %>%
  group_by(key, LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, driver, legacy_last_year) %>%
  summarise(
    baseline_n = n(),
    baseline_min = safe_min(new_value),
    baseline_max = safe_max(new_value),
    baseline_median = median(new_value, na.rm = TRUE),
    baseline_mad = safe_mad(new_value),
    .groups = "drop"
  )

peer_pool <- new_long %>%
  filter(!is.na(new_value)) %>%
  left_join(coord_ref, by = "key")

# Compare each site-year to the nearest nearby sites with data in that same year
build_spatial_peer_stats <- function(target_rows, peer_rows, max_peers = 5L) {
  if (!nrow(target_rows)) {
    return(data.frame())
  }

  out <- vector("list", nrow(target_rows))

  for (i in seq_len(nrow(target_rows))) {
    this_row <- target_rows[i, , drop = FALSE]
    peers <- peer_rows %>%
      filter(
        driver == this_row$driver,
        year == this_row$year,
        key != this_row$key,
        !is.na(Latitude),
        !is.na(Longitude)
      )

    if (!nrow(peers) || is.na(this_row$Latitude) || is.na(this_row$Longitude)) {
      out[[i]] <- data.frame(
        key = this_row$key,
        driver = this_row$driver,
        year = this_row$year,
        spatial_peer_n = NA_integer_,
        spatial_peer_median = NA_real_,
        spatial_peer_mad = NA_real_,
        spatial_peer_nearest_km = NA_real_,
        spatial_peer_farthest_used_km = NA_real_,
        stringsAsFactors = FALSE
      )
      next
    }

    peers$distance_km <- haversine_km(
      this_row$Latitude[[1]],
      this_row$Longitude[[1]],
      peers$Latitude,
      peers$Longitude
    )
    peers <- peers[order(peers$distance_km), , drop = FALSE]
    peers <- peers[seq_len(min(max_peers, nrow(peers))), , drop = FALSE]

    out[[i]] <- data.frame(
      key = this_row$key,
      driver = this_row$driver,
      year = this_row$year,
      spatial_peer_n = nrow(peers),
      spatial_peer_median = median(peers$new_value, na.rm = TRUE),
      spatial_peer_mad = safe_mad(peers$new_value),
      spatial_peer_nearest_km = min(peers$distance_km, na.rm = TRUE),
      spatial_peer_farthest_used_km = max(peers$distance_km, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }

  bind_rows(out)
}

new_year_rows_with_coords <- new_year_rows %>%
  left_join(coord_ref, by = "key") %>%
  left_join(elevation_ref, by = "key")

spatial_peer_year <- build_spatial_peer_stats(new_year_rows_with_coords, peer_pool)

# Flag values that look implausible relative to site history or nearby sites
review <- new_year_rows_with_coords %>%
  left_join(
    site_baseline,
    by = c("key", "LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name", "driver", "legacy_last_year")
  ) %>%
  left_join(spatial_peer_year, by = c("key", "driver", "year")) %>%
  mutate(
    outside_site_range = !is.na(baseline_min) & !is.na(baseline_max) &
      (new_value < baseline_min | new_value > baseline_max),
    site_mad_score = if_else(!is.na(baseline_mad) & baseline_mad > 0, abs(new_value - baseline_median) / baseline_mad, NA_real_),
    spatial_peer_mad_score = if_else(!is.na(spatial_peer_mad) & spatial_peer_mad > 0, abs(new_value - spatial_peer_median) / spatial_peer_mad, NA_real_),
    site_history_flag = baseline_n >= 5 & (outside_site_range | (!is.na(site_mad_score) & site_mad_score >= 4)),
    spatial_peer_flag = spatial_peer_n >= 3 & !is.na(spatial_peer_mad_score) & spatial_peer_mad_score >= 4,
    paired_annual_temp = case_when(
      driver == "snow" ~ new_long$new_value[match(
        paste(key, "airtemp", year, sep = "||"),
        paste(new_long$key, new_long$driver, new_long$year, sep = "||")
      )],
      TRUE ~ NA_real_
    ),
    snow_unlikely_tropical_lowland = driver == "snow" &
      !is.na(Latitude) & !is.na(elevation_m) &
      abs(Latitude) < 23.5 & elevation_m < 1500 &
      new_value > 0,
    snow_unlikely_warm_lowland = driver == "snow" &
      !is.na(Latitude) & !is.na(elevation_m) &
      abs(Latitude) < 35 & elevation_m < 500 &
      new_value >= 5,
    snow_unlikely_given_annual_temp = driver == "snow" &
      !is.na(paired_annual_temp) &
      (
        (paired_annual_temp >= 18 & new_value > 0) |
        (paired_annual_temp >= 12 & new_value >= 10)
      ),
    plausibility_flag = case_when(
      snow_unlikely_tropical_lowland ~ "review_snow_unlikely_tropical_lowland",
      snow_unlikely_warm_lowland ~ "review_snow_unlikely_warm_lowland",
      snow_unlikely_given_annual_temp ~ "review_snow_unlikely_given_annual_temp",
      baseline_n < 5 & (is.na(spatial_peer_n) | spatial_peer_n < 3) ~ "limited_baseline_and_spatial_peer_context",
      baseline_n < 5 & spatial_peer_flag ~ "review_nearby_site_outlier",
      baseline_n < 5 ~ "limited_site_baseline_check",
      site_history_flag & spatial_peer_flag ~ "review_site_history_and_nearby_sites",
      site_history_flag ~ "review_site_history",
      spatial_peer_flag ~ "review_nearby_site_outlier",
      TRUE ~ "plausible"
    )
  ) %>%
  select(
    LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
    driver, year, new_value, legacy_last_year,
    baseline_n, baseline_min, baseline_max, baseline_median, baseline_mad,
    Latitude, Longitude, elevation_m,
    paired_annual_temp,
    spatial_peer_n, spatial_peer_median, spatial_peer_mad,
    spatial_peer_nearest_km, spatial_peer_farthest_used_km,
    outside_site_range, site_mad_score, spatial_peer_mad_score,
    plausibility_flag
  ) %>%
  arrange(plausibility_flag, LTER, Stream_Name, driver, year)

summary <- review %>%
  count(driver, plausibility_flag, sort = TRUE, name = "n_rows")

write.csv(review, detail_file, row.names = FALSE, na = "")
write.csv(summary, summary_file, row.names = FALSE, na = "")

# Plot the mix of plausibility flags by driver
summary_plot <- summary %>%
  ggplot(aes(x = driver, y = n_rows, fill = plausibility_flag)) +
  geom_col(position = "stack") +
  theme_bw(base_size = 10) +
  labs(
    title = "Reasonable New Values QAQC",
    x = "Driver",
    y = "Flagged row count",
    fill = "QA flag"
  )
ggsave(
  filename = file.path(out_dir, review_file_name("new-values-plausibility-flags", date_tag, "png")),
  plot = summary_plot,
  width = 11,
  height = 7,
  dpi = 180
)
