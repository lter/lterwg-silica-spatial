# Compare the older combined table to the newer local combined table, then
# write review files for shared sites and new-only sites
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readxl)
})

source(file.path("04_combine_qaqc", "00_qaqc_config.R"))
source(file.path("04_combine_qaqc", "00_qaqc_functions.R"))

out_dir <- harmonization_dir

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

has_any_payload <- function(df) {
  if (!ncol(df)) {
    return(rep(FALSE, nrow(df)))
  }

  apply(df, 1, function(row) {
    any(vapply(row, function(val) {
      if (is.na(val)) {
        return(FALSE)
      }
      nzchar(trimws(as.character(val)))
    }, logical(1)))
  })
}

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

safe_year_min <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return(NA_integer_)
  }
  min(x)
}

safe_year_max <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return(NA_integer_)
  }
  max(x)
}

build_shared_overlap_review <- function(old_long, new_long, shared_keys) {
  if (!nrow(old_long) || !nrow(new_long) || !length(shared_keys)) {
    empty_detail <- data.frame(
      key = character(0),
      LTER = character(0),
      Stream_Name = character(0),
      Discharge_File_Name = character(0),
      Shapefile_Name = character(0),
      driver = character(0),
      metric_col = character(0),
      year = integer(0),
      old_value = numeric(0),
      new_value = numeric(0),
      abs_diff = numeric(0),
      pct_diff = numeric(0),
      overlap_conflict = logical(0),
      stringsAsFactors = FALSE
    )

    empty_summary <- data.frame(
      driver = character(0),
      n_pairs = integer(0),
      n_sites = integer(0),
      year_min = integer(0),
      year_max = integer(0),
      cor = numeric(0),
      n_conflicts = integer(0),
      mae = numeric(0),
      median_abs = numeric(0),
      p95_abs = numeric(0),
      stringsAsFactors = FALSE
    )

    return(list(detail = empty_detail, summary = empty_summary))
  }

  shared_long <- old_long %>%
    filter(key %in% shared_keys) %>%
    inner_join(
      new_long %>% filter(key %in% shared_keys),
      by = c(
        "key",
        "LTER",
        "Stream_Name",
        "Discharge_File_Name",
        "Shapefile_Name",
        "driver",
        "metric_col",
        "year"
      )
    ) %>%
    filter(!is.na(old_value), !is.na(new_value))

  shared_detail <- shared_long %>%
    mutate(
      abs_diff = abs(new_value - old_value),
      pct_diff = dplyr::if_else(
        !is.na(old_value) & old_value != 0,
        abs(new_value - old_value) / abs(old_value),
        NA_real_
      ),
      overlap_conflict = abs_diff > 1e-8
    )

  shared_summary <- shared_detail %>%
    group_by(driver) %>%
    summarise(
      n_pairs = n(),
      n_sites = n_distinct(key),
      year_min = safe_year_min(year),
      year_max = safe_year_max(year),
      cor = if (n() > 1) suppressWarnings(cor(old_value, new_value)) else NA_real_,
      n_conflicts = sum(overlap_conflict, na.rm = TRUE),
      mae = mean(abs_diff, na.rm = TRUE),
      median_abs = median(abs_diff, na.rm = TRUE),
      p95_abs = as.numeric(stats::quantile(abs_diff, 0.95, na.rm = TRUE)),
      .groups = "drop"
    )

  list(detail = shared_detail, summary = shared_summary)
}

# Load any manual notes so they can override default follow-up labels
read_site_followup_notes <- function(site_followup_file) {
  if (!nzchar(site_followup_file) || !file.exists(site_followup_file)) {
    return(NULL)
  }

  notes <- read.csv(
    site_followup_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  prepare_combined_table(notes)
}

# Build the list of new-only sites that still need spatial follow-up
build_spatial_followup <- function(new_only, coord_ref, site_followup_notes = NULL) {
  spatial_followup <- new_only %>%
    filter(!has_any_spatial_data) %>%
    select(
      key,
      LTER,
      Stream_Name,
      Discharge_File_Name,
      Shapefile_Name,
      missing_shapefile_name
    ) %>%
    left_join(coord_ref %>% select(key, drainSqKm, Shapefile_Source), by = "key") %>%
    mutate(
      hydrosheds_possible = !is.na(drainSqKm) & drainSqKm > 100,
      default_reason = case_when(
        missing_shapefile_name & hydrosheds_possible ~ "No shapefile is listed yet, but this site is large enough to try HydroSHEDS.",
        missing_shapefile_name & !is.na(drainSqKm) & !hydrosheds_possible ~ "No shapefile is listed, and the site is below the usual HydroSHEDS size cutoff.",
        missing_shapefile_name ~ "No shapefile is listed, and drainage area is unknown.",
        TRUE ~ "A shapefile name exists, but the site still did not extract data. Check the shapefile for corruption or projection problems."
      ),
      default_next_step = case_when(
        missing_shapefile_name & hydrosheds_possible ~ "Try HydroSHEDS.",
        missing_shapefile_name & !is.na(drainSqKm) & !hydrosheds_possible ~ "Add or fix an artisanal shapefile.",
        missing_shapefile_name ~ "Review the site and choose a watershed source.",
        TRUE ~ "Check the shapefile for corruption, projection problems, or other extraction failures."
      ),
      followup_status = "needs_review",
      why_missing = default_reason,
      next_step = default_next_step,
      is_concern = TRUE,
      exclude_from_followup = FALSE,
      notes = NA_character_
    )

  if (!is.null(site_followup_notes) && nrow(site_followup_notes) > 0) {
    spatial_followup <- spatial_followup %>%
      left_join(
        site_followup_notes %>%
          transmute(
            key,
            followup_status_note = if ("followup_status" %in% names(.)) norm_chr(followup_status) else NA_character_,
            why_missing_note = if ("why_missing" %in% names(.)) norm_chr(why_missing) else NA_character_,
            next_step_note = if ("next_step" %in% names(.)) norm_chr(next_step) else NA_character_,
            is_concern_note = if ("is_concern" %in% names(.)) is_concern else NA,
            exclude_note = if ("exclude_from_followup" %in% names(.)) exclude_from_followup else NA,
            notes_note = if ("notes" %in% names(.)) norm_chr(notes) else NA_character_
          ),
        by = "key"
      ) %>%
      mutate(
        followup_status = coalesce(followup_status_note, followup_status),
        why_missing = coalesce(why_missing_note, why_missing),
        next_step = coalesce(next_step_note, next_step),
        is_concern = ifelse(is.na(is_concern_note), is_concern, parse_yes_no(is_concern_note, default = TRUE)),
        exclude_from_followup = ifelse(is.na(exclude_note), exclude_from_followup, parse_yes_no(exclude_note, default = FALSE)),
        notes = coalesce(notes_note, notes)
      ) %>%
      select(-followup_status_note, -why_missing_note, -next_step_note, -is_concern_note, -exclude_note, -notes_note)
  }

  spatial_followup %>%
    arrange(exclude_from_followup, desc(is_concern), LTER, Stream_Name)
}

# Compare each new-only site to the nearest vetted legacy site by driver means
build_nearest_legacy_review <- function(new_only, old, old_long, new_long, coord_ref) {
  old_annual_means <- old_long %>%
    group_by(key, driver) %>%
    summarise(old_driver_mean = mean(old_value, na.rm = TRUE), .groups = "drop")

  new_annual_means <- new_long %>%
    filter(key %in% new_only$key) %>%
    group_by(key, LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, driver) %>%
    summarise(new_driver_mean = mean(new_value, na.rm = TRUE), .groups = "drop")

  new_sites_with_coords <- new_only %>%
    select(key, LTER, Stream_Name, Discharge_File_Name, Shapefile_Name) %>%
    left_join(coord_ref, by = "key") %>%
    filter(!is.na(Latitude), !is.na(Longitude))

  old_sites_with_coords <- old %>%
    select(key, LTER, Stream_Name, Discharge_File_Name, Shapefile_Name) %>%
    left_join(coord_ref, by = "key") %>%
    filter(!is.na(Latitude), !is.na(Longitude))

  nearest_rows <- list()
  for (i in seq_len(nrow(new_sites_with_coords))) {
    dists <- haversine_km(
      new_sites_with_coords$Latitude[i],
      new_sites_with_coords$Longitude[i],
      old_sites_with_coords$Latitude,
      old_sites_with_coords$Longitude
    )
    if (!length(dists) || all(is.na(dists))) next
    j <- which.min(dists)
    nearest_rows[[length(nearest_rows) + 1]] <- data.frame(
      new_key = new_sites_with_coords$key[i],
      nearest_old_key = old_sites_with_coords$key[j],
      nearest_old_distance_km = dists[j],
      stringsAsFactors = FALSE
    )
  }

  nearest_tbl <- bind_rows(nearest_rows)

  new_annual_means %>%
    left_join(nearest_tbl, by = c("key" = "new_key")) %>%
    left_join(
      old_sites_with_coords %>%
        select(
          nearest_old_key = key,
          nearest_old_LTER = LTER,
          nearest_old_Stream_Name = Stream_Name
        ),
      by = "nearest_old_key"
    ) %>%
    left_join(
      old_annual_means %>% rename(nearest_old_key = key),
      by = c("nearest_old_key", "driver")
    ) %>%
    mutate(
      abs_diff = abs(new_driver_mean - old_driver_mean),
      ratio = dplyr::if_else(
        !is.na(old_driver_mean) & old_driver_mean != 0,
        new_driver_mean / old_driver_mean,
        NA_real_
      )
    )
}

build_shortlist_outputs <- function(nearest_compare, spatial_followup) {
  missing_key <- unique(paste(
    spatial_followup$LTER,
    spatial_followup$Stream_Name,
    spatial_followup$Discharge_File_Name,
    sep = "||"
  ))

  nearest_compare$key <- paste(
    nearest_compare$LTER,
    nearest_compare$Stream_Name,
    nearest_compare$Discharge_File_Name,
    sep = "||"
  )

  usable_new_sites <- nearest_compare %>%
    filter(
      !key %in% missing_key,
      !is.na(new_driver_mean),
      !is.na(old_driver_mean)
    )

  shortlist_sites <- usable_new_sites %>%
    distinct(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name) %>%
    arrange(LTER, Stream_Name)

  shortlist_summary <- usable_new_sites %>%
    group_by(LTER, Stream_Name, driver) %>%
    summarise(
      nearest_old_LTER = first(nearest_old_LTER),
      nearest_old_Stream_Name = first(nearest_old_Stream_Name),
      nearest_old_distance_km = mean(nearest_old_distance_km, na.rm = TRUE),
      abs_diff = mean(abs_diff, na.rm = TRUE),
      ratio = mean(ratio, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      nearest_old_distance_km = round(nearest_old_distance_km, 3),
      abs_diff = round(abs_diff, 3),
      ratio = round(ratio, 3)
    ) %>%
    arrange(LTER, Stream_Name, driver)

  list(
    sites = shortlist_sites,
    summary = shortlist_summary
  )
}

build_extension_review_flags <- function(old_long, new_long) {
  overlap <- old_long %>%
    inner_join(
      new_long,
      by = c(
        "key",
        "LTER",
        "Stream_Name",
        "Discharge_File_Name",
        "Shapefile_Name",
        "driver",
        "year"
      )
    ) %>%
    filter(!is.na(old_value), !is.na(new_value))

  overlap_stats <- overlap %>%
    group_by(
      key,
      LTER,
      Stream_Name,
      Discharge_File_Name,
      Shapefile_Name,
      driver
    ) %>%
    summarise(
      overlap_years = n(),
      overlap_correlation = if (n() >= 2) {
        suppressWarnings(cor(old_value, new_value, use = "complete.obs"))
      } else {
        NA_real_
      },
      mean_abs_error = mean(abs(new_value - old_value), na.rm = TRUE),
      .groups = "drop"
    )

  extension_rows <- new_long %>%
    filter(!is.na(new_value)) %>%
    anti_join(
      old_long %>% filter(!is.na(old_value)),
      by = c(
        "key",
        "LTER",
        "Stream_Name",
        "Discharge_File_Name",
        "Shapefile_Name",
        "driver",
        "year"
      )
    )

  extension_rows %>%
    left_join(
      overlap_stats,
      by = c(
        "key",
        "LTER",
        "Stream_Name",
        "Discharge_File_Name",
        "Shapefile_Name",
        "driver"
      )
    ) %>%
    mutate(
      review_flag = case_when(
        is.na(overlap_years) | overlap_years == 0 ~ "extension_without_overlap_baseline",
        driver == "snow" & !is.na(mean_abs_error) & mean_abs_error > 15 ~ "review_snow_overlap_error",
        overlap_years < 5 | (!is.na(overlap_correlation) & overlap_correlation < 0.7) ~ "review_low_overlap_correlation",
        TRUE ~ "extension_plausible_check"
      )
    ) %>%
    arrange(review_flag, LTER, Stream_Name, driver, year)
}

driver_specs <- tibble::tribble(
  ~driver, ~regex,
  "evapo", "^evapotrans_([0-9]{4})_kg_m2$",
  "greenup", "^greenup_cycle[01]_([0-9]{4})MMDD$",
  "precip", "^precip_([0-9]{4})_mm_per_day$",
  "airtemp", "^temp_([0-9]{4})_degC$",
  "snow", "^snow_([0-9]{4})_num_days$",
  "npp", "^npp_([0-9]{4})_kgC_m2_year$"
)

old <- read_combined_table(old_file)
new <- read_combined_table(new_file, sanitize_new = TRUE)
ref <- prepare_combined_table(read_excel(ref_file))

new_only <- new %>% filter(!key %in% old$key)
shared_keys <- intersect(old$key, new$key)

# Mark which new-only rows still lack extracted spatial payload
spatial_value_cols <- grep(
  "^(evapotrans_|greenup_cycle[01]_|precip_|temp_|snow_|npp_|elevation_|basin_slope_|major_rock$|rocks_|permafrost_|major_soil$|soil_)",
  names(new_only),
  value = TRUE
)

new_only <- new_only %>%
  mutate(
    has_any_spatial_data = has_any_payload(dplyr::pick(dplyr::all_of(spatial_value_cols))),
    missing_shapefile_name = is.na(Shapefile_Name)
  )

old_long <- extract_driver_long(old, driver_specs, "old_value")
new_long <- extract_driver_long(new, driver_specs, "new_value")

# Review how well the overlapping site-year values agree
overlap_review <- build_shared_overlap_review(old_long, new_long, shared_keys)
shared_detail <- overlap_review$detail
shared_summary <- overlap_review$summary

shared_detail_out <- file.path(
  out_dir,
  review_file_name("shared-sites-overlap-detail", date_tag)
)
shared_conflicts_out <- file.path(
  out_dir,
  review_file_name("shared-sites-overlap-conflicts", date_tag)
)

write.csv(
  shared_detail,
  shared_detail_out,
  row.names = FALSE,
  na = ""
)
write.csv(
  shared_detail %>% filter(overlap_conflict),
  shared_conflicts_out,
  row.names = FALSE,
  na = ""
)

write.csv(
  shared_summary,
  file.path(out_dir, review_file_name("shared-sites-overlap-only-driver-check", date_tag)),
  row.names = FALSE
)

if (nrow(shared_detail) > 0) {
  shared_plot <- ggplot(shared_detail, aes(x = old_value, y = new_value)) +
    geom_point(alpha = 0.12, size = 0.7) +
    geom_abline(intercept = 0, slope = 1, color = "firebrick", linewidth = 0.4) +
    facet_wrap(~driver, scales = "free") +
    theme_bw(base_size = 10) +
    labs(
      title = "Old vs New Overlap Check",
      subtitle = "Shared site-year annual driver values",
      x = "Old value",
      y = "New value"
    )
  ggsave(
    filename = file.path(out_dir, review_file_name("shared-sites-overlap-driver-scatter", date_tag, "png")),
    plot = shared_plot,
    width = 12,
    height = 8,
    dpi = 180
  )
}

shared_conflicts_only <- shared_detail %>% filter(overlap_conflict)
if (nrow(shared_conflicts_only) > 0) {
  shared_conflict_plot <- shared_conflicts_only %>%
    ggplot(aes(x = driver, y = abs_diff)) +
    geom_boxplot(outlier.alpha = 0.2) +
    theme_bw(base_size = 10) +
    labs(
      title = "Overlap Conflicts by Driver",
      x = "Driver",
      y = "Absolute old-new difference"
    )
  ggsave(
    filename = file.path(out_dir, review_file_name("shared-sites-overlap-conflicts", date_tag, "png")),
    plot = shared_conflict_plot,
    width = 10,
    height = 7,
    dpi = 180
  )
}

coord_ref <- ref %>%
  transmute(
    key,
    drainSqKm = suppressWarnings(as.numeric(drainSqKm)),
    Shapefile_Source = if ("Shapefile_Source" %in% names(.)) norm_chr(Shapefile_Source) else NA_character_,
    Latitude = suppressWarnings(as.numeric(Latitude)),
    Longitude = suppressWarnings(as.numeric(Longitude))
  )

# Build and write the new-only site follow-up tables
site_followup_notes <- read_site_followup_notes(site_followup_file)
spatial_followup <- build_spatial_followup(new_only, coord_ref, site_followup_notes)

site_followup_out <- file.path(
  out_dir,
  review_file_name("sites-needing-spatial-followup", date_tag)
)
site_followup_summary_out <- file.path(
  out_dir,
  review_file_name("sites-needing-spatial-followup-summary", date_tag)
)

write.csv(
  spatial_followup,
  site_followup_out,
  row.names = FALSE,
  na = ""
)

write.csv(
  spatial_followup %>%
    count(followup_status, next_step, is_concern, exclude_from_followup, sort = TRUE),
  site_followup_summary_out,
  row.names = FALSE
)

# Plot the largest groups of sites still needing spatial follow-up
followup_plot <- spatial_followup %>%
  filter(!exclude_from_followup) %>%
  count(LTER, sort = TRUE) %>%
  slice_max(n, n = 20, with_ties = FALSE) %>%
  ggplot(aes(x = reorder(LTER, n), y = n)) +
  geom_col(fill = "#4C78A8") +
  coord_flip() +
  theme_bw(base_size = 10) +
  labs(
    title = "Sites Still Needing Spatial Follow-Up",
    x = NULL,
    y = "Site count"
  )
ggsave(
  filename = file.path(out_dir, review_file_name("sites-needing-spatial-followup-by-lter", date_tag, "png")),
  plot = followup_plot,
  width = 10,
  height = 7,
  dpi = 180
)

# Compare new-only sites to the nearest vetted site for manual screening
nearest_compare <- build_nearest_legacy_review(new_only, old, old_long, new_long, coord_ref)

if (write_detailed_review) {
  write.csv(
    nearest_compare,
    file.path(out_dir, review_file_name("new-sites-vs-nearest-legacy-site", date_tag)),
    row.names = FALSE,
    na = ""
  )

  nearest_plot <- nearest_compare %>%
    filter(!is.na(old_driver_mean), !is.na(new_driver_mean)) %>%
    ggplot(aes(x = old_driver_mean, y = new_driver_mean)) +
    geom_point(alpha = 0.35, size = 0.8) +
    geom_abline(intercept = 0, slope = 1, color = "firebrick", linewidth = 0.4) +
    facet_wrap(~driver, scales = "free") +
    theme_bw(base_size = 10) +
    labs(
      title = "New-only Sites vs Nearest Legacy Site",
      subtitle = "Driver means for reasonableness screening",
      x = "Nearest old-site mean",
      y = "New-only site mean"
    )
  ggsave(
    filename = file.path(out_dir, review_file_name("new-sites-vs-nearest-legacy-site-scatter", date_tag, "png")),
    plot = nearest_plot,
    width = 12,
    height = 8,
    dpi = 180
  )

  shortlist <- build_shortlist_outputs(nearest_compare, spatial_followup)
  write.csv(
    shortlist$sites,
    file.path(out_dir, paste0("current_harmonization_shortlist_sites_", date_tag, ".csv")),
    row.names = FALSE,
    na = ""
  )
  write.csv(
    shortlist$summary,
    file.path(out_dir, paste0("current_harmonization_shortlist_driver_summary_", date_tag, ".csv")),
    row.names = FALSE,
    na = ""
  )

  review_flags <- build_extension_review_flags(old_long, new_long)
  write.csv(
    review_flags,
    file.path(inputs$review_root, "year_extension", paste0("new_years_review_flags_", date_tag, ".csv")),
    row.names = FALSE,
    na = ""
  )
}
