library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# Compare the new AND ERA5-Land exports with the reference spatial-driver columns.
# The script looks in a few likely folders so it can be rerun from either
# downloaded Drive files or the repo output folder.
downloads_folder <- "/Users/sidneybush/Downloads"
today_label <- format(Sys.Date(), "%Y%m%d")

# Only search Downloads when that folder is available on this computer.
download_folders <- if (dir.exists(downloads_folder)) {
  list.dirs(downloads_folder, recursive = FALSE, full.names = TRUE)
} else {
  character(0)
}

places_to_check_for_era5 <- c(
  file.path(
    "generated_outputs",
    paste0("and_era5_spatial_driver_comparison_", today_label),
    "era5_land_AND_fine_scale_2001_2023"
  ),
  download_folders,
  downloads_folder,
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/spatial-data-files/gee/earth-engine-outputs"
) %>%
  unique()

reference_driver_path <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/spatial-data-files/appeears-nasa/all-data_si-extract_3_20260629.csv"
drive_folder_id <- Sys.getenv(
  "SILICA_ERA5_COMPARISON_DRIVE_FOLDER_ID",
  unset = "1hYedMgoR1907nwtOjjjqYFzjG28gk3-T"
)
drive_main_folder_name <- Sys.getenv(
  "SILICA_ERA5_COMPARISON_DRIVE_SUBFOLDER",
  unset = "Andrews sites only testing"
)
drive_plot_folder_name <- Sys.getenv(
  "SILICA_ERA5_COMPARISON_PLOT_FOLDER",
  unset = "plots"
)
drive_csv_folder_name <- Sys.getenv(
  "SILICA_ERA5_COMPARISON_CSV_FOLDER",
  unset = "csv files"
)
drive_account <- Sys.getenv("SILICA_GOOGLE_DRIVE_ACCOUNT", unset = "")
upload_to_drive <- toupper(Sys.getenv(
  "SILICA_UPLOAD_ERA5_COMPARISON_TO_DRIVE",
  unset = "TRUE"
)) == "TRUE"
drive_overwrite <- toupper(Sys.getenv(
  "SILICA_GOOGLE_DRIVE_OVERWRITE",
  unset = "TRUE"
)) == "TRUE"

output_folder <- file.path(
  "generated_outputs",
  paste0("and_era5_spatial_driver_comparison_", today_label)
)

dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

era5_pattern <- "^era5_land_[0-9]{4}_AND_fine_scale_watershed_extract\\.csv$"

# Check each likely folder and keep the ERA5-Land CSVs found there.
era5_files_by_folder <- lapply(places_to_check_for_era5, function(folder) {
  # Missing folders are fine; they just mean there are no files there.
  if (!dir.exists(folder)) {
    return(character(0))
  }

  list.files(
    folder,
    pattern = era5_pattern,
    full.names = TRUE
  )
})

era5_folder_summary <- tibble(
  folder = places_to_check_for_era5,
  n_files = lengths(era5_files_by_folder),

  # For each folder, record when its newest matching file was modified.
  latest_file_time = as.POSIXct(vapply(
    era5_files_by_folder,
    function(files) {
      # Empty folders get no date, so they will not be chosen below.
      if (!length(files)) {
        return(NA_real_)
      }

      max(file.info(files)$mtime)
    },
    numeric(1)
  ), origin = "1970-01-01")
) %>%
  filter(n_files > 0) %>%
  arrange(desc(n_files), desc(latest_file_time))

era5_folder <- era5_folder_summary$folder[1]

# Stop early with a clear next step if none of the expected files are present.
if (is.na(era5_folder)) {
  stop(
    "Could not find the corrected AND_fine_scale ERA5-Land files. Run the Colab notebook and download the Drive exports first."
  )
}

era5_files <- sort(era5_files_by_folder[[match(
  era5_folder,
  places_to_check_for_era5
)]])

# Some source files store percentages as 0-100 and others as 0-1.
# This keeps everything on the same 0-1 fraction scale.
as_fraction <- function(x) {
  # If a whole column is missing, leave it missing instead of warning.
  if (all(is.na(x))) {
    return(x)
  }

  # Values larger than 1.5 are almost certainly percentages.
  if (max(x, na.rm = TRUE) > 1.5) {
    return(x / 100)
  }

  x
}

# Pull the four-digit year out of a driver column or export file name.
parse_year <- function(x) {
  as.integer(sub(".*_(\\d{4})(_|$).*", "\\1", x))
}

# Earth Engine sometimes writes TRUE/FALSE flags as text, so accept both forms.
is_true_flag <- function(x) {
  tolower(as.character(x)) %in% c("true", "1")
}

# Format small numbers for plot labels without long decimal tails.
nice_number <- function(x) {
  # Keep missing values visible as missing labels.
  if (is.na(x)) {
    return(NA_character_)
  }

  # Very small non-zero values are easier to read with two significant digits.
  if (x != 0 && abs(x) < 0.01) {
    return(format(signif(x, 2), scientific = FALSE, trim = TRUE))
  }

  format(round(x, 2), trim = TRUE)
}

# Summarize how well ERA5-Land matches the reference product in each group.
summarize_fit <- function(data, group_columns) {
  data %>%
    filter(!is.na(reference_value), !is.na(era5_value)) %>%
    group_by(across(all_of(group_columns))) %>%
    summarise(
      n = n(),
      reference_min = min(reference_value),
      reference_max = max(reference_value),
      era5_min = min(era5_value),
      era5_max = max(era5_value),

      # Only fit a line when there are enough points and both axes vary.
      r_squared = if (
        n() >= 3 &&
          length(unique(reference_value)) >= 2 &&
          length(unique(era5_value)) >= 2
      ) {
        summary(lm(era5_value ~ reference_value))$r.squared
      } else {
        NA_real_
      },

      # Use the same guard before reporting the fitted line's slope.
      slope = if (
        n() >= 3 &&
          length(unique(reference_value)) >= 2 &&
          length(unique(era5_value)) >= 2
      ) {
        unname(coef(lm(era5_value ~ reference_value))[2])
      } else {
        NA_real_
      },

      # Use the same guard before reporting the fitted line's intercept.
      intercept = if (
        n() >= 3 &&
          length(unique(reference_value)) >= 2 &&
          length(unique(era5_value)) >= 2
      ) {
        unname(coef(lm(era5_value ~ reference_value))[1])
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    mutate(
      label = if_else(
        is.na(r_squared),
        "R2 = NA",
        paste0(
          "R2 = ", vapply(r_squared, nice_number, character(1))
        )
      )
    )
}

# Choose readable year marks for the plot legend.
year_breaks <- function(years) {
  years <- sort(unique(years[!is.na(years)]))

  # If there are only a few years, show each one.
  if (length(years) <= 8) {
    return(years)
  }

  pretty(years, n = 6)
}

# Return 365 or 366 so daily precipitation can be converted to annual totals.
days_in_year <- function(year) {
  ifelse((year %% 4 == 0 & year %% 100 != 0) | year %% 400 == 0, 366, 365)
}

# Find watersheds that have identical ERA5-Land values across all years.
# These groups get outlined in the plots so they are easy to spot.
find_sites_with_same_era5_values <- function(data) {
  data %>%
    filter(!is.na(era5_value)) %>%
    mutate(
      site_panel = paste0(stream_name, "\n", shapefile_name),
      era5_value_for_match = format(round(era5_value, 6), nsmall = 6, trim = TRUE),
      era5_year_value = paste(year, era5_value_for_match, sep = ":")
    ) %>%
    arrange(comparison, site_panel, year) %>%
    group_by(comparison, site_panel) %>%
    summarise(
      era5_value_history = paste(era5_year_value, collapse = "|"),
      .groups = "drop"
    ) %>%
    add_count(comparison, era5_value_history, name = "n_sites_sharing_era5") %>%
    group_by(comparison, era5_value_history) %>%
    mutate(
      shared_era5_sites = paste(site_panel, collapse = "; "),
      shared_era5_group = paste(sub("\\n.*", "", site_panel), collapse = ", ")
    ) %>%
    ungroup() %>%
    mutate(
      shared_era5_values = n_sites_sharing_era5 > 1,

      # Keep the shared-site notes only for repeated ERA5-Land histories.
      shared_era5_sites = if_else(
        shared_era5_values,
        shared_era5_sites,
        NA_character_
      ),
      shared_era5_group = if_else(
        shared_era5_values,
        shared_era5_group,
        NA_character_
      )
    ) %>%
    select(
      comparison,
      site_panel,
      shared_era5_values,
      n_sites_sharing_era5,
      shared_era5_sites,
      shared_era5_group
    )
}

# Build one comparison plot, faceted by site, with labels and shared-value outlines.
make_site_plot <- function(data, title, x_label, y_label) {
  complete <- data %>%
    filter(!is.na(reference_value), !is.na(era5_value)) %>%
    mutate(site_panel = paste0(stream_name, "\n", shapefile_name))

  fit_labels <- summarize_fit(complete, c("comparison", "site_panel")) %>%
    left_join(
      complete %>%
        distinct(
          comparison,
          site_panel,
          shared_era5_values,
          n_sites_sharing_era5,
          shared_era5_sites,
          shared_era5_group
        ),
      by = c("comparison", "site_panel")
    ) %>%
    mutate(
      x = -Inf,
      y = Inf
    )

  panels_to_outline <- fit_labels %>%
    filter(shared_era5_values %in% TRUE) %>%
    mutate(
      box_xmin = -Inf,
      box_xmax = Inf,
      box_ymin = -Inf,
      box_ymax = Inf
    )

  outline_colors <- c(
    "#2C7FB8",
    "#31A354",
    "#E6550D",
    "#756BB1",
    "#636363",
    "#E7298A"
  )
  groups_with_same_era5 <- sort(unique(panels_to_outline$shared_era5_group))
  outline_color_for_group <- setNames(
    outline_colors[seq_along(groups_with_same_era5)],
    groups_with_same_era5
  )

  # Only add the plot caption when at least one shared-value group exists.
  outline_note <- if (nrow(panels_to_outline) > 0) {
    "Box colors identify groups of watersheds with the same ERA5-Land annual values."
  } else {
    NULL
  }

  ggplot(complete, aes(x = reference_value, y = era5_value)) +
    # The dotted 1:1 line shows where ERA5-Land would equal the reference driver.
    geom_abline(
      slope = 1,
      intercept = 0,
      color = "grey45",
      linetype = "dotted",
      linewidth = 0.45
    ) +
    geom_point(
      aes(fill = year),
      shape = 21,
      color = "white",
      stroke = 0.15,
      size = 2.8,
      alpha = 0.9
    ) +
    geom_rect(
      data = panels_to_outline,
      aes(
        xmin = box_xmin,
        xmax = box_xmax,
        ymin = box_ymin,
        ymax = box_ymax,
        color = shared_era5_group
      ),
      inherit.aes = FALSE,
      fill = NA,
      linewidth = 0.7
    ) +
    geom_text(
      data = fit_labels,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = -0.05,
      vjust = 1.45,
      size = 2.6,
      color = "grey15"
    ) +
    facet_wrap(~site_panel, scales = "free", ncol = 4) +
    scale_fill_gradientn(
      name = "Year",
      colors = c("#D8ECF8", "#08306B"),
      breaks = year_breaks(complete$year)
    ) +
    scale_color_manual(
      name = "Same ERA5-Land values",
      values = outline_color_for_group,
      na.translate = FALSE
    ) +
    guides(
      fill = guide_colorbar(
        barwidth = grid::unit(2.8, "in"),
        barheight = grid::unit(0.18, "in")
      )
    ) +
    labs(title = title, x = x_label, y = y_label, caption = outline_note) +
    theme_minimal(base_size = 10.5) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.caption = element_text(size = 8.5, color = "grey35"),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

# Sign in to Google Drive once before making folders or uploading files.
authenticate_drive <- function() {
  # Keep the script runnable without Drive if upload is turned off.
  if (!upload_to_drive) {
    return(invisible(NULL))
  }

  # Stop with a clear message instead of failing later inside googledrive.
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Install the googledrive package or set SILICA_UPLOAD_ERA5_COMPARISON_TO_DRIVE=FALSE.",
      call. = FALSE
    )
  }

  # Use a saved token when available; otherwise let googledrive open auth.
  auth_ok <- tryCatch(
    {
      if (nzchar(drive_account)) {
        googledrive::drive_auth(email = drive_account)
      } else {
        googledrive::drive_auth()
      }
      TRUE
    },
    error = function(error) FALSE
  )

  # A readable stop makes it clear what the user needs to do next.
  if (!auth_ok) {
    stop(
      "Google Drive auth failed. Run googledrive::drive_auth() once from R, then rerun this script.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# Reuse a Drive folder when it exists; otherwise make it.
drive_child_folder <- function(folder_name, parent) {
  folder_matches <- googledrive::drive_ls(parent) %>%
    filter(name == folder_name)

  if (nrow(folder_matches) > 0) {
    return(folder_matches[1, ])
  }

  googledrive::drive_mkdir(folder_name, path = parent)
}

# Upload a finished output file to the requested Drive subfolder.
upload_output_to_drive <- function(path, drive_folder) {
  # Keep the script runnable without Drive if upload is turned off.
  if (!upload_to_drive) {
    return(invisible(NULL))
  }

  uploaded <- googledrive::drive_upload(
    media = path,
    path = googledrive::as_id(drive_folder),
    name = basename(path),
    overwrite = drive_overwrite
  )
  message("Uploaded to Google Drive: ", uploaded$name)
  invisible(uploaded)
}

# Read each yearly ERA5-Land CSV and stack them into one table.
era5_raw <- era5_files %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  bind_rows()

# Previous exports may not include this column, so add it as blank when needed.
if (!"used_centroid_fallback" %in% names(era5_raw)) {
  era5_raw$used_centroid_fallback <- NA
}

# Previous exports may not include this column, so add it as blank when needed.
if (!"used_fine_scale_fallback" %in% names(era5_raw)) {
  stop(
    "The ERA5-Land files do not include used_fine_scale_fallback. ",
    "These are not the corrected small-watershed exports from the Colab notebook.",
    call. = FALSE
  )
}

# Keep the AND rows and label how each ERA5-Land value was extracted.
era5 <- era5_raw %>%
  filter(tolower(lter) == "and" | grepl("^and__", site_id)) %>%
  mutate(
    shapefile_key = toupper(shapefile_name),
    stream_name = toupper(stream_name),
    used_centroid_fallback = if_else(
      is.na(used_centroid_fallback),
      NA_character_,
      as.character(used_centroid_fallback)
    ),
    used_fine_scale_fallback = if_else(
      is.na(used_fine_scale_fallback),
      NA_character_,
      as.character(used_fine_scale_fallback)
    ),

    # This label makes the plots easier to audit if a small watershed needed a fallback.
    era5_fallback_method = case_when(
      is_true_flag(used_fine_scale_fallback) ~ "Fine-scale polygon retry",
      is_true_flag(used_centroid_fallback) ~ "Centroid fill",
      TRUE ~ "Native-scale polygon mean"
    )
  )

reference_drivers <- read_csv(reference_driver_path, show_col_types = FALSE) %>%
  filter(LTER == "AND") %>%
  mutate(
    shapefile_key = toupper(Shapefile_Name),
    Stream_Name = toupper(Stream_Name)
  )

modis_et_cols <- grep("^evapotrans_[0-9]{4}_kg_m2$", names(reference_drivers), value = TRUE)
modis_snow_cols <- grep("^snow_[0-9]{4}_max_prop_area$", names(reference_drivers), value = TRUE)
noaa_temp_cols <- grep("^temp_[0-9]{4}_degC$", names(reference_drivers), value = TRUE)
gpcp_precip_cols <- grep("^precip_[0-9]{4}_mm_per_day$", names(reference_drivers), value = TRUE)

modis_et <- reference_drivers %>%
  select(
    LTER,
    Shapefile_Name,
    Stream_Name,
    shapefile_key,
    all_of(modis_et_cols)
  ) %>%
  pivot_longer(
    all_of(modis_et_cols),
    names_to = "reference_variable",
    values_to = "reference_value"
  ) %>%
  mutate(year = parse_year(reference_variable))

modis_snow <- reference_drivers %>%
  select(
    LTER,
    Shapefile_Name,
    Stream_Name,
    shapefile_key,
    all_of(modis_snow_cols)
  ) %>%
  pivot_longer(
    all_of(modis_snow_cols),
    names_to = "reference_variable",
    values_to = "reference_value"
  ) %>%
  mutate(year = parse_year(reference_variable))

noaa_temp <- reference_drivers %>%
  select(
    LTER,
    Shapefile_Name,
    Stream_Name,
    shapefile_key,
    all_of(noaa_temp_cols)
  ) %>%
  pivot_longer(
    all_of(noaa_temp_cols),
    names_to = "reference_variable",
    values_to = "reference_value"
  ) %>%
  mutate(year = parse_year(reference_variable))

gpcp_precip <- reference_drivers %>%
  select(
    LTER,
    Shapefile_Name,
    Stream_Name,
    shapefile_key,
    all_of(gpcp_precip_cols)
  ) %>%
  pivot_longer(
    all_of(gpcp_precip_cols),
    names_to = "reference_variable",
    values_to = "reference_value"
  ) %>%
  mutate(
    year = parse_year(reference_variable),
    reference_value = reference_value * days_in_year(year)
  )

et_points <- era5 %>%
  inner_join(
    modis_et,
    by = c("shapefile_key", "year"),
    suffix = c("_era5", "_modis")
  ) %>%
  transmute(
    comparison = "Evapotranspiration",
    reference_product = "MODIS ET driver",
    reference_variable,
    site_id,
    shapefile_name,
    stream_name,
    year,
    tiny_watershed,
    used_fine_scale_fallback,
    used_centroid_fallback,
    era5_fallback_method,
    polygon_area_km2,
    era5_value = evapotrans_mm,
    reference_value,
    point_label = paste0(stream_name, " ", year)
  )

snow_points <- era5 %>%
  inner_join(
    modis_snow,
    by = c("shapefile_key", "year"),
    suffix = c("_era5", "_modis")
  ) %>%
  transmute(
    comparison = "Snow cover",
    reference_product = "MODIS snow-cover driver",
    reference_variable,
    site_id,
    shapefile_name,
    stream_name,
    year,
    tiny_watershed,
    used_fine_scale_fallback,
    used_centroid_fallback,
    era5_fallback_method,
    polygon_area_km2,
    era5_value = as_fraction(snow_cover_fraction),
    reference_value = as_fraction(reference_value),
    point_label = paste0(stream_name, " ", year)
  )

temp_points <- era5 %>%
  inner_join(
    noaa_temp,
    by = c("shapefile_key", "year"),
    suffix = c("_era5", "_noaa")
  ) %>%
  transmute(
    comparison = "Air temperature",
    reference_product = "NOAA temperature driver",
    reference_variable,
    site_id,
    shapefile_name,
    stream_name,
    year,
    tiny_watershed,
    used_fine_scale_fallback,
    used_centroid_fallback,
    era5_fallback_method,
    polygon_area_km2,
    era5_value = temp_degC,
    reference_value,
    point_label = paste0(stream_name, " ", year)
  )

precip_points <- era5 %>%
  inner_join(
    gpcp_precip,
    by = c("shapefile_key", "year"),
    suffix = c("_era5", "_gpcp")
  ) %>%
  transmute(
    comparison = "Precipitation",
    reference_product = "GPCP precipitation driver",
    reference_variable,
    site_id,
    shapefile_name,
    stream_name,
    year,
    tiny_watershed,
    used_fine_scale_fallback,
    used_centroid_fallback,
    era5_fallback_method,
    polygon_area_km2,
    era5_value = precip_mm,
    reference_value,
    point_label = paste0(stream_name, " ", year)
  )

comparison_points <- bind_rows(et_points, snow_points, temp_points, precip_points)

# Stop here if the two data sources do not share any AND site-year rows.
if (nrow(comparison_points) == 0) {
  stop("No matching AND rows were found between the ERA5-Land and reference driver file.")
}

same_era5_notes <- find_sites_with_same_era5_values(comparison_points)

comparison_points <- comparison_points %>%
  mutate(site_panel = paste0(stream_name, "\n", shapefile_name)) %>%
  left_join(
    same_era5_notes,
    by = c("comparison", "site_panel")
  )

site_stats <- summarize_fit(
  comparison_points,
  c("comparison", "site_panel")
) %>%
  left_join(
    same_era5_notes,
    by = c("comparison", "site_panel")
  )

et_points <- comparison_points %>%
  filter(comparison == "Evapotranspiration")

snow_points <- comparison_points %>%
  filter(comparison == "Snow cover")

temp_points <- comparison_points %>%
  filter(comparison == "Air temperature")

precip_points <- comparison_points %>%
  filter(comparison == "Precipitation")

et_site_plot <- make_site_plot(
  et_points,
  "AND sites: ERA5-Land versus MODIS ET driver",
  "MODIS ET driver (kg m-2 yr-1)",
  "ERA5-Land ET (mm yr-1)"
)

snow_site_plot <- make_site_plot(
  snow_points,
  "AND sites: ERA5-Land versus MODIS snow-cover driver",
  "MODIS snow-cover driver (fraction)",
  "ERA5-Land annual snow cover (fraction)"
)

temp_site_plot <- make_site_plot(
  temp_points,
  "AND sites: ERA5-Land versus NOAA temperature driver",
  "NOAA temperature driver (deg C)",
  "ERA5-Land air temperature (deg C)"
)

precip_site_plot <- make_site_plot(
  precip_points,
  "AND sites: ERA5-Land versus GPCP precipitation driver",
  "GPCP precipitation driver (mm yr-1)",
  "ERA5-Land precipitation (mm yr-1)"
)

comparison_points_file <- file.path(
  output_folder,
  "and_era5land_spatial_driver_comparison_points.csv"
)
site_stats_file <- file.path(
  output_folder,
  "and_era5land_spatial_driver_site_regression_stats.csv"
)
et_plot_file <- file.path(
  output_folder,
  "and_era5land_evapotranspiration_by_site.png"
)
snow_plot_file <- file.path(
  output_folder,
  "and_era5land_snow_cover_by_site.png"
)
temp_plot_file <- file.path(
  output_folder,
  "and_era5land_temperature_by_site.png"
)
precip_plot_file <- file.path(
  output_folder,
  "and_era5land_precipitation_by_site.png"
)

write_csv(comparison_points, comparison_points_file)
write_csv(site_stats, site_stats_file)

ggsave(
  et_plot_file,
  et_site_plot,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  snow_plot_file,
  snow_site_plot,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  temp_plot_file,
  temp_site_plot,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  precip_plot_file,
  precip_site_plot,
  width = 12,
  height = 8,
  dpi = 300
)

message("Wrote comparison outputs to: ", normalizePath(output_folder))
message("ERA5-Land years used: ", paste(sort(unique(era5$year)), collapse = ", "))

# Send the finished comparison products to tidy folders in Google Drive.
if (upload_to_drive) {
  authenticate_drive()
  drive_main_folder <- drive_child_folder(
    drive_main_folder_name,
    googledrive::as_id(drive_folder_id)
  )
  drive_plot_folder <- drive_child_folder(
    drive_plot_folder_name,
    googledrive::as_id(drive_main_folder)
  )
  drive_csv_folder <- drive_child_folder(
    drive_csv_folder_name,
    googledrive::as_id(drive_main_folder)
  )

  invisible(lapply(
    c(et_plot_file, snow_plot_file, temp_plot_file, precip_plot_file),
    upload_output_to_drive,
    drive_folder = drive_plot_folder
  ))
  invisible(lapply(
    c(comparison_points_file, site_stats_file),
    upload_output_to_drive,
    drive_folder = drive_csv_folder
  ))
}
