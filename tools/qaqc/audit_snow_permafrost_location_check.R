librarian::shelf(dplyr, readr)

date_tag <- Sys.getenv("SILICA_FINAL_HARMONIZED_DATE", unset = "20260629")
box_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn"
data_root <- file.path(box_root, "spatial-data-extractions")
audit_dir <- file.path(data_root, "qaqc")
annual_with_wrtds_dir <- file.path(
  data_root,
  "spatial-data-files",
  "appeears-nasa",
  "annual-with-wrtds"
)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

full_spatial_file <- file.path(
  annual_with_wrtds_dir,
  paste0("final_annual_dataset_", date_tag, ".csv")
)
site_ref_file <- file.path(
  data_root,
  "master-datasets",
  "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv"
)

source(file.path("tools", "name_keys.R"))

read_site_reference_coords <- function() {
  read_csv(site_ref_file, show_col_types = FALSE) %>%
    mutate(
      .site_key = site_key_from_parts(LTER, Stream_Name),
      latitude = suppressWarnings(as.numeric(Latitude)),
      longitude = suppressWarnings(as.numeric(Longitude))
    ) %>%
    group_by(.site_key) %>%
    summarise(
      latitude = first(latitude[!is.na(latitude)]),
      longitude = first(longitude[!is.na(longitude)]),
      LTER = first(LTER),
      Stream_Name = first(Stream_Name),
      .groups = "drop"
    )
}

site_extent <- function(x) {
  x %>%
    summarise(
      first_year = min(Year, na.rm = TRUE),
      last_year = max(Year, na.rm = TRUE),
      years = n_distinct(Year),
      max_snow_cover = suppressWarnings(max(snow_cover, na.rm = TRUE)),
      max_snow_num_days = suppressWarnings(max(snow_num_days, na.rm = TRUE)),
      max_permafrost = suppressWarnings(max(permafrost, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      max_snow_cover = ifelse(is.infinite(max_snow_cover), NA_real_, max_snow_cover),
      max_snow_num_days = ifelse(is.infinite(max_snow_num_days), NA_real_, max_snow_num_days),
      max_permafrost = ifelse(is.infinite(max_permafrost), NA_real_, max_permafrost)
    )
}

audit_one <- function(dataset, path, coords) {
  read_csv(path, show_col_types = FALSE) %>%
    mutate(.site_key = site_key_from_stream_id(Stream_ID)) %>%
    group_by(Stream_ID, .site_key) %>%
    site_extent() %>%
    left_join(coords, by = ".site_key") %>%
    mutate(
      dataset = dataset,
      abs_latitude = abs(latitude),
      snow_low_latitude_review = !is.na(abs_latitude) &
        abs_latitude < 32 &
        (
          (!is.na(max_snow_cover) & max_snow_cover > 0.005) |
            (!is.na(max_snow_num_days) & max_snow_num_days > 0)
        ),
      permafrost_low_latitude_review = !is.na(abs_latitude) &
        abs_latitude < 45 &
        !is.na(max_permafrost) &
        max_permafrost > 0,
      missing_latitude = is.na(latitude)
    ) %>%
    select(
      dataset,
      Stream_ID,
      LTER,
      Stream_Name,
      latitude,
      longitude,
      first_year,
      last_year,
      years,
      max_snow_cover,
      max_snow_num_days,
      max_permafrost,
      snow_low_latitude_review,
      permafrost_low_latitude_review,
      missing_latitude
    ) %>%
    arrange(desc(snow_low_latitude_review), desc(permafrost_low_latitude_review), Stream_ID)
}

coords <- read_site_reference_coords()

audits <- bind_rows(
  audit_one("full", full_spatial_file, coords)
)

flagged <- audits %>%
  filter(snow_low_latitude_review | permafrost_low_latitude_review | missing_latitude)

summary <- audits %>%
  group_by(dataset) %>%
  summarise(
    sites = n(),
    missing_latitude_sites = sum(missing_latitude),
    snow_low_latitude_review_sites = sum(snow_low_latitude_review, na.rm = TRUE),
    permafrost_low_latitude_review_sites = sum(permafrost_low_latitude_review, na.rm = TRUE),
    .groups = "drop"
  )

all_out <- file.path(audit_dir, paste0("snow_permafrost_location_check_by_site_", date_tag, ".csv"))
flagged_out <- file.path(audit_dir, paste0("snow_permafrost_location_check_flags_", date_tag, ".csv"))
summary_out <- file.path(audit_dir, paste0("snow_permafrost_location_check_summary_", date_tag, ".csv"))

write_csv(audits, all_out)
write_csv(flagged, flagged_out)
write_csv(summary, summary_out)

message("WROTE:", all_out)
message("WROTE:", flagged_out)
message("WROTE:", summary_out)
