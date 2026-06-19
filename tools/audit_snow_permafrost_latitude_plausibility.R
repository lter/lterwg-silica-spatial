librarian::shelf(dplyr, readr)

date_tag <- Sys.getenv("SILICA_FINAL_HARMONIZED_DATE", unset = "20260608")
box_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn"
data_root <- file.path(box_root, "spatial-data-extractions")
final_data_dir <- file.path(data_root, "final-data")
audit_dir <- file.path(final_data_dir, "audit-summaries")

full_spatial_file <- file.path(
  final_data_dir,
  "full-dataset",
  paste0("final_full_spatial_drivers_annual_", date_tag, ".csv")
)
esom_spatial_file <- file.path(
  final_data_dir,
  "esom",
  paste0("ESOM_spatial_drivers_annual_", date_tag, ".csv")
)
site_ref_file <- file.path(
  data_root,
  "master",
  "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv"
)

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
  data.frame(
    LTER = ifelse(has_sep, sub("__.*$", "", stream_id), NA_character_),
    Stream_Name = ifelse(has_sep, sub("^[^_]*__", "", stream_id), stream_id),
    stringsAsFactors = FALSE
  )
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
  audit_one("full", full_spatial_file, coords),
  audit_one("esom", esom_spatial_file, coords)
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

all_out <- file.path(audit_dir, paste0("snow_permafrost_latitude_plausibility_by_site_", date_tag, ".csv"))
flagged_out <- file.path(audit_dir, paste0("snow_permafrost_latitude_plausibility_flags_", date_tag, ".csv"))
summary_out <- file.path(audit_dir, paste0("snow_permafrost_latitude_plausibility_summary_", date_tag, ".csv"))

write_csv(audits, all_out)
write_csv(flagged, flagged_out)
write_csv(summary, summary_out)

message("WROTE:", all_out)
message("WROTE:", flagged_out)
message("WROTE:", summary_out)
