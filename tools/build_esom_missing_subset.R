#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
})

source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)

default_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial_data_extractions"
data_root <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else default_root
date_tag <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else format(Sys.Date(), "%Y%m%d")
hydro_threshold_km2 <- if (length(args) >= 3 && nzchar(args[[3]])) suppressWarnings(as.numeric(args[[3]])) else 100

miss_file <- file.path(data_root, "data_checking", "ESOM_MissingSpatialData.csv")
final_file <- file.path(
  data_root,
  "si-extracted-data",
  "all_data_extractions",
  "all-data_si-extract_4_20260518_aurora-hydrosheds-patched.csv"
)
ref_file <- file.path(data_root, "master", "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv")
out_dir <- file.path(data_root, "review", "rerun")
out_file <- file.path(out_dir, paste0("esom_missing_subset_", date_tag, ".csv"))
blocked_file <- file.path(out_dir, paste0("esom_missing_subset_blocked_under100km2_", date_tag, ".csv"))

if (!file.exists(miss_file)) {
  stop("Missing ESOM missing-spatial file: ", miss_file, call. = FALSE)
}

if (!file.exists(final_file)) {
  stop("Missing final combined file: ", final_file, call. = FALSE)
}

if (!file.exists(ref_file)) {
  stop("Missing reference table: ", ref_file, call. = FALSE)
}

if (is.na(hydro_threshold_km2) || hydro_threshold_km2 < 0) {
  stop("HydroSHEDS threshold must be a non-negative number.", call. = FALSE)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

norm_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA")] <- NA_character_
  x
}

build_site_key <- function(lter, stream) {
  paste(
    normalize_lter_key(lter),
    normalize_stream_key(stream),
    sep = "||"
  )
}

read_loose_csv <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- make.unique(ifelse(is.na(names(x)) | trimws(names(x)) == "", "blank_col", names(x)))
  x
}

miss <- read_loose_csv(miss_file) %>%
  clean_lter_column() %>%
  transmute(
    LTER = norm_chr(clean_lter_label(LTER)),
    Stream_Name = norm_chr(Stream_Name),
    key = build_site_key(LTER, Stream_Name)
  ) %>%
  distinct(key, .keep_all = TRUE)

fin <- read_loose_csv(final_file) %>%
  clean_lter_column() %>%
  transmute(
    LTER_final = norm_chr(clean_lter_label(LTER)),
    Stream_Name_final = norm_chr(Stream_Name),
    Discharge_File_Name = norm_chr(Discharge_File_Name),
    Shapefile_Name = norm_chr(Shapefile_Name),
    key = build_site_key(LTER, Stream_Name)
  ) %>%
  arrange(key, desc(!is.na(Shapefile_Name)), desc(!is.na(Discharge_File_Name))) %>%
  distinct(key, .keep_all = TRUE)

ref <- read_loose_csv(ref_file) %>%
  clean_lter_column() %>%
  transmute(
    LTER_ref = norm_chr(clean_lter_label(LTER)),
    Stream_Name_ref = norm_chr(Stream_Name),
    Discharge_File_Name_ref = norm_chr(Discharge_File_Name),
    Shapefile_Name_ref = norm_chr(Shapefile_Name),
    drainSqKm = suppressWarnings(as.numeric(drainSqKm)),
    key = build_site_key(LTER, Stream_Name)
  ) %>%
  arrange(key, desc(!is.na(Shapefile_Name_ref)), desc(!is.na(Discharge_File_Name_ref))) %>%
  distinct(key, .keep_all = TRUE)

full_tbl <- miss %>%
  left_join(ref, by = "key") %>%
  left_join(fin, by = "key") %>%
  mutate(
    LTER = coalesce(LTER_ref, LTER_final, LTER),
    Stream_Name = coalesce(Stream_Name_ref, Stream_Name_final, Stream_Name),
    Discharge_File_Name = coalesce(Discharge_File_Name_ref, Discharge_File_Name),
    Shapefile_Name = coalesce(Shapefile_Name_ref, Shapefile_Name),
    eligible_for_hydrosheds = is.na(Shapefile_Name) & !is.na(drainSqKm) & drainSqKm > hydro_threshold_km2,
    blocked_under_threshold = is.na(Shapefile_Name) & !is.na(drainSqKm) & drainSqKm <= hydro_threshold_km2,
    blocked_missing_area = is.na(Shapefile_Name) & is.na(drainSqKm),
    Watershed_Source = ifelse(is.na(Shapefile_Name), "hydrosheds", "artisanal"),
    Force_HydroSHEDS = ifelse(is.na(Shapefile_Name), "TRUE", "FALSE")
  ) %>%
  distinct()

subset_tbl <- full_tbl %>%
  filter(!blocked_under_threshold, !blocked_missing_area) %>%
  transmute(
    LTER,
    Stream_Name,
    Discharge_File_Name,
    Shapefile_Name,
    Watershed_Source,
    Force_HydroSHEDS
  ) %>%
  distinct()

blocked_tbl <- full_tbl %>%
  filter(blocked_under_threshold | blocked_missing_area) %>%
  transmute(
    LTER,
    Stream_Name,
    Discharge_File_Name,
    Shapefile_Name,
    drainSqKm,
    blocked_reason = ifelse(blocked_under_threshold, paste0("drainSqKm <= ", hydro_threshold_km2), "missing drainSqKm")
  ) %>%
  distinct()

write.csv(subset_tbl, out_file, row.names = FALSE, na = "")
write.csv(blocked_tbl, blocked_file, row.names = FALSE, na = "")

cat(out_file, "\n")
