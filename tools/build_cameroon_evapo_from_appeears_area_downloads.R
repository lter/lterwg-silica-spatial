#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(terra)
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(readr)
})

source(file.path(getwd(), "tools", "workflow_paths.R"))

args <- commandArgs(trailingOnly = TRUE)
run_date <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else format(Sys.Date(), "%Y%m%d")
run_label <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else paste0("aurora-cameroon-evapo-direct-", run_date)

root_path <- resolve_silica_data_root()
review_dir <- file.path(silica_review_root(root_path), "01_pre_aurora_run", "05_appeears_backfill")
appeears_dir <- file.path(review_dir, paste0(run_date, "_cameroon_evapo_v061"))
if (!dir.exists(appeears_dir)) {
  stop("Missing AppEEARS folder: ", appeears_dir, call. = FALSE)
}

downloads_dirs <- c(
  file.path(appeears_dir, "downloads_2000_2012"),
  file.path(appeears_dir, "downloads_2023"),
  file.path(appeears_dir, "downloads_2023_retry")
)
downloads_dirs <- downloads_dirs[dir.exists(downloads_dirs)]
if (!length(downloads_dirs)) {
  stop("No AppEEARS download directories found under ", appeears_dir, call. = FALSE)
}

site_map <- tibble::tribble(
  ~aid, ~Shapefile_Name, ~Shpfl_N, ~Discharge_File_Name, ~Stream_Name, ~LTER,
  "aid0001", "Awout_Messam", "Awout_Messam", "Messam_Q", "Messam", "Cameroon",
  "aid0002", "Nyong_Mbalmayo", "Nyong_Mbalmayo", "Mbalmayo_Q", "Mbalmayo", "Cameroon",
  "aid0003", "Nyong_Olama", "Nyong_Olama", "Olama_Q", "Olama", "Cameroon",
  "aid0004", "Soo_PontSoo", "Soo_PontSoo", "Pont_So'o_Q", "Pont_So'o", "Cameroon"
)

active_congo_dir <- file.path(silica_raw_driver_data_dir(root_path), "raw-evapo-v061", "congo")
active_files <- list.files(active_congo_dir, pattern = "^MOD16A2GF\\.061_ET_500m_.*\\.tif$", full.names = TRUE)

appeears_files <- unlist(lapply(downloads_dirs, function(d) {
  list.files(d, pattern = "^MOD16A2GF\\.061_ET_500m_.*\\.tif$", recursive = TRUE, full.names = TRUE)
}), use.names = FALSE)

all_files <- unique(c(active_files, appeears_files))
if (!length(all_files)) {
  stop("No MOD16A2GF.061 ET GeoTIFF files found.", call. = FALSE)
}

parse_file_info <- function(path) {
  nm <- basename(path)
  tibble(
    path = path,
    file_name = nm,
    year = str_match(nm, "doy(\\d{4})")[,2],
    doy = str_match(nm, "doy\\d{4}(\\d{3})")[,2],
    aid = str_match(nm, "(aid\\d{4})")[,2]
  )
}

files_tbl <- purrr::map_dfr(all_files, parse_file_info) %>%
  filter(!is.na(year), !is.na(doy), !is.na(aid)) %>%
  left_join(site_map, by = "aid") %>%
  filter(!is.na(Shapefile_Name))

if (!nrow(files_tbl)) {
  stop("No files matched expected Cameroon aid-to-site mapping.", call. = FALSE)
}

summarise_raster_mean <- function(path) {
  tryCatch({
    r <- terra::rast(path)
    vals <- terra::values(r, mat = FALSE)
    vals <- vals[!is.na(vals)]
    vals <- vals[vals <= 3270 & vals >= -3276.7]
    if (!length(vals)) return(NA_real_)
    mean(vals)
  }, error = function(e) {
    message("Skipping unreadable ET raster: ", basename(path), " (", conditionMessage(e), ")")
    NA_real_
  })
}

message("Summarizing ", nrow(files_tbl), " ET rasters.")

daily_tbl <- files_tbl %>%
  mutate(
    year = as.integer(year),
    doy = as.integer(doy),
    value = purrr::map_dbl(path, summarise_raster_mean),
    daily_val = ifelse(doy == 361, value / 5, value / 8)
  ) %>%
  filter(!is.na(daily_val))

if (!nrow(daily_tbl)) {
  stop("No valid ET values produced from raster summaries.", call. = FALSE)
}

expand_daily <- function(df) {
  base <- df %>% select(LTER, Shapefile_Name, Shpfl_N, Discharge_File_Name, Stream_Name, year, doy, daily_val)
  extra <- purrr::map_dfr(0:7, function(i) base %>% mutate(doy = doy + i))
  extra %>%
    filter(doy <= 366)
}

expanded <- expand_daily(daily_tbl)

expanded <- expanded %>%
  group_by(LTER, Shapefile_Name, Shpfl_N, Discharge_File_Name, Stream_Name, year, doy) %>%
  summarize(daily_val = mean(daily_val, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = as.Date(doy - 1, origin = paste0(year, "-01-01")))

annual <- expanded %>%
  group_by(LTER, Shapefile_Name, Shpfl_N, Discharge_File_Name, Stream_Name, year) %>%
  summarize(value = sum(daily_val, na.rm = TRUE), .groups = "drop") %>%
  mutate(name = paste0("evapotrans_", year, "_kg_m2")) %>%
  select(-year) %>%
  pivot_wider(names_from = name, values_from = value)

monthly <- expanded %>%
  mutate(month_label = tolower(format(date, "%b"))) %>%
  group_by(LTER, Shapefile_Name, Shpfl_N, Discharge_File_Name, Stream_Name, month_label) %>%
  summarize(value = mean(daily_val, na.rm = TRUE), .groups = "drop") %>%
  mutate(name = paste0("evapotrans_", month_label, "_kg_m2")) %>%
  select(-month_label) %>%
  pivot_wider(names_from = name, values_from = value)

out <- annual %>%
  full_join(monthly, by = c("LTER", "Shapefile_Name", "Shpfl_N", "Discharge_File_Name", "Stream_Name")) %>%
  arrange(match(Shapefile_Name, site_map$Shapefile_Name))

out_dir <- file.path(root_path, "run-outputs", paste0(run_date, "_", run_label), "extracted-data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_file <- file.path(out_dir, paste0("si-extract_evapo_v061_", run_date, "_", run_label, ".csv"))
readr::write_csv(out, out_file, na = "")

message("Wrote Cameroon evapo direct-output file: ", out_file)
