#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(stringr)
  library(sf)
})

keys <- c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")

norm_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

norm_lter <- function(x) {
  x <- norm_chr(x)
  dplyr::recode(
    x,
    "Swedish Goverment" = "Sweden",
    "Swedish Government" = "Sweden",
    "Cameroon" = "Congo Basin",
    "Cameroon Site" = "Congo Basin",
    "Cameroon Sites" = "Congo Basin",
    .default = x
  )
}

first_existing <- function(paths) {
  paths <- paths[nzchar(paths)]
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) return("")
  hit[[1]]
}

detect_data_root <- function() {
  candidates <- c(
    file.path(getwd(), "si-watershed-extract"),
    file.path(dirname(getwd()), "si-watershed-extract"),
    "/home/shares/lter-si/si-watershed-extract",
    "/Users/sidneybush/si-watershed-extract",
    "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/si-watershed-extract"
  )
  hit <- candidates[dir.exists(candidates)]
  if (length(hit) == 0) return("")
  normalizePath(hit[[1]], mustWork = FALSE)
}

max_year <- function(nms, rx) {
  y <- as.integer(stringr::str_match(nms, rx)[, 2])
  y <- y[!is.na(y)]
  if (length(y) == 0) return(NA_integer_)
  max(y)
}

data_root <- Sys.getenv("SILICA_DATA_ROOT", "")
site_coord_dir <- Sys.getenv("SILICA_SITE_COORD_DIR", "")
extracted_dir <- Sys.getenv("SILICA_EXTRACTED_DIR", "")

if (!nzchar(data_root) && (!nzchar(site_coord_dir) || !nzchar(extracted_dir))) {
  data_root <- detect_data_root()
}

if (nzchar(data_root) && dir.exists(data_root)) {
  data_root <- normalizePath(data_root, mustWork = TRUE)
} else if (nzchar(data_root) && !dir.exists(data_root)) {
  stop("SILICA_DATA_ROOT does not exist: ", data_root, call. = FALSE)
}

if (!nzchar(site_coord_dir)) {
  if (!nzchar(data_root)) {
    stop(
      "Set SILICA_SITE_COORD_DIR (or SILICA_DATA_ROOT).",
      call. = FALSE
    )
  }
  site_coord_dir <- file.path(data_root, "site-coordinates")
}
if (!nzchar(extracted_dir)) {
  if (!nzchar(data_root)) {
    stop(
      "Set SILICA_EXTRACTED_DIR (or SILICA_DATA_ROOT).",
      call. = FALSE
    )
  }
  extracted_dir <- file.path(data_root, "extracted-data")
}

sc <- normalizePath(site_coord_dir, mustWork = FALSE)
ex <- normalizePath(extracted_dir, mustWork = FALSE)

if (!dir.exists(sc)) {
  stop("Missing site-coordinates directory: ", sc, call. = FALSE)
}
if (!dir.exists(ex)) {
  stop("Missing extracted-data directory: ", ex, call. = FALSE)
}

site_file <- file.path(sc, "silica-coords_RAW.xlsx")
if (!file.exists(site_file)) {
  stop("Missing site table: ", site_file, call. = FALSE)
}

default_drivers <- c(
  "si-extract_evapo_2-v061.csv",
  "si-extract_greenup_2_v061.csv",
  "si-extract_precip_2.csv",
  "si-extract_air-temp_2.csv",
  "si-extract_snow_2_v061.csv",
  "si-extract_npp_2_v061.csv",
  "si-extract_elevation_2.csv",
  "si-extract_lithology_2.csv",
  "si-extract_permafrost_2.csv",
  "si-extract_soil_2.csv"
)

drivers_env <- Sys.getenv("SILICA_DRIVER_FILES", "")
driver_files <- if (nzchar(drivers_env)) {
  trimws(unlist(strsplit(drivers_env, ",", fixed = TRUE)))
} else {
  default_drivers
}
driver_files <- driver_files[nzchar(driver_files)]
canonicalize_obidos <- tolower(Sys.getenv("SILICA_CANONICALIZE_OBIDOS", "true")) == "true"

is_noncanonical_obidos <- function(df) {
  lter <- norm_lter(df$LTER)
  stream <- norm_chr(df$Stream_Name)
  discharge <- norm_chr(df$Discharge_File_Name)
  shp <- norm_chr(df$Shapefile_Name)

  (lter == "Amazon" & shp == "Amazon_Obidos" & discharge == "Obidos_Q") |
    (lter == "HYBAM" & stream == "Obidos")
}

ref_raw <- readxl::read_excel(site_file)
base <- ref_raw %>%
  transmute(
    LTER = norm_lter(LTER),
    Stream_Name = norm_chr(Stream_Name),
    Discharge_File_Name = norm_chr(Discharge_File_Name),
    Shapefile_Name = norm_chr(Shapefile_Name)
  ) %>%
  distinct()

dropped_obidos <- base[0, , drop = FALSE]
if (canonicalize_obidos) {
  obidos_drop_idx <- is_noncanonical_obidos(base)
  dropped_obidos <- base[obidos_drop_idx, , drop = FALSE]
  base <- base[!obidos_drop_idx, , drop = FALSE]
}

if (nzchar(data_root)) cat("data_root=", data_root, "\n", sep = "")
cat("site_coord_dir=", sc, "\n", sep = "")
cat("extracted_dir=", ex, "\n", sep = "")
cat("ref_raw_rows=", nrow(ref_raw), "\n", sep = "")
cat("ref_distinct_key_rows=", nrow(base), "\n", sep = "")

out <- base
driver_summary <- list()
missing_driver_files <- character(0)

for (f in driver_files) {
  p <- file.path(ex, f)
  if (!file.exists(p)) {
    missing_driver_files <- c(missing_driver_files, f)
    message("Skipping missing: ", f)
    next
  }

  d <- read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
  for (k in keys) if (!k %in% names(d)) d[[k]] <- NA_character_

  d <- d %>%
    mutate(
      LTER = norm_lter(LTER),
      Stream_Name = norm_chr(Stream_Name),
      Discharge_File_Name = norm_chr(Discharge_File_Name),
      Shapefile_Name = norm_chr(Shapefile_Name)
    )

  dup_n <- d %>%
    count(across(all_of(keys)), name = "n") %>%
    filter(n > 1)
  if (nrow(dup_n) > 0) {
    warning("Driver has duplicate key rows; keeping first row per key: ", f, call. = FALSE)
  }
  d <- d %>%
    distinct(across(all_of(keys)), .keep_all = TRUE)

  keep <- setdiff(names(d), keys)
  out <- out %>%
    left_join(d %>% select(all_of(c(keys, keep))), by = keys)

  ref_keys <- base %>% distinct(LTER, Shapefile_Name)
  drv_keys <- d %>% distinct(LTER, Shapefile_Name)
  driver_summary[[f]] <- data.frame(
    driver = f,
    ref_keys = nrow(ref_keys),
    driver_keys = nrow(drv_keys),
    matched = nrow(inner_join(ref_keys, drv_keys, by = c("LTER", "Shapefile_Name"))),
    unmatched = nrow(anti_join(ref_keys, drv_keys, by = c("LTER", "Shapefile_Name"))),
    stringsAsFactors = FALSE
  )

  message("Adding ", f)
}

if (length(missing_driver_files) > 0) {
  message("Missing driver files: ", paste(missing_driver_files, collapse = ", "))
}

out_file <- Sys.getenv(
  "SILICA_OUTPUT_FILE",
  file.path(ex, paste0("all-data_si-extract_3_", format(Sys.Date(), "%Y%m%d"), "_fromSiteRef_full_canonical.csv"))
)
write.csv(out, out_file, row.names = FALSE, na = "")
cat("WROTE:", out_file, "\n", sep = "")
cat("out_rows=", nrow(out), "\n", sep = "")

n <- names(out)
cat("evapo=", max_year(n, "^evapotrans_([0-9]{4})_kg_m2$"), "\n", sep = "")
cat("greenup=", max_year(n, "^greenup_cycle[01]_([0-9]{4})MMDD$"), "\n", sep = "")
cat("precip=", max_year(n, "^precip_([0-9]{4})_mm_per_day$"), "\n", sep = "")
cat("airtemp=", max_year(n, "^temp_([0-9]{4})_degC$"), "\n", sep = "")
cat("snow=", max_year(n, "^snow_([0-9]{4})_num_days$"), "\n", sep = "")
cat("npp=", max_year(n, "^npp_([0-9]{4})_kgC_m2_year$"), "\n", sep = "")

qa_dir <- Sys.getenv("SILICA_QA_DIR", file.path(getwd(), "qa"))
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

if (canonicalize_obidos) {
  kept_obidos <- out %>%
    filter(LTER == "GRO", Discharge_File_Name == "GRO_Obidos_Q") %>%
    mutate(harmonization_role = "kept_canonical")

  obidos_audit <- bind_rows(
    dropped_obidos %>% mutate(harmonization_role = "dropped_noncanonical"),
    kept_obidos
  )

  obidos_file <- file.path(
    qa_dir,
    paste0("obidos_canonicalization_", format(Sys.Date(), "%Y%m%d"), ".csv")
  )
  write.csv(obidos_audit, obidos_file, row.names = FALSE)
  cat("WROTE:", obidos_file, "\n", sep = "")
}

if (length(driver_summary) > 0) {
  sumtab <- bind_rows(driver_summary)
  sumtab_file <- file.path(qa_dir, paste0("key_match_summary_local_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  write.csv(sumtab, sumtab_file, row.names = FALSE)
  cat("WROTE:", sumtab_file, "\n", sep = "")
}

watershed_file <- file.path(sc, "silica-watersheds.shp")
if (file.exists(watershed_file)) {
  sheds <- sf::st_read(watershed_file, quiet = TRUE)
  if ("shp_nm" %in% names(sheds)) names(sheds)[names(sheds) == "shp_nm"] <- "Shapefile_Name"
  if (!("LTER" %in% names(sheds))) sheds$LTER <- NA_character_

  shed_keys <- sheds %>%
    sf::st_drop_geometry() %>%
    transmute(
      LTER = norm_lter(LTER),
      Shapefile_Name = norm_chr(Shapefile_Name)
    ) %>%
    distinct()

  named_ref <- base %>%
    filter(!is.na(Shapefile_Name), !Shapefile_Name %in% c("?", "MISSING")) %>%
    distinct(LTER, Shapefile_Name)

  miss_poly <- anti_join(named_ref, shed_keys, by = c("LTER", "Shapefile_Name"))
  miss_poly_file <- file.path(qa_dir, paste0("sites_true_missing_polygon_keys_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  write.csv(miss_poly, miss_poly_file, row.names = FALSE)
  cat("true_missing_polygon_keys=", nrow(miss_poly), "\n", sep = "")
  cat("WROTE:", miss_poly_file, "\n", sep = "")
}

old_file <- first_existing(c(
  Sys.getenv("SILICA_OLD_COMBINED", ""),
  file.path(ex, "all-data_si-extract_2_20250325.csv"),
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/CQ_Site_Map/all-data_si-extract_2_20250325.csv"
))

if (nzchar(old_file) && file.exists(old_file)) {
  old <- read.csv(old_file, stringsAsFactors = FALSE, check.names = FALSE)

  key_fn <- function(d) {
    paste(norm_lter(d$LTER), norm_chr(d$Shapefile_Name), sep = "||")
  }

  oldk <- unique(key_fn(old))
  newk <- unique(key_fn(out))
  oldk <- oldk[!is.na(oldk)]
  newk <- newk[!is.na(newk)]

  comp <- data.frame(
    old_rows = nrow(old),
    new_rows = nrow(out),
    old_unique_site_keys = length(oldk),
    new_unique_site_keys = length(newk),
    dropped_from_old = length(setdiff(oldk, newk)),
    added_vs_old = length(setdiff(newk, oldk)),
    stringsAsFactors = FALSE
  )
  comp_file <- file.path(qa_dir, paste0("old_vs_new_site_counts_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  write.csv(comp, comp_file, row.names = FALSE)
  cat("old_unique_site_keys=", comp$old_unique_site_keys, "\n", sep = "")
  cat("new_unique_site_keys=", comp$new_unique_site_keys, "\n", sep = "")
  cat("dropped_from_old=", comp$dropped_from_old, "\n", sep = "")
  cat("added_vs_old=", comp$added_vs_old, "\n", sep = "")
  cat("WROTE:", comp_file, "\n", sep = "")
}
