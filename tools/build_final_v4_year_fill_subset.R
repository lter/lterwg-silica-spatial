#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
})

source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

sf::sf_use_s2(FALSE)

env_or_default <- function(env_name, default_value) {
  value <- trimws(Sys.getenv(env_name, unset = ""))
  if (nzchar(value)) value else default_value
}

read_loose_csv <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- make.unique(ifelse(is.na(names(x)) | trimws(names(x)) == "", "blank_col", names(x)))
  x
}

norm_blank <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN")] <- NA_character_
  x
}

has_missing_required <- function(df, cols) {
  cols <- intersect(cols, names(df))
  if (!length(cols)) {
    return(rep(TRUE, nrow(df)))
  }
  rowSums(is.na(df[, cols, drop = FALSE]) | df[, cols, drop = FALSE] == "") > 0
}

site_key <- function(lter, shp) {
  paste(normalize_lter_key(lter), normalize_site_key(shp), sep = "||")
}

date_tag <- env_or_default("SILICA_YEAR_FILL_DATE", format(Sys.Date(), "%Y%m%d"))
combined_file <- env_or_default(
  "SILICA_YEAR_FILL_COMBINED_FILE",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/si-extracted-data/all_data_extractions/all-data_si-extract_4_20260523_final-extract-merge-v4-airtemp2025-domain-spatial-data-extractions.csv"
)
watershed_file <- env_or_default(
  "SILICA_YEAR_FILL_WATERSHED_FILE",
  "/private/tmp/final_v4_followup_extract_20260522/site-coordinates/silica-watersheds_20260522_final-v4-followup-extract-20260522.shp"
)
out_dir <- env_or_default(
  "SILICA_YEAR_FILL_OUT_DIR",
  file.path(getwd(), "generated_outputs", "rerun")
)

if (!file.exists(combined_file)) stop("Missing combined file: ", combined_file, call. = FALSE)
if (!file.exists(watershed_file)) stop("Missing watershed file: ", watershed_file, call. = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

x <- read_loose_csv(combined_file)
for (col in c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) {
  if (!col %in% names(x)) x[[col]] <- NA_character_
  x[[col]] <- norm_blank(x[[col]])
}

watersheds <- st_read(watershed_file, quiet = TRUE)
watershed_keys <- unique(site_key(watersheds$LTER, watersheds$shp_nm))

required_cols <- list(
  airtemp = paste0("temp_", 2002:2025, "_degC"),
  evapo = paste0("evapotrans_", 2002:2025, "_kg_m2"),
  greenup = c(paste0("greenup_cycle0_", 2002:2024, "MMDD"), paste0("greenup_cycle1_", 2002:2024, "MMDD")),
  npp = paste0("npp_", 2002:2025, "_kgC_m2_year"),
  precip = paste0("precip_", 2002:2025, "_mm_per_day"),
  snow = paste0("snow_", 2002:2025, "_num_days")
)

missing_by_family <- lapply(required_cols, has_missing_required, df = x)
names(missing_by_family) <- names(required_cols)

missing_family_text <- vapply(seq_len(nrow(x)), function(i) {
  fam <- names(missing_by_family)[vapply(missing_by_family, function(z) z[[i]], logical(1))]
  paste(fam, collapse = ";")
}, character(1))

x$.site_key <- site_key(x$LTER, x$Shapefile_Name)
x$.has_active_watershed <- x$.site_key %in% watershed_keys
x$.missing_required_families <- missing_family_text
x$.missing_any_required <- nzchar(x$.missing_required_families)

targets <- x %>%
  filter(
    .has_active_watershed,
    .missing_any_required,
    normalize_lter_key(LTER) != "mcm"
  ) %>%
  distinct(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, .missing_required_families, .keep_all = FALSE) %>%
  transmute(
    LTER,
    Stream_Name,
    Discharge_File_Name,
    Shapefile_Name,
    Watershed_Source = "existing_final_inventory",
    Force_HydroSHEDS = "FALSE",
    Region = "",
    Rerun_Reason = paste0("year_fill_missing_", gsub(";", "_", .missing_required_families))
  ) %>%
  arrange(LTER, Stream_Name, Shapefile_Name)

summary_by_lter <- targets %>%
  count(LTER, sort = TRUE, name = "n_sites")

summary_by_reason <- targets %>%
  count(Rerun_Reason, sort = TRUE, name = "n_sites")

subset_file <- file.path(out_dir, paste0("final_v4_year_fill_subset_", date_tag, ".csv"))
lter_summary_file <- file.path(out_dir, paste0("final_v4_year_fill_subset_by_lter_", date_tag, ".csv"))
reason_summary_file <- file.path(out_dir, paste0("final_v4_year_fill_subset_by_reason_", date_tag, ".csv"))

write.csv(targets, subset_file, row.names = FALSE, na = "")
write.csv(summary_by_lter, lter_summary_file, row.names = FALSE, na = "")
write.csv(summary_by_reason, reason_summary_file, row.names = FALSE, na = "")

cat("WROTE:", subset_file, "\n", sep = "")
cat("WROTE:", lter_summary_file, "\n", sep = "")
cat("WROTE:", reason_summary_file, "\n", sep = "")
cat("target_sites=", nrow(targets), "\n", sep = "")
cat("active_watershed_rows=", sum(x$.has_active_watershed), "\n", sep = "")
print(as.data.frame(summary_by_reason))
