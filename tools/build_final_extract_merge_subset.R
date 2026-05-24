#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

date_tag <- Sys.getenv("SILICA_AUDIT_DATE", unset = format(Sys.Date(), "%Y%m%d"))
audit_label <- Sys.getenv("SILICA_AUDIT_LABEL", unset = "allreruns-esom-final-spatial-data-extractions")
gap_file <- Sys.getenv(
  "SILICA_GAP_ACTIONS_FILE",
  unset = file.path(
    getwd(),
    "generated_outputs",
    "review",
    "harmonization",
    paste0(audit_label, "_final_run_gap_actions_", date_tag, ".csv")
  )
)
out_dir <- Sys.getenv(
  "SILICA_RERUN_OUT_DIR",
  unset = file.path(getwd(), "generated_outputs", "rerun")
)
out_file <- file.path(out_dir, paste0("final_extract_merge_subset_", date_tag, ".csv"))

if (!file.exists(gap_file)) {
  stop("Missing gap actions file: ", gap_file, call. = FALSE)
}

clean_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "nan")] <- NA_character_
  x
}

driver_order <- c("evapo", "greenup", "npp", "snow")

gap_actions <- read.csv(gap_file, stringsAsFactors = FALSE, check.names = FALSE) %>%
  mutate(
    blocker_class = clean_chr(blocker_class),
    matched_LTER = clean_chr(matched_LTER),
    matched_Stream_Name = clean_chr(matched_Stream_Name),
    matched_Discharge_File_Name = clean_chr(matched_Discharge_File_Name),
    matched_Shapefile_Name = clean_chr(matched_Shapefile_Name),
    dynamic_region = clean_chr(dynamic_region),
    driver = clean_chr(driver),
    year = suppressWarnings(as.integer(year))
  )

subset_rows <- gap_actions %>%
  filter(
    blocker_class == "extract_or_merge_missing",
    !is.na(matched_LTER),
    !is.na(matched_Stream_Name),
    !is.na(matched_Shapefile_Name),
    !is.na(dynamic_region)
  ) %>%
  group_by(
    LTER = matched_LTER,
    Stream_Name = matched_Stream_Name,
    Discharge_File_Name = matched_Discharge_File_Name,
    Shapefile_Name = matched_Shapefile_Name,
    Region = dynamic_region
  ) %>%
  summarise(
    Rerun_Drivers = paste(intersect(driver_order, unique(driver)), collapse = ","),
    Rerun_Years = paste(sort(unique(year[!is.na(year)])), collapse = ","),
    Rerun_Reason = "extract_or_merge_missing",
    .groups = "drop"
  ) %>%
  mutate(
    Watershed_Source = ifelse(grepl("_hydrosheds_", Shapefile_Name, ignore.case = TRUE), "hydrosheds", "existing"),
    Force_HydroSHEDS = ifelse(Watershed_Source == "hydrosheds", "TRUE", "FALSE"),
    Region = normalize_region_key(Region)
  ) %>%
  distinct(LTER, Stream_Name, Shapefile_Name, .keep_all = TRUE) %>%
  arrange(Region, LTER, Stream_Name, Shapefile_Name)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(subset_rows, out_file, row.names = FALSE, na = "")

cat("WROTE:", out_file, "\n", sep = "")
cat("rows=", nrow(subset_rows), "\n", sep = "")
cat("regions=", paste(sort(unique(subset_rows$Region)), collapse = ","), "\n", sep = "")
print(
  subset_rows %>%
    count(Region, name = "n_sites") %>%
    arrange(Region),
  row.names = FALSE
)
