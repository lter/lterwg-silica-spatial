librarian::shelf(dplyr)

source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

env_or_default <- function(env_name, default_value) {
  value <- trimws(Sys.getenv(env_name, unset = ""))
  if (nzchar(value)) value else default_value
}

norm_blank <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "nan")] <- NA_character_
  x
}

read_loose_csv <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- make.unique(ifelse(is.na(names(x)) | trimws(names(x)) == "", "blank_col", names(x)))
  x
}

date_tag <- env_or_default("SILICA_FINAL_REMAINING_DATE", "20260526")
gap_file <- env_or_default(
  "SILICA_FINAL_REMAINING_GAP_ACTION_FILE",
  file.path(
    "/private/tmp/final_20260526_final-extract-merge-v4-year-fill-repatched-domain-copy_readiness",
    "final-extract-merge-v4-year-fill-repatched-domain-copy_final_run_gap_actions_20260526.csv"
  )
)
out_dir <- env_or_default(
  "SILICA_FINAL_REMAINING_OUT_DIR",
  file.path(getwd(), "generated_outputs", "rerun", "active-final-run")
)

if (!file.exists(gap_file)) {
  stop("Missing gap action file: ", gap_file, call. = FALSE)
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

gap_actions <- read_loose_csv(gap_file)
required_cols <- c(
  "blocker_class", "dynamic_region", "driver", "year",
  "matched_LTER", "matched_Stream_Name", "matched_Discharge_File_Name",
  "matched_Shapefile_Name"
)
missing_cols <- setdiff(required_cols, names(gap_actions))
if (length(missing_cols)) {
  stop("Gap action file is missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

active_shapefile_aliases <- tibble::tribble(
  ~LTER, ~Shapefile_Name, ~Active_Shapefile_Name,
  "LMP", "LMP_hydrosheds_5", "lmp_hydrosheds_nor27"
)

targets <- gap_actions %>%
  filter(blocker_class == "extract_or_merge_missing") %>%
  filter(driver %in% c("evapo", "greenup", "npp", "snow")) %>%
  transmute(
    LTER = norm_blank(matched_LTER),
    Stream_Name = norm_blank(matched_Stream_Name),
    Discharge_File_Name = norm_blank(matched_Discharge_File_Name),
    Shapefile_Name = norm_blank(matched_Shapefile_Name),
    Region = norm_blank(dynamic_region),
    driver = norm_blank(driver),
    year = suppressWarnings(as.integer(year))
  ) %>%
  filter(
    !is.na(LTER),
    !is.na(Shapefile_Name),
    !is.na(Region),
    !is.na(driver),
    !is.na(year)
  ) %>%
  left_join(active_shapefile_aliases, by = c("LTER", "Shapefile_Name")) %>%
  mutate(
    Active_Shapefile_Name = coalesce(Active_Shapefile_Name, Shapefile_Name),
    Watershed_Source = "existing_final_inventory",
    Force_HydroSHEDS = "FALSE"
  )

if (!nrow(targets)) {
  stop("No remaining dynamic extraction targets found in: ", gap_file, call. = FALSE)
}

write_driver_files <- function(driver_name) {
  driver_targets <- targets %>% filter(driver == driver_name)
  if (!nrow(driver_targets)) {
    return(invisible(NULL))
  }

  subset_out <- driver_targets %>%
    distinct(
      LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
      Active_Shapefile_Name, Watershed_Source, Force_HydroSHEDS, Region
    ) %>%
    mutate(Rerun_Reason = paste0("final_remaining_missing_", driver_name)) %>%
    select(
      LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
      Active_Shapefile_Name, Watershed_Source, Force_HydroSHEDS, Region,
      Rerun_Reason
    ) %>%
    arrange(Region, LTER, Stream_Name, Shapefile_Name)

  region_year_out <- driver_targets %>%
    distinct(driver, region = Region, year) %>%
    arrange(driver, region, year)

  subset_path <- file.path(
    out_dir,
    paste0("final_remaining_dynamic_", driver_name, "_subset_", date_tag, ".csv")
  )
  region_year_path <- file.path(
    out_dir,
    paste0("final_remaining_dynamic_", driver_name, "_region_years_", date_tag, ".csv")
  )

  write.csv(subset_out, subset_path, row.names = FALSE, na = "")
  write.csv(region_year_out, region_year_path, row.names = FALSE, na = "")

  cat("WROTE:", subset_path, "\n", sep = "")
  cat("WROTE:", region_year_path, "\n", sep = "")
  cat(driver_name, "_sites=", nrow(subset_out), "\n", sep = "")
  cat(driver_name, "_region_years=", nrow(region_year_out), "\n", sep = "")
}

for (driver_name in c("evapo", "greenup", "npp", "snow")) {
  write_driver_files(driver_name)
}
