# Harmonization inputs

env_or_default <- function(env_name, default_value) {
  env_value <- trimws(Sys.getenv(env_name, unset = ""))
  if (nzchar(env_value)) {
    return(env_value)
  }
  default_value
}

first_existing_path <- function(candidates, label) {
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    stop("Missing ", label, ". Checked:\n- ", paste(candidates, collapse = "\n- "), call. = FALSE)
  }
  existing[[1]]
}

latest_combined_file <- function(data_root) {
  candidates <- c(
    list.files(
      file.path(data_root, "final-data", "full-dataset"),
      pattern = "^all-data_si-extract_.*\\.csv$",
      full.names = TRUE
    ),
    list.files(
      file.path(data_root, "review", "harmonization"),
      pattern = "^combined-spatial-dataset_.*\\.csv$",
      full.names = TRUE
    ),
    list.files(
      file.path(data_root, "si-extracted-data", "all_data_extractions"),
      pattern = "^all-data_si-extract_[34]_.*\\.csv$",
      full.names = TRUE
    ),
    list.files(
      file.path(data_root, "si-extracted-data"),
      pattern = "^all-data_si-extract_3_.*\\.csv$",
      full.names = TRUE
    )
  )
  candidates <- candidates[file.exists(candidates)]

  if (!length(candidates)) {
    stop(
      "No combined spatial data files found under final-data/full-dataset, review/harmonization, si-extracted-data/all_data_extractions, or si-extracted-data.",
      call. = FALSE
    )
  }

  candidates[which.max(file.info(candidates)$mtime)]
}

data_root <- env_or_default(
  "SILICA_QAQC_DATA_ROOT",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions"
)

# Latest vetted combined table from 04_combine_qaqc
combined_file <- env_or_default(
  "SILICA_HARMONIZATION_COMBINED_FILE",
  latest_combined_file(data_root)
)

# Harmonization outputs
review_root <- file.path(data_root, "review")
output_dir <- env_or_default(
  "SILICA_HARMONIZATION_OUTPUT_DIR",
  file.path(review_root, "harmonization")
)
date_tag <- env_or_default("SILICA_HARMONIZATION_DATE", format(Sys.Date(), "%Y%m%d"))

# Extra lookup tables and draft harmonization inputs
harmonization_input_dir <- file.path(data_root, "spatial_data_harmonization")
harmonization_master_dir <- file.path(harmonization_input_dir, "master_datasets")
master_dir <- file.path(data_root, "master")

# Q and discharge inputs
daily_discharge_file <- first_existing_path(
  c(
    file.path(master_dir, "20260106_masterdata_discharge.csv")
  ),
  "daily discharge file"
)
wrtds_annual_file <- first_existing_path(
  c(
    file.path(harmonization_master_dir, "Full_Results_WRTDS_kalman_annual.csv"),
    file.path(master_dir, "Full_Results_WRTDS_kalman_annual.csv")
  ),
  "annual WRTDS file"
)

# Lookup and fill inputs
kg_file <- first_existing_path(
  c(
    file.path(harmonization_input_dir, "Koeppen_Geiger_2.csv")
  ),
  "Koeppen-Geiger file"
)
krycklan_slope_file <- first_existing_path(
  c(
    file.path(harmonization_input_dir, "Krycklan_basin_slopes.csv")
  ),
  "Krycklan basin slope file"
)
us_slope_file <- first_existing_path(
  c(
    file.path(harmonization_input_dir, "DSi_Basin_Slope_missing_sites.csv")
  ),
  "missing basin slope fill file"
)
stream_id_key_file <- first_existing_path(
  c(
    file.path(harmonization_input_dir, "basin_stream_id_conversions.csv")
  ),
  "stream ID conversion file"
)
wrtds_reference_file <- first_existing_path(
  c(
    file.path(master_dir, "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv"),
    file.path(master_dir, "Site_Reference_Table - WRTDS_Reference_Table_LTER_V2.csv")
  ),
  "WRTDS reference file"
)

# Review-only lookup used to track which newly added sites still need land-cover
# rows maintained in the external harmonization inputs.
lulc_file <- first_existing_path(
  c(
    file.path(harmonization_master_dir, "DSi_LULC_filled_interpolated_Simple.csv")
  ),
  "LULC harmonization file"
)

# Turn pieces on or off here
add_q_summary <- TRUE
build_rbi <- TRUE
build_recession_slope <- TRUE
add_kg_class <- TRUE
gap_fill_basin_slope <- TRUE
gap_fill_elevation <- TRUE
add_max_daylength <- TRUE

# Required base file check
if (!file.exists(combined_file)) {
  stop("Missing combined_file: ", combined_file, call. = FALSE)
}

# Output folder
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
