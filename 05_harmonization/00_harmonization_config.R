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
  extracted_dirs <- c(
    file.path(data_root, "extracted-data"),
    file.path(data_root, "extracted-data", "all_data_extractions"),
    file.path(data_root, "si-extracted-data", "all_data_extractions")
  )
  extracted_dirs <- extracted_dirs[dir.exists(extracted_dirs)]
  extracted_candidates <- unlist(lapply(extracted_dirs, function(dir) {
    list.files(
      dir,
      pattern = "^all-data_si-extract_3_.*\\.csv$",
      full.names = TRUE
    )
  }))

  review_combined_candidates <- list.files(
    file.path(data_root, "review", "harmonization"),
    pattern = "^combined-spatial-dataset_.*\\.csv$",
    full.names = TRUE
  )
  review_combined_candidates <- review_combined_candidates[
    !grepl("_summary\\.csv$", basename(review_combined_candidates))
  ]

  candidates <- c(extracted_candidates, review_combined_candidates)
  candidates <- candidates[file.exists(candidates)]

  if (!length(candidates)) {
    stop(
      "No combined spatial data files found under extracted-data or review/harmonization.",
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

# Extra lookup tables and harmonization inputs
master_dir <- file.path(data_root, "master-datasets")
gee_glc_dir <- file.path(
  data_root,
  "spatial-data-files",
  "appeears-nasa",
  "glc-lulc-from-gee",
  "merged-master-checkpoints"
)

# Q and discharge inputs
daily_discharge_file <- first_existing_path(
  c(
    file.path(master_dir, "20260106_masterdata_discharge.csv")
  ),
  "daily discharge file"
)
wrtds_annual_file <- first_existing_path(
  c(
    file.path(master_dir, "Full_Results_WRTDS_kalman_annual.csv")
  ),
  "annual WRTDS file"
)

# Lookup and fill inputs
kg_file <- first_existing_path(
  c(
    file.path(master_dir, "Koeppen_Geiger_2.csv")
  ),
  "Koeppen-Geiger file"
)
krycklan_slope_file <- first_existing_path(
  c(
    file.path(master_dir, "Krycklan_basin_slopes.csv")
  ),
  "Krycklan basin slope file"
)
us_slope_file <- first_existing_path(
  c(
    file.path(master_dir, "DSi_Basin_Slope_missing_sites.csv")
  ),
  "missing basin slope fill file"
)
stream_id_key_file <- first_existing_path(
  c(
    file.path(master_dir, "basin_stream_id_keys.csv")
  ),
  "stream ID key file"
)
wrtds_reference_file <- first_existing_path(
  c(
    file.path(master_dir, "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv"),
    file.path(master_dir, "Site_Reference_Table - WRTDS_Reference_Table_LTER_V2.csv")
  ),
  "WRTDS reference file"
)

# GEE/GLC land-cover source used for the June 2026 final rebuild
lulc_file <- first_existing_path(
  c(
    file.path(gee_glc_dir, "DSi_LULC_filled_interpolated_Simple_20260524_nor27.csv")
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
add_gee_glc_land_cover_columns <- TRUE

# Required base file check
if (!file.exists(combined_file)) {
  stop("Missing combined_file: ", combined_file, call. = FALSE)
}

# Output folder
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
