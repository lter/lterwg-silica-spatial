# Reset the global workspace when this file is sourced interactively so QAQC
# behaves like a fresh session.
if (identical(environment(), .GlobalEnv)) {
  rm(list = ls(envir = .GlobalEnv, all.names = TRUE), envir = .GlobalEnv)
}

# QAQC paths and run settings
# Workflow:
# 1. Set data_root
# 2. If needed, change old_combined_filename
# 3. Otherwise the newest local combined candidate is auto-detected
# 4. Run Rscript 04_combine_qaqc/01_import-and-qaqc.R
#
# Everything else is derived from data_root. Environment-variable overrides
# still exist for debugging, but they are not the intended entry path.

env_or_default <- function(env_name, default_value) {
  env_value <- trimws(Sys.getenv(env_name, unset = ""))
  if (nzchar(env_value)) {
    return(env_value)
  }
  default_value
}

latest_matching_path <- function(paths) {
  existing <- paths[file.exists(paths)]
  if (!length(existing)) {
    return("")
  }
  info <- file.info(existing)
  normalizePath(existing[which.max(info$mtime)], mustWork = TRUE)
}

# One user-editable root path for the local spatial extraction tree

data_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial_data_extractions"
data_root <- env_or_default("SILICA_QAQC_DATA_ROOT", data_root)

if (!dir.exists(data_root)) {
  stop("Missing data_root: ", data_root, call. = FALSE)
}

# Derived folders used by the QAQC workflow
extracted_dir <- file.path(data_root, "si-extracted-data")
sitecoord_dir <- file.path(data_root, "silica-shapefiles", "site-coordinates")
review_root <- file.path(data_root, "review")
all_extract_dir <- file.path(extracted_dir, "all_data_extractions")

# Optional user-editable filenames inside data_root
old_combined_filename <- "all-data_si-extract_2_20250325.csv"
new_file_default <- latest_matching_path(c(
  Sys.glob(file.path(all_extract_dir, "all-data_si-extract_4_*.csv")),
  Sys.glob(file.path(all_extract_dir, "all-data_si-extract_3_*.csv")),
  Sys.glob(file.path(extracted_dir, "all-data_si-extract_3_*.csv"))
))
sitecoord_filename <- "silica-coords_RAW.xlsx"

# Advanced overrides for debugging only
extracted_dir <- env_or_default("SILICA_QAQC_EXTRACTED_DIR", extracted_dir)
sitecoord_dir <- env_or_default("SILICA_QAQC_SITECOORD_DIR", sitecoord_dir)
review_root <- env_or_default("SILICA_QAQC_REVIEW_ROOT", review_root)

if (!nzchar(new_file_default)) {
  stop(
    "Could not find a candidate new combined spatial file under either:\n- ",
    all_extract_dir,
    "\n- ",
    extracted_dir,
    call. = FALSE
  )
}

harmonization_dir <- file.path(review_root, "harmonization")
year_extension_dir <- file.path(review_root, "year_extension")
glimpse_dir <- file.path(review_root, "glimpse")

# Input files used by import, comparison, and review
old_file <- file.path(all_extract_dir, old_combined_filename)
new_file <- new_file_default
ref_file <- file.path(sitecoord_dir, sitecoord_filename)

# Advanced file-level overrides for debugging only
old_file <- env_or_default("SILICA_QAQC_OLD_FILE", old_file)
new_file <- env_or_default("SILICA_QAQC_NEW_FILE", new_file)
ref_file <- env_or_default("SILICA_QAQC_REF_FILE", ref_file)

# Optional review inputs and run-level settings
site_followup_file <- file.path(getwd(), "site_followup_notes.csv")
# Optional template for manual notes:
# 04_combine_qaqc/templates/site_followup_notes_template.csv
date_tag <- format(Sys.Date(), "%Y%m%d")
write_detailed_review <- FALSE

# Fail early if the required QAQC inputs are missing
for (path_obj in c(old_file, new_file, ref_file)) {
  if (!file.exists(path_obj)) {
    stop("Missing QAQC input file: ", path_obj, call. = FALSE)
  }
}

# Create review output folders up front
dir.create(harmonization_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(year_extension_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(glimpse_dir, recursive = TRUE, showWarnings = FALSE)
