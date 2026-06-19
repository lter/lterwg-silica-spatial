librarian::shelf(tools)

source(file.path(getwd(), "tools", "workflow_paths.R"))

latest_file <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return("")
  }
  paths[which.max(file.info(paths)$mtime)]
}

data_root <- resolve_silica_data_root()
extracted_dir <- silica_extracted_data_dir(data_root)
review_harmonization_dir <- file.path(data_root, "review", "harmonization")

cat("data_root=", data_root, "\n", sep = "")
cat("extracted_dir=", extracted_dir, "\n", sep = "")
cat("review_harmonization_dir=", review_harmonization_dir, "\n", sep = "")

cat("\n[1/3] Rebuilding the local combined driver table from extracted CSVs\n")
source(file.path("04_combine_qaqc", "combine_from_site_ref_local.R"), local = new.env(parent = globalenv()))

new_combined <- latest_file(c(
  list.files(extracted_dir, pattern = "^all-data_si-extract_3_.*\\.csv$", full.names = TRUE)
))
if (!nzchar(new_combined)) {
  stop("Failed to locate a rebuilt local combined file in ", extracted_dir, call. = FALSE)
}
Sys.setenv(SILICA_QAQC_NEW_FILE = new_combined)
cat("Using new combined candidate for QAQC: ", new_combined, "\n", sep = "")

cat("\n[2/3] Running combine-stage QA/QC and vetted combined build\n")
source(file.path("04_combine_qaqc", "01_import-and-qaqc.R"), local = new.env(parent = globalenv()))

vetted_combined <- latest_file(c(
  list.files(review_harmonization_dir, pattern = "^combined-spatial-dataset_.*\\.csv$", full.names = TRUE)
))
if (!nzchar(vetted_combined)) {
  stop(
    "Failed to locate a vetted combined spatial dataset under ",
    review_harmonization_dir,
    call. = FALSE
  )
}
Sys.setenv(SILICA_HARMONIZATION_COMBINED_FILE = vetted_combined)
cat("Using vetted combined file for harmonization: ", vetted_combined, "\n", sep = "")

cat("\n[3/3] Building harmonized site-level driver outputs\n")
source(file.path("05_harmonization", "01_build-harmonized-drivers.R"), local = new.env(parent = globalenv()))

cat("\nWorkflow complete.\n")
