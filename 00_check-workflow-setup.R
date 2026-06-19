## Optional setup checker for new machines.
## This is not required for normal reruns once the repo is already working.

librarian::shelf(dplyr)

source(file.path(getwd(), "tools", "workflow_paths.R"))

check_ok <- function(label, value = "") {
  cat("[ok] ", label, if (nzchar(value)) paste0(": ", value) else "", "\n", sep = "")
}

check_warn <- function(label, value = "") {
  cat("[warn] ", label, if (nzchar(value)) paste0(": ", value) else "", "\n", sep = "")
}

check_fail <- function(label, value = "") {
  cat("[fail] ", label, if (nzchar(value)) paste0(": ", value) else "", "\n", sep = "")
}

required_repo_files <- c(
  "02_run-workflow.R",
  "01_run_config.R",
  "03_spatial_extraction/modes/full_run_all_sites.R",
  "03_spatial_extraction/modes/targeted_sites_run.R",
  "03_spatial_extraction/modes/hydrosheds_sites_run.R",
  "05_harmonization/00_harmonization_config.R",
  "05_harmonization/00_harmonization_functions.R",
  "05_harmonization/01_build-harmonized-drivers.R",
  "04_combine_qaqc/00_qaqc_config.R",
  "04_combine_qaqc/01_import-and-qaqc.R",
  "03_spatial_extraction/extraction_scripts/extract-soil.R",
  "03_spatial_extraction/extraction_scripts/extract-precip.R",
  "02_watershed_delineation/01_wrangle-artisanal-watersheds.R",
  "02_watershed_delineation/02_wrangle-hydrosheds.R",
  "02_watershed_delineation/03_combine-artisanal-hydrosheds.R",
  "04_combine_qaqc/build-combined-spatial-dataset.R",
  "04_combine_qaqc/qaqc-new-vs-old.R",
  "04_combine_qaqc/qaqc-reasonable-new-values.R",
  "04_combine_qaqc/combine_from_site_ref_local.R",
  "tools/build_targeted_rerun_manifest.R",
  "tools/build_hydrosheds_followup_subset.R",
  "tools/run_targeted_subset_workflow.R",
  "tools/workflow_paths.R",
  "tools/subset_and_output_helpers.R"
)

missing_repo_files <- required_repo_files[!file.exists(required_repo_files)]
if (!length(missing_repo_files)) {
  check_ok("repo files are in place")
} else {
  for (f in missing_repo_files) {
    check_fail("missing repo file", f)
  }
}

parse_targets <- c(
  required_repo_files[grepl("\\.R$", required_repo_files)],
  "03_spatial_extraction/aurora/run_targeted_subset_aurora.sh",
  "03_spatial_extraction/aurora/run_hydrosheds_followup_20260510.sh",
  "03_spatial_extraction/aurora/submit_targeted_subset_aurora.sbatch"
)

for (f in parse_targets[grepl("\\.R$", parse_targets)]) {
  tryCatch(
    {
      parse(file = f)
      check_ok("R parses", f)
    },
    error = function(e) {
      check_fail("R parse failed", paste(f, "-", conditionMessage(e)))
    }
  )
}

shell_targets <- parse_targets[grepl("\\.(sh|sbatch)$", parse_targets)]
for (f in shell_targets) {
  status <- system2("bash", c("-n", f))
  if (identical(status, 0L)) {
    check_ok("shell parses", f)
  } else {
    check_fail("shell parse failed", f)
  }
}

root_path <- tryCatch(resolve_silica_data_root(), error = function(e) e)
if (inherits(root_path, "error")) {
  check_fail("could not find data root", conditionMessage(root_path))
  quit(status = 1)
}
check_ok("data root", root_path)

site_dir <- tryCatch(silica_site_coordinates_dir(root_path), error = function(e) e)
raw_dir <- tryCatch(silica_raw_driver_data_dir(root_path), error = function(e) e)
hydro_dir <- tryCatch(silica_hydrosheds_raw_dir(root_path), error = function(e) e)
extract_dir <- tryCatch(silica_extracted_data_dir(root_path), error = function(e) e)

required_runtime_dirs <- list(
  list(label = "site-coordinates directory", value = site_dir),
  list(label = "extracted-data directory", value = extract_dir)
)

optional_extraction_dirs <- list(
  list(label = "raw-driver-data directory", value = raw_dir),
  list(label = "hydrosheds-raw directory", value = hydro_dir)
)

for (item in required_runtime_dirs) {
  if (inherits(item$value, "error")) {
    check_fail(item$label, conditionMessage(item$value))
  } else {
    check_ok(item$label, item$value)
  }
}

for (item in optional_extraction_dirs) {
  if (inherits(item$value, "error")) {
    check_warn(item$label, paste(conditionMessage(item$value), "(needed only for full extraction runs)"))
  } else {
    check_ok(item$label, item$value)
  }
}

if (!inherits(site_dir, "error")) {
  workbook <- file.path(site_dir, "silica-coords_RAW.xlsx")
  if (file.exists(workbook)) {
    check_ok("reference workbook", workbook)
  } else {
    check_fail("missing reference workbook", workbook)
  }
}

if (!inherits(hydro_dir, "error")) {
  hydro_files <- c(
    "hybas_af_lev00_v1c.shp",
    "hybas_ar_lev00_v1c.shp",
    "hybas_as_lev00_v1c.shp",
    "hybas_au_lev00_v1c.shp",
    "hybas_eu_lev00_v1c.shp",
    "hybas_gr_lev00_v1c.shp",
    "hybas_na_lev00_v1c.shp",
    "hybas_sa_lev00_v1c.shp",
    "hybas_si_lev00_v1c.shp"
  )
  missing_hydro <- hydro_files[!file.exists(file.path(hydro_dir, hydro_files))]
  if (!length(missing_hydro)) {
    check_ok("HydroSHEDS continent files are in place")
  } else {
    for (f in missing_hydro) {
      check_warn("missing HydroSHEDS file", f)
    }
  }
}

if (!inherits(site_dir, "error") && !inherits(extract_dir, "error")) {
  check_ok("combine / QAQC prerequisites", "site coordinates and extracted data are available")
}

if (!inherits(raw_dir, "error") && !inherits(hydro_dir, "error")) {
  check_ok("full extraction prerequisites", "raw drivers and HydroSHEDS inputs are available")
} else {
  check_warn("full extraction prerequisites", "missing optional inputs for full extraction; combine / QAQC can still run")
}

check_warn("heavy extraction runs were not started", "this script only checks structure and paths")
