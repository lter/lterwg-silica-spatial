# Run the locked integration gate, harmonization, and post-harmonization QA

source(file.path("tools", "workflow_paths.R"))

### Arguments

args <- commandArgs(trailingOnly = TRUE)
manifest_path <- cli_value(
  args,
  "--manifest",
  file.path(
    "04_combine_qaqc", "config", "final_spatial_sources_20260805.tsv"
  )
)
integration_root <- cli_value(
  args,
  "--integration-root",
  file.path("generated_outputs", "final-integration", "final-spatial-20260805")
)
harmonization_root <- cli_value(
  args,
  "--harmonization-root",
  file.path(integration_root, "harmonized")
)
date_tag <- cli_value(args, "--date", "20260805")

### Integration gate

gate_status <- system2(
  Sys.which("Rscript"),
  c(
    "tools/build_final_spatial_integration.R",
    "--manifest", manifest_path,
    "--outdir", integration_root
  )
)
if (!identical(gate_status, 0L)) {
  stop("The final integration gate failed; harmonization was not started.")
}

### Registered GEE inputs

data_root <- resolve_silica_data_root()
manifest <- read.delim(
  manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  quote = ""
)

registered_path <- function(family) {
  rows <- manifest[manifest$family == family, , drop = FALSE]
  if (nrow(rows) != 1L) {
    stop("Expected one registered source for family ", family)
  }
  path <- expand_workflow_path(rows$path[[1]], data_root = data_root)
  if (!file.exists(path)) stop("Missing registered ", family, ": ", path)
  normalizePath(path, mustWork = TRUE)
}

combined_path <- normalizePath(
  file.path(integration_root, "final_combined_spatial_drivers.csv"),
  mustWork = TRUE
)
expected_site_rows <- nrow(read.csv(
  combined_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
))
lulc_path <- registered_path("land_cover")
era5_path <- registered_path("era5_annual")
human_path <- registered_path("human_impacts")
dir.create(harmonization_root, recursive = TRUE, showWarnings = FALSE)
harmonization_root <- normalizePath(harmonization_root, mustWork = TRUE)

Sys.setenv(
  SILICA_DATA_ROOT = data_root,
  SILICA_HARMONIZATION_COMBINED_FILE = combined_path,
  SILICA_HARMONIZATION_OUTPUT_DIR = harmonization_root,
  SILICA_HARMONIZATION_DATE = date_tag,
  SILICA_LULC_FILE = lulc_path,
  SILICA_GEE_ERA5_DIR = era5_path,
  SILICA_GEE_HUMAN_IMPACTS_FILE = human_path,
  SILICA_ADD_GEE_ERA5 = "true",
  SILICA_ADD_GEE_HUMAN_IMPACTS = "true"
)

source(
  file.path("05_harmonization", "01_build-harmonized-drivers.R"),
  local = new.env(parent = globalenv())
)

### Post-harmonization QA

site_path <- file.path(
  harmonization_root,
  paste0("harmonized-spatial-drivers_", date_tag, ".csv")
)
annual_path <- file.path(
  harmonization_root,
  paste0("harmonized-spatial-drivers-annual_", date_tag, ".csv")
)
if (!file.exists(site_path) || !file.exists(annual_path)) {
  stop("Harmonization did not produce the expected site and annual tables.")
}

site <- read.csv(site_path, stringsAsFactors = FALSE, check.names = FALSE)
annual <- read.csv(annual_path, stringsAsFactors = FALSE, check.names = FALSE)
analysis_annual <- annual[annual$Year %in% 2002:2025, , drop = FALSE]
era_columns <- grep("^gee_era5_", names(analysis_annual), value = TRUE)
human_columns <- grep("^gee_human_", names(site), value = TRUE)
annual_human_columns <- grep("^gee_human_", names(analysis_annual), value = TRUE)

post_summary <- data.frame(
  check = c(
    "harmonized_site_rows",
    "harmonized_canonical_row_ids",
    "harmonized_watershed_ids",
    "land_cover_watershed_matches",
    "analysis_site_year_rows",
    "analysis_site_year_duplicates",
    "gee_era5_columns",
    "gee_era5_complete_site_years",
    "gee_human_static_columns",
    "gee_human_annual_columns"
  ),
  status = c(
    if (nrow(site) == expected_site_rows) "PASS" else "FAIL",
    if (
      "canonical_row_id" %in% names(site) &&
        length(unique(site$canonical_row_id)) == expected_site_rows
    ) {
      "PASS"
    } else {
      "FAIL"
    },
    if (
      "gee_watershed_id" %in% names(site) &&
        sum(!is.na(site$gee_watershed_id) & nzchar(site$gee_watershed_id)) ==
          expected_site_rows
    ) {
      "PASS"
    } else {
      "FAIL"
    },
    if (
      "gee_glc_match" %in% names(site) && all(site$gee_glc_match %in% TRUE)
    ) {
      "PASS"
    } else {
      "FAIL"
    },
    if (nrow(analysis_annual) == expected_site_rows * 24L) "PASS" else "FAIL",
    if (
      all(c("canonical_row_id", "Year") %in% names(analysis_annual)) &&
        !anyDuplicated(analysis_annual[, c("canonical_row_id", "Year")])
    ) {
      "PASS"
    } else {
      "FAIL"
    },
    if (length(era_columns) == 6L) "PASS" else "FAIL",
    if (
      length(era_columns) == 6L &&
        all(rowSums(!is.na(analysis_annual[, era_columns, drop = FALSE])) == 6L)
    ) {
      "PASS"
    } else {
      "FAIL"
    },
    if (length(human_columns) > 0L) "PASS" else "FAIL",
    if (length(annual_human_columns) > 0L) "PASS" else "FAIL"
  ),
  result = c(
    nrow(site),
    if ("canonical_row_id" %in% names(site)) {
      length(unique(site$canonical_row_id))
    } else {
      0L
    },
    if ("gee_watershed_id" %in% names(site)) {
      sum(!is.na(site$gee_watershed_id) & nzchar(site$gee_watershed_id))
    } else {
      0L
    },
    if ("gee_glc_match" %in% names(site)) sum(site$gee_glc_match %in% TRUE) else 0L,
    nrow(analysis_annual),
    if (all(c("canonical_row_id", "Year") %in% names(analysis_annual))) {
      sum(duplicated(analysis_annual[, c("canonical_row_id", "Year")]))
    } else {
      nrow(analysis_annual)
    },
    length(era_columns),
    if (length(era_columns)) {
      sum(rowSums(!is.na(analysis_annual[, era_columns, drop = FALSE])) == length(era_columns))
    } else {
      0L
    },
    length(human_columns),
    length(annual_human_columns)
  ),
  stringsAsFactors = FALSE
)
write.csv(
  post_summary,
  file.path(integration_root, "post_harmonization_qa.csv"),
  row.names = FALSE
)
print(post_summary, row.names = FALSE)

if (any(post_summary$status == "FAIL")) {
  stop("Post-harmonization QA failed.")
}
cat("Final harmonization and post-QA passed.\n")
