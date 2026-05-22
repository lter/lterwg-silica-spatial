#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
})

source(file.path(getwd(), "04_combine_qaqc", "00_qaqc_functions.R"))

fill_major_rock_from_percentages <- function(df) {
  rock_cols <- grep("^rocks_", names(df), value = TRUE)
  if (!length(rock_cols)) {
    return(df)
  }

  if (!"major_rock" %in% names(df)) {
    df$major_rock <- NA_character_
  }

  rock_vals <- as.data.frame(lapply(df[, rock_cols, drop = FALSE], function(x) {
    suppressWarnings(as.numeric(x))
  }))
  names(rock_vals) <- rock_cols

  needs_major <- is.na(df$major_rock) | trimws(as.character(df$major_rock)) == ""
  has_rock_values <- rowSums(!is.na(rock_vals)) > 0
  fill_rows <- which(needs_major & has_rock_values)

  if (length(fill_rows)) {
    df$major_rock[fill_rows] <- vapply(fill_rows, function(i) {
      vals <- unlist(rock_vals[i, , drop = TRUE], use.names = TRUE)
      vals <- vals[!is.na(vals)]
      if (!length(vals)) {
        return(NA_character_)
      }
      winners <- names(vals)[vals == max(vals, na.rm = TRUE)]
      paste(gsub("^rocks_", "", winners), collapse = "; ")
    }, character(1))
  }

  df
}

args <- commandArgs(trailingOnly = TRUE)
staging_dir <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else ""
run_label <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else "aurora-hydrosheds-patched"
run_date <- if (length(args) >= 3 && nzchar(args[[3]])) args[[3]] else format(Sys.Date(), "%Y%m%d")

if (!nzchar(staging_dir)) {
  stop("Usage: Rscript tools/build_final_spatial_master_from_driver_patches.R <staging_dir> [run_label] [run_date]", call. = FALSE)
}

staging_dir <- normalizePath(staging_dir, mustWork = TRUE)
repo_root <- getwd()

default_data_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial_data_extractions"
data_root <- Sys.getenv("SILICA_QAQC_DATA_ROOT", unset = default_data_root)
if (!dir.exists(data_root)) {
  stop("Missing SILICA_QAQC_DATA_ROOT/data_root: ", data_root, call. = FALSE)
}

canonical_file_default <- file.path(
  data_root,
  "si-extracted-data",
  "all_data_extractions",
  "all-data_si-extract_3_full-canonical_20260323.csv"
)
legacy_file_default <- file.path(
  data_root,
  "si-extracted-data",
  "all_data_extractions",
  "all-data_si-extract_2_20250325.csv"
)
sitecoord_dir_default <- file.path(data_root, "silica-shapefiles", "site-coordinates")

canonical_file <- normalizePath(Sys.getenv("SILICA_CANONICAL_FILE", unset = canonical_file_default), mustWork = TRUE)
legacy_file <- normalizePath(Sys.getenv("SILICA_LEGACY_FILE", unset = legacy_file_default), mustWork = TRUE)
sitecoord_dir <- normalizePath(Sys.getenv("SILICA_SITE_COORD_DIR", unset = sitecoord_dir_default), mustWork = TRUE)
patch_base_file <- normalizePath(Sys.getenv("SILICA_PATCH_BASE_FILE", unset = canonical_file), mustWork = TRUE)

driver_files <- list.files(staging_dir, pattern = "^si-extract_.*\\.csv$", full.names = TRUE)
if (!length(driver_files)) {
  stop("No si-extract_*.csv files found in staging dir: ", staging_dir, call. = FALSE)
}

driver_files <- sort(driver_files)
output_root <- file.path(repo_root, "generated_outputs", "final_combine", paste0(run_date, "_", run_label))
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

patch_output <- file.path(output_root, paste0("patch_driver_overlay_", run_date, "_", run_label, ".csv"))
review_root <- file.path(output_root, "review_patch")
dir.create(review_root, recursive = TRUE, showWarnings = FALSE)

message("Building patch overlay from staged driver files:")
message("Patch base file: ", patch_base_file)
for (f in driver_files) message(" - ", basename(f))

driver_env <- paste(basename(driver_files), collapse = ",")
combine_env <- c(
  paste0("SILICA_SITE_COORD_DIR=", sitecoord_dir),
  paste0("SILICA_EXTRACTED_DIR=", staging_dir),
  paste0("SILICA_BASE_FILE=", patch_base_file),
  paste0("SILICA_DRIVER_FILES=", driver_env),
  paste0("SILICA_OUTPUT_FILE=", patch_output),
  paste0("SILICA_QA_ROOT=", review_root),
  paste0("SILICA_RUN_LABEL=", paste0(run_label, "-patch"))
)

status <- system2(
  "Rscript",
  c("04_combine_qaqc/combine_from_site_ref_local.R"),
  env = combine_env
)
if (!identical(status, 0L)) {
  stop("combine_from_site_ref_local.R failed with status ", status, call. = FALSE)
}

first_non_missing <- function(x) {
  keep <- !(is.na(x) | trimws(as.character(x)) == "")
  if (!any(keep)) {
    if (is.numeric(x)) return(NA_real_)
    return(NA)
  }
  x[which(keep)[1]]
}

collapse_duplicate_sites_local <- function(df) {
  annual_cols <- grep("^(evapotrans|greenup_cycle[01]|precip_|temp_|snow_|npp_)", names(df), value = TRUE)
  non_key_cols <- setdiff(names(df), c("Stream_ID", "LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name", "key"))

  df <- df %>%
    mutate(
      Stream_ID = if ("Stream_ID" %in% names(.)) Stream_ID else build_stream_id(.),
      .site_id = Stream_ID,
      .has_named_shapefile = !is.na(Shapefile_Name) & Shapefile_Name != "",
      .annual_non_na = if (length(annual_cols)) rowSums(!is.na(across(all_of(annual_cols)))) else 0L,
      .non_key_non_na = if (length(non_key_cols)) rowSums(!is.na(across(all_of(non_key_cols))) & across(all_of(non_key_cols)) != "") else 0L
    ) %>%
    arrange(.site_id, desc(.annual_non_na), desc(.has_named_shapefile), desc(.non_key_non_na))

  dup_summary <- df %>%
    count(.site_id, name = "n") %>%
    filter(n > 1)

  collapsed <- df %>%
    group_by(.site_id) %>%
    summarise(
      across(
        .cols = -c(.annual_non_na, .non_key_non_na, .has_named_shapefile),
        .fns = first_non_missing
      ),
      .groups = "drop"
    ) %>%
    select(-.site_id)

  attr(collapsed, "duplicate_site_groups") <- dup_summary
  collapsed
}

merge_prefer_old <- function(old_file, new_file, sanitize_new = FALSE) {
  old <- read_combined_table(old_file)
  new <- read_combined_table(new_file, sanitize_new = sanitize_new)

  blank_to_na <- function(df) {
    for (nm in names(df)) {
      if (is.character(df[[nm]])) {
        vals <- trimws(df[[nm]])
        vals[vals == ""] <- NA_character_
        df[[nm]] <- vals
      }
    }
    df
  }

  old <- blank_to_na(old)
  new <- blank_to_na(new)

  all_cols <- union(names(old), names(new))
  for (nm in setdiff(all_cols, names(old))) old[[nm]] <- NA
  for (nm in setdiff(all_cols, names(new))) new[[nm]] <- NA
  old <- old[, all_cols, drop = FALSE]
  new <- new[, all_cols, drop = FALSE]

  shared_keys <- intersect(old$key, new$key)
  old_only_keys <- setdiff(old$key, new$key)
  new_only_keys <- setdiff(new$key, old$key)

  old_shared <- old[match(shared_keys, old$key), , drop = FALSE]
  new_shared <- new[match(shared_keys, new$key), , drop = FALSE]
  merged_shared <- old_shared

  for (nm in setdiff(all_cols, "key")) {
    merged_shared[[nm]] <- dplyr::coalesce(old_shared[[nm]], new_shared[[nm]])
  }

  out <- bind_rows(
    merged_shared,
    old[old$key %in% old_only_keys, , drop = FALSE],
    new[new$key %in% new_only_keys, , drop = FALSE]
  )

  out <- collapse_duplicate_sites_local(out)
  out$key <- NULL
  out
}

canonical_plus_legacy <- merge_prefer_old(canonical_file, legacy_file, sanitize_new = FALSE)
canonical_plus_legacy_file <- file.path(output_root, paste0("canonical_plus_legacy_", run_date, "_", run_label, ".csv"))
write.csv(canonical_plus_legacy, canonical_plus_legacy_file, row.names = FALSE, na = "")

final_master <- merge_prefer_old(patch_output, canonical_plus_legacy_file, sanitize_new = FALSE)
join_artifact_cols <- grep("^(geom|Shpfl_N)(\\.|$)", names(final_master), value = TRUE)
if (length(join_artifact_cols) > 0) {
  final_master <- final_master %>% select(-all_of(join_artifact_cols))
}
final_master <- fill_major_rock_from_percentages(final_master)
final_out_file <- file.path(
  data_root,
  "si-extracted-data",
  "all_data_extractions",
  paste0("all-data_si-extract_4_", run_date, "_", run_label, ".csv")
)
write.csv(final_master, final_out_file, row.names = FALSE, na = "")

summary_tbl <- data.frame(
  canonical_file = canonical_file,
  legacy_file = legacy_file,
  patch_base_file = patch_base_file,
  staging_dir = staging_dir,
  staged_driver_files = paste(basename(driver_files), collapse = ";"),
  patch_output = patch_output,
  canonical_plus_legacy_file = canonical_plus_legacy_file,
  final_output = final_out_file,
  evapo_min = min_year_from_names(names(final_master), "^evapotrans_([0-9]{4})_kg_m2$"),
  evapo_max = max_year_from_names(names(final_master), "^evapotrans_([0-9]{4})_kg_m2$"),
  greenup_max = max_year_from_names(names(final_master), "^greenup_cycle[01]_([0-9]{4})MMDD$"),
  precip_max = max_year_from_names(names(final_master), "^precip_([0-9]{4})_mm_per_day$"),
  airtemp_max = max_year_from_names(names(final_master), "^temp_([0-9]{4})_degC$"),
  snow_max = max_year_from_names(names(final_master), "^snow_([0-9]{4})_num_days$"),
  npp_max = max_year_from_names(names(final_master), "^npp_([0-9]{4})_kgC_m2_year$"),
  stringsAsFactors = FALSE
)

summary_file <- file.path(output_root, paste0("final_combine_summary_", run_date, "_", run_label, ".csv"))
write.csv(summary_tbl, summary_file, row.names = FALSE, na = "")

message("Patch overlay file: ", patch_output)
message("Canonical+legacy file: ", canonical_plus_legacy_file)
message("Final combined file: ", final_out_file)
message("Summary file: ", summary_file)
