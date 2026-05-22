# This script looks at the reference table plus the current and legacy
# combined outputs, then writes rerun lists for sites that still need help.
# The main HydroSHEDS approval use case is intentionally narrow:
# - drainage area > threshold
# - and either no shapefile name is listed
# - or the previous run had no dynamic spatial data
# - and the current combined table still has no dynamic spatial data
#
# That approval-first list is what should be reviewed before running Aurora.
# The broader manifest is still written so we keep the full reasoning trail.
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(readxl)
})

args <- commandArgs(trailingOnly = TRUE)
source(file.path(getwd(), "tools", "workflow_paths.R"))
source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit)) {
    return(default)
  }
  args[hit[1] + 1]
}

resolve_input_path <- function(cli_value, env_var, candidates, label) {
  if (!is.null(cli_value)) {
    if (!file.exists(cli_value)) {
      stop(label, " file does not exist: ", cli_value, call. = FALSE)
    }
    return(cli_value)
  }

  env_value <- Sys.getenv(env_var, unset = NA_character_)
  if (!is.na(env_value) && nzchar(env_value)) {
    if (!file.exists(env_value)) {
      stop(label, " file from ", env_var, " does not exist: ", env_value, call. = FALSE)
    }
    return(env_value)
  }

  existing <- candidates[file.exists(candidates)]
  if (length(existing)) {
    return(existing[[1]])
  }

  stop(
    paste0(
      "Could not locate ", label, " file. Supply ", label, " via ",
      if (label == "reference") "--ref or SILICA_TARGETED_REF_PATH" else "--combined or SILICA_TARGETED_COMBINED_PATH",
      ". Checked:\n- ",
      paste(candidates, collapse = "\n- ")
    ),
    call. = FALSE
  )
}

latest_matching_paths <- function(pattern) {
  hits <- Sys.glob(pattern)
  if (!length(hits)) {
    return(character(0))
  }
  hits[order(file.info(hits)$mtime, decreasing = TRUE)]
}

# Find the input files from a command-line flag first, then an environment
# variable, then a short built-in list of expected locations.
default_root <- silica_default_box_data_root()
current_combined_candidates <- c(
  latest_matching_paths(file.path(default_root, "si-extracted-data", "all_data_extractions", "all-data_si-extract_3_*.csv")),
  latest_matching_paths(file.path(default_root, "all-data_si-extract_3_*.csv"))
)
current_combined_candidates <- current_combined_candidates[
  !grepl("rerun|combinedlocal|hydrosheds-full-record-recoverable", basename(current_combined_candidates), ignore.case = TRUE)
]
ref_path <- resolve_input_path(
  cli_value = get_arg("--ref"),
  env_var = "SILICA_TARGETED_REF_PATH",
  candidates = c(
    file.path(default_root, "master", "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv"),
    "/Users/sidneybush/Documents/GitHub/NCEAS_SiSyn_CQ/raw_data/Site_Reference_Table - WRTDS_Reference_Table_LTER_V2.csv"
  ),
  label = "reference"
)
combined_path <- resolve_input_path(
  cli_value = get_arg("--combined"),
  env_var = "SILICA_TARGETED_COMBINED_PATH",
  candidates = c(
    current_combined_candidates,
    file.path(default_root, "all-data_si-extract_3_20260323.csv"),
    file.path(default_root, "_archive_current_workflow_cleanup_20260411", "root", "all-data_si-extract_3_20260323.csv"),
    file.path(default_root, "si-extracted-data", "all-data_si-extract_3_20260323_fromSiteRef_full_canonical.csv")
  ),
  label = "combined"
)
legacy_path <- resolve_input_path(
  cli_value = get_arg("--legacy"),
  env_var = "SILICA_TARGETED_LEGACY_PATH",
  candidates = c(
    file.path(default_root, "all-data_si-extract_2_20250325.csv"),
    file.path(default_root, "_archive_current_workflow_cleanup_20260411", "root", "all-data_si-extract_2_20250325.csv")
  ),
  label = "legacy combined"
)
qa_root <- get_arg("--outdir", silica_review_root(resolve_silica_data_root()))
rerun_dir <- file.path(qa_root, "rerun")
harmonization_dir <- file.path(qa_root, "harmonization")
large_basin_km2 <- suppressWarnings(as.numeric(get_arg("--large-basin-km2", "100")))
target_years <- silica_target_years()
review_root_default <- silica_review_root(resolve_silica_data_root())
prior_missingness_candidates <- unique(c(
  Sys.glob(file.path(qa_root, "rerun", "targeted_rerun_subset_from_missingness_*.csv")),
  Sys.glob(file.path(review_root_default, "rerun", "targeted_rerun_subset_from_missingness_*.csv")),
  Sys.glob(file.path(review_root_default, "qa", "targeted_rerun_subset_from_missingness_*.csv"))
))
prior_missingness_path <- if (length(prior_missingness_candidates)) {
  prior_missingness_candidates[which.max(file.info(prior_missingness_candidates)$mtime)]
} else {
  NA_character_
}

dir.create(rerun_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(harmonization_dir, recursive = TRUE, showWarnings = FALSE)

message("Using reference file: ", ref_path)
message("Using legacy file: ", legacy_path)
message("Using combined file: ", combined_path)

norm_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

read_reference_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xls", "xlsx")) {
    out <- readxl::read_excel(path)
  } else if (ext == "csv") {
    out <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop(
      "Unsupported reference file type for ", path,
      ". Expected .csv, .xls, or .xlsx.",
      call. = FALSE
    )
  }

  # Some exported tables repeat column names like "Notes". Give them unique
  # names here so the rest of the script can read the table safely.
  if (anyDuplicated(names(out))) {
    names(out) <- make.unique(names(out), sep = "__dup")
  }

  out
}

has_dynamic_signal <- function(df) {
  dyn_re <- "^(evapotrans_[0-9]{4}_kg_m2|greenup_cycle[01]_[0-9]{4}MMDD|precip_[0-9]{4}_mm_per_day|temp_[0-9]{4}_degC|snow_[0-9]{4}_num_days|npp_[0-9]{4}_kgC_m2_year)$"
  dyn_cols <- names(df)[grepl(dyn_re, names(df))]
  if (!length(dyn_cols)) {
    return(rep(FALSE, nrow(df)))
  }
  rowSums(!is.na(df[, dyn_cols, drop = FALSE])) > 0
}

year_driver_specs <- tribble(
  ~driver,    ~regex,
  "evapo",    "^evapotrans_([0-9]{4})_kg_m2$",
  "greenup",  "^greenup_cycle[01]_([0-9]{4})MMDD$",
  "precip",   "^precip_([0-9]{4})_mm_per_day$",
  "airtemp",  "^temp_([0-9]{4})_degC$",
  "snow",     "^snow_([0-9]{4})_num_days$",
  "npp",      "^npp_([0-9]{4})_kgC_m2_year$"
)

extract_years_from_names <- function(nms, rx) {
  hits <- stringr::str_match(nms, rx)[, 2]
  suppressWarnings(as.integer(hits))
}

site_driver_years <- function(df, specs) {
  if (!nrow(df)) {
    return(tibble::tibble())
  }

  out <- list()
  for (i in seq_len(nrow(specs))) {
    cols <- names(df)[grepl(specs$regex[i], names(df))]
    if (!length(cols)) {
      next
    }
    yrs <- extract_years_from_names(cols, specs$regex[i])
    for (row_i in seq_len(nrow(df))) {
      vals <- !is.na(unlist(df[row_i, cols, drop = FALSE], use.names = FALSE))
      if (!any(vals)) {
        next
      }
      out[[length(out) + 1]] <- tibble::tibble(
        key = df$key[row_i],
        driver = specs$driver[i],
        year = sort(unique(yrs[vals]))
      )
    }
  }

  dplyr::bind_rows(out)
}

ref <- read_reference_table(ref_path)
legacy <- read.csv(legacy_path, stringsAsFactors = FALSE, check.names = FALSE)
combined <- read.csv(combined_path, stringsAsFactors = FALSE, check.names = FALSE)

dynamic_cols <- names(combined)[grepl("^(evapotrans|greenup_cycle[01]|precip_|temp_|snow_|npp_)", names(combined))]
if (!length(dynamic_cols)) {
  stop("No dynamic driver columns found in combined dataset: ", combined_path)
}

# Normalize the three main inputs onto one shared key so we can compare them.
ref_key <- ref %>%
  transmute(
    LTER = normalize_lter_key(LTER),
    Stream_Name = norm_chr(Stream_Name),
    Discharge_File_Name = norm_chr(Discharge_File_Name),
    Shapefile_Name = norm_chr(Shapefile_Name),
    Shapefile_Source = norm_chr(Shapefile_Source),
    Shapefile_CRS_EPSG = suppressWarnings(as.numeric(Shapefile_CRS_EPSG)),
    drainSqKm = suppressWarnings(as.numeric(drainSqKm)),
    key = paste(
      LTER,
      normalize_stream_key(Stream_Name),
      tolower(Discharge_File_Name),
      tolower(Shapefile_Name),
      sep = "||"
    ),
    site_id = paste(
      LTER,
      normalize_stream_key(Stream_Name),
      sep = "||"
    )
  ) %>%
  distinct()

legacy_key <- legacy %>%
  mutate(
    LTER = normalize_lter_key(LTER),
    Stream_Name = norm_chr(Stream_Name),
    Discharge_File_Name = norm_chr(Discharge_File_Name),
    Shapefile_Name = norm_chr(Shapefile_Name),
    key = paste(
      LTER,
      normalize_stream_key(Stream_Name),
      tolower(Discharge_File_Name),
      tolower(Shapefile_Name),
      sep = "||"
    ),
    has_dynamic_legacy = NA
  )
legacy_key$has_dynamic_legacy <- has_dynamic_signal(legacy_key)
legacy_key <- legacy_key %>%
  distinct(key, .keep_all = TRUE) %>%
  transmute(key, has_dynamic_legacy)

combined_key <- combined %>%
  mutate(
    LTER = normalize_lter_key(LTER),
    Stream_Name = norm_chr(Stream_Name),
    Discharge_File_Name = norm_chr(Discharge_File_Name),
    Shapefile_Name = norm_chr(Shapefile_Name),
    key = paste(
      LTER,
      normalize_stream_key(Stream_Name),
      tolower(Discharge_File_Name),
      tolower(Shapefile_Name),
      sep = "||"
    ),
    has_dynamic_current = NA
  )
combined_key$has_dynamic_current <- has_dynamic_signal(combined_key)
combined_key <- combined_key %>%
  distinct(key, .keep_all = TRUE) %>%
  transmute(key, has_dynamic_current)

legacy_years <- legacy %>%
  mutate(
    LTER = normalize_lter_key(LTER),
    Stream_Name = norm_chr(Stream_Name),
    Discharge_File_Name = norm_chr(Discharge_File_Name),
    Shapefile_Name = norm_chr(Shapefile_Name),
    key = paste(
      LTER,
      normalize_stream_key(Stream_Name),
      tolower(Discharge_File_Name),
      tolower(Shapefile_Name),
      sep = "||"
    )
  ) %>%
  site_driver_years(year_driver_specs)

current_years <- combined %>%
  mutate(
    LTER = normalize_lter_key(LTER),
    Stream_Name = norm_chr(Stream_Name),
    Discharge_File_Name = norm_chr(Discharge_File_Name),
    Shapefile_Name = norm_chr(Shapefile_Name),
    key = paste(
      LTER,
      tolower(Stream_Name),
      tolower(Discharge_File_Name),
      tolower(Shapefile_Name),
      sep = "||"
    )
  ) %>%
  site_driver_years(year_driver_specs)

historical_driver_years <- dplyr::bind_rows(legacy_years, current_years) %>%
  dplyr::distinct(key, driver, year)

if (length(target_years)) {
  pre_target_year <- min(target_years)
  historical_driver_presence <- historical_driver_years %>%
    dplyr::filter(year < pre_target_year) %>%
    dplyr::distinct(key, driver) %>%
    dplyr::mutate(had_driver_before_target = TRUE)

  current_target_coverage <- tidyr::crossing(
    key = unique(c(historical_driver_years$key, ref_key$key)),
    driver = year_driver_specs$driver,
    target_year = target_years
  ) %>%
    dplyr::left_join(
      current_years %>%
        dplyr::transmute(key, driver, target_year = year, has_target_year = TRUE),
      by = c("key", "driver", "target_year")
    ) %>%
    dplyr::mutate(has_target_year = ifelse(is.na(has_target_year), FALSE, has_target_year))

  missing_target_years_tbl <- current_target_coverage %>%
    dplyr::left_join(historical_driver_presence, by = c("key", "driver")) %>%
    dplyr::mutate(had_driver_before_target = ifelse(is.na(had_driver_before_target), FALSE, had_driver_before_target)) %>%
    dplyr::filter(had_driver_before_target, !has_target_year) %>%
    dplyr::group_by(key) %>%
    dplyr::summarise(
      missing_target_years = paste(sort(unique(target_year)), collapse = ","),
      missing_target_drivers = paste(sort(unique(driver)), collapse = ";"),
      year_incomplete_for_targets = TRUE,
      .groups = "drop"
    )
} else {
  missing_target_years_tbl <- tibble::tibble(
    key = character(0),
    missing_target_years = character(0),
    missing_target_drivers = character(0),
    year_incomplete_for_targets = logical(0)
  )
}

prior_missingness_keys <- if (!is.na(prior_missingness_path) && file.exists(prior_missingness_path)) {
  read.csv(prior_missingness_path, stringsAsFactors = FALSE, check.names = FALSE) %>%
    dplyr::transmute(
      site_id = paste(
        normalize_lter_key(LTER),
        normalize_stream_key(Stream_Name),
        sep = "||"
      ),
      in_prior_missingness_subset = TRUE
    ) %>%
    dplyr::distinct(site_id, .keep_all = TRUE)
} else {
  tibble::tibble(site_id = character(0), in_prior_missingness_subset = logical(0))
}

# A small set of Amazon sites already have named shapefiles, but they still
# need to be rerun because the projection metadata is known to be wrong or
# missing.
fixed_crs_named <- tribble(
  ~LTER, ~Stream_Name, ~Discharge_File_Name, ~Shapefile_Name,
  "Amazon", "Rio Japura", "Rio Japura_Q", "Rio_Japura",
  "Amazon", "Rio Jurua", "Rio Jurua_Q", "Rio_Jurua",
  "Amazon", "Rio Jutai", "Rio Jutai_Q", "Rio_Jutai",
  "Amazon", "Amazon River at Santo Antonio do Ica", "Santo Antonio do Ica_Q", "Amazon_SantoAntonio",
  "Amazon", "Rio Ica", "Rio Ica_Q", "Rio_Ica",
  "Amazon", "Rio Negro", "Rio Negro_Q", "Rio_Negro",
  "Amazon", "Amazon River at Vargem Grande", "Vargem Grande_Q", "Amazon_VergemGrande",
  "Amazon", "Amazon River at Manacapuru", "Manacapuru_Q", "Amazon_Manacapuru",
  "Amazon", "Rio Madeira", "Rio Madeira_Q", "Rio_Madeira",
  "Amazon", "Rio Purus", "Rio Purus_Q", "Rio_Purus",
  "Amazon", "Amazon River at Itapeua", "Itapeua_Q", "Itapeua"
  ) %>%
  mutate(
    key = paste(
      normalize_lter_key(LTER),
      normalize_stream_key(Stream_Name),
      tolower(Discharge_File_Name),
      tolower(Shapefile_Name),
      sep = "||"
    )
  ) %>%
  mutate(
    Region = "amazon",
    rerun_group = "fixed_crs_named_shapefile",
    preferred_action = "assign_known_crs_eckert_iv_then_reproject_and_rerun",
    reason = "Named shapefile exists but Amazon-family .prj is missing/undefined; repo reprojection log documents Eckert IV.",
    priority = 1L,
    Watershed_Source = NA_character_,
    Force_HydroSHEDS = NA_character_,
    HydroSHEDS_Rule = NA_character_,
    has_dynamic_legacy = NA,
    has_dynamic_current = NA
  )

hydrosheds_candidates <- ref_key %>%
  mutate(
    Region = dplyr::case_when(
      LTER == "WesternAustralia" ~ "australia",
      LTER == "HYBAM" ~ "amazon",
      LTER %in% c("Tanguro", "Tanguro(Jankowski)") ~ "amazon",
      LTER == "USGS" ~ "north-america-usa",
      LTER == "Finnish Environmental Institute" ~ "scandinavia",
      TRUE ~ NA_character_
    ),
    rerun_group = "hydrosheds_candidate",
    preferred_action = "derive_or_recover_hydrosheds_polygon_then_rerun",
    reason = case_when(
      str_detect(coalesce(Shapefile_Source, ""), regex("hydroshed", ignore_case = TRUE)) &
        !is.na(drainSqKm) & drainSqKm >= large_basin_km2 ~
        "Reference table marks HydroSHEDs provenance and drainage area is large enough for a targeted HydroSHEDS rerun.",
      str_detect(coalesce(Shapefile_Source, ""), regex("hydroshed", ignore_case = TRUE)) ~
        "Reference table marks HydroSHEDs provenance but shapefile linkage is still blank.",
      TRUE ~
        "No shapefile name in reference table and drainage area is large enough for HydroSHEDS-based extraction."
    ),
    priority = 2L
  ) %>%
  left_join(legacy_key, by = "key") %>%
  left_join(combined_key, by = "key") %>%
  left_join(missing_target_years_tbl, by = "key") %>%
  left_join(prior_missingness_keys, by = "site_id") %>%
  mutate(
    has_dynamic_legacy = ifelse(is.na(has_dynamic_legacy), FALSE, has_dynamic_legacy),
    has_dynamic_current = ifelse(is.na(has_dynamic_current), FALSE, has_dynamic_current),
    missing_shapefile_name = is.na(Shapefile_Name),
    year_incomplete_for_targets = ifelse(is.na(year_incomplete_for_targets), FALSE, year_incomplete_for_targets),
    in_prior_missingness_subset = ifelse(is.na(in_prior_missingness_subset), FALSE, in_prior_missingness_subset),
    hydrosheds_provenance = stringr::str_detect(coalesce(Shapefile_Source, ""), regex("hydroshed", ignore_case = TRUE)),
    Watershed_Source = "hydrosheds",
    Force_HydroSHEDS = "TRUE",
    HydroSHEDS_Rule = "missing_shapefile_or_source"
  ) %>%
  filter(
    !is.na(drainSqKm),
    drainSqKm > large_basin_km2,
    (
      missing_shapefile_name |
      !has_dynamic_legacy |
      !has_dynamic_current |
      (
        length(target_years) > 0 &
        in_prior_missingness_subset
      )
    )
  ) %>%
  mutate(
    HydroSHEDS_Rule = dplyr::case_when(
      missing_shapefile_name & !has_dynamic_legacy ~ "missing_shapefile_and_no_legacy_data",
      missing_shapefile_name ~ "missing_shapefile_name",
      !has_dynamic_legacy ~ "named_or_known_site_no_legacy_dynamic_data",
      !has_dynamic_current ~ "named_or_known_site_no_current_dynamic_data",
      length(target_years) > 0 & in_prior_missingness_subset ~ "carry_forward_hydrosheds_update_years_site",
      TRUE ~ HydroSHEDS_Rule
    )
  )

legacy_gap_candidates <- ref_key %>%
  left_join(legacy_key, by = "key") %>%
  left_join(combined_key, by = "key") %>%
  mutate(
    has_dynamic_legacy = ifelse(is.na(has_dynamic_legacy), FALSE, has_dynamic_legacy),
    has_dynamic_current = ifelse(is.na(has_dynamic_current), FALSE, has_dynamic_current)
  ) %>%
  filter(
    !is.na(drainSqKm),
    drainSqKm >= large_basin_km2,
    !is.na(Shapefile_Name),
    !has_dynamic_legacy,
    !has_dynamic_current
  ) %>%
  mutate(
    Region = dplyr::case_when(
      LTER == "WesternAustralia" ~ "australia",
      LTER %in% c("Amazon", "HYBAM", "Tanguro", "Tanguro(Jankowski)") ~ "amazon",
      LTER == "Canada" ~ "north-america-canada",
      LTER == "MD" ~ "australia",
      LTER == "GRO" ~ "south-america",
      TRUE ~ NA_character_
    ),
    rerun_group = "hydrosheds_no_data_named_shapefile",
    preferred_action = "derive_or_recover_hydrosheds_polygon_then_rerun",
    reason = "Named shapefile exists, but no dynamic spatial signal was present in either the legacy March 2025 or current May 2026 combined outputs.",
    priority = 3L
  )

legacy_gap_candidates <- legacy_gap_candidates %>%
  mutate(
    Watershed_Source = "hydrosheds",
    Force_HydroSHEDS = "TRUE",
    HydroSHEDS_Rule = "named_shapefile_no_data_legacy_and_current"
  )

# One table keeps the full reasoning. The smaller CSVs are there to feed later
# rerun steps without extra columns getting in the way.
manifest <- bind_rows(fixed_crs_named, hydrosheds_candidates, legacy_gap_candidates) %>%
  mutate(
    has_dynamic_current = ifelse(is.na(has_dynamic_current), FALSE, has_dynamic_current),
    rerun_needed = TRUE
  ) %>%
  arrange(priority, rerun_group, LTER, Stream_Name) %>%
  distinct(key, .keep_all = TRUE) %>%
  arrange(priority, rerun_group, LTER, Stream_Name)

shared_polygons <- ref_key %>%
  filter(!is.na(Shapefile_Name)) %>%
  add_count(Shapefile_Name, name = "shared_rows") %>%
  filter(shared_rows > 1L) %>%
  arrange(Shapefile_Name, LTER, Stream_Name)

obidos_override <- tribble(
  ~LTER, ~Stream_Name, ~Discharge_File_Name, ~Shapefile_Name, ~shared_polygon, ~harmonization_role, ~canonical_site_id, ~note,
  "Amazon", "Amazon River at Obidos", "Obidos_Q", "Amazon_Obidos", "Amazon_Obidos", "shared_polygon_only", "GRO||GRO_Obidos_Q", "Polygon is shared, but this is not the canonical analytical Obidos row.",
  "GRO", "Obidos", "GRO_Obidos_Q", "Amazon_Obidos", "Amazon_Obidos", "canonical_site", "GRO||GRO_Obidos_Q", "Use this row as the canonical Obidos site in harmonization and release outputs.",
  "HYBAM", "Obidos", "Obidos_Q", NA_character_, "Amazon_Obidos", "drop_or_alias", "GRO||GRO_Obidos_Q", "Do not treat HYBAM Obidos as the canonical Obidos site."
)

date_tag <- format(Sys.Date(), "%Y%m%d")
manifest_path <- file.path(rerun_dir, paste0("targeted_rerun_manifest_", date_tag, ".csv"))
subset_path <- file.path(rerun_dir, paste0("targeted_rerun_subset_", date_tag, ".csv"))
hydrosheds_subset_path <- file.path(rerun_dir, paste0("targeted_rerun_subset_hydrosheds_fallback_", date_tag, ".csv"))
hydrosheds_approval_path <- file.path(rerun_dir, paste0("targeted_rerun_hydrosheds_candidates_for_approval_", date_tag, ".csv"))
hydrosheds_approval_subset_path <- file.path(rerun_dir, paste0("targeted_rerun_subset_hydrosheds_for_approval_", date_tag, ".csv"))
shared_path <- file.path(harmonization_dir, paste0("shared_polygon_sites_", date_tag, ".csv"))
obidos_path <- file.path(harmonization_dir, paste0("obidos_harmonization_override_", date_tag, ".csv"))

hydrosheds_approval <- manifest %>%
  dplyr::filter(grepl("^hydrosheds", rerun_group)) %>%
  mutate(
    run_type = dplyr::case_when(
      length(target_years) > 0 & in_prior_missingness_subset & has_dynamic_legacy ~ "update_years",
      TRUE ~ "full_record"
    ),
    approval_reason = dplyr::case_when(
      length(target_years) > 0 & in_prior_missingness_subset ~ paste0(
        "Previously approved large-basin HydroSHEDS rerun site. Carry forward for target years: ",
        paste(target_years, collapse = ",")
      ),
      year_incomplete_for_targets ~ paste0(
        "HydroSHEDS-derived or previously approved rerun site is missing target years: ",
        missing_target_years,
        ifelse(
          is.na(missing_target_drivers) | missing_target_drivers == "",
          "",
          paste0(" (drivers: ", missing_target_drivers, ")")
        )
      ),
      is.na(Shapefile_Name) & !has_dynamic_legacy ~
        "No shapefile name and no dynamic spatial data in the previous run.",
      is.na(Shapefile_Name) ~
        "No shapefile name is listed for this site.",
      !has_dynamic_legacy ~
        "Previous run had no dynamic spatial data for this site.",
      TRUE ~
        "HydroSHEDS follow-up candidate."
    )
  ) %>%
  dplyr::distinct(
    LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
    .keep_all = TRUE
  ) %>%
  dplyr::arrange(desc(drainSqKm), LTER, Stream_Name)

write.csv(manifest, manifest_path, row.names = FALSE, na = "")
write.csv(
  manifest %>%
    dplyr::select(
      LTER, Stream_Name, Shapefile_Name, Region,
      Watershed_Source, Force_HydroSHEDS, HydroSHEDS_Rule, rerun_group,
      has_dynamic_legacy, has_dynamic_current
    ) %>%
    dplyr::distinct(),
  subset_path,
  row.names = FALSE,
  na = ""
)
write.csv(
  hydrosheds_approval %>%
    dplyr::select(
      LTER, Stream_Name, Shapefile_Name, Region,
      Watershed_Source, Force_HydroSHEDS, HydroSHEDS_Rule, rerun_group,
      run_type, has_dynamic_legacy, has_dynamic_current, drainSqKm,
      year_incomplete_for_targets, missing_target_years, missing_target_drivers
    ) %>%
    dplyr::distinct(),
  hydrosheds_subset_path,
  row.names = FALSE,
  na = ""
)
write.csv(
  hydrosheds_approval %>%
    dplyr::select(
      LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, Region,
      run_type, drainSqKm, has_dynamic_legacy, has_dynamic_current, rerun_group,
      HydroSHEDS_Rule, year_incomplete_for_targets, missing_target_years,
      missing_target_drivers, approval_reason, reason, preferred_action
    ) %>%
    dplyr::distinct(),
  hydrosheds_approval_path,
  row.names = FALSE,
  na = ""
)
write.csv(
  hydrosheds_approval %>%
    dplyr::select(
      LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
      Watershed_Source, Force_HydroSHEDS, run_type
    ) %>%
    dplyr::distinct(),
  hydrosheds_approval_subset_path,
  row.names = FALSE,
  na = ""
)
write.csv(shared_polygons, shared_path, row.names = FALSE, na = "")
write.csv(obidos_override, obidos_path, row.names = FALSE, na = "")

message("Wrote manifest: ", manifest_path)
message("Wrote rerun subset: ", subset_path)
message("Wrote HydroSHEDS review candidates: ", hydrosheds_approval_path)
message("Wrote HydroSHEDS ready subset: ", hydrosheds_approval_subset_path)
message("Wrote shared-polygon review: ", shared_path)
message("Wrote Obidos override table: ", obidos_path)
message(
  "Summary: ",
  nrow(manifest), " manifest rows, ",
  nrow(hydrosheds_approval), " HydroSHEDS approval rows, ",
  dplyr::n_distinct(shared_polygons$Shapefile_Name), " shared-polygon names."
)
