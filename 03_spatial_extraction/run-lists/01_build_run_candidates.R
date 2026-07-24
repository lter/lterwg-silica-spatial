librarian::shelf(dplyr, readxl)

args <- commandArgs(trailingOnly = TRUE)
source(file.path("03_spatial_extraction", "run-lists", "00_run_list_paths.R"))

get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit)) {
    return(default)
  }
  if (hit[1] == length(args)) {
    stop("Missing value for ", flag, call. = FALSE)
  }
  args[hit[1] + 1]
}

norm_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

read_reference_table <- function(path) {
  ext <- tolower(tools::file_ext(path))
  out <- if (ext %in% c("xls", "xlsx")) {
    readxl::read_excel(path)
  } else {
    read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
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

build_site_key <- function(df) {
  df %>%
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
      site_id = paste(
        LTER,
        normalize_stream_key(Stream_Name),
        sep = "||"
      )
    )
}

today_tag <- format(Sys.Date(), "%Y%m%d")
review_root <- get_arg("--outdir", silica_review_root(resolve_silica_data_root()))
target_years <- silica_target_years()
default_review_root <- silica_review_root(resolve_silica_data_root())
default_root <- silica_default_box_data_root()
override_path <- get_arg(
  "--overrides",
  file.path(
    "03_spatial_extraction",
    "config",
    "run_candidate_overrides.tsv"
  )
)
if (!file.exists(override_path)) {
  stop("Run-candidate override table does not exist: ", override_path, call. = FALSE)
}
override_rules <- read.delim(
  override_path,
  sep = "\t",
  quote = "",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
required_override_columns <- c(
  "LTER", "Stream_Name", "Action", "Rule_Status", "Reason"
)
missing_override_columns <- setdiff(
  required_override_columns,
  names(override_rules)
)
if (length(missing_override_columns)) {
  stop(
    "Override table is missing columns: ",
    paste(missing_override_columns, collapse = ", "),
    call. = FALSE
  )
}

ensure_run_list_dirs(review_root)

message("Step 1 of 4: build strict HydroSHEDS candidates")
status_run_list <- system2(
  "Rscript",
  c("tools/build_targeted_rerun_list.R", "--outdir", run_list_root(review_root))
)

if (!identical(status_run_list, 0L)) {
  stop("Run-list review stopped while building the strict HydroSHEDS candidate list.", call. = FALSE)
}

strict_candidates_path <- latest_run_list_file(
  file.path(run_list_root(review_root), "rerun"),
  "targeted_rerun_hydrosheds_candidates_for_approval_*.csv"
)

if (is.na(strict_candidates_path) || !file.exists(strict_candidates_path)) {
  stop("Could not find the strict HydroSHEDS candidate file after building the rerun list.", call. = FALSE)
}

ref_path <- resolve_workflow_input(
  cli_path = get_arg("--ref"),
  env_var = "SILICA_TARGETED_REF_PATH",
  candidates = c(
    file.path(default_root, "master-datasets", "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv"),
    file.path(default_root, "master-datasets", "Site_Reference_Table - WRTDS_Reference_Table_LTER_V2.csv")
  ),
  label = "reference table",
  cli_flag = "--ref"
)

current_combined_candidates <- c(
  latest_existing_paths(file.path(default_root, "all-data_si-extract_3_*.csv"))
)
current_combined_candidates <- current_combined_candidates[
  !grepl("rerun|combinedlocal|hydrosheds-full-record-recoverable", basename(current_combined_candidates), ignore.case = TRUE)
]

current_path <- resolve_workflow_input(
  cli_path = get_arg("--combined"),
  env_var = "SILICA_TARGETED_COMBINED_PATH",
  candidates = current_combined_candidates,
  label = "current combined table",
  cli_flag = "--combined"
)
legacy_path <- resolve_workflow_input(
  cli_path = get_arg("--legacy"),
  env_var = "SILICA_TARGETED_LEGACY_PATH",
  label = "legacy comparison table",
  cli_flag = "--legacy"
)

ref <- read_reference_table(ref_path)
legacy <- read.csv(legacy_path, check.names = FALSE, stringsAsFactors = FALSE)
current <- read.csv(current_path, check.names = FALSE, stringsAsFactors = FALSE)
ref$Spatial_Data_Version <- if (!"Spatial_Data_Version" %in% names(ref)) {
  NA_character_
} else {
  ref[["Spatial_Data_Version"]]
}

ref_cols <- c(
  "LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name",
  "drainSqKm", "Use_WRTDS", "Spatial_Data_Version", "Shapefile_Source",
  "Notes__dup1"
)

ref_key <- build_site_key(ref) %>%
  select(any_of(ref_cols), key, site_id) %>%
  distinct()

legacy_key <- build_site_key(legacy)
legacy_key$has_dynamic_legacy <- has_dynamic_signal(legacy_key)
legacy_key <- legacy_key %>%
  distinct(key, .keep_all = TRUE) %>%
  select(key, has_dynamic_legacy)

current_key <- build_site_key(current)
current_key$has_dynamic_current <- has_dynamic_signal(current_key)
current_key <- current_key %>%
  distinct(key, .keep_all = TRUE) %>%
  select(key, has_dynamic_current)

strict_full <- read.csv(strict_candidates_path, check.names = FALSE, stringsAsFactors = FALSE) %>%
  filter(
    !is.na(has_dynamic_legacy),
    !is.na(has_dynamic_current),
    !has_dynamic_legacy,
    !has_dynamic_current
  )

missingness_candidates <- unique(c(
  Sys.glob(file.path(review_root, "rerun", "targeted_rerun_subset_from_missingness_*.csv")),
  Sys.glob(file.path(review_root, "qa", "targeted_rerun_subset_from_missingness_*.csv")),
  Sys.glob(file.path(default_review_root, "rerun", "targeted_rerun_subset_from_missingness_*.csv")),
  Sys.glob(file.path(default_review_root, "qa", "targeted_rerun_subset_from_missingness_*.csv"))
))
if (!length(missingness_candidates)) {
  stop("Could not find a targeted_rerun_subset_from_missingness CSV.", call. = FALSE)
}
missingness_path <- missingness_candidates[which.max(file.info(missingness_candidates)$mtime)]
missingness <- read.csv(missingness_path, check.names = FALSE, stringsAsFactors = FALSE)
update_networks <- override_rules %>%
  filter(Action == "include_update_years", Stream_Name == "*") %>%
  pull(LTER)

update_sites <- merge(
  missingness,
  ref[, ref_cols],
  by = c("LTER", "Stream_Name", "Shapefile_Name"),
  all.x = TRUE
) %>%
  filter(
    LTER %in% update_networks,
    !is.na(drainSqKm),
    drainSqKm > 100
  ) %>%
  mutate(
    candidate_group = "hydrosheds_update_years",
    hydrosheds_applicability = "required",
    Watershed_Source = "hydrosheds",
    Force_HydroSHEDS = "TRUE",
    run_type = "update_years",
    target_years = if (length(target_years)) paste(target_years, collapse = ",") else "",
    recommend_run_now = "yes",
    recommendation_basis = "Previously rerun HydroSHEDS site carried forward from the missingness review list.",
    approval_reason = "Carry forward previously rerun HydroSHEDS site for newer target years."
  ) %>%
  select(
    LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
    candidate_group, hydrosheds_applicability, Watershed_Source, Force_HydroSHEDS,
    run_type, target_years, drainSqKm, Use_WRTDS, Spatial_Data_Version,
    Shapefile_Source,
    recommend_run_now, recommendation_basis, approval_reason
  ) %>%
  mutate(
    across(
      c(
        LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
        candidate_group, hydrosheds_applicability, Watershed_Source, Force_HydroSHEDS,
        run_type, target_years, Use_WRTDS, Spatial_Data_Version,
        Shapefile_Source,
        recommend_run_now, recommendation_basis, approval_reason
      ),
      as.character
    )
  )

full_sites <- merge(
  strict_full,
  ref[, ref_cols],
  by = c("LTER", "Stream_Name", "Discharge_File_Name"),
  all.x = TRUE,
  suffixes = c("", "_ref")
) %>%
  mutate(
    Shapefile_Name = ifelse(is.na(Shapefile_Name), Shapefile_Name_ref, Shapefile_Name),
    drainSqKm = ifelse(is.na(drainSqKm), drainSqKm_ref, drainSqKm),
    candidate_group = "hydrosheds_full_record",
    hydrosheds_applicability = "required",
    Watershed_Source = "hydrosheds",
    Force_HydroSHEDS = "TRUE",
    run_type = "full_record",
    target_years = silica_full_record_label(),
    recommend_run_now = "yes",
    recommendation_basis = "Large-basin site with no successful legacy dynamic spatial data.",
    approval_reason = approval_reason
  ) %>%
  select(
    LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
    candidate_group, hydrosheds_applicability, Watershed_Source, Force_HydroSHEDS,
    run_type, target_years, drainSqKm, Use_WRTDS, Spatial_Data_Version,
    Shapefile_Source,
    recommend_run_now, recommendation_basis, approval_reason
  ) %>%
  mutate(
    across(
      c(
        LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
        candidate_group, hydrosheds_applicability, Watershed_Source, Force_HydroSHEDS,
        run_type, target_years, Use_WRTDS, Spatial_Data_Version,
        Shapefile_Source,
        recommend_run_now, recommendation_basis, approval_reason
      ),
      as.character
    )
  )

hydrosheds_mixed <- bind_rows(full_sites, update_sites) %>%
  mutate(
    LTER = as.character(LTER),
    Stream_Name = as.character(Stream_Name),
    Discharge_File_Name = as.character(Discharge_File_Name),
    Shapefile_Name = as.character(Shapefile_Name),
    recommend_run_now = as.character(recommend_run_now),
    recommendation_basis = as.character(recommendation_basis),
    approval_reason = as.character(approval_reason),
    hydrosheds_applicability = as.character(hydrosheds_applicability),
    Watershed_Source = as.character(Watershed_Source),
    Force_HydroSHEDS = as.character(Force_HydroSHEDS),
    candidate_group = as.character(candidate_group),
    run_type = as.character(run_type),
    target_years = as.character(target_years),
    Use_WRTDS = as.character(Use_WRTDS),
    Spatial_Data_Version = as.character(Spatial_Data_Version),
    Shapefile_Source = as.character(Shapefile_Source)
  ) %>%
  distinct()

# Documented exceptions belong in the editable override table, not this code.
force_rules <- override_rules %>%
  filter(Action == "force_hydrosheds", Stream_Name != "*")
forced_hydrosheds_full_record <- ref %>%
  inner_join(
    force_rules[, c("LTER", "Stream_Name", "Rule_Status", "Reason")],
    by = c("LTER", "Stream_Name")
  ) %>%
  mutate(
    candidate_group = "hydrosheds_full_record_corrupt_named_shapefile",
    hydrosheds_applicability = "required",
    Watershed_Source = "hydrosheds",
    Force_HydroSHEDS = "TRUE",
    run_type = "full_record",
    target_years = silica_full_record_label(),
    recommend_run_now = "yes",
    recommendation_basis = Reason,
    approval_reason = Reason
  ) %>%
  select(
    LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
    candidate_group, hydrosheds_applicability, Watershed_Source, Force_HydroSHEDS,
    run_type, target_years, drainSqKm, Use_WRTDS, Spatial_Data_Version,
    Shapefile_Source,
    recommend_run_now, recommendation_basis, approval_reason
  ) %>%
  mutate(
    across(
      c(
        LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
        candidate_group, hydrosheds_applicability, Watershed_Source, Force_HydroSHEDS,
        run_type, target_years, Use_WRTDS, Spatial_Data_Version,
        Shapefile_Source,
        recommend_run_now, recommendation_basis, approval_reason
      ),
      as.character
    )
  )

hydrosheds_mixed <- bind_rows(
  hydrosheds_mixed,
  forced_hydrosheds_full_record
) %>%
  distinct(
    LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
    .keep_all = TRUE
  )

provided_shapefile_candidates <- ref_key %>%
  left_join(legacy_key, by = "key") %>%
  left_join(current_key, by = "key") %>%
  mutate(
    has_dynamic_legacy = ifelse(is.na(has_dynamic_legacy), FALSE, has_dynamic_legacy),
    has_dynamic_current = ifelse(is.na(has_dynamic_current), FALSE, has_dynamic_current),
    shapefile_source_lower = tolower(coalesce(Shapefile_Source, "")),
    use_wrtds_lower = tolower(coalesce(Use_WRTDS, ""))
  ) %>%
  filter(
    !is.na(Shapefile_Name),
    Shapefile_Name != "",
    !grepl("hydroshed", shapefile_source_lower),
    !has_dynamic_legacy,
    !has_dynamic_current
  ) %>%
  anti_join(
    hydrosheds_mixed %>%
      transmute(
        LTER = LTER,
        Stream_Name = Stream_Name,
        Discharge_File_Name = Discharge_File_Name,
        Shapefile_Name = Shapefile_Name
      ) %>%
      distinct(),
    by = c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")
  ) %>%
  mutate(
    candidate_group = "provided_shapefile_full_record",
    hydrosheds_applicability = "not_needed_named_shapefile_present",
    Watershed_Source = "artisanal",
    Force_HydroSHEDS = "FALSE",
    run_type = "full_record",
    target_years = silica_full_record_label(),
    recommend_run_now = ifelse(use_wrtds_lower %in% c("yes", "y", "true"), "yes", "review"),
    recommendation_basis = case_when(
      recommend_run_now == "yes" ~ "Named shapefile exists and no dynamic spatial data were found in either combined baseline.",
      TRUE ~ "Named shapefile exists and no dynamic spatial data were found, but the reference table does not clearly indicate this site should be prioritized now."
    ),
    approval_reason = "New or previously unsuccessful shapefile-backed site should run as a full-record non-HydroSHEDS extraction."
  ) %>%
  select(
    LTER, Stream_Name, Discharge_File_Name, Shapefile_Name,
    candidate_group, hydrosheds_applicability, Watershed_Source, Force_HydroSHEDS,
    run_type, target_years, drainSqKm, Use_WRTDS, Spatial_Data_Version,
    Shapefile_Source,
    recommend_run_now, recommendation_basis, approval_reason
  ) %>%
  mutate(
    LTER = as.character(LTER),
    Stream_Name = as.character(Stream_Name),
    Discharge_File_Name = as.character(Discharge_File_Name),
    Shapefile_Name = as.character(Shapefile_Name),
    recommend_run_now = as.character(recommend_run_now),
    recommendation_basis = as.character(recommendation_basis),
    approval_reason = as.character(approval_reason),
    hydrosheds_applicability = as.character(hydrosheds_applicability),
    Watershed_Source = as.character(Watershed_Source),
    Force_HydroSHEDS = as.character(Force_HydroSHEDS),
    candidate_group = as.character(candidate_group),
    run_type = as.character(run_type),
    target_years = as.character(target_years),
    Use_WRTDS = as.character(Use_WRTDS),
    Spatial_Data_Version = as.character(Spatial_Data_Version),
    Shapefile_Source = as.character(Shapefile_Source)
  ) %>%
  distinct()

approval_mixed <- bind_rows(hydrosheds_mixed, provided_shapefile_candidates) %>%
  arrange(run_type, desc(Watershed_Source == "hydrosheds"), LTER, Stream_Name) %>%
  distinct()

holdout_rules <- override_rules %>%
  filter(Action == "holdout", Stream_Name != "*")
holdouts <- ref[, ref_cols] %>%
  inner_join(
    holdout_rules[, c("LTER", "Stream_Name", "Rule_Status", "Reason")],
    by = c("LTER", "Stream_Name")
  ) %>%
  mutate(
    candidate_group = "manual_review_holdout",
    hydrosheds_applicability = "not_auto_flagged",
    rule_status = Rule_Status,
    recommendation = Reason
  )

approval_path <- file.path(
  run_list_approvals_dir(review_root),
  paste0("run_list_approval_mixed_", today_tag, ".csv")
)
holdout_path <- file.path(
  run_list_approvals_dir(review_root),
  paste0("run_list_holdouts_review_", today_tag, ".csv")
)
strict_copy_path <- file.path(
  run_list_candidates_dir(review_root),
  paste0("strict_hydrosheds_full_record_candidates_", today_tag, ".csv")
)
provided_copy_path <- file.path(
  run_list_candidates_dir(review_root),
  paste0("provided_shapefile_full_record_candidates_", today_tag, ".csv")
)
summary_path <- file.path(
  run_list_reports_dir(review_root),
  paste0("run_list_review_summary_", today_tag, ".csv")
)

write.csv(approval_mixed, approval_path, row.names = FALSE, na = "")
write.csv(holdouts, holdout_path, row.names = FALSE, na = "")
write.csv(full_sites, strict_copy_path, row.names = FALSE, na = "")
write.csv(provided_shapefile_candidates, provided_copy_path, row.names = FALSE, na = "")
write.csv(
  bind_rows(
    approval_mixed %>% count(candidate_group, run_type, Watershed_Source, name = "n_sites"),
    holdouts %>% count(candidate_group, name = "n_sites") %>% mutate(run_type = NA_character_, Watershed_Source = NA_character_)
  ),
  summary_path,
  row.names = FALSE
)

message("Step 2 of 4: decide which sites need HydroSHEDS")
message("  See hydrosheds_applicability and Watershed_Source in: ", approval_path)
message("Step 3 of 4: wrote the mixed run-list approval file")
message("  ", approval_path)
message("Step 4 of 4: wrote supporting review files")
message("  ", holdout_path)
message("  ", strict_copy_path)
message("  ", provided_copy_path)
message("  ", summary_path)
message("Review the mixed approval CSV next, then run:")
message("  Rscript 03_spatial_extraction/run-lists/02_split_approved_runs.R")
