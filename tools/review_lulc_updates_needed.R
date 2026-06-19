librarian::shelf(dplyr)

source(file.path(getwd(), "tools", "workflow_paths.R"))
source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))
source(file.path(getwd(), "04_combine_qaqc", "00_qaqc_functions.R"))

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

latest_matching_path <- function(pattern) {
  hits <- Sys.glob(pattern)
  if (!length(hits)) {
    return(NA_character_)
  }
  hits[[which.max(file.info(hits)$mtime)]]
}

data_root <- env_or_default(
  "SILICA_QAQC_DATA_ROOT",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions"
)

combined_file <- env_or_default(
  "SILICA_HARMONIZATION_COMBINED_FILE",
  first_existing_path(
    c(
      latest_matching_path(file.path(data_root, "si-extracted-data", "all_data_extractions", "all-data_si-extract_4_*.csv")),
      latest_matching_path(file.path(data_root, "si-extracted-data", "all_data_extractions", "all-data_si-extract_3_*.csv"))
    ),
    "combined spatial file"
  )
)

legacy_file <- env_or_default(
  "SILICA_HARMONIZATION_LEGACY_FILE",
  first_existing_path(
    c(
      file.path(data_root, "si-extracted-data", "all_data_extractions", "all-data_si-extract_2_20250325.csv")
    ),
    "legacy combined file"
  )
)

lulc_file <- env_or_default(
  "SILICA_HARMONIZATION_LULC_FILE",
  first_existing_path(
    c(
      file.path(data_root, "spatial_data_harmonization", "master_datasets", "DSi_LULC_filled_interpolated_Simple.csv")
    ),
    "LULC harmonization file"
  )
)

review_dir <- file.path(data_root, "review", "harmonization")
date_tag <- format(Sys.Date(), "%Y%m%d")
dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)

site_cols <- c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")

read_site_table <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  df <- prepare_combined_table(df, sanitize_new = FALSE)
  df[, intersect(site_cols, names(df)), drop = FALSE] %>%
    distinct() %>%
    mutate(
      key = build_key(.),
      Stream_Name_norm = normalize_stream_key(Stream_Name)
    )
}

lulc_raw <- read.csv(lulc_file, stringsAsFactors = FALSE, check.names = FALSE)
bad_names <- is.na(names(lulc_raw)) | names(lulc_raw) == ""
if (any(bad_names)) {
  names(lulc_raw)[bad_names] <- paste0("unnamed_col_", seq_len(sum(bad_names)))
}

combined_sites <- read_site_table(combined_file)
legacy_sites <- read_site_table(legacy_file) %>%
  select(key) %>%
  mutate(in_legacy = TRUE)

lulc <- lulc_raw %>%
  transmute(
    Stream_Name = as.character(Stream_Name),
    Stream_Name_norm = normalize_stream_key(Stream_Name)
  ) %>%
  filter(!is.na(Stream_Name_norm), Stream_Name_norm != "") %>%
  distinct(Stream_Name_norm, .keep_all = TRUE)

review <- combined_sites %>%
  left_join(legacy_sites, by = "key") %>%
  mutate(is_new_vs_extract2 = is.na(in_legacy)) %>%
  select(-in_legacy) %>%
  left_join(
    lulc %>% rename(LULC_Stream_Name = Stream_Name),
    by = "Stream_Name_norm"
  ) %>%
  mutate(has_lulc = !is.na(LULC_Stream_Name))

missing_all <- review %>%
  filter(!has_lulc) %>%
  arrange(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name)

missing_new <- missing_all %>%
  filter(is_new_vs_extract2)

summary_df <- bind_rows(
  missing_all %>%
    count(LTER, name = "n_sites_missing_lulc") %>%
    mutate(scope = "all_combined_sites_missing_lulc"),
  missing_new %>%
    count(LTER, name = "n_sites_missing_lulc") %>%
    mutate(scope = "new_vs_extract2_sites_missing_lulc")
) %>%
  select(scope, LTER, n_sites_missing_lulc) %>%
  arrange(scope, desc(n_sites_missing_lulc), LTER)

write.csv(
  missing_all,
  file.path(review_dir, paste0("sites_missing_lulc_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)

write.csv(
  missing_new,
  file.path(review_dir, paste0("new_sites_missing_lulc_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)

write.csv(
  summary_df,
  file.path(review_dir, paste0("sites_missing_lulc_summary_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)
