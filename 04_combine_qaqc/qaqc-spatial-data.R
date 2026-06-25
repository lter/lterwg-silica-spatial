# Check the combined spatial table after 04_combine_qaqc/combine-spatial-data.R
librarian::shelf(dplyr, stringr, quiet = TRUE)

source(file.path(getwd(), "tools", "workflow_paths.R"))

keys <- c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")

driver_family_patterns <- c(
  evapo = "^evapotrans_",
  greenup = "^greenup_",
  precip = "^precip_",
  airtemp = "^temp_",
  snow = "^snow_",
  npp = "^npp_",
  elev = "^elevation_|^basin_slope_",
  lith = "^major_rock$|^rocks_",
  permafrost = "^permafrost_",
  soil = "^major_soil$|^soil_"
)

date_tag <- Sys.getenv("SILICA_OUTPUT_DATE", unset = format(Sys.Date(), "%Y%m%d"))

norm_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

norm_lter <- function(x) {
  x <- clean_lter_label(norm_chr(x))
  dplyr::recode(
    x,
    "Swedish Goverment" = "Sweden",
    "Swedish Government" = "Sweden",
    "Carey" = "PIE",
    "Cameroon" = "Congo Basin",
    "Cameroon Site" = "Congo Basin",
    "Cameroon Sites" = "Congo Basin",
    .default = x
  )
}

join_chr <- function(x) {
  tolower(norm_chr(x))
}

join_lter <- function(x) {
  x <- join_chr(clean_lter_label(x))
  dplyr::recode(
    x,
    "swedish goverment" = "sweden",
    "swedish government" = "sweden",
    "carey" = "pie",
    "cameroon" = "congo basin",
    "cameroon site" = "congo basin",
    "cameroon sites" = "congo basin",
    .default = x
  )
}

site_key <- function(df) {
  paste(
    join_lter(df$LTER),
    join_chr(df$Stream_Name),
    join_chr(df$Discharge_File_Name),
    join_chr(df$Shapefile_Name),
    sep = "||"
  )
}

region_from_lter <- function(x) {
  x <- join_lter(x)
  dplyr::case_when(
    x %in% c("amazon", "hybam", "gro") ~ "amazon",
    x == "westernaustralia" ~ "australia",
    x == "seine" ~ "europe-france",
    x %in% c("congo basin", "congo_basin") ~ "africa-central",
    TRUE ~ NA_character_
  )
}

row_has_any_values <- function(df, cols) {
  if (!length(cols)) {
    return(rep(FALSE, nrow(df)))
  }

  vals <- as.data.frame(lapply(df[, cols, drop = FALSE], function(x) {
    !is.na(x) & trimws(as.character(x)) != ""
  }))
  rowSums(vals) > 0
}

max_year_from_names <- function(nms, rx) {
  years <- as.integer(stringr::str_match(nms, rx)[, 2])
  years <- years[!is.na(years)]
  if (!length(years)) {
    return(NA_integer_)
  }
  max(years)
}

latest_file <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    return("")
  }
  paths[which.max(file.info(paths)$mtime)]
}

find_combined_file <- function(data_root, extracted_dir) {
  env_file <- latest_file(c(
    Sys.getenv("SILICA_QAQC_FILE", unset = ""),
    Sys.getenv("SILICA_QAQC_NEW_FILE", unset = ""),
    Sys.getenv("SILICA_HARMONIZATION_COMBINED_FILE", unset = ""),
    Sys.getenv("SILICA_OUTPUT_FILE", unset = "")
  ))
  if (nzchar(env_file)) {
    return(env_file)
  }

  search_dirs <- c(
    extracted_dir,
    file.path(extracted_dir, "all_data_extractions"),
    file.path(data_root, "extracted-data", "all_data_extractions"),
    file.path(data_root, "si-extracted-data", "all_data_extractions")
  )
  search_dirs <- unique(search_dirs[dir.exists(search_dirs)])

  candidates <- unlist(lapply(search_dirs, function(dir) {
    list.files(
      dir,
      pattern = "^all-data_si-extract_3_.*\\.csv$",
      full.names = TRUE
    )
  }))

  latest_file(candidates)
}

find_previous_combined_file <- function(extracted_dir, current_file) {
  explicit_old <- latest_file(c(Sys.getenv("SILICA_OLD_COMBINED", unset = "")))
  if (nzchar(explicit_old)) {
    return(explicit_old)
  }

  candidates <- list.files(
    extracted_dir,
    pattern = "^all-data_si-extract_.*\\.csv$",
    full.names = TRUE
  )
  candidates <- candidates[file.exists(candidates)]
  candidates <- setdiff(
    normalizePath(candidates, mustWork = FALSE),
    normalizePath(current_file, mustWork = FALSE)
  )
  latest_file(candidates)
}

read_combined_table <- function(path) {
  df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  for (key in keys) {
    if (!key %in% names(df)) {
      df[[key]] <- NA_character_
    }
  }
  df$LTER <- norm_lter(df$LTER)
  df$Stream_Name <- norm_chr(df$Stream_Name)
  df$Discharge_File_Name <- norm_chr(df$Discharge_File_Name)
  df$Shapefile_Name <- norm_chr(df$Shapefile_Name)
  df$site_key <- site_key(df)
  df$Stream_ID <- paste(norm_lter(df$LTER), normalize_stream_key(df$Stream_Name), sep = "__")
  df
}

data_root <- resolve_silica_data_root()
extracted_dir <- silica_extracted_data_dir(data_root)
qa_root <- Sys.getenv("SILICA_QA_ROOT", unset = silica_review_root(data_root))
out_dir <- file.path(qa_root, "qa")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

combined_file <- find_combined_file(data_root, extracted_dir)
if (!nzchar(combined_file)) {
  stop(
    "Could not find a combined spatial file to check. Run combine-spatial-data.R or set SILICA_QAQC_FILE.",
    call. = FALSE
  )
}

combined <- read_combined_table(combined_file)

duplicate_keys <- combined %>%
  count(site_key, name = "n_site_key_rows") %>%
  filter(n_site_key_rows > 1)

site_flags <- combined %>%
  select(all_of(keys), Stream_ID, site_key) %>%
  mutate(
    region = region_from_lter(LTER),
    missing_key_field = if_any(all_of(keys), ~ is.na(.x) | .x == ""),
    duplicate_site_key = site_key %in% duplicate_keys$site_key
  )

for (family in names(driver_family_patterns)) {
  family_cols <- grep(driver_family_patterns[[family]], names(combined), value = TRUE)
  site_flags[[paste0("has_", family)]] <- row_has_any_values(combined, family_cols)
  site_flags[[paste0("missing_", family)]] <- !site_flags[[paste0("has_", family)]]
}

missing_cols <- paste0("missing_", names(driver_family_patterns))
site_flags$n_missing_driver_families <- rowSums(site_flags[, missing_cols, drop = FALSE])
site_flags$missing_driver_families <- apply(
  site_flags[, missing_cols, drop = FALSE],
  1,
  function(x) paste(names(driver_family_patterns)[as.logical(x)], collapse = ";")
)
site_flags$all_spatial_drivers_missing <- site_flags$n_missing_driver_families == length(driver_family_patterns)
site_flags$needs_followup <- site_flags$missing_key_field |
  site_flags$duplicate_site_key |
  site_flags$n_missing_driver_families > 0
site_flags$followup_reason <- dplyr::case_when(
  site_flags$duplicate_site_key ~ "duplicate_site_key",
  site_flags$missing_key_field ~ "missing_site_key_field",
  site_flags$all_spatial_drivers_missing ~ "all_spatial_drivers_missing",
  site_flags$n_missing_driver_families > 0 ~ "missing_some_driver_families",
  TRUE ~ "ok"
)
site_flags$priority <- dplyr::case_when(
  site_flags$duplicate_site_key | site_flags$all_spatial_drivers_missing ~ 1L,
  site_flags$missing_key_field ~ 2L,
  site_flags$n_missing_driver_families > 0 ~ 3L,
  TRUE ~ 9L
)

driver_summary <- bind_rows(lapply(names(driver_family_patterns), function(family) {
  family_cols <- grep(driver_family_patterns[[family]], names(combined), value = TRUE)
  data.frame(
    check = paste0("driver_family_", family),
    value = sum(row_has_any_values(combined, family_cols)),
    note = paste("sites with at least one", family, "value"),
    stringsAsFactors = FALSE
  )
}))

year_summary <- bind_rows(
  data.frame(
    check = "max_evapo_year",
    value = max_year_from_names(names(combined), "^evapotrans_([0-9]{4})_kg_m2$"),
    note = "latest evapotrans annual column",
    stringsAsFactors = FALSE
  ),
  data.frame(
    check = "max_greenup_year",
    value = max_year_from_names(names(combined), "^greenup_cycle[01]_([0-9]{4})MMDD$"),
    note = "latest greenup annual column",
    stringsAsFactors = FALSE
  ),
  data.frame(
    check = "max_precip_year",
    value = max_year_from_names(names(combined), "^precip_([0-9]{4})_mm_per_day$"),
    note = "latest precip annual column",
    stringsAsFactors = FALSE
  ),
  data.frame(
    check = "max_airtemp_year",
    value = max_year_from_names(names(combined), "^temp_([0-9]{4})_degC$"),
    note = "latest air temperature annual column",
    stringsAsFactors = FALSE
  ),
  data.frame(
    check = "max_snow_year",
    value = max_year_from_names(names(combined), "^snow_([0-9]{4})_num_days$"),
    note = "latest snow annual column",
    stringsAsFactors = FALSE
  ),
  data.frame(
    check = "max_npp_year",
    value = max_year_from_names(names(combined), "^npp_([0-9]{4})_kgC_m2_year$"),
    note = "latest npp annual column",
    stringsAsFactors = FALSE
  )
)

summary <- bind_rows(
  data.frame(check = "combined_file", value = NA_real_, note = combined_file, stringsAsFactors = FALSE),
  data.frame(check = "row_count", value = nrow(combined), note = "rows in checked file", stringsAsFactors = FALSE),
  data.frame(check = "site_key_count", value = dplyr::n_distinct(combined$site_key), note = "unique site keys", stringsAsFactors = FALSE),
  data.frame(check = "duplicate_site_key_groups", value = nrow(duplicate_keys), note = "site keys with more than one row", stringsAsFactors = FALSE),
  data.frame(check = "sites_needing_followup", value = sum(site_flags$needs_followup), note = "missing keys, duplicate keys, or missing driver families", stringsAsFactors = FALSE),
  driver_summary,
  year_summary
)

old_file <- find_previous_combined_file(extracted_dir, combined_file)
if (nzchar(old_file)) {
  old <- read_combined_table(old_file)
  old_keys <- unique(old$site_key)
  new_keys <- unique(combined$site_key)

  summary <- bind_rows(
    summary,
    data.frame(check = "comparison_file", value = NA_real_, note = old_file, stringsAsFactors = FALSE),
    data.frame(check = "old_site_key_count", value = length(old_keys), note = "unique site keys in comparison file", stringsAsFactors = FALSE),
    data.frame(check = "new_sites_vs_comparison", value = length(setdiff(new_keys, old_keys)), note = "site keys added vs comparison file", stringsAsFactors = FALSE),
    data.frame(check = "dropped_sites_vs_comparison", value = length(setdiff(old_keys, new_keys)), note = "site keys present in comparison file but absent now", stringsAsFactors = FALSE)
  )
}

site_flags_out <- file.path(out_dir, paste0("spatial_qaqc_site_flags_", date_tag, ".csv"))
summary_out <- file.path(out_dir, paste0("spatial_qaqc_summary_", date_tag, ".csv"))
run_list_out <- file.path(out_dir, paste0("targeted_rerun_list_from_missingness_", date_tag, ".csv"))
run_subset_out <- file.path(out_dir, paste0("targeted_rerun_subset_from_missingness_", date_tag, ".csv"))

write.csv(site_flags, site_flags_out, row.names = FALSE, na = "")
write.csv(summary, summary_out, row.names = FALSE, na = "")

run_list <- site_flags %>%
  filter(needs_followup) %>%
  arrange(priority, LTER, Stream_Name) %>%
  select(
    all_of(keys), region, n_missing_driver_families, missing_driver_families,
    followup_reason, priority
  )

write.csv(run_list, run_list_out, row.names = FALSE, na = "")
write.csv(
  run_list %>% select(LTER, Stream_Name, Shapefile_Name, region) %>% distinct(),
  run_subset_out,
  row.names = FALSE,
  na = ""
)

cat("checked_file=", combined_file, "\n", sep = "")
cat("rows=", nrow(combined), "\n", sep = "")
cat("site_keys=", dplyr::n_distinct(combined$site_key), "\n", sep = "")
cat("sites_needing_followup=", sum(site_flags$needs_followup), "\n", sep = "")
cat("WROTE:", site_flags_out, "\n", sep = "")
cat("WROTE:", summary_out, "\n", sep = "")
cat("WROTE:", run_list_out, "\n", sep = "")
cat("WROTE:", run_subset_out, "\n", sep = "")
