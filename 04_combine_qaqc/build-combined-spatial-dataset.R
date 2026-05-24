# Build one combined table that keeps all old rows, all new rows, and the
# shared rows merged together on a common site key
suppressPackageStartupMessages({
  library(dplyr)
})

source(file.path("04_combine_qaqc", "00_qaqc_config.R"))
source(file.path("04_combine_qaqc", "00_qaqc_functions.R"))

review_file_tag <- function(date_tag) {
  mode_tag <- trimws(Sys.getenv("SILICA_RUN_MODE", unset = "manual"))
  label_tag <- trimws(Sys.getenv("SILICA_RUN_LABEL", unset = ""))

  clean <- function(x) {
    x <- tolower(x)
    x <- gsub("[^a-z0-9]+", "-", x)
    gsub("(^-+|-+$)", "", x)
  }

  mode_tag <- clean(mode_tag)
  label_tag <- clean(label_tag)

  parts <- c("combined-spatial-dataset", mode_tag)
  if (nzchar(label_tag)) parts <- c(parts, label_tag)
  parts <- c(parts, date_tag)
  paste(parts, collapse = "_")
}

first_non_missing <- function(x) {
  keep <- !(is.na(x) | trimws(as.character(x)) == "")
  if (!any(keep)) {
    if (is.numeric(x)) return(NA_real_)
    return(NA)
  }
  x[which(keep)[1]]
}

# When duplicate site rows exist, keep the most complete version first
collapse_duplicate_sites <- function(df) {
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

date_tag <- format(Sys.Date(), "%Y%m%d")
file_tag <- review_file_tag(date_tag)
out_file <- file.path(harmonization_dir, paste0(file_tag, ".csv"))
summary_file <- file.path(harmonization_dir, paste0(file_tag, "_summary.csv"))

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(summary_file), recursive = TRUE, showWarnings = FALSE)

old <- read_combined_table(old_file)
new <- read_combined_table(new_file, sanitize_new = TRUE)

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

# For shared rows, prefer the old value when it exists and fill from the new
# table only when the old table is blank
for (nm in setdiff(all_cols, "key")) {
  merged_shared[[nm]] <- dplyr::coalesce(old_shared[[nm]], new_shared[[nm]])
}

out <- bind_rows(
  merged_shared,
  old[old$key %in% old_only_keys, , drop = FALSE],
  new[new$key %in% new_only_keys, , drop = FALSE]
)

out <- collapse_duplicate_sites(out)
dup_summary <- attr(out, "duplicate_site_groups")

annual_cols <- grep("^(evapotrans|greenup_cycle[01]|precip_|temp_|snow_|npp_)", names(out), value = TRUE)
annual_non_na <- rowSums(!is.na(out[, annual_cols, drop = FALSE]))

summary <- data.frame(
  old_rows = nrow(old),
  new_rows = nrow(new),
  shared_keys = length(shared_keys),
  old_only_keys = length(old_only_keys),
  new_only_keys = length(new_only_keys),
  duplicate_site_groups_collapsed = if (is.null(dup_summary)) 0L else nrow(dup_summary),
  master_rows = nrow(out),
  master_with_any_annual = sum(annual_non_na > 0),
  master_zero_annual = sum(annual_non_na == 0),
  evapo_max = max_year_from_names(names(out), "^evapotrans_([0-9]{4})_kg_m2$"),
  greenup_max = max_year_from_names(names(out), "^greenup_cycle[01]_([0-9]{4})MMDD$"),
  precip_max = max_year_from_names(names(out), "^precip_([0-9]{4})_mm_per_day$"),
  airtemp_max = max_year_from_names(names(out), "^temp_([0-9]{4})_degC$"),
  snow_max = max_year_from_names(names(out), "^snow_([0-9]{4})_num_days$"),
  npp_max = max_year_from_names(names(out), "^npp_([0-9]{4})_kgC_m2_year$"),
  stringsAsFactors = FALSE
)

out$key <- NULL

write.csv(out, out_file, row.names = FALSE, na = "")
write.csv(summary, summary_file, row.names = FALSE)
