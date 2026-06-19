librarian::shelf(dplyr, tidyr, stringr, readr)

source(file.path(getwd(), "04_combine_qaqc", "00_qaqc_functions.R"))

args <- commandArgs(trailingOnly = TRUE)
old_file <- if (length(args) >= 1) args[[1]] else ""
new_file <- if (length(args) >= 2) args[[2]] else ""
out_dir <- if (length(args) >= 3) args[[3]] else file.path("generated_outputs", "combined_delta")

if (!nzchar(old_file) || !nzchar(new_file)) {
  stop("Usage: Rscript tools/summarize_combined_delta.R <old_file> <new_file> [out_dir]", call. = FALSE)
}

old <- read_combined_table(old_file)
new <- read_combined_table(new_file)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

driver_specs <- tibble::tribble(
  ~driver, ~regex,
  "evapo", "^evapotrans_([0-9]{4})_kg_m2$",
  "greenup", "^greenup_cycle[01]_([0-9]{4})MMDD$",
  "precip", "^precip_([0-9]{4})_mm_per_day$",
  "airtemp", "^temp_([0-9]{4})_degC$",
  "snow", "^snow_([0-9]{4})_num_days$",
  "npp", "^npp_([0-9]{4})_kgC_m2_year$"
)

site_cols <- c("key", "LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")

new_only <- new %>%
  filter(!key %in% old$key) %>%
  select(all_of(site_cols)) %>%
  arrange(LTER, Stream_Name)

old_only <- old %>%
  filter(!key %in% new$key) %>%
  select(all_of(site_cols)) %>%
  arrange(LTER, Stream_Name)

count_non_empty <- function(df, cols) {
  if (!length(cols)) return(rep(0L, nrow(df)))
  vals <- df[, cols, drop = FALSE]
  rowSums(!is.na(vals) & vals != "")
}

shared_keys <- intersect(old$key, new$key)

driver_summary <- bind_rows(lapply(seq_len(nrow(driver_specs)), function(i) {
  driver <- driver_specs$driver[i]
  rx <- driver_specs$regex[i]
  old_cols <- grep(rx, names(old), value = TRUE)
  new_cols <- grep(rx, names(new), value = TRUE)

  old_shared <- old[match(shared_keys, old$key), c("key", old_cols), drop = FALSE]
  new_shared <- new[match(shared_keys, new$key), c("key", new_cols), drop = FALSE]

  old_counts <- count_non_empty(old_shared, old_cols)
  new_counts <- count_non_empty(new_shared, new_cols)

  tibble(
    driver = driver,
    old_min_year = min_year_from_names(names(old), rx),
    old_max_year = max_year_from_names(names(old), rx),
    new_min_year = min_year_from_names(names(new), rx),
    new_max_year = max_year_from_names(names(new), rx),
    old_non_missing_cells = sum(old_counts),
    new_non_missing_cells = sum(new_counts),
    added_non_missing_cells = sum(new_counts) - sum(old_counts),
    shared_sites_with_more_years = sum(new_counts > old_counts),
    shared_sites_with_less_years = sum(new_counts < old_counts),
    old_sites_with_any = sum(old_counts > 0),
    new_sites_with_any = sum(new_counts > 0)
  )
}))

build_driver_year_counts <- function(df, specs) {
  bind_rows(lapply(seq_len(nrow(specs)), function(i) {
    driver <- specs$driver[i]
    rx <- specs$regex[i]
    cols <- grep(rx, names(df), value = TRUE)
    if (!length(cols)) return(tibble())
    yrs <- as.integer(str_match(cols, rx)[,2])
    tibble(
      driver = driver,
      year = yrs,
      non_missing_sites = vapply(cols, function(col) sum(!is.na(df[[col]]) & df[[col]] != ""), integer(1))
    )
  }))
}

old_year_counts <- build_driver_year_counts(old, driver_specs) %>% rename(old_non_missing_sites = non_missing_sites)
new_year_counts <- build_driver_year_counts(new, driver_specs) %>% rename(new_non_missing_sites = non_missing_sites)

old_year_counts <- old_year_counts %>%
  group_by(driver, year) %>%
  summarise(old_non_missing_sites = sum(old_non_missing_sites), .groups = "drop")

new_year_counts <- new_year_counts %>%
  group_by(driver, year) %>%
  summarise(new_non_missing_sites = sum(new_non_missing_sites), .groups = "drop")

year_delta <- full_join(old_year_counts, new_year_counts, by = c("driver", "year")) %>%
  mutate(
    old_non_missing_sites = coalesce(old_non_missing_sites, 0L),
    new_non_missing_sites = coalesce(new_non_missing_sites, 0L),
    delta_non_missing_sites = new_non_missing_sites - old_non_missing_sites
  ) %>%
  arrange(driver, year)

site_driver_gap <- bind_rows(lapply(seq_len(nrow(driver_specs)), function(i) {
  driver <- driver_specs$driver[i]
  rx <- driver_specs$regex[i]
  cols <- grep(rx, names(new), value = TRUE)
  years <- as.integer(str_match(cols, rx)[,2])
  if (!length(cols)) return(tibble())

  mat <- new[, c(site_cols, cols), drop = FALSE]
  present_counts <- rowSums(!is.na(mat[, cols, drop = FALSE]) & mat[, cols, drop = FALSE] != "")

  min_present <- apply(mat[, cols, drop = FALSE], 1, function(row) {
    idx <- which(!is.na(row) & row != "")
    if (!length(idx)) return(NA_integer_)
    min(years[idx])
  })
  max_present <- apply(mat[, cols, drop = FALSE], 1, function(row) {
    idx <- which(!is.na(row) & row != "")
    if (!length(idx)) return(NA_integer_)
    max(years[idx])
  })

  tibble(
    LTER = mat$LTER,
    Stream_Name = mat$Stream_Name,
    Discharge_File_Name = mat$Discharge_File_Name,
    Shapefile_Name = mat$Shapefile_Name,
    driver = driver,
    n_years_present = present_counts,
    min_year_present = min_present,
    max_year_present = max_present,
    missing_all_years = present_counts == 0
  )
})) %>%
  arrange(LTER, Stream_Name, driver)

gap_summary <- site_driver_gap %>%
  group_by(driver) %>%
  summarise(
    sites_missing_all_years = sum(missing_all_years),
    sites_with_any_years = sum(!missing_all_years),
    .groups = "drop"
  )

breakdown_file <- file.path(out_dir, "combined_delta_summary.csv")
new_only_file <- file.path(out_dir, "new_only_sites.csv")
old_only_file <- file.path(out_dir, "old_only_sites.csv")
driver_file <- file.path(out_dir, "driver_delta_summary.csv")
year_file <- file.path(out_dir, "driver_year_delta.csv")
gap_file <- file.path(out_dir, "site_driver_gap_detail.csv")
gap_summary_file <- file.path(out_dir, "site_driver_gap_summary.csv")

summary_tbl <- tibble(
  old_rows = nrow(old),
  new_rows = nrow(new),
  shared_rows = length(shared_keys),
  new_only_rows = nrow(new_only),
  old_only_rows = nrow(old_only)
)

write_csv(summary_tbl, breakdown_file, na = "")
write_csv(new_only, new_only_file, na = "")
write_csv(old_only, old_only_file, na = "")
write_csv(driver_summary, driver_file, na = "")
write_csv(year_delta, year_file, na = "")
write_csv(site_driver_gap, gap_file, na = "")
write_csv(gap_summary, gap_summary_file, na = "")

message("Wrote: ", breakdown_file)
message("Wrote: ", new_only_file)
message("Wrote: ", old_only_file)
message("Wrote: ", driver_file)
message("Wrote: ", year_file)
message("Wrote: ", gap_file)
message("Wrote: ", gap_summary_file)
