# First step before running harmonization
# save the latest extracted spatial data files from Aurora to your local machine
# For a clean run in Positron or RStudio, restart the R session before running
# this script rather than clearing the workspace inside the workflow code.

# Example
# scp bush@aurora.nceas.ucsb.edu:/home/shares/lter-si/si-watershed-extract/extracted-data/*.csv /Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial_data_extractions/extracted-data/
# scp bush@aurora.nceas.ucsb.edu:/home/shares/lter-si/si-watershed-extract/site-coordinates/* /Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial_data_extractions/site-coordinates/
# so this workflow can import the latest combined and driver extraction files
# from that local data root
# Harmonization imports two core files
# old = the legacy combined spatial extraction file
# new = the current combined local spatial extraction file

suppressPackageStartupMessages({
  library(dplyr)
})

source(file.path("04_combine_qaqc", "00_qaqc_config.R"))
source(file.path("04_combine_qaqc", "00_qaqc_functions.R"))

# Import the vetted old table and the incoming new table
old <- read_combined_table(old_file)
new <- read_combined_table(new_file, sanitize_new = TRUE)
glimpse_checkpoint(old, "old combined input", glimpse_dir, date_tag)
glimpse_checkpoint(new, "new combined input", glimpse_dir, date_tag)

# Inspect which sites and years are present in the incoming file
new_driver_specs <- list(
  evapo = "^evapotrans_([0-9]{4})_kg_m2$",
  greenup = "^greenup_cycle[01]_([0-9]{4})MMDD$",
  precip = "^precip_([0-9]{4})_mm_per_day$",
  airtemp = "^temp_([0-9]{4})_degC$",
  snow_days = "^snow_([0-9]{4})_num_days$",
  snow_area = "^snow_([0-9]{4})_max_prop_area$",
  npp = "^npp_([0-9]{4})_kgC_m2_year$"
)

new_site_inventory <- summarize_site_inventory(new)
new_site_inventory_summary <- new_site_inventory %>%
  count(LTER, name = "n_sites", sort = TRUE)
new_temporal_extent <- summarize_driver_year_extent(new, new_driver_specs)
glimpse_checkpoint(
  new_site_inventory,
  "new site inventory",
  glimpse_dir,
  date_tag
)
glimpse_checkpoint(
  new_site_inventory_summary,
  "new site inventory summary",
  glimpse_dir,
  date_tag
)
glimpse_checkpoint(
  new_temporal_extent,
  "new temporal extent",
  glimpse_dir,
  date_tag
)

write.csv(
  new_site_inventory,
  file.path(
    harmonization_dir,
    paste0("new-data-site-inventory_", date_tag, ".csv")
  ),
  row.names = FALSE,
  na = ""
)
write.csv(
  new_site_inventory_summary,
  file.path(
    harmonization_dir,
    paste0("new-data-site-inventory-summary_", date_tag, ".csv")
  ),
  row.names = FALSE
)
write.csv(
  new_temporal_extent,
  file.path(
    year_extension_dir,
    paste0("new-data-temporal-extent_", date_tag, ".csv")
  ),
  row.names = FALSE,
  na = ""
)

# Compare overlapping old and new values before building the combined file
source(
  file.path("04_combine_qaqc", "qaqc-new-vs-old.R"),
  echo = FALSE
)
glimpse_checkpoint(
  shared_summary,
  "shared overlap summary",
  glimpse_dir,
  date_tag
)
glimpse_checkpoint(new_only, "new only sites", glimpse_dir, date_tag)
glimpse_checkpoint(spatial_followup, "spatial followup", glimpse_dir, date_tag)

# Build the combined table after overlap QA is written
source(
  file.path("04_combine_qaqc", "build-combined-spatial-dataset.R"),
  echo = FALSE
)
glimpse_checkpoint(out, "harmonized master output", glimpse_dir, date_tag)
glimpse_checkpoint(summary, "harmonized master summary", glimpse_dir, date_tag)

coverage_out <- file.path(
  review_root,
  "year_extension",
  paste0("new_years_coverage_summary_", date_tag, ".csv")
)
dir.create(dirname(coverage_out), recursive = TRUE, showWarnings = FALSE)

# Summarize where the new file extends the vetted old record
driver_specs <- list(
  evapo = "^evapotrans_([0-9]{4})_kg_m2$",
  greenup = "^greenup_cycle[01]_([0-9]{4})MMDD$",
  precip = "^precip_([0-9]{4})_mm_per_day$",
  airtemp = "^temp_([0-9]{4})_degC$",
  snow = "^snow_([0-9]{4})_num_days$",
  npp = "^npp_([0-9]{4})_kgC_m2_year$"
)

coverage <- summarize_overlap_coverage(old, new, driver_specs)
shared_site_summary <- summarize_shared_site_counts(old, new, driver_specs)
glimpse_checkpoint(coverage, "new year coverage summary", glimpse_dir, date_tag)
glimpse_checkpoint(
  shared_site_summary,
  "shared sites new year counts",
  glimpse_dir,
  date_tag
)

write.csv(coverage, coverage_out, row.names = FALSE, na = "")
shared_out <- file.path(
  review_root,
  "year_extension",
  paste0("shared_sites_new_year_counts_", date_tag, ".csv")
)
dir.create(dirname(shared_out), recursive = TRUE, showWarnings = FALSE)
write.csv(shared_site_summary, shared_out, row.names = FALSE, na = "")

# Run the reasonableness screen on newly added years
source(
  file.path("04_combine_qaqc", "qaqc-reasonable-new-values.R"),
  echo = FALSE
)
glimpse_checkpoint(
  review,
  "new years plausibility review",
  glimpse_dir,
  date_tag
)
glimpse_checkpoint(
  summary,
  "new years plausibility summary",
  glimpse_dir,
  date_tag
)

cat("import-and-qaqc complete\n")
