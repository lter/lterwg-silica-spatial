# Build the site-level harmonized driver table
#
# This script starts from the latest vetted combined table, then adds
# site-level harmonized variables. It does not replace combine_qaqc, and it
# intentionally excludes the older GRL yearly assembly pieces that are out of
# scope for this workflow.
#
# Keep this script source-safe. New users should be able to run it from a clean
# R session or from the end-to-end wrapper without losing already-loaded state.

suppressPackageStartupMessages({
  library(dplyr)
})

source(file.path("05_harmonization", "00_harmonization_config.R"))
source(file.path("05_harmonization", "00_harmonization_functions.R"))

# Step 1
# Read the latest vetted combined table
harmonized <- read_harmonized_base_table(combined_file)

# Step 2
# Add site-level Q summary variables
if (add_q_summary && nzchar(wrtds_annual_file) && file.exists(wrtds_annual_file)) {
  wrtds_q <- read_wrtds_annual_q(wrtds_annual_file)
  q_summary <- summarize_wrtds_q(wrtds_q)
  harmonized <- add_wrtds_q_summary(harmonized, q_summary)
}

# Step 3
# Add RBI and recession slope
if ((build_rbi || build_recession_slope) && nzchar(daily_discharge_file) && file.exists(daily_discharge_file)) {
  daily_q <- read_daily_discharge_input(daily_discharge_file)
  discharge_metrics <- compute_discharge_metrics(daily_q)
  harmonized <- harmonized %>% left_join(discharge_metrics, by = "Stream_ID")
}

# Step 4
# Add Köppen-Geiger classes
if (add_kg_class && nzchar(kg_file) && file.exists(kg_file)) {
  harmonized <- add_kg_table(harmonized, kg_file)
}

# Step 5
# Fill missing basin slope values
if (
  gap_fill_basin_slope &&
  nzchar(us_slope_file) &&
  nzchar(krycklan_slope_file) &&
  nzchar(stream_id_key_file)
) {
  harmonized <- gap_fill_basin_slope_values(
    harmonized,
    us_slope_path = us_slope_file,
    krycklan_slope_path = krycklan_slope_file,
    stream_id_key_path = stream_id_key_file
  )
}

# Step 6
# Mark which elevation values still need a fill source
if (gap_fill_elevation) {
  harmonized$elevation_gap_fill_pending <- is.na(harmonized$elevation_mean_m)
}

# Step 7
# Add max daylength from latitude
if (add_max_daylength) {
  harmonized <- add_max_daylength_values(
    harmonized,
    reference_path = wrtds_reference_file,
    kg_path = kg_file
  )
}

# Step 8
# Write the harmonized output tables
write_harmonization_outputs(harmonized, output_dir, date_tag)
