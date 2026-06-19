librarian::shelf(dplyr, ggplot2, readr, tidyr, quiet = TRUE)

# Final input files -----------------------------------------------------------

full_spatial_file <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/final-data/full-dataset/final_full_spatial_drivers_annual_20260608.csv"
full_harmonized_file <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/final-data/full-dataset/final_full_harmonized_annual_20260608.csv"
esom_harmonized_file <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/final-data/esom/ESOM_final_harmonized_annual_20260608.csv"

out_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/final-data/diagnostics/final_dataset_diagnostics_20260608"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Read data -------------------------------------------------------------------

full_spatial <- read_csv(full_spatial_file, show_col_types = FALSE, guess_max = 100000)
full_harmonized <- read_csv(full_harmonized_file, show_col_types = FALSE, guess_max = 100000)
esom_harmonized <- read_csv(esom_harmonized_file, show_col_types = FALSE, guess_max = 100000)

# Row and site checks ---------------------------------------------------------

dataset_summary <- tibble(
  dataset = c("full_spatial", "full_harmonized", "esom_harmonized"),
  file = c(full_spatial_file, full_harmonized_file, esom_harmonized_file),
  rows = c(nrow(full_spatial), nrow(full_harmonized), nrow(esom_harmonized)),
  cols = c(ncol(full_spatial), ncol(full_harmonized), ncol(esom_harmonized)),
  sites = c(
    n_distinct(full_spatial$Stream_ID),
    n_distinct(full_harmonized$Stream_ID),
    n_distinct(esom_harmonized$Stream_ID)
  ),
  first_year = c(min(full_spatial$Year), min(full_harmonized$Year), min(esom_harmonized$Year)),
  last_year = c(max(full_spatial$Year), max(full_harmonized$Year), max(esom_harmonized$Year)),
  duplicate_site_year_rows = c(
    nrow(full_spatial) - n_distinct(full_spatial$Stream_ID, full_spatial$Year),
    nrow(full_harmonized) - n_distinct(full_harmonized$Stream_ID, full_harmonized$Year),
    nrow(esom_harmonized) - n_distinct(esom_harmonized$Stream_ID, esom_harmonized$Year)
  )
)

write_csv(
  dataset_summary,
  file.path(out_dir, "final_dataset_input_summary_20260608.csv")
)

year_counts <- bind_rows(
  full_spatial %>%
    group_by(Year) %>%
    summarise(rows = n(), sites = n_distinct(Stream_ID), .groups = "drop") %>%
    mutate(dataset = "full_spatial", .before = 1),
  full_harmonized %>%
    group_by(Year) %>%
    summarise(rows = n(), sites = n_distinct(Stream_ID), .groups = "drop") %>%
    mutate(dataset = "full_harmonized", .before = 1),
  esom_harmonized %>%
    group_by(Year) %>%
    summarise(rows = n(), sites = n_distinct(Stream_ID), .groups = "drop") %>%
    mutate(dataset = "esom_harmonized", .before = 1)
)

write_csv(
  year_counts,
  file.path(out_dir, "final_dataset_rows_by_year_20260608.csv")
)

# Numeric values in long format ----------------------------------------------

numeric_long <- bind_rows(
  full_spatial %>%
    mutate(dataset = "full_spatial", .before = 1) %>%
    select(dataset, Stream_ID, Year, where(is.numeric)),
  full_harmonized %>%
    mutate(dataset = "full_harmonized", .before = 1) %>%
    select(dataset, Stream_ID, Year, where(is.numeric)),
  esom_harmonized %>%
    mutate(dataset = "esom_harmonized", .before = 1) %>%
    select(dataset, Stream_ID, Year, where(is.numeric))
) %>%
  pivot_longer(
    cols = -c(dataset, Stream_ID, Year),
    names_to = "variable",
    values_to = "value"
  )

# This table feeds the coverage and range plots below.

variable_coverage_by_year <- numeric_long %>%
  group_by(dataset, variable, Year) %>%
  summarise(
    rows = n(),
    nonmissing_rows = sum(!is.na(value)),
    pct_nonmissing = mean(!is.na(value)),
    nonmissing_sites = n_distinct(Stream_ID[!is.na(value)]),
    min = if (all(is.na(value))) NA_real_ else min(value, na.rm = TRUE),
    p01 = if (all(is.na(value))) NA_real_ else as.numeric(quantile(value, 0.01, na.rm = TRUE)),
    median = if (all(is.na(value))) NA_real_ else median(value, na.rm = TRUE),
    p99 = if (all(is.na(value))) NA_real_ else as.numeric(quantile(value, 0.99, na.rm = TRUE)),
    max = if (all(is.na(value))) NA_real_ else max(value, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  variable_coverage_by_year,
  file.path(out_dir, "final_dataset_variable_coverage_by_year_20260608.csv")
)

# Basic plausibility flags ----------------------------------------------------

numeric_variables <- sort(unique(numeric_long$variable))

must_be_nonnegative <- grep(
  "^(drainage_area|precip|Q|npp|evapotrans|elevation|RBI|RCS|basin_slope)$",
  numeric_variables,
  value = TRUE
)

snow_fraction_variables <- c(
  "snow_cover",
  grep("^snow_.*_avg_prop_area$", numeric_variables, value = TRUE)
)

monthly_snow_day_variables <- grep("^snow_.*_num_days$", numeric_variables, value = TRUE)
monthly_snow_day_variables <- setdiff(monthly_snow_day_variables, "snow_num_days")

percent_variables <- c(
  "permafrost",
  grep("^(rocks_|land_)", numeric_variables, value = TRUE)
)

flagged_values <- bind_rows(
  numeric_long %>%
    filter(variable %in% must_be_nonnegative, !is.na(value), value < 0) %>%
    mutate(rule = "negative_value"),
  numeric_long %>%
    filter(variable %in% snow_fraction_variables, !is.na(value), value < 0 | value > 1) %>%
    mutate(rule = "snow_fraction_outside_0_1"),
  numeric_long %>%
    filter(variable == "snow_num_days", !is.na(value), value < 0 | value > 366) %>%
    mutate(rule = "annual_snow_days_outside_0_366"),
  numeric_long %>%
    filter(variable %in% monthly_snow_day_variables, !is.na(value), value < 0 | value > 40) %>%
    mutate(rule = "monthly_snow_days_outside_0_40"),
  numeric_long %>%
    filter(variable %in% percent_variables, !is.na(value), value < 0 | value > 100) %>%
    mutate(rule = "percent_outside_0_100"),
  numeric_long %>%
    filter(variable == "greenup_day", !is.na(value), value < 1 | value > 366) %>%
    mutate(rule = "greenup_day_outside_1_366"),
  numeric_long %>%
    filter(variable == "temp", !is.na(value), value < -80 | value > 60) %>%
    mutate(rule = "temperature_outside_plausible_range")
)

value_flags <- flagged_values %>%
  group_by(dataset, rule, variable) %>%
  summarise(
    flagged_rows = n(),
    flagged_sites = n_distinct(Stream_ID),
    first_flagged_year = first(Year, order_by = Year, default = NA_real_),
    last_flagged_year = last(Year, order_by = Year, default = NA_real_),
    min = first(value, order_by = value, default = NA_real_),
    max = last(value, order_by = value, default = NA_real_),
    .groups = "drop"
  )

write_csv(
  value_flags,
  file.path(out_dir, "final_dataset_basic_value_flags_20260608.csv")
)

# Diagnostic plots ------------------------------------------------------------

core_variables <- c(
  "FNConc", "FNYield", "GenConc", "GenYield",
  "drainage_area", "precip", "Q", "temp", "snow_cover", "snow_num_days",
  "npp", "evapotrans", "greenup_day", "permafrost", "elevation",
  "RBI", "RCS", "basin_slope"
)

core_coverage <- variable_coverage_by_year %>%
  filter(variable %in% core_variables)

rows_by_year_plot <- ggplot(year_counts, aes(x = Year, y = rows, color = dataset)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.4) +
  facet_wrap(~dataset, scales = "free_y") +
  labs(x = NULL, y = "Rows", title = "Final Dataset Rows By Year") +
  theme_bw(base_size = 11) +
  theme(legend.position = "none")

ggsave(
  file.path(out_dir, "final_dataset_rows_by_year_20260608.png"),
  rows_by_year_plot,
  width = 10,
  height = 6,
  dpi = 300
)

core_coverage_plot <- ggplot(core_coverage, aes(x = Year, y = nonmissing_rows, color = dataset)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1) +
  facet_wrap(~variable, scales = "free_y", ncol = 3) +
  labs(x = NULL, y = "Nonmissing Rows", title = "Core Variable Coverage By Year") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "final_dataset_core_variable_nonmissing_by_year_20260608.png"),
  core_coverage_plot,
  width = 13,
  height = 10,
  dpi = 300
)

core_heatmap <- ggplot(core_coverage, aes(x = Year, y = variable, fill = pct_nonmissing)) +
  geom_tile(color = "white", linewidth = 0.15) +
  scale_fill_viridis_c(option = "magma", limits = c(0, 1), labels = scales::percent) +
  facet_wrap(~dataset, ncol = 1) +
  labs(x = NULL, y = NULL, fill = "Nonmissing", title = "Core Variable Percent Nonmissing") +
  theme_bw(base_size = 10)

ggsave(
  file.path(out_dir, "final_dataset_core_variable_coverage_heatmap_20260608.png"),
  core_heatmap,
  width = 11,
  height = 10,
  dpi = 300
)

range_variables <- c(
  "precip", "Q", "temp", "snow_cover", "snow_num_days",
  "npp", "evapotrans", "greenup_day", "RBI", "RCS"
)

range_plot_data <- variable_coverage_by_year %>%
  filter(variable %in% range_variables, nonmissing_rows > 0)

range_plot <- ggplot(range_plot_data, aes(x = Year, color = dataset, fill = dataset)) +
  geom_ribbon(aes(ymin = p01, ymax = p99), alpha = 0.14, color = NA) +
  geom_line(aes(y = median), linewidth = 0.7) +
  facet_wrap(~variable, scales = "free_y", ncol = 2) +
  labs(x = NULL, y = "Median with 1st-99th percentile band", title = "Key Variable Ranges By Year") +
  theme_bw(base_size = 10) +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "final_dataset_key_variable_ranges_by_year_20260608.png"),
  range_plot,
  width = 13,
  height = 12,
  dpi = 300
)
