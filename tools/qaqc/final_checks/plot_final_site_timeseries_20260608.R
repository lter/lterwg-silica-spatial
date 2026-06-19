librarian::shelf(dplyr, ggplot2, readr, tidyr, quiet = TRUE)

# Final input files -----------------------------------------------------------

full_spatial_file <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/final-data/full-dataset/final_full_spatial_drivers_annual_20260608.csv"
full_harmonized_file <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/final-data/full-dataset/final_full_harmonized_annual_20260608.csv"
esom_harmonized_file <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/final-data/esom/ESOM_final_harmonized_annual_20260608.csv"

out_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/final-data/diagnostics/site_timeseries_20260608"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Read data -------------------------------------------------------------------

full_spatial <- read_csv(
  full_spatial_file,
  show_col_types = FALSE,
  guess_max = 100000
)
full_harmonized <- read_csv(
  full_harmonized_file,
  show_col_types = FALSE,
  guess_max = 100000
)
esom_harmonized <- read_csv(
  esom_harmonized_file,
  show_col_types = FALSE,
  guess_max = 100000
)

# Variables to plot -----------------------------------------------------------

harmonized_time_varying_variables <- c(
  "FNConc",
  "FNYield",
  "GenConc",
  "GenYield",
  "precip",
  "Q",
  "temp",
  "snow_cover",
  "snow_num_days",
  "npp",
  "evapotrans",
  "greenup_day"
)

monthly_snow_variables <- c(
  grep("^snow_.*_avg_prop_area$", names(full_spatial), value = TRUE),
  grep("^snow_.*_num_days$", names(full_spatial), value = TRUE)
)

snow_fraction_variables <- c(
  "snow_cover",
  grep("^snow_.*_avg_prop_area$", names(full_spatial), value = TRUE)
)

monthly_snow_day_variables <- grep(
  "^snow_.*_num_days$",
  names(full_spatial),
  value = TRUE
)

spatial_time_varying_variables <- c(
  "precip",
  "Q",
  "temp",
  "snow_cover",
  "snow_num_days",
  setdiff(monthly_snow_variables, "snow_num_days"),
  "npp",
  "evapotrans",
  "greenup_day"
)

# Y-axis anchors keep all-zero snow panels from auto-scaling around zero.

plot_axis_limits <- bind_rows(
  tibble(variable = snow_fraction_variables, value = 0, Year = 2002),
  tibble(variable = snow_fraction_variables, value = 1, Year = 2002),
  tibble(variable = "snow_num_days", value = 0, Year = 2002),
  tibble(variable = "snow_num_days", value = 366, Year = 2002),
  tibble(variable = monthly_snow_day_variables, value = 0, Year = 2002),
  tibble(variable = monthly_snow_day_variables, value = 31, Year = 2002)
)

# One page per site PDFs ------------------------------------------------------

save_site_timeseries_pdf <- function(
  data,
  dataset_name,
  variables,
  pdf_file,
  page_width,
  page_height,
  panel_columns
) {
  plot_data <- data %>%
    select(Stream_ID, Year, all_of(variables)) %>%
    pivot_longer(
      cols = all_of(variables),
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(variable = factor(variable, levels = variables))

  site_ids <- sort(unique(plot_data$Stream_ID))

  site_page_index <- tibble(
    dataset = dataset_name,
    Stream_ID = site_ids,
    page = seq_along(site_ids),
    pdf = basename(pdf_file)
  )

  write_csv(site_page_index, sub("\\.pdf$", "_page_index.csv", pdf_file))

  axis_limits <- plot_axis_limits %>%
    filter(variable %in% variables) %>%
    mutate(variable = factor(variable, levels = variables))

  grDevices::pdf(
    pdf_file,
    width = page_width,
    height = page_height,
    onefile = TRUE
  )

  for (site_id in site_ids) {
    site_plot_data <- plot_data %>%
      filter(Stream_ID == site_id)

    site_plot <- ggplot(site_plot_data, aes(x = Year, y = value)) +
      geom_blank(
        data = axis_limits,
        inherit.aes = FALSE,
        aes(x = Year, y = value)
      ) +
      geom_line(linewidth = 0.3, na.rm = TRUE) +
      geom_point(size = 0.7, na.rm = TRUE) +
      facet_wrap(~variable, scales = "free_y", ncol = panel_columns) +
      scale_x_continuous(limits = c(2002, 2025), breaks = seq(2002, 2025, 4)) +
      labs(
        x = NULL,
        y = NULL,
        title = paste(dataset_name, site_id),
        subtitle = "One panel per time-varying variable"
      ) +
      theme_bw(base_size = 8) +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 9),
        strip.text = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        panel.grid.minor = element_blank()
      )

    grid::grid.newpage()
    grid::grid.draw(ggplotGrob(site_plot))
  }

  invisible(grDevices::dev.off())
}

save_site_timeseries_pdf(
  full_harmonized,
  "Full harmonized:",
  harmonized_time_varying_variables,
  file.path(out_dir, "full_harmonized_site_timeseries_20260608.pdf"),
  page_width = 11,
  page_height = 8.5,
  panel_columns = 4
)

save_site_timeseries_pdf(
  esom_harmonized,
  "ESOM harmonized:",
  harmonized_time_varying_variables,
  file.path(out_dir, "esom_harmonized_site_timeseries_20260608.pdf"),
  page_width = 11,
  page_height = 8.5,
  panel_columns = 4
)

save_site_timeseries_pdf(
  full_spatial,
  "Full spatial drivers:",
  spatial_time_varying_variables,
  file.path(out_dir, "full_spatial_drivers_site_timeseries_20260608.pdf"),
  page_width = 14,
  page_height = 11,
  panel_columns = 5
)
