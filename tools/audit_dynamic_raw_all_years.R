#!/usr/bin/env Rscript

source(file.path(getwd(), "tools", "workflow_paths.R"))
source(file.path(getwd(), "tools", "subset_and_output_helpers.R"))

root <- resolve_silica_data_root()
raw_driver_dir <- silica_raw_driver_data_dir(root)
date_tag <- Sys.getenv("SILICA_AUDIT_DATE", unset = format(Sys.Date(), "%Y%m%d"))
out_dir <- file.path(root, "review", "rerun", paste0("coverage_checks_", date_tag))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Snow has the limiting complete-record start, so the full dynamic readiness
# window is aligned to 2002 for all dynamic drivers.
complete_record_start_year <- 2002L

regions <- c(
  "north-america-usa",
  "north-america-arctic",
  "cropped-russia-west",
  "cropped-russia-west-2",
  "cropped-russia-center",
  "cropped-russia-east",
  "puerto-rico",
  "scandinavia",
  "amazon",
  "australia",
  "canada",
  "congo",
  "germany",
  "united-kingdom"
)

drivers <- list(
  evapo = list(
    dir = "raw-evapo-v061",
    pattern = "MOD16A2GF\\.061_ET_500m",
    years = complete_record_start_year:2025,
    product_years = complete_record_start_year:2025,
    expected = 46L
  ),
  greenup = list(
    dir = "raw-greenup-v061",
    pattern = "MCD12Q2.*Greenup",
    years = complete_record_start_year:2024,
    product_years = complete_record_start_year:2024,
    expected = 2L
  ),
  npp = list(
    dir = "raw-npp-v061",
    pattern = "MOD17A3HGF.*Npp",
    years = complete_record_start_year:2025,
    product_years = complete_record_start_year:2025,
    expected = 1L
  ),
  snow = list(
    dir = "raw-snow-v061",
    pattern = "MOD10A2.*Snow",
    years = complete_record_start_year:2025,
    product_years = complete_record_start_year:2025,
    expected = 46L
  )
)

count_files <- function(driver_dir, pattern, region, year) {
  d <- file.path(raw_driver_dir, driver_dir, region)
  if (!dir.exists(d)) {
    return(0L)
  }
  files <- list.files(d, full.names = FALSE, recursive = TRUE)
  sum(grepl(pattern, files) & grepl(paste0("doy", year), files))
}

rows <- list()
for (driver_name in names(drivers)) {
  cfg <- drivers[[driver_name]]
  for (region in regions) {
    for (year in cfg$years) {
      within_product_window <- year %in% cfg$product_years
      expected <- if (within_product_window) cfg$expected else NA_integer_
      n <- count_files(cfg$dir, cfg$pattern, region, year)
      rows[[length(rows) + 1]] <- data.frame(
        driver = driver_name,
        region = region,
        year = year,
        within_product_window = within_product_window,
        file_count = n,
        expected_file_count = expected,
        missing_or_incomplete = within_product_window && n < expected,
        stringsAsFactors = FALSE
      )
    }
  }
}

audit <- do.call(rbind, rows)
out_file <- file.path(out_dir, paste0("dynamic_raw_all_years_by_region_", date_tag, ".csv"))
write.csv(audit, out_file, row.names = FALSE, na = "")

cat("WROTE:", out_file, "\n", sep = "")

cat("\nIncomplete raw groups within product windows:\n")
print(
  audit[audit$missing_or_incomplete, c(
    "driver",
    "region",
    "year",
    "file_count",
    "expected_file_count"
  )],
  row.names = FALSE
)

cat("\nRows outside required workflow windows:\n")
print(
  audit[!audit$within_product_window, c("driver", "region", "year", "file_count")],
  row.names = FALSE
)
