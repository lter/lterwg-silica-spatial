#!/usr/bin/env Rscript

source(file.path(getwd(), "tools", "workflow_paths.R"))

root <- resolve_silica_data_root()
raw_driver_dir <- silica_raw_driver_data_dir(root)
out_dir <- file.path(root, "review", "rerun", "coverage_checks_20260522")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

base_regions <- c(
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

driver_regions <- list(
  evapo = c(base_regions, "mali"),
  greenup = c(base_regions, "mali"),
  npp = c(base_regions, "mali"),
  snow = base_regions
)

drivers <- list(
  evapo = list(dir = "raw-evapo-v061", pattern = "MOD16A2GF\\.061_ET_500m", years = 2020:2025),
  greenup = list(dir = "raw-greenup-v061", pattern = "MCD12Q2.*Greenup", years = 2020:2024),
  npp = list(dir = "raw-npp-v061", pattern = "MOD17A3HGF.*Npp", years = 2020:2025),
  snow = list(dir = "raw-snow-v061", pattern = "MOD10A2.*Snow", years = 2020:2025)
)

count_files <- function(driver_dir, pattern, region, year) {
  d <- file.path(raw_driver_dir, driver_dir, region)
  if (!dir.exists(d)) {
    return(0L)
  }
  files <- list.files(d, full.names = FALSE, recursive = TRUE)
  sum(grepl(pattern, files) & grepl(paste0("doy", year), files))
}

expected_files <- function(driver_name, year) {
  if (driver_name %in% c("evapo", "snow")) {
    return(ifelse(year == 2025, 46L, 46L))
  }
  if (driver_name == "greenup") {
    return(2L)
  }
  if (driver_name == "npp") {
    return(1L)
  }
  NA_integer_
}

rows <- list()
for (driver_name in names(drivers)) {
  cfg <- drivers[[driver_name]]
  for (region in driver_regions[[driver_name]]) {
    for (year in cfg$years) {
      n <- count_files(cfg$dir, cfg$pattern, region, year)
      expected <- expected_files(driver_name, year)
      rows[[length(rows) + 1]] <- data.frame(
        driver = driver_name,
        region = region,
        year = year,
        file_count = n,
        expected_file_count = expected,
        missing_or_incomplete = is.na(expected) || n < expected,
        stringsAsFactors = FALSE
      )
    }
  }
}

audit <- do.call(rbind, rows)
out_file <- file.path(out_dir, "dynamic_raw_recent_years_by_region_20260522.csv")
write.csv(audit, out_file, row.names = FALSE, na = "")

cat("WROTE:", out_file, "\n", sep = "")
cat("\nIncomplete recent-year raw groups:\n")
print(
  audit[audit$missing_or_incomplete, c("driver", "region", "year", "file_count", "expected_file_count")],
  row.names = FALSE
)
