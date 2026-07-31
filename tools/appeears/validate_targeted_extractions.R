# Check isolated AppEEARS extraction outputs before their source rasters are
# removed. The saved RDS contains a short batch summary and one QA row per
# watershed and product.

suppressPackageStartupMessages(library(dplyr))

source(file.path("tools", "cli_helpers.R"))
source(file.path("tools", "identifier_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
run_root <- require_input_dir(
  cli_value(args, "--run-root", required = TRUE),
  "prepared AppEEARS run"
)
qa_output <- cli_value(
  args,
  "--qa-output",
  file.path(run_root, "extraction_qa.rds")
)
requested_drivers <- unique(cli_values(args, "--driver"))

manifest_path <- require_input_file(
  file.path(run_root, "run_manifest.csv"),
  "run manifest"
)
timing_path <- require_input_file(
  file.path(run_root, "extraction_driver_timing.csv"),
  "driver timing table"
)
manifest <- read.csv(manifest_path, stringsAsFactors = FALSE, check.names = FALSE)
timing <- read.csv(timing_path, stringsAsFactors = FALSE, check.names = FALSE)
assert_required_columns(
  manifest,
  c("watershed_key", "subset_file"),
  "run manifest"
)
assert_required_columns(
  timing,
  c("watershed_key", "driver", "status", "output_file"),
  "driver timing table"
)

drivers <- if (length(requested_drivers)) {
  requested_drivers
} else {
  sort(unique(timing$driver))
}
unknown_drivers <- setdiff(drivers, unique(timing$driver))
if (length(unknown_drivers)) {
  stop(
    "No timing records found for driver(s): ",
    paste(unknown_drivers, collapse = ", "),
    call. = FALSE
  )
}

identifier_columns <- c(
  "LTER", "Stream_Name", "Shapefile_Name", "Discharge_File_Name"
)

count_present <- function(data) {
  value_columns <- setdiff(names(data), identifier_columns)
  if (!length(value_columns)) return(0L)
  sum(vapply(data[value_columns], function(column) {
    if (is.character(column)) {
      sum(!is.na(column) & nzchar(trimws(column)))
    } else {
      sum(!is.na(column))
    }
  }, integer(1)))
}

check_output <- function(watershed_key, driver, subset_file) {
  record <- timing %>%
    filter(.data$watershed_key == !!watershed_key, .data$driver == !!driver)
  if (nrow(record) != 1L) {
    return(data.frame(
      watershed_key, driver, status = "failed", rows = 0L,
      value_columns = 0L, present_values = 0L, output_file = "",
      message = "Expected one driver timing record."
    ))
  }

  output_file <- record$output_file[[1]]
  if (record$status[[1]] != "complete" ||
      !file.exists(output_file) || file.info(output_file)$size <= 0) {
    return(data.frame(
      watershed_key, driver, status = "failed", rows = 0L,
      value_columns = 0L, present_values = 0L, output_file,
      message = "The extraction is not complete or its output is missing."
    ))
  }

  expected <- read.csv(subset_file, stringsAsFactors = FALSE, check.names = FALSE)
  output <- tryCatch(
    read.csv(output_file, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(error) NULL
  )
  if (is.null(output) ||
      !all(c("LTER", "Shapefile_Name") %in% names(output))) {
    return(data.frame(
      watershed_key, driver, status = "failed", rows = 0L,
      value_columns = 0L, present_values = 0L, output_file,
      message = "The output could not be read or is missing site identifiers."
    ))
  }

  site_rows <- output %>%
    filter(
      normalize_lter_key(.data$LTER) == normalize_lter_key(expected$LTER[[1]]),
      normalize_site_key(.data$Shapefile_Name) ==
        normalize_site_key(expected$Shapefile_Name[[1]])
    )
  value_columns <- setdiff(names(site_rows), identifier_columns)
  present_values <- count_present(site_rows)
  passed <- nrow(site_rows) == 1L && length(value_columns) > 0L &&
    present_values > 0L

  data.frame(
    watershed_key,
    driver,
    status = if (passed) "complete" else "failed",
    rows = nrow(site_rows),
    value_columns = length(value_columns),
    present_values,
    output_file,
    message = if (passed) {
      "The expected watershed has nonblank extracted values."
    } else {
      "The expected watershed is missing, duplicated, or has no extracted values."
    }
  )
}

qa_rows <- bind_rows(lapply(seq_len(nrow(manifest)), function(index) {
  bind_rows(lapply(drivers, function(driver) {
    check_output(
      manifest$watershed_key[[index]],
      driver,
      manifest$subset_file[[index]]
    )
  }))
})) %>% arrange(watershed_key, driver)

summary <- data.frame(
  checked_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  watersheds = nrow(manifest),
  drivers = length(drivers),
  outputs_expected = nrow(manifest) * length(drivers),
  outputs_complete = sum(qa_rows$status == "complete"),
  outputs_failed = sum(qa_rows$status != "complete")
)
prepare_output_dir(qa_output, is_file = TRUE)
saveRDS(list(summary = summary, outputs = qa_rows), qa_output)

cat(
  "Checked", summary$outputs_expected, "outputs for", summary$watersheds,
  "watershed(s):", summary$outputs_complete, "complete and",
  summary$outputs_failed, "failed.\n"
)
if (summary$outputs_failed > 0L) {
  stop("Extraction QA failed; source rasters must be retained.", call. = FALSE)
}
