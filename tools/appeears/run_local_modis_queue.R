# Process completed AppEEARS requests one watershed at a time. Each watershed
# is downloaded, extracted, checked, added to four shared product tables, and
# cleaned before the next watershed begins

### Inputs

suppressPackageStartupMessages(library(dplyr))

source(file.path("tools", "cli_helpers.R"))
source(file.path("tools", "identifier_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
run_root <- require_input_dir(
  cli_value(args, "--run-root", required = TRUE),
  "AppEEARS run root"
)
watershed_file <- require_input_file(
  cli_value(args, "--watershed-file", required = TRUE),
  "watershed file"
)
status_file <- require_input_file(
  cli_value(args, "--status-file", required = TRUE),
  "task status file"
)
token_file <- require_input_file(
  cli_value(args, "--token-file", required = TRUE),
  "AppEEARS token file"
)
output_root <- cli_value(args, "--output-root", required = TRUE)
prepare_output_dir(output_root)
output_root <- normalizePath(output_root, mustWork = TRUE)

output_date <- cli_value(args, "--output-date", format(Sys.Date(), "%Y%m%d"))
run_label <- cli_value(args, "--run-label", "new-watersheds")
start_year <- cli_integer(args, "--start-year", 2002L)
end_year <- cli_integer(args, "--end-year", 2025L)
download_workers <- cli_integer(args, "--download-workers", 24L, minimum = 1L)
limit <- cli_integer(args, "--limit", NULL, minimum = 1L)
requested_keys <- unique(cli_values(args, "--watershed-key"))
allow_missing_drivers <- cli_boolean(args, "--allow-missing-drivers", FALSE)

driver_products <- c(
  greenup = "MCD12Q2.061",
  npp = "MOD17A3HGF.061",
  evapo = "MOD16A2GF.061",
  snow = "MOD10A2.061"
)

python <- cli_value(args, "--python", Sys.which("python3"))
rscript <- Sys.which("Rscript")
if (!nzchar(python) || !nzchar(rscript)) {
  stop("Python 3 and Rscript must both be available.", call. = FALSE)
}

### Task selection

status <- read.csv(status_file, stringsAsFactors = FALSE, check.names = FALSE)
assert_required_columns(
  status,
  c(
    "task_name", "watershed_key", "Shapefile_Name", "start_year",
    "end_year"
  ),
  "task status file"
)
if (anyDuplicated(status$task_name)) {
  stop("Task names must be unique in the status file.", call. = FALSE)
}

task_groups <- split(status, status$watershed_key)
all_task_groups <- task_groups
task_signature <- function(task_names) {
  paste(sort(unique(task_names)), collapse = " | ")
}
coverage_years <- function(group) {
  sort(unique(unlist(Map(
    seq.int,
    as.integer(group$start_year),
    as.integer(group$end_year)
  ))))
}
if (length(requested_keys)) {
  missing_keys <- setdiff(requested_keys, names(task_groups))
  if (length(missing_keys)) {
    stop(
      "Unknown watershed key(s): ",
      paste(missing_keys, collapse = ", "),
      call. = FALSE
    )
  }
  task_groups <- task_groups[requested_keys]
}

qa_path <- file.path(output_root, "standard_modis_extraction_qa.rds")
existing_qa <- if (file.exists(qa_path)) readRDS(qa_path) else NULL
completed_keys <- if (!is.null(existing_qa) && nrow(existing_qa$outputs) &&
    "task_signature" %in% names(existing_qa$outputs)) {
  candidates <- existing_qa$outputs %>%
    group_by(watershed_key) %>%
    summarize(
      complete = n() >= 1L && all(status == "complete") &&
        n_distinct(task_signature) == 1L,
      saved_signature = first(task_signature),
      .groups = "drop"
    ) %>%
    filter(complete)
  candidates$signature_matches <- vapply(
    seq_len(nrow(candidates)),
    function(index) {
      key <- candidates$watershed_key[[index]]
      key %in% names(task_groups) && identical(
        candidates$saved_signature[[index]],
        task_signature(task_groups[[key]]$task_name)
      )
    },
    logical(1)
  )
  candidates$watershed_key[candidates$signature_matches]
} else {
  character()
}
task_groups <- task_groups[!names(task_groups) %in% completed_keys]
task_groups <- task_groups[order(
  vapply(task_groups, nrow, integer(1)),
  names(task_groups)
)]
if (!is.null(limit)) task_groups <- head(task_groups, limit)

### Command and output helpers

run_command <- function(command, command_args, label) {
  status_code <- system2(command, command_args)
  if (!identical(status_code, 0L)) {
    stop(label, " failed with exit code ", status_code, ".", call. = FALSE)
  }
}

run_command_with_retries <- function(
  command,
  command_args,
  label,
  attempts = 3L,
  wait_seconds = 5L
) {
  for (attempt in seq_len(attempts)) {
    status_code <- system2(command, command_args)
    if (identical(status_code, 0L)) return(invisible(NULL))
    if (attempt < attempts) {
      cat(
        label, "attempt", attempt, "of", attempts,
        "did not finish; retrying incomplete files.\n"
      )
      Sys.sleep(wait_seconds)
    }
  }
  stop(
    label, " failed after ", attempts, " attempts.",
    call. = FALSE
  )
}

output_filename <- function(driver) {
  file.path(
    output_root,
    "extracted-data",
    paste0("si-extract_", driver, "_v061_", output_date, "_", run_label, ".csv")
  )
}

write_csv_atomic <- function(data, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp")
  write.csv(data, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    stop("Could not replace output table: ", path, call. = FALSE)
  }
}

merge_product_rows <- function(existing, additions) {
  identifier_columns <- c(
    "LTER", "Shapefile_Name", "Discharge_File_Name", "Stream_Name"
  )
  for (column in intersect(identifier_columns, names(additions))) {
    additions[[column]] <- as.character(additions[[column]])
  }
  if (!is.null(existing)) {
    for (column in intersect(identifier_columns, names(existing))) {
      existing[[column]] <- as.character(existing[[column]])
    }
  }

  key <- function(data) {
    paste(
      normalize_lter_key(data$LTER),
      normalize_site_key(data$Shapefile_Name),
      sep = "__"
    )
  }
  additions$.merge_key <- key(additions)
  if (anyDuplicated(additions$.merge_key)) {
    stop("A product update contains duplicate watershed rows.", call. = FALSE)
  }
  if (!is.null(existing)) {
    existing$.merge_key <- key(existing)
    existing <- existing[!existing$.merge_key %in% additions$.merge_key, , drop = FALSE]
  }
  bind_rows(existing, additions) %>%
    arrange(LTER, Shapefile_Name) %>%
    select(-.merge_key)
}

save_batch <- function(batch_qa) {
  batch_qa$outputs <- batch_qa$outputs %>% arrange(watershed_key, driver)
  for (driver in unique(batch_qa$outputs$driver)) {
    source_files <- batch_qa$outputs$output_file[batch_qa$outputs$driver == driver]
    additions <- bind_rows(lapply(
      source_files,
      read.csv,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
    destination <- output_filename(driver)
    existing <- if (file.exists(destination)) {
      read.csv(destination, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      NULL
    }
    combined <- merge_product_rows(existing, additions)
    write_csv_atomic(combined, destination)
    batch_qa$outputs$output_file[batch_qa$outputs$driver == driver] <- destination
  }

  prior_outputs <- if (!is.null(existing_qa)) existing_qa$outputs else NULL
  if (!is.null(prior_outputs) && nrow(prior_outputs)) {
    replaced <- paste(batch_qa$outputs$watershed_key, batch_qa$outputs$driver)
    prior_outputs <- prior_outputs[
      !paste(prior_outputs$watershed_key, prior_outputs$driver) %in% replaced,
      ,
      drop = FALSE
    ]
  }
  outputs <- bind_rows(prior_outputs, batch_qa$outputs) %>%
    arrange(watershed_key, driver)
  qa <- list(
    summary = data.frame(
      checked_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      watersheds = n_distinct(outputs$watershed_key),
      drivers = n_distinct(outputs$driver),
      outputs_expected = nrow(outputs),
      outputs_complete = sum(outputs$status == "complete"),
      outputs_failed = sum(outputs$status != "complete")
    ),
    outputs = outputs
  )
  temporary <- paste0(qa_path, ".tmp")
  saveRDS(qa, temporary)
  if (!file.rename(temporary, qa_path)) {
    stop("Could not replace the combined QA record.", call. = FALSE)
  }
  existing_qa <<- qa
}

mark_cleaned <- function(task_names) {
  timing_path <- file.path(run_root, "download_timing.csv")
  timing <- read.csv(timing_path, stringsAsFactors = FALSE, check.names = FALSE)
  timing$status[timing$task_name %in% task_names] <- "processed_and_cleaned"
  write_csv_atomic(timing, timing_path)
}

record_missing_drivers <- function(watershed_key, task_names, missing_drivers) {
  if (!length(missing_drivers)) return(invisible(NULL))
  path <- file.path(output_root, "missing_driver_coverage.rds")
  additions <- data.frame(
    checked_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    watershed_key = watershed_key,
    task_names = paste(task_names, collapse = " | "),
    driver = missing_drivers,
    reason = "AppEEARS bundle contains no GeoTIFF for this product",
    stringsAsFactors = FALSE
  )
  existing <- if (file.exists(path)) readRDS(path) else NULL
  combined <- bind_rows(existing, additions) %>%
    arrange(watershed_key, driver) %>%
    distinct(watershed_key, driver, .keep_all = TRUE)
  saveRDS(combined, path)
}

### Resume cleanup

# Finish cleanup after a run that stopped with verified outputs already saved
for (completed_key in intersect(completed_keys, names(all_task_groups))) {
  completed_group <- all_task_groups[[completed_key]]
  completed_slug <- gsub(
    "(^-+|-+$)",
    "",
    gsub("[^a-z0-9]+", "-", tolower(completed_key))
  )
  unlink(
    file.path(run_root, "downloads", completed_group$task_name),
    recursive = TRUE,
    force = TRUE
  )
  unlink(
    file.path(output_root, "_work", completed_slug),
    recursive = TRUE,
    force = TRUE
  )
  timing_path <- file.path(run_root, "download_timing.csv")
  if (file.exists(timing_path)) mark_cleaned(completed_group$task_name)
}

### Process watersheds

if (!length(task_groups)) {
  cat("No unprocessed watersheds were selected.\n")
  quit(save = "no", status = 0L)
}

cat("Selected", length(task_groups), "watershed(s) for local MODIS processing.\n")
for (group_index in seq_along(task_groups)) {
  group <- task_groups[[group_index]]
  watershed_key <- group$watershed_key[[1]]
  task_names <- group$task_name
  slug <- gsub("(^-+|-+$)", "", gsub("[^a-z0-9]+", "-", tolower(watershed_key)))
  work_root <- file.path(output_root, "_work", slug)
  dir.create(work_root, recursive = TRUE, showWarnings = FALSE)
  cat("[", group_index, "/", length(task_groups), "] ", watershed_key, "\n", sep = "")

  download_args <- c(
    "tools/appeears/download_completed_tasks.py",
    "--run-root", run_root,
    "--token-file", token_file,
    "--status-file", status_file,
    "--workers", as.character(download_workers),
    "--retries", "6"
  )
  for (task_name in task_names) {
    download_args <- c(download_args, "--task-name", task_name)
  }
  run_command_with_retries(
    python,
    download_args,
    "AppEEARS download",
    attempts = 3L
  )

  available_drivers <- names(driver_products)
  if (allow_missing_drivers) {
    manifest <- readRDS(file.path(run_root, "download-manifest.rds"))$files
    task_files <- manifest$file_name[manifest$task_name %in% task_names]
    available_drivers <- names(driver_products)[vapply(
      driver_products,
      function(product) any(startsWith(basename(task_files), product)),
      logical(1)
    )]
    missing_drivers <- setdiff(names(driver_products), available_drivers)
    if (length(missing_drivers)) {
      cat(
        "AppEEARS returned no ", paste(missing_drivers, collapse = ", "),
        " GeoTIFF for ", watershed_key, "; recording it as unavailable.\n",
        sep = ""
      )
      record_missing_drivers(watershed_key, task_names, missing_drivers)
    }
    if (!length(available_drivers)) {
      stop("The AppEEARS bundle contains no requested MODIS products.", call. = FALSE)
    }
  }

  prepare_args <- c(
    "tools/appeears/prepare_targeted_extraction_inputs.R",
    "--run-root", run_root,
    "--watershed-file", watershed_file,
    "--status-file", status_file,
    "--output-root", work_root,
    "--expected-task-count", as.character(length(task_names)),
    "--start-year", as.character(start_year),
    "--end-year", as.character(end_year)
  )
  for (task_name in task_names) {
    prepare_args <- c(prepare_args, "--task-name", task_name)
  }
  if (allow_missing_drivers) {
    for (driver in available_drivers) {
      prepare_args <- c(prepare_args, "--driver", driver)
    }
  }
  run_command(rscript, prepare_args, "Input preparation")

  extraction_args <- c(
    "tools/appeears/run_targeted_extractions.py",
    "--run-root", work_root,
    "--run-label", run_label,
    "--output-date", output_date,
    "--start-year", as.character(start_year),
    "--end-year", as.character(end_year),
    "--workers", "1"
  )
  if (allow_missing_drivers) {
    for (driver in available_drivers) {
      extraction_args <- c(extraction_args, "--driver", driver)
    }
  }
  run_command(python, extraction_args, "MODIS extraction")

  batch_qa_path <- file.path(work_root, "extraction_qa.rds")
  qa_args <- c(
    "tools/appeears/validate_targeted_extractions.R",
    "--run-root", work_root,
    "--qa-output", batch_qa_path
  )
  if (allow_missing_drivers) {
    for (driver in available_drivers) {
      qa_args <- c(qa_args, "--driver", driver)
    }
  }
  run_command(rscript, qa_args, "Extraction QA")
  batch_qa <- readRDS(batch_qa_path)
  if (batch_qa$summary$outputs_failed[[1]] != 0L) {
    stop("Extraction QA failed; raw files were retained.", call. = FALSE)
  }
  covered_years <- coverage_years(group)
  batch_qa$outputs$task_signature <- task_signature(task_names)
  batch_qa$outputs$coverage_start_year <- min(covered_years)
  batch_qa$outputs$coverage_end_year <- max(covered_years)
  batch_qa$outputs$covered_years <- paste(covered_years, collapse = ",")
  save_batch(batch_qa)

  download_dirs <- file.path(run_root, "downloads", task_names)
  unlink(download_dirs, recursive = TRUE, force = TRUE)
  unlink(work_root, recursive = TRUE, force = TRUE)
  mark_cleaned(task_names)
  cat("Saved and cleaned", watershed_key, "\n")
}

### Finish

work_parent <- file.path(output_root, "_work")
if (dir.exists(work_parent) && !length(list.files(work_parent))) {
  unlink(work_parent, recursive = TRUE)
}
cat("Local MODIS queue complete.\n")
