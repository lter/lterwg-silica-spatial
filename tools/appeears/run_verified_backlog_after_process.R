# Wait for another process, rebuild a verified request backlog, and run
# each continuous date range. Failed steps retry after preserving their output.

source(file.path("tools", "cli_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
wait_for_pid <- cli_integer(
  args, "--wait-for-pid", default = "0", minimum = 0L
)
site_table <- require_input_file(
  cli_value(args, "--site-table", required = TRUE),
  "finalized site-reference table"
)
request_root <- require_input_dir(
  cli_value(args, "--request-root", "generated_outputs/rerun/appeears-requests"),
  "AppEEARS request root"
)
shapefile_root <- require_input_dir(
  cli_value(args, "--shapefile-root", required = TRUE),
  "versioned shapefile root"
)
current_qa <- cli_value(args, "--current-qa", "")
run_root <- cli_value(args, "--run-root", required = TRUE)
output_root <- cli_value(args, "--output-root", required = TRUE)
token_file <- cli_value(args, "--token-file", "/tmp/appeears_login.json")
download_workers <- cli_integer(
  args, "--download-workers", default = "24", minimum = 1L
)
poll_seconds <- cli_integer(
  args, "--poll-seconds", default = "300", minimum = 15L
)
output_date <- cli_value(args, "--output-date", format(Sys.Date(), "%Y%m%d"))
batch_pattern <- cli_value(args, "--batch-pattern", ".*")
shape_aliases <- cli_value(
  args,
  "--shape-aliases",
  file.path("tools", "appeears", "config", "shapefile_aliases.tsv")
)
status_path <- file.path(run_root, "automation_status.rds")

dir.create(run_root, recursive = TRUE, showWarnings = FALSE)
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

write_status <- function(phase, detail = "") {
  saveRDS(
    data.frame(
      checked_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      phase = phase,
      detail = detail,
      stringsAsFactors = FALSE
    ),
    status_path
  )
  cat("[", format(Sys.time()), "] ", phase,
      if (nzchar(detail)) paste0(": ", detail) else "", "\n", sep = "")
}

process_running <- function(pid) {
  identical(
    suppressWarnings(system2("kill", c("-0", as.character(pid)),
      stdout = FALSE, stderr = FALSE)),
    0L
  )
}

token_is_valid <- function(path, buffer_seconds = 300) {
  if (!file.exists(path)) return(FALSE)
  token <- tryCatch(jsonlite::fromJSON(path), error = function(error) NULL)
  if (is.null(token) || is.null(token$token) || !nzchar(token$token)) return(FALSE)
  if (is.null(token$expiration)) return(TRUE)
  expiration <- as.POSIXct(
    token$expiration,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )
  !is.na(expiration) && expiration > Sys.time() + buffer_seconds
}

wait_for_token <- function() {
  while (!token_is_valid(token_file)) {
    write_status(
      "waiting_for_token",
      paste("Replace", token_file, "with a fresh AppEEARS login token")
    )
    Sys.sleep(poll_seconds)
  }
}

run_until_success <- function(command, command_args, phase, needs_token = FALSE) {
  repeat {
    if (needs_token) wait_for_token()
    write_status(phase)
    status <- system2(command, command_args)
    if (identical(status, 0L)) return(invisible(NULL))
    write_status(
      paste0(phase, "_retry"),
      paste("exit", status, "- retrying in", poll_seconds, "seconds")
    )
    Sys.sleep(poll_seconds)
  }
}

if (wait_for_pid > 0L) {
  write_status("waiting_for_current_queue", paste("PID", wait_for_pid))
  while (process_running(wait_for_pid)) Sys.sleep(30)
}

audit_args <- c(
  "tools/appeears/prepare_verified_request_backlog.R",
  "--site-table", site_table,
  "--request-root", request_root,
  "--shapefile-root", shapefile_root,
  "--output-root", run_root,
  "--batch-pattern", batch_pattern,
  "--shape-aliases", shape_aliases
)
if (nzchar(current_qa)) {
  audit_args <- c(audit_args, "--current-qa", current_qa)
}
run_until_success("Rscript", audit_args, "auditing_verified_backlog")

queue_roots <- list.dirs(run_root, recursive = FALSE, full.names = TRUE)
queue_roots <- queue_roots[grepl("^coverage-[0-9]{4}-[0-9]{4}$", basename(queue_roots))]
if (!length(queue_roots)) {
  write_status("complete", "No verified AppEEARS tasks remain")
  quit(save = "no", status = 0L)
}
queue_years <- strcapture(
  "^coverage-([0-9]{4})-([0-9]{4})$",
  basename(queue_roots),
  proto = list(start_year = integer(), end_year = integer())
)
queue_roots <- queue_roots[order(
  queue_years$end_year - queue_years$start_year,
  queue_years$start_year
)]

for (queue_root in queue_roots) {
  years <- strcapture(
    "^coverage-([0-9]{4})-([0-9]{4})$",
    basename(queue_root),
    proto = list(start_year = integer(), end_year = integer())
  )
  coverage_key <- paste(years$start_year, years$end_year, sep = "-")

  build_args <- c(
    "tools/appeears/build_download_lists.R",
    "--run-root", queue_root,
    "--status-json", file.path(queue_root, "verified_tasks.json"),
    "--token-file", token_file
  )
  run_until_success(
    "Rscript", build_args,
    paste0("waiting_for_completed_tasks_", coverage_key),
    needs_token = TRUE
  )

  queue_output <- file.path(output_root, paste0("coverage-", coverage_key))
  extraction_args <- c(
    "tools/appeears/run_local_modis_queue.R",
    "--run-root", queue_root,
    "--watershed-file", file.path(queue_root, "verified_watersheds.gpkg"),
    "--status-file", file.path(queue_root, "verified_task_status.csv"),
    "--token-file", token_file,
    "--output-root", queue_output,
    "--output-date", output_date,
    "--run-label", paste0("verified_", gsub("-", "_", coverage_key)),
    "--start-year", years$start_year,
    "--end-year", years$end_year,
    "--download-workers", download_workers,
    "--allow-missing-drivers", "true"
  )
  run_until_success(
    "Rscript", extraction_args,
    paste0("extracting_", coverage_key),
    needs_token = TRUE
  )

  alias_args <- c(
    "tools/appeears/expand_extraction_aliases.R",
    "--output-root", queue_output,
    "--alias-file", file.path(queue_root, "shape_aliases.rds")
  )
  run_until_success(
    "Rscript", alias_args,
    paste0("expanding_aliases_", coverage_key)
  )
}

write_status("complete", "All verified AppEEARS extractions finished")
