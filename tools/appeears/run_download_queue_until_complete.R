# Poll AppEEARS and resumably download or extract completed tasks

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

source(file.path("tools", "cli_helpers.R"))

### Inputs

args <- commandArgs(trailingOnly = TRUE)
mode <- match.arg(cli_value(args, "--mode", "download"), c("download", "extract"))
run_root <- require_input_dir(
  cli_value(args, "--run-root", required = TRUE),
  "AppEEARS run directory"
)
status_path <- require_input_file(
  cli_value(args, "--status-file", required = TRUE),
  "AppEEARS task status"
)
token_file <- cli_value(
  args,
  "--token-file",
  env_value("SILICA_APPEEARS_TOKEN_FILE", "/tmp/appeears_login.json")
)
expected_tasks <- cli_integer(
  args,
  "--expected-tasks",
  if (mode == "extract") "441" else "154",
  minimum = 1L
)
poll_seconds <- cli_integer(args, "--poll-seconds", "120", minimum = 30L)
download_workers <- cli_integer(
  args,
  "--download-workers",
  if (mode == "extract") "24" else "16",
  minimum = 1L
)
free_space_buffer_gb <- cli_numeric(
  args,
  "--free-space-buffer-gb",
  "10",
  minimum = 1
)

output_root <- watershed_path <- full_status_path <- recent_status_path <- ""
output_date <- run_label <- ""
if (mode == "extract") {
  output_root <- cli_value(args, "--output-root", required = TRUE)
  prepare_output_dir(output_root)
  watershed_path <- require_input_file(
    cli_value(args, "--watershed-file", required = TRUE),
    "watershed file"
  )
  full_status_path <- require_input_file(
    cli_value(args, "--full-status-file", required = TRUE),
    "full-history task status"
  )
  recent_status_path <- require_input_file(
    cli_value(args, "--recent-status-file", required = TRUE),
    "recent task status"
  )
  output_date <- cli_value(args, "--output-date", required = TRUE)
  run_label <- cli_value(args, "--run-label", required = TRUE)
}

coordinator_path <- file.path(run_root, "coordinator_status.csv")
live_status_path <- file.path(run_root, "live_task_status.csv")
ready_status_path <- file.path(run_root, "ready_tasks.json")
lock_dir <- file.path(
  run_root,
  if (mode == "extract") ".coordinator.lock" else ".download-queue.lock"
)
download_timing_path <- file.path(run_root, "download_timing.csv")
all_manifest_path <- file.path(run_root, "download-manifest-all.rds")
qa_path <- file.path(output_root, "standard_modis_extraction_qa.rds")
id_column <- if (mode == "extract") "watershed_key" else "task_name"

### Helpers

write_csv_atomic <- function(data, path) {
  temporary <- paste0(path, ".tmp.", Sys.getpid())
  write.csv(data, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) stop("Could not replace ", path)
}

write_json_atomic <- function(data, path) {
  temporary <- paste0(path, ".tmp.", Sys.getpid())
  write_json(data, temporary, auto_unbox = TRUE, pretty = TRUE, na = "null")
  if (!file.rename(temporary, path)) stop("Could not replace ", path)
}

write_status <- function(phase, detail, live = NULL, completed = 0L) {
  statuses <- if (is.null(live)) character() else live$api_status
  active <- c("pending", "queued", "processing")
  status <- data.frame(
    checked_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    pid = Sys.getpid(),
    mode = mode,
    phase = phase,
    detail = detail,
    tasks_done = sum(statuses == "done"),
    tasks_active = sum(statuses %in% active),
    tasks_error = sum(nzchar(statuses) & !statuses %in% c("done", active)),
    completed = completed,
    stringsAsFactors = FALSE
  )
  write_csv_atomic(status, coordinator_path)
  cat("[", format(Sys.time()), "] ", phase, ": ", detail, "\n", sep = "")
  flush.console()
}

token_is_valid <- function(buffer_seconds = 300) {
  if (!file.exists(token_file)) {
    return(FALSE)
  }
  token <- tryCatch(fromJSON(token_file), error = function(error) NULL)
  if (is.null(token) || is.null(token$token) || !nzchar(token$token)) {
    return(FALSE)
  }
  expiration <- as.POSIXct(
    token$expiration,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )
  !is.na(expiration) && expiration > Sys.time() + buffer_seconds
}

fetch_live <- function(tasks) {
  token <- fromJSON(token_file, simplifyVector = FALSE)
  response <- RETRY(
    "GET",
    "https://appeears.earthdatacloud.nasa.gov/api/task",
    query = list(limit = 5000, offset = 0),
    add_headers(Authorization = paste("Bearer", token$token)),
    times = 5,
    pause_base = 2,
    terminate_on = c(400, 401, 403)
  )
  stop_for_status(response)
  task_list <- fromJSON(
    content(response, as = "text", encoding = "UTF-8"),
    simplifyDataFrame = TRUE
  )
  hit <- match(tasks$task_name, task_list$task_name)
  if (anyNA(hit)) stop("AppEEARS is missing ", sum(is.na(hit)), " tasks")
  tasks$task_id <- task_list$task_id[hit]
  tasks$api_status <- task_list$status[hit]
  tasks$last_checked_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  tasks
}

completed_ids <- function() {
  if (mode == "download") {
    if (!file.exists(download_timing_path)) {
      return(character())
    }
    timing <- read.csv(download_timing_path, stringsAsFactors = FALSE)
    return(unique(timing$task_name[timing$status == "complete"]))
  }
  if (!file.exists(qa_path)) {
    return(character())
  }
  qa <- readRDS(qa_path)
  if (is.null(qa$outputs) || !nrow(qa$outputs)) {
    return(character())
  }
  groups <- split(qa$outputs$status, qa$outputs$watershed_key)
  names(groups)[vapply(groups, function(value) all(value == "complete"), logical(1))]
}

write_ready_tasks <- function(tasks) {
  lter_column <- if ("LTER" %in% names(tasks)) "LTER" else "Source_Key"
  records <- lapply(seq_len(nrow(tasks)), function(index) {
    list(
      task_name = tasks$task_name[[index]],
      task_id = tasks$task_id[[index]],
      LTER = tasks[[lter_column]][[index]],
      Stream_Name = tasks$Stream_Name[[index]],
      Shapefile_Name = tasks$Shapefile_Name[[index]]
    )
  })
  names(records) <- NULL
  write_json_atomic(
    list(
      generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      tasks = records
    ),
    ready_status_path
  )
}

build_download_lists <- function(tasks) {
  if (!nrow(tasks)) {
    return(TRUE)
  }
  write_ready_tasks(tasks)
  identical(system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      "tools/appeears/build_download_lists.R",
      "--run-root", shQuote(run_root),
      "--status-json", shQuote(ready_status_path),
      "--token-file", shQuote(token_file)
    )
  ), 0L)
}

merge_download_manifest <- function() {
  current <- readRDS(file.path(run_root, "download-manifest.rds"))
  previous <- if (file.exists(all_manifest_path)) readRDS(all_manifest_path) else NULL
  files <- if (is.null(previous)) current$files else rbind(previous$files, current$files)
  files <- files[!duplicated(paste(files$task_id, files$file_id)), , drop = FALSE]
  saveRDS(list(generated_at_utc = Sys.time(), files = files), all_manifest_path)
}

available_bytes <- function(path) {
  output <- system2("df", c("-Pk", shQuote(path)), stdout = TRUE)
  fields <- strsplit(trimws(tail(output, 1L)), "[[:space:]]+")[[1]]
  as.numeric(fields[[4]]) * 1024
}

remaining_download_bytes <- function() {
  if (!file.exists(all_manifest_path)) {
    return(0)
  }
  manifest <- readRDS(all_manifest_path)$files
  destinations <- file.path(
    run_root, "downloads", manifest$task_name, basename(manifest$file_name)
  )
  existing_sizes <- rep(0, length(destinations))
  existing <- file.exists(destinations)
  existing_sizes[existing] <- file.info(destinations[existing])$size
  sum(pmax(0, manifest$file_size_bytes - existing_sizes), na.rm = TRUE)
}

run_downloads <- function(ready) {
  if (!nrow(ready)) {
    return(TRUE)
  }
  if (!build_download_lists(ready)) {
    return(FALSE)
  }
  merge_download_manifest()
  required <- remaining_download_bytes() + free_space_buffer_gb * 1024^3
  if (required > available_bytes(run_root)) {
    return(FALSE)
  }
  identical(system2(
    "python3",
    c(
      "tools/appeears/download_completed_tasks.py",
      "--run-root", shQuote(run_root),
      "--token-file", shQuote(token_file),
      "--workers", as.character(download_workers)
    )
  ), 0L)
}

run_local_extractions <- function(ready) {
  if (!nrow(ready)) {
    return(TRUE)
  }
  if (!build_download_lists(ready)) {
    return(FALSE)
  }
  run_group <- function(rows, status_file, start_year) {
    if (!nrow(rows)) {
      return(TRUE)
    }
    command_args <- c(
      "tools/appeears/run_local_modis_queue.R",
      "--run-root", run_root,
      "--watershed-file", watershed_path,
      "--status-file", status_file,
      "--token-file", token_file,
      "--output-root", output_root,
      "--output-date", output_date,
      "--run-label", run_label,
      "--start-year", as.character(start_year),
      "--end-year", "2025",
      "--download-workers", as.character(download_workers),
      "--allow-missing-drivers", "true"
    )
    for (key in rows$watershed_key) {
      command_args <- c(command_args, "--watershed-key", shQuote(key))
    }
    identical(system2("Rscript", command_args), 0L)
  }
  full <- ready[as.integer(ready$start_year) == 2002L, , drop = FALSE]
  recent <- ready[as.integer(ready$start_year) == 2024L, , drop = FALSE]
  run_group(full, full_status_path, 2002L) &&
    run_group(recent, recent_status_path, 2024L)
}

### Validate and lock

if (dir.exists(lock_dir)) {
  stop("The AppEEARS coordinator lock already exists: ", lock_dir)
}
if (!dir.create(lock_dir)) stop("Could not create AppEEARS coordinator lock")
writeLines(as.character(Sys.getpid()), file.path(lock_dir, "pid"))
on.exit(unlink(lock_dir, recursive = TRUE), add = TRUE)

tasks <- read.csv(status_path, stringsAsFactors = FALSE, check.names = FALSE)
required <- c("task_name", "task_id", "Stream_Name", "Shapefile_Name")
if (mode == "extract") {
  required <- c(required, "watershed_key", "start_year", "end_year")
}
assert_required_columns(tasks, required, "task status")
if (nrow(tasks) != expected_tasks || anyDuplicated(tasks[[id_column]]) ||
  anyDuplicated(tasks$task_id)) {
  stop("Task status does not contain the expected unique task set")
}

### Poll and process

repeat {
  completed <- completed_ids()
  if (length(completed) == expected_tasks) {
    write_status("complete", paste("Completed all", expected_tasks, "tasks"), completed = length(completed))
    break
  }
  if (!token_is_valid()) {
    write_status("waiting_for_token", paste("Refresh", token_file), completed = length(completed))
    Sys.sleep(poll_seconds)
    next
  }

  live <- fetch_live(tasks)
  write_csv_atomic(live, live_status_path)
  if (mode == "download") write_csv_atomic(live, status_path)
  unexpected <- !live$api_status %in% c("done", "pending", "queued", "processing")
  if (any(unexpected)) {
    write_status(
      "task_error",
      paste(unique(live$api_status[unexpected]), collapse = ", "),
      live,
      length(completed)
    )
    stop("One or more AppEEARS tasks entered an unexpected state")
  }

  ready <- live[
    live$api_status == "done" & !live[[id_column]] %in% completed, ,
    drop = FALSE
  ]
  write_status(
    "processing",
    paste(nrow(ready), "ready;", length(completed), "complete"),
    live,
    length(completed)
  )
  process_ok <- if (mode == "extract") {
    run_local_extractions(ready)
  } else {
    run_downloads(ready)
  }
  if (!process_ok) {
    phase <- if (token_is_valid(0)) "processing_retry" else "waiting_for_token"
    write_status(phase, paste("Retrying in", poll_seconds, "seconds"), live, length(completed_ids()))
    Sys.sleep(poll_seconds)
    next
  }

  completed <- completed_ids()
  if (length(completed) == expected_tasks && all(live$api_status == "done")) {
    write_status("complete", paste("Completed all", expected_tasks, "tasks"), live, length(completed))
    break
  }
  Sys.sleep(poll_seconds)
}
