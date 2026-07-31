# Build checked AppEEARS GeoTIFF download lists from a completed-task JSON file.
# The lists are temporary inputs for the downloader; the RDS manifest preserves
# the useful task, file-count, size, and checksum record without keeping raw data.

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

source(file.path("tools", "cli_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
run_root <- cli_value(args, "--run-root", required = TRUE)
status_json <- require_input_file(
  cli_value(args, "--status-json", required = TRUE),
  "completed-task status JSON"
)
token_file <- require_input_file(
  cli_value(args, "--token-file", "/tmp/appeears_login.json"),
  "AppEEARS token file"
)
requested_tasks <- unique(cli_values(args, "--task-name"))
overwrite <- cli_boolean(args, "--overwrite", FALSE)

status_document <- fromJSON(status_json, simplifyVector = FALSE)
if (is.null(status_document$tasks) || !length(status_document$tasks)) {
  stop("The status JSON contains no tasks.", call. = FALSE)
}

task_records <- lapply(status_document$tasks, function(task) {
  required <- c("task_name", "task_id", "LTER", "Stream_Name", "Shapefile_Name")
  missing <- required[vapply(required, function(name) {
    is.null(task[[name]]) || !nzchar(task[[name]])
  }, logical(1))]
  if (length(missing)) {
    stop("Task status is missing: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  data.frame(
    task_name = task$task_name,
    task_id = task$task_id,
    LTER = task$LTER,
    Stream_Name = task$Stream_Name,
    Shapefile_Name = task$Shapefile_Name,
    stringsAsFactors = FALSE
  )
})
task_records <- do.call(rbind, task_records)
if (anyDuplicated(task_records$task_name) || anyDuplicated(task_records$task_id)) {
  stop("Task names and task IDs must be unique.", call. = FALSE)
}

if (length(requested_tasks)) {
  missing_tasks <- setdiff(requested_tasks, task_records$task_name)
  if (length(missing_tasks)) {
    stop(
      "Requested task(s) are not in the status JSON: ",
      paste(missing_tasks, collapse = ", "),
      call. = FALSE
    )
  }
  task_records <- task_records[task_records$task_name %in% requested_tasks, , drop = FALSE]
}

auth <- fromJSON(token_file, simplifyVector = FALSE)
if (is.null(auth$token) || !nzchar(auth$token)) {
  stop("The AppEEARS token file is incomplete.", call. = FALSE)
}
if (!is.null(auth$expiration)) {
  expiration <- as.POSIXct(
    auth$expiration,
    format = "%Y-%m-%dT%H:%M:%OSZ",
    tz = "UTC"
  )
  if (is.na(expiration) || expiration <= Sys.time()) {
    stop("The AppEEARS token has expired.", call. = FALSE)
  }
}

api_root <- "https://appeears.earthdatacloud.nasa.gov/api"
headers <- add_headers(Authorization = paste("Bearer", auth$token))
list_dir <- file.path(run_root, "download-lists")
dir.create(list_dir, recursive = TRUE, showWarnings = FALSE)

manifests <- vector("list", nrow(task_records))
for (i in seq_len(nrow(task_records))) {
  task <- task_records[i, ]
  task_url <- paste0(api_root, "/task/", task$task_id)
  task_response <- RETRY(
    "GET", task_url, headers,
    times = 4, pause_base = 2, terminate_on = c(400, 401, 403, 404)
  )
  stop_for_status(task_response)
  live_task <- content(task_response, as = "parsed", type = "application/json")
  if (!identical(live_task$task_name, task$task_name) ||
      !identical(live_task$status, "done")) {
    stop(
      "Task is not a matching completed task: ", task$task_name,
      " [", live_task$status, "]",
      call. = FALSE
    )
  }

  bundle_response <- RETRY(
    "GET", paste0(api_root, "/bundle/", task$task_id), headers,
    times = 4, pause_base = 2, terminate_on = c(400, 401, 403, 404)
  )
  stop_for_status(bundle_response)
  bundle <- content(bundle_response, as = "parsed", type = "application/json")
  files <- bundle$files
  if (is.null(files) || !length(files)) {
    stop("Completed task has no bundle files: ", task$task_name, call. = FALSE)
  }
  tif_files <- Filter(function(file) {
    !is.null(file$file_name) && grepl("[.]tif$", file$file_name, ignore.case = TRUE)
  }, files)
  if (!length(tif_files)) {
    stop("Completed task has no GeoTIFF files: ", task$task_name, call. = FALSE)
  }
  file_names <- vapply(tif_files, `[[`, character(1), "file_name")
  if (anyDuplicated(file_names)) {
    stop("Bundle contains duplicate GeoTIFF names: ", task$task_name, call. = FALSE)
  }

  list_path <- file.path(list_dir, paste0(task$task_name, "-download-list.txt"))
  urls <- vapply(tif_files, function(file) {
    # AppEEARS may return file_name with an internal folder prefix. Its
    # download endpoint accepts only the GeoTIFF's basename.
    download_name <- basename(file$file_name)
    paste0(
      api_root, "/bundle/", task$task_id, "/", file$file_id, "/",
      download_name
    )
  }, character(1))
  if (file.exists(list_path) && !overwrite) {
    previous <- readLines(list_path, warn = FALSE)
    if (!identical(previous, urls)) {
      stop("Existing download list differs: ", list_path, call. = FALSE)
    }
  } else {
    writeLines(urls, list_path, useBytes = TRUE)
  }

  manifests[[i]] <- data.frame(
    task_name = task$task_name,
    task_id = task$task_id,
    LTER = task$LTER,
    Stream_Name = task$Stream_Name,
    Shapefile_Name = task$Shapefile_Name,
    file_name = file_names,
    file_id = vapply(tif_files, `[[`, character(1), "file_id"),
    file_size_bytes = vapply(tif_files, `[[`, numeric(1), "file_size"),
    sha256 = vapply(tif_files, `[[`, character(1), "sha256"),
    stringsAsFactors = FALSE
  )
  cat(
    task$task_name, ": ", length(tif_files), " GeoTIFF files\n",
    sep = ""
  )
}

manifest <- do.call(rbind, manifests)
manifest_path <- file.path(run_root, "download-manifest.rds")
saveRDS(
  list(
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    task_summary = aggregate(
      file_size_bytes ~ task_name + task_id + LTER + Stream_Name + Shapefile_Name,
      data = manifest,
      FUN = function(value) c(files = length(value), bytes = sum(value))
    ),
    files = manifest
  ),
  manifest_path
)
cat("Wrote ", nrow(manifest), " GeoTIFF links and ", manifest_path, "\n", sep = "")
