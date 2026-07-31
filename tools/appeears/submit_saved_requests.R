# Submit checked AppEEARS request JSON files without rebuilding their geometry.

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
})

source(file.path("tools", "cli_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
request_dirs <- unique(cli_values(args, "--request-dir"))
request_files <- unique(cli_values(args, "--request-file"))
output_status <- cli_value(args, "--output-status", required = TRUE)
token_file <- require_input_file(
  cli_value(args, "--token-file", "/tmp/appeears_login.json"),
  "AppEEARS token file"
)
submit_missing <- cli_boolean(args, "--submit", FALSE)

for (directory in request_dirs) {
  require_input_dir(directory, "request directory")
  request_files <- c(
    request_files,
    list.files(directory, pattern = "[.]json$", full.names = TRUE)
  )
}
request_files <- sort(unique(request_files))
if (!length(request_files)) stop("No request JSON files were supplied.", call. = FALSE)
invisible(lapply(request_files, require_input_file, label = "request JSON"))

requests <- lapply(request_files, fromJSON, simplifyVector = FALSE)
task_names <- vapply(requests, function(request) request$task_name, character(1))
if (any(!nzchar(task_names)) || anyDuplicated(task_names)) {
  stop("Request task names must be present and unique.", call. = FALSE)
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

read_task_page <- function(offset, limit = 1000L) {
  response <- RETRY(
    "GET",
    paste0(api_root, "/task"),
    query = list(limit = limit, offset = offset),
    headers,
    times = 5,
    pause_base = 2,
    terminate_on = c(400, 401, 403)
  )
  stop_for_status(response)
  fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
}

existing <- list()
offset <- 0L
repeat {
  page <- read_task_page(offset)
  if (!length(page)) break
  existing <- c(existing, page)
  if (length(page) < 1000L) break
  offset <- offset + length(page)
}
existing_names <- vapply(existing, function(task) task$task_name, character(1))

status <- vector("list", length(requests))
for (index in seq_along(requests)) {
  task_name <- task_names[[index]]
  existing_index <- match(task_name, existing_names)
  if (!is.na(existing_index)) {
    task <- existing[[existing_index]]
    action <- "existing_task"
    http_status <- 200L
  } else if (!submit_missing) {
    task <- list(task_id = "", status = "not_submitted")
    action <- "missing_task"
    http_status <- NA_integer_
  } else {
    response <- RETRY(
      "POST",
      paste0(api_root, "/task"),
      body = requests[[index]],
      encode = "json",
      headers,
      times = 5,
      pause_base = 2,
      terminate_on = c(400, 401, 403, 413)
    )
    stop_for_status(response)
    task <- fromJSON(
      content(response, as = "text", encoding = "UTF-8"),
      simplifyVector = FALSE
    )
    action <- "submitted"
    http_status <- status_code(response)
  }

  status[[index]] <- list(
    task_name = task_name,
    task_id = if (is.null(task$task_id)) "" else task$task_id,
    status = if (is.null(task$status)) "" else task$status,
    action = action,
    http_status = http_status,
    request_file = request_files[[index]]
  )
  cat(task_name, ": ", action, "\n", sep = "")
}

dir.create(dirname(output_status), recursive = TRUE, showWarnings = FALSE)
write_json(
  list(
    checked_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    tasks = status
  ),
  output_status,
  auto_unbox = TRUE,
  pretty = TRUE,
  na = "null"
)
cat("Wrote ", output_status, "\n", sep = "")
