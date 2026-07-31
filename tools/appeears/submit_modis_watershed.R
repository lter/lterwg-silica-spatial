# Build and optionally submit MODIS AppEEARS requests for one watershed.
# Repeat --period to split a long record into separate tasks. Repeat --driver
# to request only the needed products; the default is all four products.

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(sf)
})

source(file.path("tools", "cli_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
watershed_file <- require_input_file(
  cli_value(args, "--watershed-file", required = TRUE),
  "watershed file"
)
lter <- cli_value(args, "--lter", required = TRUE)
stream_name <- cli_value(args, "--stream-name", required = TRUE)
shapefile_name <- cli_value(args, "--shapefile-name", required = TRUE)
task_prefix <- cli_value(args, "--task-prefix", required = TRUE)
period_values <- cli_values(args, "--period")
requested_drivers <- unique(cli_values(args, "--driver"))
run_root <- cli_value(args, "--run-root", required = TRUE)
token_file <- cli_value(
  args,
  "--token-file",
  env_value("SILICA_APPEEARS_TOKEN_FILE", "/tmp/appeears_login.json")
)
submit_tasks <- cli_boolean(
  args,
  "--submit",
  env_boolean("SILICA_APPEEARS_SUBMIT", FALSE)
)
expected_area <- suppressWarnings(as.numeric(
  cli_value(args, "--expected-area-km2", NA_character_)
))
area_tolerance_pct <- suppressWarnings(as.numeric(
  cli_value(args, "--area-tolerance-pct", "0.1")
))
simplify_m <- suppressWarnings(as.numeric(
  cli_value(args, "--simplify-m", "0")
))
task_date <- cli_value(args, "--task-date", format(Sys.Date(), "%Y%m%d"))

if (!length(period_values)) {
  stop("Provide at least one --period START:END.", call. = FALSE)
}
if (!is.finite(area_tolerance_pct) || area_tolerance_pct < 0) {
  stop("--area-tolerance-pct must be a non-negative number.", call. = FALSE)
}
if (!is.finite(simplify_m) || simplify_m < 0) {
  stop("--simplify-m must be a non-negative number.", call. = FALSE)
}

parse_period <- function(value) {
  parts <- strsplit(value, ":", fixed = TRUE)[[1]]
  years <- suppressWarnings(as.integer(parts))
  if (length(years) != 2L || anyNA(years) || years[[1]] > years[[2]]) {
    stop("Invalid --period ", value, "; use START:END.", call. = FALSE)
  }
  years
}
periods <- lapply(period_values, parse_period)
period_keys <- vapply(periods, paste, collapse = "-", FUN.VALUE = character(1))
if (anyDuplicated(period_keys)) {
  stop("Each --period must be unique.", call. = FALSE)
}

request_dir <- file.path(run_root, "requests")
status_dir <- file.path(run_root, "submission-status")
dir.create(request_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(status_dir, recursive = TRUE, showWarnings = FALSE)

watershed <- st_read(watershed_file, quiet = TRUE)
watershed <- st_transform(st_make_valid(watershed), 4326)
watershed <- st_sf(
  aoi_name = shapefile_name,
  site_id = paste(lter, stream_name, sep = "__"),
  geometry = st_union(st_geometry(watershed))
)
if (nrow(watershed) != 1L || any(st_is_empty(watershed)) ||
    any(!st_is_valid(watershed))) {
  stop("The watershed must resolve to one valid, non-empty feature.", call. = FALSE)
}

projected <- st_transform(watershed, 6933)
source_area_km2 <- as.numeric(st_area(projected)) / 1e6
if (is.finite(expected_area)) {
  expected_error_pct <- 100 * (source_area_km2 - expected_area) / expected_area
  if (abs(expected_error_pct) > area_tolerance_pct) {
    stop(
      "Watershed area differs from --expected-area-km2 by ",
      round(expected_error_pct, 4), "%.",
      call. = FALSE
    )
  }
}

request_projected <- if (simplify_m > 0) {
  st_simplify(projected, dTolerance = simplify_m, preserveTopology = TRUE)
} else {
  projected
}
request_area_km2 <- as.numeric(st_area(request_projected)) / 1e6
request_area_error_pct <- 100 *
  (request_area_km2 - source_area_km2) / source_area_km2
if (abs(request_area_error_pct) > area_tolerance_pct) {
  stop("Request simplification changed the watershed area too much.", call. = FALSE)
}

request_geometry <- st_transform(request_projected, 4326)
geometry_path <- tempfile(fileext = ".geojson")
st_write(
  request_geometry,
  geometry_path,
  driver = "GeoJSON",
  delete_dsn = TRUE,
  quiet = TRUE,
  layer_options = "COORDINATE_PRECISION=7"
)
geojson <- fromJSON(geometry_path, simplifyVector = FALSE)
roundtrip <- st_read(geometry_path, quiet = TRUE)
roundtrip_area_km2 <- as.numeric(st_area(st_transform(roundtrip, 6933))) / 1e6
roundtrip_error_pct <- 100 *
  (roundtrip_area_km2 - source_area_km2) / source_area_km2
if (!identical(geojson$type, "FeatureCollection") ||
    length(geojson$features) != 1L ||
    !geojson$features[[1]]$geometry$type %in% c("Polygon", "MultiPolygon") ||
    abs(roundtrip_error_pct) > area_tolerance_pct) {
  stop("GeoJSON round-trip validation failed.", call. = FALSE)
}

layer_catalog <- list(
  evapo = list(product = "MOD16A2GF.061", layer = "ET_500m"),
  npp = list(product = "MOD17A3HGF.061", layer = "Npp_500m"),
  snow = list(product = "MOD10A2.061", layer = "Eight_Day_Snow_Cover"),
  greenup = list(product = "MCD12Q2.061", layer = "Greenup")
)
if (!length(requested_drivers)) requested_drivers <- names(layer_catalog)
unknown_drivers <- setdiff(requested_drivers, names(layer_catalog))
if (length(unknown_drivers)) {
  stop(
    "Unknown --driver value(s): ",
    paste(unknown_drivers, collapse = ", "),
    ". Use evapo, npp, snow, or greenup.",
    call. = FALSE
  )
}
layers <- unname(layer_catalog[requested_drivers])
driver_label <- if (setequal(requested_drivers, names(layer_catalog))) {
  "all4"
} else {
  paste(requested_drivers, collapse = "-")
}

task_names <- vapply(
  periods,
  function(years) paste(
    task_prefix, driver_label, years[[1]], years[[2]], task_date, sep = "-"
  ),
  character(1)
)
if (any(nchar(task_names) > 100L) || anyDuplicated(task_names)) {
  stop("Generated task names must be unique and no longer than 100 characters.",
       call. = FALSE)
}

payloads <- lapply(seq_along(periods), function(i) {
  years <- periods[[i]]
  list(
    task_type = "area",
    task_name = task_names[[i]],
    params = list(
      geo = geojson,
      dates = list(list(
        startDate = sprintf("01-01-%d", years[[1]]),
        endDate = sprintf("12-31-%d", years[[2]])
      )),
      layers = layers,
      output = list(
        format = list(type = "geotiff", filename_date = "calendar"),
        projection = "geographic"
      )
    )
  )
})

for (i in seq_along(payloads)) {
  write_json(
    payloads[[i]],
    file.path(request_dir, paste0(task_names[[i]], ".json")),
    auto_unbox = TRUE,
    pretty = TRUE,
    digits = NA
  )
}

status <- data.frame(
  task_name = task_names,
  LTER = lter,
  Stream_Name = stream_name,
  Shapefile_Name = shapefile_name,
  source_file = watershed_file,
  source_area_km2 = source_area_km2,
  request_area_km2 = roundtrip_area_km2,
  request_area_error_pct = roundtrip_error_pct,
  request_vertices = nrow(st_coordinates(request_geometry)),
  start_year = vapply(periods, `[[`, integer(1), 1L),
  end_year = vapply(periods, `[[`, integer(1), 2L),
  drivers = paste(requested_drivers, collapse = ","),
  action = if (submit_tasks) "pending_submission" else "dry_run",
  task_id = NA_character_,
  status = NA_character_,
  http_status = NA_integer_,
  stringsAsFactors = FALSE
)
status_path <- file.path(status_dir, paste0(task_prefix, ".rds"))
saveRDS(status, status_path)

if (!submit_tasks) {
  cat("Validated ", nrow(status), " request(s) for ",
      round(source_area_km2, 3), " km2.\n", sep = "")
  quit(save = "no", status = 0L)
}

token_file <- require_input_file(token_file, "AppEEARS token file")
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
authorization <- paste("Bearer", auth$token)
api_root <- "https://appeears.earthdatacloud.nasa.gov/api"

task_response <- RETRY(
  "GET",
  paste0(api_root, "/task"),
  query = list(limit = 1000, offset = 0),
  add_headers(Authorization = authorization),
  times = 5,
  pause_base = 2,
  terminate_on = c(400, 401, 403)
)
stop_for_status(task_response)
task_list <- fromJSON(
  content(task_response, as = "text", encoding = "UTF-8"),
  simplifyDataFrame = TRUE
)
existing_names <- if (is.data.frame(task_list) &&
    "task_name" %in% names(task_list)) task_list$task_name else character()

for (i in seq_along(payloads)) {
  existing_index <- match(task_names[[i]], existing_names)
  if (!is.na(existing_index)) {
    status$action[[i]] <- "existing_task"
    status$task_id[[i]] <- task_list$task_id[[existing_index]]
    status$status[[i]] <- task_list$status[[existing_index]]
    status$http_status[[i]] <- 200L
  } else {
    response <- RETRY(
      "POST",
      paste0(api_root, "/task"),
      body = payloads[[i]],
      encode = "json",
      add_headers(
        Authorization = authorization,
        `Content-Type` = "application/json"
      ),
      times = 5,
      pause_base = 2,
      terminate_on = c(400, 401, 403, 413)
    )
    status$http_status[[i]] <- status_code(response)
    response_text <- content(response, as = "text", encoding = "UTF-8")
    if (status_code(response) < 200L || status_code(response) >= 300L) {
      status$action[[i]] <- "submission_failed"
      status$status[[i]] <- "error"
      saveRDS(status, status_path)
      stop(
        "AppEEARS submission failed for ", task_names[[i]], ": HTTP ",
        status_code(response), " ", substr(response_text, 1, 500),
        call. = FALSE
      )
    }
    submitted <- fromJSON(response_text, simplifyVector = FALSE)
    status$action[[i]] <- "submitted"
    status$task_id[[i]] <- submitted$task_id
    status$status[[i]] <- if (is.null(submitted$status)) {
      "pending"
    } else {
      submitted$status
    }
  }
  saveRDS(status, status_path)
}

cat("Submitted or matched ", nrow(status), " request(s) for ",
    round(source_area_km2, 3), " km2.\n", sep = "")
for (i in seq_len(nrow(status))) {
  cat(status$task_name[[i]], ": ", status$task_id[[i]], " [",
      status$status[[i]], "]\n", sep = "")
}
