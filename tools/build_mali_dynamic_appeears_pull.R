#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(sf)
  library(jsonlite)
})

source(file.path(getwd(), "tools", "workflow_paths.R"))

args <- commandArgs(trailingOnly = TRUE)
run_date <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else format(Sys.Date(), "%Y%m%d")
watershed_override <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else ""

out_dir <- file.path(
  getwd(),
  "generated_outputs",
  "rerun",
  paste0(run_date, "_mali_niger_dynamic_appeears")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

old_files <- list.files(
  out_dir,
  pattern = "^(appeears_task_.*\\.json|appeears_task_manifest_.*\\.json|.*_aoi_.*\\.geojson|.*_targets_.*\\.csv|.*\\.sh|README\\.md)$",
  full.names = TRUE
)
if (length(old_files)) {
  unlink(old_files)
}

default_active_inventory <- "/private/tmp/final_active_watershed_inventory_20260522/silica-watersheds_20260522_final-active-inventory.shp"
watershed_path <- if (nzchar(watershed_override)) {
  normalizePath(watershed_override, mustWork = TRUE)
} else if (file.exists(default_active_inventory)) {
  normalizePath(default_active_inventory, mustWork = TRUE)
} else {
  root_path <- resolve_silica_data_root()
  silica_watershed_file(root_path)
}

read_watersheds <- function(path) {
  x <- sf::st_read(path, quiet = TRUE)
  if ("shp_nm" %in% names(x)) {
    names(x)[names(x) == "shp_nm"] <- "Shapefile_Name"
  }
  if ("Strm_Nm" %in% names(x)) {
    names(x)[names(x) == "Strm_Nm"] <- "Stream_Name"
  }
  if ("Dsc_F_N" %in% names(x)) {
    names(x)[names(x) == "Dsc_F_N"] <- "Discharge_File_Name"
  }
  x
}

clean_chr <- function(x) {
  trimws(as.character(x))
}

make_aoi_geojson <- function(x, group_key) {
  x <- sf::st_transform(x, 4326)
  x <- sf::st_make_valid(x)
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  if (!nrow(x)) {
    stop("No polygon geometry remained after cleaning AOI for ", group_key, call. = FALSE)
  }

  geom <- suppressWarnings(sf::st_union(x))
  geom <- suppressWarnings(sf::st_make_valid(geom))
  geom <- suppressWarnings(sf::st_collection_extract(geom, "POLYGON"))
  geom <- geom[!sf::st_is_empty(geom)]
  if (!length(geom)) {
    stop("AOI union did not produce polygon geometry for ", group_key, call. = FALSE)
  }
  if (any(!sf::st_is_valid(geom))) {
    stop("AOI union produced invalid polygon geometry for ", group_key, call. = FALSE)
  }

  aoi <- sf::st_sf(aoi_name = group_key, geometry = geom)
  out <- file.path(out_dir, paste0(group_key, "_aoi_", run_date, ".geojson"))
  sf::st_write(aoi, out, delete_dsn = TRUE, quiet = TRUE)
  out
}

build_payload <- function(group_key, key, window_label, start, end, layers, geo_obj) {
  task_name <- paste(group_key, key, "v061", window_label, run_date, sep = "-")
  payload <- list(
    task_type = "area",
    task_name = task_name,
    params = list(
      dates = list(list(startDate = start, endDate = end)),
      layers = layers,
      output = list(
        format = list(type = "geotiff"),
        projection = "geographic"
      ),
      geo = geo_obj
    )
  )

  json_path <- file.path(out_dir, paste0("appeears_task_", task_name, ".json"))
  jsonlite::write_json(payload, json_path, auto_unbox = TRUE, pretty = TRUE)
  list(
    task_name = task_name,
    key = key,
    start = start,
    end = end,
    json_path = json_path
  )
}

sheds <- read_watersheds(watershed_path)
if (!all(c("LTER", "Shapefile_Name") %in% names(sheds))) {
  stop("Watershed file must include LTER and Shapefile_Name columns: ", watershed_path, call. = FALSE)
}

mali <- sheds[
  clean_chr(sheds$LTER) == "Mali" &
    clean_chr(sheds$Shapefile_Name) == "Niger_Bamako",
]

if (nrow(mali) != 1) {
  stop("Expected exactly one Mali/Niger_Bamako polygon, found ", nrow(mali), call. = FALSE)
}

if (!"Stream_Name" %in% names(mali)) {
  mali$Stream_Name <- "Niger"
}
if (!"Discharge_File_Name" %in% names(mali)) {
  mali$Discharge_File_Name <- "NigerRiver_Q"
}
mali$Stream_Name[is.na(mali$Stream_Name) | !nzchar(clean_chr(mali$Stream_Name))] <- "Niger"
mali$Discharge_File_Name[is.na(mali$Discharge_File_Name) | !nzchar(clean_chr(mali$Discharge_File_Name))] <- "NigerRiver_Q"

valid_mali <- sf::st_make_valid(sf::st_transform(mali, 4326))
valid_area_km2 <- as.numeric(sf::st_area(sf::st_transform(valid_mali, 6933))) / 1e6
message("Using Mali/Niger_Bamako AOI from: ", watershed_path)
message("Valid AOI area km2: ", round(valid_area_km2, 1))

group_key <- "mali_niger_dynamic"
aoi_geojson <- make_aoi_geojson(mali, group_key)
geo_obj <- jsonlite::fromJSON(aoi_geojson, simplifyVector = FALSE)

target_df <- sf::st_drop_geometry(
  mali[, intersect(c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name"), names(mali))]
)
target_csv <- file.path(out_dir, paste0(group_key, "_targets_", run_date, ".csv"))
utils::write.csv(target_df, target_csv, row.names = FALSE, na = "")

evapo_windows <- list(
  c("2002", "2006"),
  c("2007", "2011"),
  c("2012", "2016"),
  c("2017", "2021"),
  c("2022", "2025")
)

product_specs <- c(
  lapply(evapo_windows, function(win) {
    list(
      key = "evapo",
      window_label = paste(win, collapse = "-"),
      start = paste0("01-01-", win[[1]]),
      end = paste0("12-31-", win[[2]]),
      destination = "/home/shares/lter-si/si-watershed-extract/raw-driver-data/raw-evapo-v061/mali",
      layers = list(list(product = "MOD16A2GF.061", layer = "ET_500m"))
    )
  }),
  list(
    list(
      key = "greenup",
      window_label = "2002-2024",
      start = "01-01-2002",
      end = "12-31-2024",
      destination = "/home/shares/lter-si/si-watershed-extract/raw-driver-data/raw-greenup-v061/mali",
      layers = list(list(product = "MCD12Q2.061", layer = "Greenup"))
    ),
    list(
      key = "npp",
      window_label = "2002-2025",
      start = "01-01-2002",
      end = "12-31-2025",
      destination = "/home/shares/lter-si/si-watershed-extract/raw-driver-data/raw-npp-v061/mali",
      layers = list(list(product = "MOD17A3HGF.061", layer = "Npp_500m"))
    )
  )
)

manifest <- list()
for (spec in product_specs) {
  task <- build_payload(
    group_key = group_key,
    key = spec$key,
    window_label = spec$window_label,
    start = spec$start,
    end = spec$end,
    layers = spec$layers,
    geo_obj = geo_obj
  )
  task$destination <- spec$destination
  task$aoi_geojson <- aoi_geojson
  task$targets_csv <- target_csv
  manifest[[length(manifest) + 1]] <- task
}

manifest_path <- file.path(out_dir, paste0("appeears_task_manifest_", run_date, ".json"))
jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)

submit_script <- file.path(out_dir, "submit_appeears_tasks.sh")
submit_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "SCRIPT_DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"",
  "cd \"${SCRIPT_DIR}\"",
  "TOKEN_JSON=\"${APPEEARS_LOGIN_JSON:-/tmp/appeears_login.json}\"",
  "APPEEARS_TOKEN=\"${APPEEARS_TOKEN:-}\"",
  "if [[ -z \"${APPEEARS_TOKEN}\" && -s \"${TOKEN_JSON}\" ]]; then",
  "  APPEEARS_TOKEN=\"$(Rscript -e 'x <- jsonlite::fromJSON(commandArgs(TRUE)[1]); if (!is.null(x$token)) cat(x$token) else quit(status = 1)' \"${TOKEN_JSON}\" | tr -d '\\r\\n')\"",
  "fi",
  "if [[ -z \"${APPEEARS_TOKEN}\" ]]; then",
  "  echo \"Set APPEEARS_TOKEN or refresh /tmp/appeears_login.json.\" >&2",
  "  exit 1",
  "fi",
  "",
  "API_ROOT=\"https://appeears.earthdatacloud.nasa.gov/api\"",
  "RESPONSES=\"appeears_submit_responses.jsonl\"",
  paste0("TASK_CSV=\"appeears_submitted_tasks_", run_date, ".csv\""),
  "touch \"${RESPONSES}\"",
  "if [[ ! -s \"${TASK_CSV}\" ]]; then",
  "  printf 'group,task_name,key,start,end,destination,task_id,status\\n' > \"${TASK_CSV}\"",
  "fi",
  ""
)

for (spec in manifest) {
  submit_lines <- c(
    submit_lines,
    paste0("if grep -Fq '\"", spec$task_name, "\"' \"${TASK_CSV}\"; then"),
    paste0("  echo 'Skipping already submitted ", spec$task_name, "'"),
    "else",
    paste0("echo 'Submitting ", spec$task_name, "'"),
    "response=\"$(curl -sS --fail-with-body -X POST \"${API_ROOT}/task\" \\",
    "  -H \"Authorization: Bearer ${APPEEARS_TOKEN}\" \\",
    "  -H \"Content-Type: application/json\" \\",
    paste0("  --data @\"", basename(spec$json_path), "\")\""),
    "printf '%s\\n' \"${response}\" | tee -a \"${RESPONSES}\"",
    "task_id=\"$(printf '%s' \"${response}\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); cat(if (!is.null(x$task_id)) x$task_id else \"\")')\"",
    "status=\"$(printf '%s' \"${response}\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); cat(if (!is.null(x$status)) x$status else \"\")')\"",
    "if [[ -z \"${task_id}\" ]]; then",
    "  echo \"No task_id returned for response: ${response}\" >&2",
    "  exit 1",
    "fi",
    paste0(
      "printf '\"mali_niger_dynamic\",\"", spec$task_name, "\",\"", spec$key, "\",\"",
      spec$start, "\",\"", spec$end, "\",\"", spec$destination,
      "\",\"%s\",\"%s\"\\n' \"${task_id}\" \"${status}\" >> \"${TASK_CSV}\""
    ),
    "fi",
    ""
  )
}
submit_lines <- c(submit_lines, "echo \"WROTE:${TASK_CSV}\"")
writeLines(submit_lines, submit_script)
Sys.chmod(submit_script, mode = "0755")

token_script <- file.path(out_dir, "refresh_appeears_token.sh")
token_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "EDL_USER=\"${EDL_USER:-sidneyabush}\"",
  "TOKEN_JSON=\"${APPEEARS_LOGIN_JSON:-/tmp/appeears_login.json}\"",
  "read -s -p \"Earthdata password for ${EDL_USER}: \" EDL_PASS",
  "printf '\\n'",
  "curl -sS --fail-with-body \\",
  "  --request POST \\",
  "  --user \"${EDL_USER}:${EDL_PASS}\" \\",
  "  --header \"Content-Length: 0\" \\",
  "  \"https://appeears.earthdatacloud.nasa.gov/api/login\" \\",
  "  -o \"${TOKEN_JSON}\"",
  "unset EDL_PASS",
  "Rscript -e 'x <- jsonlite::fromJSON(commandArgs(TRUE)[1]); print(x[names(x) != \"token\"]); cat(\"token_chars=\", nchar(x$token), \"\\n\", sep=\"\")' \"${TOKEN_JSON}\""
)
writeLines(token_lines, token_script)
Sys.chmod(token_script, mode = "0755")

status_script <- file.path(out_dir, "check_task_status.sh")
status_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "SCRIPT_DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"",
  "cd \"${SCRIPT_DIR}\"",
  paste0("TASK_CSV=\"appeears_submitted_tasks_", run_date, ".csv\""),
  "TOKEN_JSON=\"${APPEEARS_LOGIN_JSON:-/tmp/appeears_login.json}\"",
  "APPEEARS_TOKEN=\"${APPEEARS_TOKEN:-}\"",
  "if [[ -z \"${APPEEARS_TOKEN}\" && -s \"${TOKEN_JSON}\" ]]; then",
  "  APPEEARS_TOKEN=\"$(Rscript -e 'x <- jsonlite::fromJSON(commandArgs(TRUE)[1]); if (!is.null(x$token)) cat(x$token) else quit(status = 1)' \"${TOKEN_JSON}\" | tr -d '\\r\\n')\"",
  "fi",
  "if [[ -z \"${APPEEARS_TOKEN}\" ]]; then",
  "  echo \"Set APPEEARS_TOKEN or refresh /tmp/appeears_login.json.\" >&2",
  "  exit 1",
  "fi",
  "if [[ ! -s \"${TASK_CSV}\" ]]; then",
  "  echo \"Missing submitted task CSV: ${TASK_CSV}\" >&2",
  "  exit 1",
  "fi",
  "API_ROOT=\"https://appeears.earthdatacloud.nasa.gov/api\"",
  "OUT_CSV=\"appeears_task_status_$(date +%Y%m%d_%H%M%S).csv\"",
  "printf 'task_name,task_id,status,response_json\\n' > \"${OUT_CSV}\"",
  "tail -n +2 \"${TASK_CSV}\" | while IFS=, read -r group task_name key start end destination task_id submitted_status; do",
  "  task_name=\"${task_name%\\\"}\"; task_name=\"${task_name#\\\"}\"",
  "  task_id=\"${task_id%\\\"}\"; task_id=\"${task_id#\\\"}\"",
  "  [[ -z \"${task_id}\" ]] && continue",
  "  response=\"$(curl -sS --fail-with-body -H \"Authorization: Bearer ${APPEEARS_TOKEN}\" \"${API_ROOT}/task/${task_id}\")\"",
  "  status=\"$(printf '%s' \"${response}\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); cat(if (!is.null(x$status)) x$status else \"\")')\"",
  "  escaped_response=\"$(printf '%s' \"${response}\" | python3 -c 'import csv,sys; csv.writer(sys.stdout).writerow([sys.stdin.read()])' | sed 's/,$//')\"",
  "  printf '\"%s\",\"%s\",\"%s\",%s\\n' \"${task_name}\" \"${task_id}\" \"${status}\" \"${escaped_response}\" >> \"${OUT_CSV}\"",
  "  printf '%s %s %s\\n' \"${status}\" \"${task_id}\" \"${task_name}\"",
  "done",
  "echo \"WROTE:${OUT_CSV}\""
)
writeLines(status_lines, status_script)
Sys.chmod(status_script, mode = "0755")

download_script <- file.path(out_dir, "download_product_rasters_only.sh")
download_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "SCRIPT_DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"",
  "cd \"${SCRIPT_DIR}\"",
  paste0("TASK_CSV=\"appeears_submitted_tasks_", run_date, ".csv\""),
  paste0("DOWNLOAD_ROOT=\"${APPEEARS_DOWNLOAD_ROOT:-/private/tmp/appeears_downloads_mali_", run_date, "}\""),
  "TOKEN_JSON=\"${APPEEARS_LOGIN_JSON:-/tmp/appeears_login.json}\"",
  "APPEEARS_TOKEN=\"${APPEEARS_TOKEN:-}\"",
  "if [[ -z \"${APPEEARS_TOKEN}\" && -s \"${TOKEN_JSON}\" ]]; then",
  "  APPEEARS_TOKEN=\"$(Rscript -e 'x <- jsonlite::fromJSON(commandArgs(TRUE)[1]); if (!is.null(x$token)) cat(x$token) else quit(status = 1)' \"${TOKEN_JSON}\" | tr -d '\\r\\n')\"",
  "fi",
  "if [[ -z \"${APPEEARS_TOKEN}\" ]]; then",
  "  echo \"Set APPEEARS_TOKEN or refresh /tmp/appeears_login.json.\" >&2",
  "  exit 1",
  "fi",
  "if [[ ! -s \"${TASK_CSV}\" ]]; then",
  "  echo \"Missing submitted task CSV: ${TASK_CSV}\" >&2",
  "  exit 1",
  "fi",
  "API_ROOT=\"https://appeears.earthdatacloud.nasa.gov/api\"",
  "mkdir -p \"${DOWNLOAD_ROOT}\"",
  "PRODUCT_MANIFEST=\"${DOWNLOAD_ROOT}/product_raster_manifest.tsv\"",
  ": > \"${PRODUCT_MANIFEST}\"",
  "Rscript -e '",
  "  x <- read.csv(commandArgs(TRUE)[1], stringsAsFactors = FALSE, check.names = FALSE)",
  "  for (i in seq_len(nrow(x))) {",
  "    cat(x$task_name[i], x$task_id[i], x$key[i], x$destination[i], sep = \"\\t\")",
  "    cat(\"\\n\")",
  "  }",
  "' \"${TASK_CSV}\" | while IFS=$'\\t' read -r task_name task_id key destination; do",
  "  [[ -z \"${task_id}\" ]] && continue",
  "  task_json=\"$(curl -sS --fail-with-body -H \"Authorization: Bearer ${APPEEARS_TOKEN}\" \"${API_ROOT}/task/${task_id}\")\"",
  "  status=\"$(printf '%s' \"${task_json}\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); cat(if (!is.null(x$status)) x$status else \"\")')\"",
  "  if [[ \"${status}\" != \"done\" ]]; then",
  "    printf 'SKIP status=%s task=%s\\n' \"${status}\" \"${task_name}\"",
  "    continue",
  "  fi",
  "  task_dir=\"${DOWNLOAD_ROOT}/${task_name}\"",
  "  mkdir -p \"${task_dir}\"",
  "  printf 'CHECK task=%s key=%s\\n' \"${task_name}\" \"${key}\"",
  "  bundle_json=\"$(curl -sS --fail-with-body -H \"Authorization: Bearer ${APPEEARS_TOKEN}\" \"${API_ROOT}/bundle/${task_id}\")\"",
  "  printf '%s' \"${bundle_json}\" | TASK_KEY=\"${key}\" Rscript -e '",
  "    key <- Sys.getenv(\"TASK_KEY\")",
  "    x <- jsonlite::fromJSON(file(\"stdin\"))",
  "    files <- x$files",
  "    if (!nrow(files)) quit(status = 0)",
  "    pattern <- switch(",
  "      key,",
  "      npp = \"MOD17A3HGF[.]061_Npp_500m\",",
  "      greenup = \"MCD12Q2[.]061_Greenup_\",",
  "      evapo = \"MOD16A2GF[.]061_ET_500m\",",
  "      \"\"",
  "    )",
  "    if (!nzchar(pattern)) quit(status = 0)",
  "    files <- files[files$file_type == \"tif\" & grepl(pattern, files$file_name), c(\"file_id\", \"file_name\", \"file_size\", \"sha256\"), drop = FALSE]",
  "    for (i in seq_len(nrow(files))) {",
  "      cat(files$file_id[i], files$file_name[i], files$file_size[i], files$sha256[i], sep = \"\\t\")",
  "      cat(\"\\n\")",
  "    }",
  "  ' | while IFS=$'\\t' read -r file_id file_name file_size sha256; do",
  "    out_file=\"${task_dir}/${file_name}\"",
  "    mkdir -p \"$(dirname \"${out_file}\")\"",
  "    actual_size=0",
  "    if [[ -s \"${out_file}\" ]]; then actual_size=\"$(wc -c < \"${out_file}\" | tr -d '[:space:]')\"; fi",
  "    if [[ \"${actual_size}\" == \"${file_size}\" ]]; then",
  "      printf '%s\\t%s\\t%s\\t%s\\t%s\\t%s\\n' \"${task_name}\" \"${key}\" \"${destination}\" \"${file_name}\" \"${file_size}\" \"present\" >> \"${PRODUCT_MANIFEST}\"",
  "      continue",
  "    fi",
  "    if [[ \"${actual_size}\" != \"0\" ]]; then",
  "      printf '  redownload size_mismatch expected=%s actual=%s %s\\n' \"${file_size}\" \"${actual_size}\" \"${file_name}\"",
  "    else",
  "      printf '  get %s\\n' \"${file_name}\"",
  "    fi",
  "    tmp_file=\"${out_file}.part.$$\"",
  "    curl -L -sS --fail-with-body -H \"Authorization: Bearer ${APPEEARS_TOKEN}\" \"${API_ROOT}/bundle/${task_id}/${file_id}\" -o \"${tmp_file}\"",
  "    new_size=\"$(wc -c < \"${tmp_file}\" | tr -d '[:space:]')\"",
  "    if [[ \"${new_size}\" != \"${file_size}\" ]]; then",
  "      rm -f \"${tmp_file}\"",
  "      printf 'ERROR size_mismatch_after_download expected=%s actual=%s file=%s\\n' \"${file_size}\" \"${new_size}\" \"${file_name}\" >&2",
  "      exit 1",
  "    fi",
  "    mv \"${tmp_file}\" \"${out_file}\"",
  "    printf '%s\\t%s\\t%s\\t%s\\t%s\\t%s\\n' \"${task_name}\" \"${key}\" \"${destination}\" \"${file_name}\" \"${file_size}\" \"downloaded\" >> \"${PRODUCT_MANIFEST}\"",
  "  done",
  "done",
  "echo \"PRODUCT_MANIFEST=${PRODUCT_MANIFEST}\"",
  "echo \"DOWNLOAD_ROOT=${DOWNLOAD_ROOT}\""
)
writeLines(download_lines, download_script)
Sys.chmod(download_script, mode = "0755")

flatten_script <- file.path(out_dir, "stage_product_rasters_flat.sh")
flatten_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  paste0("DOWNLOAD_ROOT=\"${APPEEARS_DOWNLOAD_ROOT:-/private/tmp/appeears_downloads_mali_", run_date, "}\""),
  paste0("FLAT_ROOT=\"${FLAT_ROOT:-/private/tmp/appeears_product_rasters_flat_mali_", run_date, "}\""),
  "PRODUCT_MANIFEST=\"${DOWNLOAD_ROOT}/product_raster_manifest.tsv\"",
  "if [[ ! -s \"${PRODUCT_MANIFEST}\" ]]; then",
  "  echo \"Missing product raster manifest: ${PRODUCT_MANIFEST}\" >&2",
  "  exit 1",
  "fi",
  "mkdir -p \"${FLAT_ROOT}\"",
  "while IFS=$'\\t' read -r task_name key destination file_name file_size state; do",
  "  [[ -z \"${task_name}\" ]] && continue",
  "  rel=\"${destination#*/raw-driver-data/}\"",
  "  src=\"${DOWNLOAD_ROOT}/${task_name}/${file_name}\"",
  "  dst_dir=\"${FLAT_ROOT}/${rel}\"",
  "  mkdir -p \"${dst_dir}\"",
  "  cp \"${src}\" \"${dst_dir}/${task_name}__$(basename \"${file_name}\")\"",
  "done < \"${PRODUCT_MANIFEST}\"",
  "echo \"FLAT_ROOT=${FLAT_ROOT}\"",
  "find \"${FLAT_ROOT}\" -type f | sed \"s#${FLAT_ROOT}/##\" | sort | wc -l | tr -d '[:space:]'",
  "echo"
)
writeLines(flatten_lines, flatten_script)
Sys.chmod(flatten_script, mode = "0755")

upload_script <- file.path(out_dir, "upload_flat_product_rasters_to_aurora.sh")
upload_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  paste0("FLAT_ROOT=\"${1:-/private/tmp/appeears_product_rasters_flat_mali_", run_date, "}\""),
  "AURORA_HOST=\"${AURORA_HOST:-bush@aurora.nceas.ucsb.edu}\"",
  "REMOTE_DATA_ROOT=\"/home/shares/lter-si/si-watershed-extract/raw-driver-data\"",
  "paths=(",
  "  \"raw-evapo-v061/mali\"",
  "  \"raw-greenup-v061/mali\"",
  "  \"raw-npp-v061/mali\"",
  ")",
  "count_local() {",
  "  local rel=\"$1\"",
  "  local pattern=\"$2\"",
  "  find \"${FLAT_ROOT}/${rel}\" -maxdepth 1 -type f -name \"${pattern}\" 2>/dev/null | wc -l | tr -d '[:space:]'",
  "}",
  "if [[ ! -d \"${FLAT_ROOT}\" ]]; then",
  "  echo \"Missing flat raster staging directory: ${FLAT_ROOT}\" >&2",
  "  exit 1",
  "fi",
  "for rel in \"${paths[@]}\"; do",
  "  if [[ ! -d \"${FLAT_ROOT}/${rel}\" ]]; then",
  "    echo \"Missing expected staging subdirectory: ${FLAT_ROOT}/${rel}\" >&2",
  "    exit 1",
  "  fi",
  "done",
  "echo \"Local Mali product raster counts:\"",
  "printf '  raw-evapo-v061/mali ET: %s\\n' \"$(count_local raw-evapo-v061/mali '*__*MOD16A2GF.061_ET_500m*.tif')\"",
  "printf '  raw-greenup-v061/mali Greenup: %s\\n' \"$(count_local raw-greenup-v061/mali '*__*MCD12Q2.061_Greenup_*.tif')\"",
  "printf '  raw-npp-v061/mali NPP: %s\\n' \"$(count_local raw-npp-v061/mali '*__*MOD17A3HGF.061_Npp_500m*.tif')\"",
  "echo",
  "echo \"Uploading Mali product rasters to ${AURORA_HOST}:${REMOTE_DATA_ROOT}\"",
  "(",
  "  cd \"${FLAT_ROOT}\"",
  "  find \"${paths[@]}\" -type f -print | tar -cf - -T -",
  ") | ssh \"${AURORA_HOST}\" '",
  "  set -e",
  "  REMOTE_DATA_ROOT=\"/home/shares/lter-si/si-watershed-extract/raw-driver-data\"",
  "  mkdir -p \\",
  "    \"${REMOTE_DATA_ROOT}/raw-evapo-v061/mali\" \\",
  "    \"${REMOTE_DATA_ROOT}/raw-greenup-v061/mali\" \\",
  "    \"${REMOTE_DATA_ROOT}/raw-npp-v061/mali\"",
  "  cd \"${REMOTE_DATA_ROOT}\"",
  "  tar --no-overwrite-dir --no-same-owner --no-same-permissions -xf -",
  "  echo",
  "  echo \"Remote Mali product raster counts:\"",
  "  printf \"  raw-evapo-v061/mali ET: \"",
  "  find raw-evapo-v061/mali -maxdepth 1 -type f -name \"*__*MOD16A2GF.061_ET_500m*.tif\" | wc -l",
  "  printf \"  raw-greenup-v061/mali Greenup: \"",
  "  find raw-greenup-v061/mali -maxdepth 1 -type f -name \"*__*MCD12Q2.061_Greenup_*.tif\" | wc -l",
  "  printf \"  raw-npp-v061/mali NPP: \"",
  "  find raw-npp-v061/mali -maxdepth 1 -type f -name \"*__*MOD17A3HGF.061_Npp_500m*.tif\" | wc -l",
  "'",
  "echo",
  "echo \"Upload complete. Keep ${FLAT_ROOT} and ${DOWNLOAD_ROOT:-/private/tmp} until Aurora extraction passes.\""
)
writeLines(upload_lines, upload_script)
Sys.chmod(upload_script, mode = "0755")

readme <- c(
  "# Mali/Niger Dynamic AppEEARS Pull",
  "",
  "Purpose: backfill the only remaining non-known full-dataset dynamic gap.",
  "",
  "Target:",
  "- Mali / Niger / Niger_Bamako",
  "- AOI is repaired with st_make_valid before writing the AppEEARS GeoJSON.",
  paste0("- Repaired AOI area: ", round(valid_area_km2, 1), " km2."),
  "",
  "Task windows:",
  "- MOD16A2GF.061 ET_500m: 2002-2025, split into five chunks.",
  "- MCD12Q2.061 Greenup: 2002-2024.",
  "- MOD17A3HGF.061 Npp_500m: 2002-2025.",
  "",
  "After tasks are done:",
  "1. If needed, run refresh_appeears_token.sh.",
  "2. Run submit_appeears_tasks.sh.",
  "3. Run check_task_status.sh until all tasks are done.",
  "4. Run download_product_rasters_only.sh.",
  "5. Run stage_product_rasters_flat.sh.",
  "6. Run upload_flat_product_rasters_to_aurora.sh.",
  "7. Run generated_outputs/rerun/run_final_v4_mali_dynamic_on_aurora_20260523.sh.",
  "",
  "Snow is intentionally not requested for this site. The final domain-fix step zeros Mali snow as a no-snow domain."
)
writeLines(readme, file.path(out_dir, "README.md"))

message("Wrote Mali/Niger AppEEARS helper files to: ", out_dir)
message("Manifest: ", manifest_path)
