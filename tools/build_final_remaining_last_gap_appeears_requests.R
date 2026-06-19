librarian::shelf(dplyr, sf, jsonlite)

env_or_default <- function(env_name, default_value) {
  value <- trimws(Sys.getenv(env_name, unset = ""))
  if (nzchar(value)) value else default_value
}

norm_blank <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "nan")] <- NA_character_
  x
}

clean_key <- function(x) {
  x <- tolower(norm_blank(x))
  gsub("[^a-z0-9]+", "", x)
}

clean_tag <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("(^-+|-+$)", "", x)
}

contiguous_year_chunks <- function(years, max_span = 5L) {
  years <- sort(unique(as.integer(years[!is.na(years)])))
  if (!length(years)) return(list())

  runs <- split(years, cumsum(c(TRUE, diff(years) != 1L)))
  out <- list()
  for (run in runs) {
    starts <- seq(1L, length(run), by = max_span)
    for (start in starts) {
      out[[length(out) + 1L]] <- run[start:min(start + max_span - 1L, length(run))]
    }
  }
  out
}

read_watersheds <- function(path) {
  x <- sf::st_read(path, quiet = TRUE)
  if ("shp_nm" %in% names(x)) names(x)[names(x) == "shp_nm"] <- "Shapefile_Name"
  if ("Strm_Nm" %in% names(x)) names(x)[names(x) == "Strm_Nm"] <- "Stream_Name"
  if ("Dsc_F_N" %in% names(x)) names(x)[names(x) == "Dsc_F_N"] <- "Discharge_File_Name"

  needed <- c("LTER", "Shapefile_Name")
  missing <- setdiff(needed, names(x))
  if (length(missing)) {
    stop("Watershed file is missing fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  x %>%
    mutate(
      LTER = norm_blank(LTER),
      Shapefile_Name = norm_blank(Shapefile_Name),
      .LTER_KEY = clean_key(LTER),
      .SHP_KEY = clean_key(Shapefile_Name)
    )
}

make_padded_bbox_geojson <- function(x, group_key, out_dir, date_tag, pad_degrees) {
  x <- sf::st_transform(x, 4326)
  x <- sf::st_make_valid(x)
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  if (!nrow(x)) {
    stop("No polygon geometry remained for ", group_key, call. = FALSE)
  }

  bbox <- sf::st_bbox(x)
  padded <- c(
    xmin = max(-180, as.numeric(bbox["xmin"]) - pad_degrees),
    ymin = max(-90, as.numeric(bbox["ymin"]) - pad_degrees),
    xmax = min(180, as.numeric(bbox["xmax"]) + pad_degrees),
    ymax = min(90, as.numeric(bbox["ymax"]) + pad_degrees)
  )
  geom <- sf::st_as_sfc(sf::st_bbox(padded, crs = sf::st_crs(x)))
  aoi <- sf::st_sf(aoi_name = group_key, geometry = geom)

  out <- file.path(out_dir, paste0(group_key, "_aoi_", date_tag, ".geojson"))
  sf::st_write(aoi, out, delete_dsn = TRUE, quiet = TRUE)
  out
}

write_target_csv <- function(x, group_key, out_dir, date_tag) {
  target_cols <- intersect(
    c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name", "Active_Shapefile_Name"),
    names(x)
  )
  target_df <- sf::st_drop_geometry(x[, target_cols])
  target_csv <- file.path(out_dir, paste0(group_key, "_targets_", date_tag, ".csv"))
  utils::write.csv(target_df, target_csv, row.names = FALSE, na = "")
  target_csv
}

product_specs <- list(
  greenup = list(
    product = "MCD12Q2.061",
    layer = "Greenup",
    raw_dir = "raw-greenup-v061",
    max_span = 5L
  ),
  snow = list(
    product = "MOD10A2.061",
    layer = "Eight_Day_Snow_Cover",
    raw_dir = "raw-snow-v061",
    max_span = 5L
  )
)

driver_shapefiles <- list(
  greenup = "/private/tmp/final_remaining_dynamic_20260526/site-coordinates-greenup/silica-watersheds_20260526_final-v4-remaining-dynamic-20260526_greenup.shp",
  snow = "/private/tmp/final_remaining_dynamic_20260526/site-coordinates-snow/silica-watersheds_20260526_final-v4-remaining-dynamic-20260526_snow.shp"
)

date_tag <- env_or_default("SILICA_LAST_GAP_DATE", "20260605")
audit_file <- env_or_default(
  "SILICA_LAST_GAP_AUDIT_FILE",
  file.path(
    getwd(),
    "generated_outputs",
    "review",
    "harmonization",
    "final-20260605",
    "final_remaining_corrected_staging_manifest_fill_audit_20260605.csv"
  )
)
out_dir <- env_or_default(
  "SILICA_LAST_GAP_APPEEARS_OUT_DIR",
  file.path(
    getwd(),
    "generated_outputs",
    "rerun",
    "appeears-requests",
    paste0(date_tag, "_final_remaining_dynamic_appeears_last_gaps")
  )
)
remote_raw_root <- env_or_default(
  "SILICA_REMOTE_RAW_ROOT",
  "/home/shares/lter-si/si-watershed-extract/raw-driver-data"
)
download_root <- env_or_default(
  "APPEEARS_DOWNLOAD_ROOT",
  "/private/tmp/appeears_downloads_20260526"
)
flat_root <- env_or_default(
  "FLAT_ROOT",
  "/private/tmp/appeears_product_rasters_flat_20260526_final_remaining"
)
pad_degrees <- as.numeric(env_or_default("SILICA_LAST_GAP_AOI_PAD_DEGREES", "0.05"))
if (!is.finite(pad_degrees) || pad_degrees <= 0) {
  stop("SILICA_LAST_GAP_AOI_PAD_DEGREES must be positive.", call. = FALSE)
}

if (!file.exists(audit_file)) stop("Missing corrected staging audit: ", audit_file, call. = FALSE)
missing_shapefiles <- driver_shapefiles[!file.exists(unlist(driver_shapefiles))]
if (length(missing_shapefiles)) {
  stop("Missing driver shapefile(s): ", paste(unlist(missing_shapefiles), collapse = ", "), call. = FALSE)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
old_files <- list.files(
  out_dir,
  pattern = "^(appeears_task_.*\\.json|appeears_task_manifest_.*\\.json|appeears_submitted_tasks_.*\\.csv|.*_aoi_.*\\.geojson|.*_targets_.*\\.csv|last_gap_targets_.*\\.csv|refresh_appeears_token\\.sh|submit_appeears_tasks\\.sh|check_task_status\\.sh|download_product_rasters_only\\.sh|stage_upload_product_rasters_only\\.sh|README\\.md)$",
  full.names = TRUE
)
if (length(old_files)) unlink(old_files)

audit <- read.csv(audit_file, stringsAsFactors = FALSE, check.names = FALSE)
required_cols <- c(
  "driver", "region", "LTER", "Stream_Name", "Shapefile_Name",
  "Active_Shapefile_Name", "year", "current_rows", "present"
)
missing_cols <- setdiff(required_cols, names(audit))
if (length(missing_cols)) {
  stop("Audit file is missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

targets <- audit %>%
  mutate(
    present_flag = tolower(as.character(present)) %in% c("true", "1", "yes"),
    driver = norm_blank(driver),
    region = norm_blank(region),
    LTER = norm_blank(LTER),
    Stream_Name = norm_blank(Stream_Name),
    Shapefile_Name = norm_blank(Shapefile_Name),
    Active_Shapefile_Name = norm_blank(Active_Shapefile_Name),
    Active_Shapefile_Name = dplyr::coalesce(Active_Shapefile_Name, Shapefile_Name),
    year = suppressWarnings(as.integer(year)),
    current_rows = suppressWarnings(as.integer(current_rows)),
    .LTER_KEY = clean_key(LTER),
    .ACTIVE_SHP_KEY = clean_key(Active_Shapefile_Name)
  ) %>%
  filter(!present_flag, current_rows > 0L) %>%
  filter(driver %in% names(product_specs)) %>%
  filter(!is.na(region), !is.na(LTER), !is.na(Active_Shapefile_Name), !is.na(year))

if (!nrow(targets)) {
  stop("No missing greenup/snow rows found in corrected staging audit.", call. = FALSE)
}

write.csv(
  targets %>%
    select(driver, region, LTER, Stream_Name, Shapefile_Name, Active_Shapefile_Name, year),
  file.path(out_dir, paste0("last_gap_targets_", date_tag, ".csv")),
  row.names = FALSE,
  na = ""
)

watersheds_by_driver <- lapply(driver_shapefiles, read_watersheds)

manifest <- list()
group_summary <- list()

site_targets <- targets %>%
  distinct(driver, region, LTER, Stream_Name, Shapefile_Name, Active_Shapefile_Name, .LTER_KEY, .ACTIVE_SHP_KEY) %>%
  arrange(driver, region, LTER, Active_Shapefile_Name)

for (i in seq_len(nrow(site_targets))) {
  site <- site_targets[i, , drop = FALSE]
  driver <- site$driver[[1]]
  region <- site$region[[1]]
  spec <- product_specs[[driver]]

  site_years <- targets %>%
    filter(
      driver == !!driver,
      region == !!region,
      .LTER_KEY == site$.LTER_KEY[[1]],
      .ACTIVE_SHP_KEY == site$.ACTIVE_SHP_KEY[[1]]
    ) %>%
    pull(year)

  ws <- watersheds_by_driver[[driver]] %>%
    filter(
      .LTER_KEY == site$.LTER_KEY[[1]],
      .SHP_KEY == site$.ACTIVE_SHP_KEY[[1]]
    )

  if (!nrow(ws)) {
    stop(
      "Missing watershed geometry for ",
      driver, " ", site$LTER[[1]], " ", site$Active_Shapefile_Name[[1]],
      call. = FALSE
    )
  }

  ws$Stream_Name <- site$Stream_Name[[1]]
  ws$Discharge_File_Name <- if ("Discharge_File_Name" %in% names(ws)) ws$Discharge_File_Name else NA_character_
  ws$Shapefile_Name <- site$Shapefile_Name[[1]]
  ws$Active_Shapefile_Name <- site$Active_Shapefile_Name[[1]]

  group_key <- paste(
    "last-gap",
    clean_tag(region),
    clean_tag(driver),
    clean_tag(site$LTER[[1]]),
    clean_tag(site$Active_Shapefile_Name[[1]]),
    sep = "-"
  )
  aoi_geojson <- make_padded_bbox_geojson(ws, group_key, out_dir, date_tag, pad_degrees)
  targets_csv <- write_target_csv(ws, group_key, out_dir, date_tag)
  geo_obj <- jsonlite::fromJSON(aoi_geojson, simplifyVector = FALSE)

  for (chunk in contiguous_year_chunks(site_years, max_span = spec$max_span)) {
    window_label <- if (length(chunk) == 1L) as.character(chunk[[1]]) else paste0(min(chunk), "-", max(chunk))
    task_name <- paste(group_key, "v061", window_label, date_tag, sep = "-")
    payload <- list(
      task_type = "area",
      task_name = task_name,
      params = list(
        dates = list(list(
          startDate = sprintf("01-01-%s", min(chunk)),
          endDate = sprintf("12-31-%s", max(chunk))
        )),
        layers = list(list(product = spec$product, layer = spec$layer)),
        output = list(
          format = list(type = "geotiff"),
          projection = "geographic"
        ),
        geo = geo_obj
      )
    )

    json_path <- file.path(out_dir, paste0("appeears_task_", task_name, ".json"))
    jsonlite::write_json(payload, json_path, auto_unbox = TRUE, pretty = TRUE)

    manifest[[length(manifest) + 1L]] <- list(
      group = group_key,
      task_name = task_name,
      key = driver,
      dynamic_region = region,
      years = paste(chunk, collapse = ","),
      start = sprintf("01-01-%s", min(chunk)),
      end = sprintf("12-31-%s", max(chunk)),
      destination = file.path(remote_raw_root, spec$raw_dir, region),
      json_path = json_path,
      aoi_geojson = aoi_geojson,
      targets_csv = targets_csv,
      n_sites = 1L,
      aoi_pad_degrees = pad_degrees
    )
  }

  group_summary[[length(group_summary) + 1L]] <- data.frame(
    dynamic_region = region,
    driver = driver,
    LTER = site$LTER[[1]],
    Stream_Name = site$Stream_Name[[1]],
    Shapefile_Name = site$Shapefile_Name[[1]],
    Active_Shapefile_Name = site$Active_Shapefile_Name[[1]],
    years = paste(sort(unique(as.integer(site_years))), collapse = ","),
    targets_csv = basename(targets_csv),
    aoi_geojson = basename(aoi_geojson),
    stringsAsFactors = FALSE
  )
}

manifest_path <- file.path(out_dir, paste0("appeears_task_manifest_", date_tag, ".json"))
jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)
submitted_csv <- paste0("appeears_submitted_tasks_", date_tag, ".csv")

submit_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "SCRIPT_DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"",
  "cd \"$SCRIPT_DIR\"",
  "API_ROOT=\"https://appeears.earthdatacloud.nasa.gov/api\"",
  paste0("MANIFEST=\"appeears_task_manifest_", date_tag, ".json\""),
  "RESPONSES=\"appeears_submit_responses.jsonl\"",
  paste0("TASK_CSV=\"", submitted_csv, "\""),
  "",
  "if [[ -z \"${APPEEARS_TOKEN:-}\" && -f /tmp/appeears_login.json ]]; then",
  "  export APPEEARS_TOKEN=\"$(Rscript -e 'x <- jsonlite::fromJSON(\"/tmp/appeears_login.json\"); if (!is.null(x$token)) cat(x$token) else quit(status=1)' | tr -d '\\r\\n')\"",
  "fi",
  "",
  "if [[ -z \"${APPEEARS_TOKEN:-}\" ]]; then",
  "  echo \"Set APPEEARS_TOKEN before running.\" >&2",
  "  exit 1",
  "fi",
  "",
  ": > \"$RESPONSES\"",
  "printf 'group,task_name,key,dynamic_region,years,start,end,destination,task_id,status\\n' > \"$TASK_CSV\"",
  "",
  "MANIFEST_PATH=\"$MANIFEST\" Rscript -e 'x <- jsonlite::fromJSON(Sys.getenv(\"MANIFEST_PATH\"), simplifyVector=FALSE); for (s in x) cat(s$group, \"\\t\", s$task_name, \"\\t\", s$key, \"\\t\", s$dynamic_region, \"\\t\", s$years, \"\\t\", s$start, \"\\t\", s$end, \"\\t\", s$destination, \"\\t\", basename(s$json_path), \"\\n\", sep=\"\")' |",
  "while IFS=$'\\t' read -r group task_name key dynamic_region years start end destination json_base; do",
  "  [[ -z \"$task_name\" ]] && continue",
  "  echo \"Submitting ${task_name}\"",
  "  response=\"$(curl -sS --fail-with-body -X POST \"$API_ROOT/task\" -H \"Authorization: Bearer $APPEEARS_TOKEN\" -H \"Content-Type: application/json\" --data @\"$json_base\")\"",
  "  printf '%s\\n' \"$response\" | tee -a \"$RESPONSES\"",
  "  task_id=\"$(printf '%s' \"$response\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); if (!is.null(x$task_id)) cat(x$task_id)')\"",
  "  status=\"$(printf '%s' \"$response\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); if (!is.null(x$status)) cat(x$status)')\"",
  "  printf '\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"\\n' \"$group\" \"$task_name\" \"$key\" \"$dynamic_region\" \"$years\" \"$start\" \"$end\" \"$destination\" \"$task_id\" \"$status\" >> \"$TASK_CSV\"",
  "done"
)
writeLines(submit_lines, file.path(out_dir, "submit_appeears_tasks.sh"))
Sys.chmod(file.path(out_dir, "submit_appeears_tasks.sh"), mode = "0755")

refresh_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "USER_NAME=\"${EARTHDATA_USERNAME:-sidneyabush}\"",
  "TOKEN_JSON=\"${APPEEARS_LOGIN_JSON:-/tmp/appeears_login.json}\"",
  "",
  "read -s -p \"Earthdata password for ${USER_NAME}: \" EDL_PASS",
  "echo",
  "",
  "curl -sS --fail-with-body \\",
  "  --request POST \\",
  "  --user \"${USER_NAME}:${EDL_PASS}\" \\",
  "  --header \"Content-Length: 0\" \\",
  "  \"https://appeears.earthdatacloud.nasa.gov/api/login\" \\",
  "  -o \"$TOKEN_JSON\"",
  "",
  "unset EDL_PASS",
  "",
  "cat \"$TOKEN_JSON\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); cat(\"token_chars=\", nchar(x$token), \"\\n\", sep=\"\")'",
  "export APPEEARS_TOKEN=\"$(cat \"$TOKEN_JSON\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); if (!is.null(x$token)) cat(x$token) else quit(status=1)' | tr -d '\\r\\n')\"",
  "echo \"Token saved to $TOKEN_JSON.\""
)
writeLines(refresh_lines, file.path(out_dir, "refresh_appeears_token.sh"))
Sys.chmod(file.path(out_dir, "refresh_appeears_token.sh"), mode = "0755")

status_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "SCRIPT_DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"",
  "cd \"$SCRIPT_DIR\"",
  paste0("TASK_CSV=\"${TASK_CSV:-", submitted_csv, "}\""),
  "OUT_CSV=\"appeears_task_status_$(date +%Y%m%d_%H%M%S).csv\"",
  "API_ROOT=\"https://appeears.earthdatacloud.nasa.gov/api\"",
  "",
  "if [[ -z \"${APPEEARS_TOKEN:-}\" && -f /tmp/appeears_login.json ]]; then",
  "  export APPEEARS_TOKEN=\"$(Rscript -e 'x <- jsonlite::fromJSON(\"/tmp/appeears_login.json\"); if (!is.null(x$token)) cat(x$token) else quit(status=1)' | tr -d '\\r\\n')\"",
  "fi",
  "",
  "if [[ -z \"${APPEEARS_TOKEN:-}\" ]]; then",
  "  echo \"Set APPEEARS_TOKEN before running.\" >&2",
  "  exit 1",
  "fi",
  "",
  "printf 'task_name,task_id,status,response_json\\n' > \"$OUT_CSV\"",
  "TASK_CSV_PATH=\"$TASK_CSV\" Rscript -e 'x <- read.csv(Sys.getenv(\"TASK_CSV_PATH\"), stringsAsFactors=FALSE); for (i in seq_len(nrow(x))) cat(x$task_name[i], \"\\t\", x$task_id[i], \"\\n\", sep=\"\")' |",
  "while IFS=$'\\t' read -r task_name task_id; do",
  "  [[ -z \"$task_id\" ]] && continue",
  "  response=\"$(curl -sS --fail-with-body -H \"Authorization: Bearer $APPEEARS_TOKEN\" \"$API_ROOT/task/$task_id\")\"",
  "  status=\"$(printf '%s' \"$response\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); if (!is.null(x$status)) cat(x$status)')\"",
  "  escaped_response=\"$(printf '%s' \"$response\" | Rscript -e 'cat(jsonlite::toJSON(jsonlite::fromJSON(file(\"stdin\")), auto_unbox=TRUE))')\"",
  "  printf '\"%s\",\"%s\",\"%s\",%s\\n' \"$task_name\" \"$task_id\" \"$status\" \"$escaped_response\" >> \"$OUT_CSV\"",
  "  printf '%s %s %s\\n' \"$status\" \"$task_id\" \"$task_name\"",
  "done",
  "echo \"WROTE:$OUT_CSV\""
)
writeLines(status_lines, file.path(out_dir, "check_task_status.sh"))
Sys.chmod(file.path(out_dir, "check_task_status.sh"), mode = "0755")

download_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "SCRIPT_DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"",
  "cd \"$SCRIPT_DIR\"",
  paste0("TASK_CSV=\"${TASK_CSV:-", submitted_csv, "}\""),
  paste0("DOWNLOAD_ROOT=\"${APPEEARS_DOWNLOAD_ROOT:-", download_root, "}\""),
  paste0("PRODUCT_MANIFEST=\"${DOWNLOAD_ROOT}/product_raster_manifest_last_gaps_", date_tag, ".tsv\""),
  "API_ROOT=\"https://appeears.earthdatacloud.nasa.gov/api\"",
  "",
  "if [[ -z \"${APPEEARS_TOKEN:-}\" && -f /tmp/appeears_login.json ]]; then",
  "  export APPEEARS_TOKEN=\"$(Rscript -e 'x <- jsonlite::fromJSON(\"/tmp/appeears_login.json\"); if (!is.null(x$token)) cat(x$token) else quit(status=1)' | tr -d '\\r\\n')\"",
  "fi",
  "",
  "if [[ -z \"${APPEEARS_TOKEN:-}\" ]]; then",
  "  echo \"Set APPEEARS_TOKEN before running.\" >&2",
  "  exit 1",
  "fi",
  "",
  "mkdir -p \"$DOWNLOAD_ROOT\"",
  "printf 'task_name\\tkey\\tdestination\\tfile_name\\tfile_size\\tstatus\\n' > \"$PRODUCT_MANIFEST\"",
  "",
  "TASK_CSV_PATH=\"$TASK_CSV\" Rscript -e 'x <- read.csv(Sys.getenv(\"TASK_CSV_PATH\"), stringsAsFactors=FALSE); for (i in seq_len(nrow(x))) cat(x$task_name[i], \"\\t\", x$task_id[i], \"\\t\", x$key[i], \"\\t\", x$destination[i], \"\\n\", sep=\"\")' | while IFS=$'\\t' read -r task_name task_id key destination; do",
  "  [[ -z \"$task_id\" ]] && continue",
  "  task_json=\"$(curl -sS --fail-with-body -H \"Authorization: Bearer $APPEEARS_TOKEN\" \"$API_ROOT/task/$task_id\")\"",
  "  status=\"$(printf '%s' \"$task_json\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); if (!is.null(x$status)) cat(x$status)')\"",
  "  if [[ \"$status\" != \"done\" ]]; then",
  "    printf 'SKIP status=%s task=%s\\n' \"$status\" \"$task_name\"",
  "    continue",
  "  fi",
  "  task_dir=\"$DOWNLOAD_ROOT/$task_name\"",
  "  mkdir -p \"$task_dir\"",
  "  bundle_json=\"$(curl -sS --fail-with-body -H \"Authorization: Bearer $APPEEARS_TOKEN\" \"$API_ROOT/bundle/$task_id\")\"",
  "  printf '%s' \"$bundle_json\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); files <- x$files; if (is.null(files) || !nrow(files)) quit(status=0); for (i in seq_len(nrow(files))) cat(files$file_id[i], \"\\t\", files$file_name[i], \"\\t\", files$file_size[i], \"\\n\", sep=\"\")' | while IFS=$'\\t' read -r file_id file_name file_size; do",
  "    case \"$key\" in",
  "      greenup) pattern='MCD12Q2.061_Greenup_' ;;",
  "      snow) pattern='MOD10A2.061_Eight_Day_Snow_Cover' ;;",
  "      *) pattern='' ;;",
  "    esac",
  "    [[ \"$file_name\" != *$pattern* ]] && continue",
  "    out_file=\"$task_dir/$(basename \"$file_name\")\"",
  "    if [[ -s \"$out_file\" ]]; then",
  "      printf 'exists %s\\n' \"$out_file\"",
  "      printf '%s\\t%s\\t%s\\t%s\\t%s\\tpresent\\n' \"$task_name\" \"$key\" \"$destination\" \"$file_name\" \"$file_size\" >> \"$PRODUCT_MANIFEST\"",
  "      continue",
  "    fi",
  "    printf 'get %s\\n' \"$file_name\"",
  "    curl -sS --fail-with-body -L -H \"Authorization: Bearer $APPEEARS_TOKEN\" \"$API_ROOT/bundle/$task_id/$file_id\" -o \"$out_file\"",
  "    printf '%s\\t%s\\t%s\\t%s\\t%s\\tdownloaded\\n' \"$task_name\" \"$key\" \"$destination\" \"$file_name\" \"$file_size\" >> \"$PRODUCT_MANIFEST\"",
  "  done",
  "done",
  "echo \"DOWNLOAD_ROOT=$DOWNLOAD_ROOT\"",
  "echo \"PRODUCT_MANIFEST=$PRODUCT_MANIFEST\""
)
writeLines(download_lines, file.path(out_dir, "download_product_rasters_only.sh"))
Sys.chmod(file.path(out_dir, "download_product_rasters_only.sh"), mode = "0755")

stage_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "SCRIPT_DIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" && pwd)\"",
  "cd \"$SCRIPT_DIR\"",
  paste0("TASK_CSV=\"${TASK_CSV:-", submitted_csv, "}\""),
  paste0("DOWNLOAD_ROOT=\"${APPEEARS_DOWNLOAD_ROOT:-", download_root, "}\""),
  paste0("FLAT_ROOT=\"${FLAT_ROOT:-", flat_root, "}\""),
  "AURORA_HOST=\"${AURORA_HOST:-bush@aurora.nceas.ucsb.edu}\"",
  "REMOTE_DATA_ROOT=\"/home/shares/lter-si/si-watershed-extract/raw-driver-data\"",
  "SSH_CONTROL_PATH=\"/tmp/silica-appeears-lastgap-%C\"",
  "SSH_OPTS=(-o ControlMaster=auto -o ControlPath=\"${SSH_CONTROL_PATH}\" -o ControlPersist=30m)",
  "STAGED_MANIFEST=\"$(mktemp /tmp/appeears-last-gap-staged.XXXXXX)\"",
  "",
  "cleanup() {",
  "  ssh \"${SSH_OPTS[@]}\" -O exit \"${AURORA_HOST}\" >/dev/null 2>&1 || true",
  "  rm -f \"$STAGED_MANIFEST\"",
  "}",
  "trap cleanup EXIT",
  "",
  "if [[ ! -d \"$DOWNLOAD_ROOT\" ]]; then",
  "  echo \"Missing download directory: $DOWNLOAD_ROOT\" >&2",
  "  exit 1",
  "fi",
  "if [[ ! -s \"$TASK_CSV\" ]]; then",
  "  echo \"Missing submitted-task CSV: $TASK_CSV\" >&2",
  "  exit 1",
  "fi",
  "",
  "mkdir -p \"$FLAT_ROOT\"",
  ": > \"$STAGED_MANIFEST\"",
  "",
  "stage_one_task() {",
  "  local task_name=\"$1\"",
  "  local key=\"$2\"",
  "  local destination=\"$3\"",
  "  local rel=\"${destination#*/raw-driver-data/}\"",
  "  local task_dir=\"$DOWNLOAD_ROOT/$task_name\"",
  "  local pattern",
  "",
  "  case \"$key\" in",
  "    greenup) pattern=\"MCD12Q2.061_Greenup_*.tif\" ;;",
  "    snow) pattern=\"MOD10A2.061_Eight_Day_Snow_Cover*.tif\" ;;",
  "    *)",
  "      echo \"Unknown product key for $task_name: $key\" >&2",
  "      exit 1",
  "      ;;",
  "  esac",
  "",
  "  if [[ ! -d \"$task_dir\" ]]; then",
  "    echo \"WARN missing downloaded task directory: $task_dir\" >&2",
  "    return 0",
  "  fi",
  "",
  "  shopt -s nullglob",
  "  local files=(\"$task_dir\"/$pattern)",
  "  shopt -u nullglob",
  "",
  "  if [[ \"${#files[@]}\" -eq 0 ]]; then",
  "    echo \"WARN no matching product rasters for $task_name ($pattern); skipping empty AppEEARS bundle\" >&2",
  "    return 0",
  "  fi",
  "",
  "  mkdir -p \"$FLAT_ROOT/$rel\"",
  "  for src in \"${files[@]}\"; do",
  "    local rel_path=\"$rel/${task_name}__$(basename \"$src\")\"",
  "    cp -p \"$src\" \"$FLAT_ROOT/$rel_path\"",
  "    printf '%s\\n' \"$rel_path\" >> \"$STAGED_MANIFEST\"",
  "  done",
  "}",
  "",
  "echo \"Staging last-gap product rasters from $DOWNLOAD_ROOT\"",
  "TASK_CSV_PATH=\"$TASK_CSV\" Rscript -e 'x <- read.csv(Sys.getenv(\"TASK_CSV_PATH\"), stringsAsFactors=FALSE); for (i in seq_len(nrow(x))) cat(x$task_name[i], \"\\t\", x$key[i], \"\\t\", x$destination[i], \"\\n\", sep=\"\")' |",
  "while IFS=$'\\t' read -r task_name key destination; do",
  "  [[ -z \"$task_name\" ]] && continue",
  "  stage_one_task \"$task_name\" \"$key\" \"$destination\"",
  "done",
  "",
  "sort -u \"$STAGED_MANIFEST\" -o \"$STAGED_MANIFEST\"",
  "staged_count=\"$(wc -l < \"$STAGED_MANIFEST\" | tr -d ' ')\"",
  "if [[ \"$staged_count\" -eq 0 ]]; then",
  "  echo \"No last-gap product rasters were staged.\" >&2",
  "  exit 1",
  "fi",
  "",
  "echo",
  "echo \"Local last-gap staged raster counts:\"",
  "awk -F/ '{ key=$1 \"/\" $2; count[key]++ } END { for (key in count) printf \"  %s: %s\\n\", key, count[key] }' \"$STAGED_MANIFEST\" | sort",
  "",
  "echo",
  "echo \"Uploading $staged_count last-gap product rasters to ${AURORA_HOST}:${REMOTE_DATA_ROOT}\"",
  "echo \"Opening one reusable SSH connection to ${AURORA_HOST}\"",
  "ssh -MNf \"${SSH_OPTS[@]}\" \"${AURORA_HOST}\"",
  "",
  "(",
  "  cd \"$FLAT_ROOT\"",
  "  tar -cf - -T \"$STAGED_MANIFEST\"",
  ") | ssh \"${SSH_OPTS[@]}\" \"${AURORA_HOST}\" '",
  "  set -e",
  "  REMOTE_DATA_ROOT=\"/home/shares/lter-si/si-watershed-extract/raw-driver-data\"",
  "  cd \"${REMOTE_DATA_ROOT}\"",
  "  tar --no-overwrite-dir --no-same-owner --no-same-permissions -xf -",
  "'",
  "",
  "echo",
  "echo \"Upload complete. Keep $DOWNLOAD_ROOT and $FLAT_ROOT until the final audits pass.\""
)
writeLines(stage_lines, file.path(out_dir, "stage_upload_product_rasters_only.sh"))
Sys.chmod(file.path(out_dir, "stage_upload_product_rasters_only.sh"), mode = "0755")

group_summary_df <- bind_rows(group_summary)
write.csv(group_summary_df, file.path(out_dir, paste0("appeears_request_groups_", date_tag, ".csv")), row.names = FALSE, na = "")

readme <- c(
  "# Final Remaining Last-Gap AppEEARS Requests",
  "",
  "This folder contains only the still-missing corrected-staging rows after the local greenup, npp, and snow rebuild.",
  "",
  "The AOIs are one padded bbox per watershed. The padding is intentional because the remaining watersheds are small relative to the 500 m products and previous tight AOIs returned empty or sparse product rasters.",
  "",
  "Run order:",
  "1. `bash refresh_appeears_token.sh` if `/tmp/appeears_login.json` is expired.",
  "2. `bash submit_appeears_tasks.sh`",
  "3. `bash check_task_status.sh` until all tasks are `done`.",
  "4. `bash download_product_rasters_only.sh`",
  "5. `bash stage_upload_product_rasters_only.sh`",
  "6. Back in the repo, rerun `Rscript tools/rebuild_final_remaining_dynamic_patch_locally.R` and the corrected staging audit.",
  "",
  paste0("Default download root: `", download_root, "`."),
  paste0("Default flat raster root: `", flat_root, "`.")
)
writeLines(readme, file.path(out_dir, "README.md"))

cat("WROTE:", out_dir, "\n", sep = "")
cat("WROTE:", manifest_path, "\n", sep = "")
cat("AOI pad degrees:", pad_degrees, "\n")
cat("request_groups=", nrow(group_summary_df), "\n", sep = "")
