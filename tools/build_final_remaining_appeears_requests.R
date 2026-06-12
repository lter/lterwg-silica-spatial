#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(jsonlite)
})

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

read_loose_csv <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- make.unique(ifelse(is.na(names(x)) | trimws(names(x)) == "", "blank_col", names(x)))
  x
}

read_watersheds <- function(path) {
  x <- sf::st_read(path, quiet = TRUE)
  if ("shp_nm" %in% names(x)) names(x)[names(x) == "shp_nm"] <- "Shapefile_Name"
  if ("Strm_Nm" %in% names(x)) names(x)[names(x) == "Strm_Nm"] <- "Stream_Name"
  if ("Dsc_F_N" %in% names(x)) names(x)[names(x) == "Dsc_F_N"] <- "Discharge_File_Name"
  x
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

make_aoi_geojson <- function(x, group_key, out_dir, date_tag, aoi_mode = "exact") {
  x <- sf::st_transform(x, 4326)
  x <- sf::st_make_valid(x)
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  if (!nrow(x)) {
    stop("No polygon geometry remained for ", group_key, call. = FALSE)
  }

  if (identical(aoi_mode, "bbox")) {
    bbox_geoms <- lapply(sf::st_geometry(x), function(g) {
      sf::st_as_sfc(sf::st_bbox(g), crs = sf::st_crs(x))[[1]]
    })
    aoi <- sf::st_sf(
      aoi_name = paste0(group_key, "-", seq_along(bbox_geoms)),
      geometry = sf::st_sfc(bbox_geoms, crs = sf::st_crs(x))
    )
  } else {
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
  }

  out <- file.path(out_dir, paste0(group_key, "_aoi_", date_tag, ".geojson"))
  sf::st_write(aoi, out, delete_dsn = TRUE, quiet = TRUE)
  out
}

write_target_csv <- function(x, group_key, out_dir, date_tag) {
  target_cols <- intersect(c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name"), names(x))
  target_df <- sf::st_drop_geometry(x[, target_cols])
  target_csv <- file.path(out_dir, paste0(group_key, "_targets_", date_tag, ".csv"))
  utils::write.csv(target_df, target_csv, row.names = FALSE, na = "")
  target_csv
}

product_specs <- list(
  evapo = list(
    product = "MOD16A2GF.061",
    layer = "ET_500m",
    raw_dir = "raw-evapo-v061",
    max_span = 5L
  ),
  greenup = list(
    product = "MCD12Q2.061",
    layer = "Greenup",
    raw_dir = "raw-greenup-v061",
    max_span = 5L
  ),
  npp = list(
    product = "MOD17A3HGF.061",
    layer = "Npp_500m",
    raw_dir = "raw-npp-v061",
    max_span = 5L
  ),
  snow = list(
    product = "MOD10A2.061",
    layer = "Eight_Day_Snow_Cover",
    raw_dir = "raw-snow-v061",
    max_span = 5L
  )
)

active_shapefile_aliases <- tibble::tribble(
  ~LTER, ~Shapefile_Name, ~active_Shapefile_Name,
  "LMP", "LMP_hydrosheds_5", "lmp_hydrosheds_nor27"
)

date_tag <- env_or_default("SILICA_APPEEARS_DATE", format(Sys.Date(), "%Y%m%d"))
gap_file <- env_or_default(
  "SILICA_GAP_ACTION_FILE",
  file.path(
    "/private/tmp/final_20260526_final-extract-merge-v4-year-fill-repatched-domain-copy_readiness",
    "final-extract-merge-v4-year-fill-repatched-domain-copy_final_run_gap_actions_20260526.csv"
  )
)
watershed_file <- env_or_default(
  "SILICA_WATERSHED_FILE",
  "/private/tmp/final_active_watershed_inventory_20260522/silica-watersheds_20260522_final-active-inventory.shp"
)
out_dir <- env_or_default(
  "SILICA_APPEEARS_OUT_DIR",
  file.path(
    getwd(),
    "generated_outputs",
    "rerun",
    "appeears-requests",
    paste0(date_tag, "_final_remaining_dynamic_appeears")
  )
)
remote_raw_root <- env_or_default(
  "SILICA_REMOTE_RAW_ROOT",
  "/home/shares/lter-si/si-watershed-extract/raw-driver-data"
)
max_sites_per_aoi <- as.integer(env_or_default("SILICA_MAX_APPEEARS_SITES_PER_AOI", "4"))
if (is.na(max_sites_per_aoi) || max_sites_per_aoi < 1L) {
  stop("SILICA_MAX_APPEEARS_SITES_PER_AOI must be a positive integer.", call. = FALSE)
}
aoi_mode <- env_or_default("SILICA_APPEEARS_AOI_MODE", "exact")
if (!aoi_mode %in% c("exact", "bbox")) {
  stop("SILICA_APPEEARS_AOI_MODE must be 'exact' or 'bbox'.", call. = FALSE)
}

if (!file.exists(gap_file)) stop("Missing gap action file: ", gap_file, call. = FALSE)
if (!file.exists(watershed_file)) stop("Missing watershed file: ", watershed_file, call. = FALSE)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
old_files <- list.files(
  out_dir,
  pattern = "^(appeears_task_.*\\.json|appeears_task_manifest_.*\\.json|appeears_submitted_tasks_.*\\.csv|.*_aoi_.*\\.geojson|.*_targets_.*\\.csv|targets_missing_active_watershed_.*\\.csv|refresh_appeears_token\\.sh|submit_appeears_tasks\\.sh|check_task_status\\.sh|download_product_rasters_only\\.sh|README\\.md)$",
  full.names = TRUE
)
if (length(old_files)) unlink(old_files)

gap_actions <- read_loose_csv(gap_file)
required_cols <- c(
  "blocker_class", "dynamic_region", "driver", "year",
  "matched_LTER", "matched_Stream_Name", "matched_Discharge_File_Name", "matched_Shapefile_Name"
)
missing_cols <- setdiff(required_cols, names(gap_actions))
if (length(missing_cols)) {
  stop("Gap action file is missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

targets <- gap_actions %>%
  filter(blocker_class == "extract_or_merge_missing") %>%
  filter(driver %in% names(product_specs)) %>%
  mutate(
    LTER = norm_blank(matched_LTER),
    Stream_Name = norm_blank(matched_Stream_Name),
    Discharge_File_Name = norm_blank(matched_Discharge_File_Name),
    Shapefile_Name = norm_blank(matched_Shapefile_Name),
    dynamic_region = norm_blank(dynamic_region),
    .target_id = paste(clean_key(LTER), clean_key(Shapefile_Name), sep = "||")
  ) %>%
  filter(!is.na(LTER), !is.na(Shapefile_Name), !is.na(dynamic_region)) %>%
  left_join(active_shapefile_aliases, by = c("LTER", "Shapefile_Name")) %>%
  mutate(
    active_Shapefile_Name = dplyr::coalesce(active_Shapefile_Name, Shapefile_Name),
    .active_target_id = paste(clean_key(LTER), clean_key(active_Shapefile_Name), sep = "||")
  )

if (!nrow(targets)) {
  stop("No extract_or_merge_missing AppEEARS targets found in ", gap_file, call. = FALSE)
}

watersheds <- read_watersheds(watershed_file) %>%
  mutate(
    Shapefile_Name = norm_blank(Shapefile_Name),
    LTER = norm_blank(LTER),
    .target_id = paste(clean_key(LTER), clean_key(Shapefile_Name), sep = "||")
  )

target_meta <- targets %>%
  distinct(
    dynamic_region, driver, LTER, Stream_Name, Discharge_File_Name,
    Shapefile_Name, active_Shapefile_Name, .target_id, .active_target_id
  )

missing_watersheds <- target_meta %>%
  anti_join(sf::st_drop_geometry(watersheds) %>% distinct(.target_id), by = c(".active_target_id" = ".target_id")) %>%
  arrange(dynamic_region, driver, LTER, Shapefile_Name)

if (nrow(missing_watersheds)) {
  write.csv(
    missing_watersheds,
    file.path(out_dir, paste0("targets_missing_active_watershed_", date_tag, ".csv")),
    row.names = FALSE,
    na = ""
  )
  stop("Some AppEEARS targets are missing from the active watershed inventory. See targets_missing_active_watershed_", date_tag, ".csv", call. = FALSE)
}

manifest <- list()
group_summary <- list()

for (region in sort(unique(targets$dynamic_region))) {
  for (driver in sort(unique(targets$driver[targets$dynamic_region == region]))) {
    group_targets_all <- targets %>%
      filter(dynamic_region == region, driver == !!driver) %>%
      distinct(LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, active_Shapefile_Name, .target_id, .active_target_id) %>%
      arrange(LTER, Shapefile_Name, Stream_Name)

    group_years <- targets %>%
      filter(dynamic_region == region, driver == !!driver) %>%
      pull(year)

    batch_id <- ceiling(seq_len(nrow(group_targets_all)) / max_sites_per_aoi)
    batches <- split(group_targets_all, batch_id)

    for (batch_index in seq_along(batches)) {
      group_targets <- batches[[batch_index]]
      group_key_base <- paste("final-gap", clean_tag(region), clean_tag(driver), sep = "-")
      group_key <- if (length(batches) > 1L) {
        paste0(group_key_base, "-part", sprintf("%02d", batch_index))
      } else {
        group_key_base
      }

      group_ws <- watersheds %>%
        semi_join(group_targets %>% select(.active_target_id), by = c(".target_id" = ".active_target_id")) %>%
        left_join(
          group_targets %>% select(.active_target_id, Stream_Name, Discharge_File_Name),
          by = c(".target_id" = ".active_target_id"),
          suffix = c("", ".target")
        )

      if (!"Stream_Name" %in% names(group_ws)) group_ws$Stream_Name <- NA_character_
      if (!"Discharge_File_Name" %in% names(group_ws)) group_ws$Discharge_File_Name <- NA_character_
      if ("Stream_Name.target" %in% names(group_ws)) {
        group_ws$Stream_Name <- dplyr::coalesce(group_ws$Stream_Name.target, group_ws$Stream_Name)
      }
      if ("Discharge_File_Name.target" %in% names(group_ws)) {
        group_ws$Discharge_File_Name <- dplyr::coalesce(group_ws$Discharge_File_Name.target, group_ws$Discharge_File_Name)
      }
      group_ws <- group_ws %>% select(-matches("\\.target$"))

      aoi_geojson <- make_aoi_geojson(group_ws, group_key, out_dir, date_tag, aoi_mode = aoi_mode)
      targets_csv <- write_target_csv(group_ws, group_key, out_dir, date_tag)
      geo_obj <- jsonlite::fromJSON(aoi_geojson, simplifyVector = FALSE)
      spec <- product_specs[[driver]]
      chunks <- contiguous_year_chunks(group_years, max_span = spec$max_span)

      for (chunk in chunks) {
        window_label <- if (length(chunk) == 1L) {
          as.character(chunk[[1]])
        } else {
          paste0(min(chunk), "-", max(chunk))
        }
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
          n_sites = nrow(group_targets)
        )
      }

      group_summary[[length(group_summary) + 1L]] <- data.frame(
        dynamic_region = region,
        driver = driver,
        batch = if (length(batches) > 1L) sprintf("%02d", batch_index) else "",
        n_sites = nrow(group_targets),
        years = paste(sort(unique(as.integer(group_years))), collapse = ","),
        targets_csv = basename(targets_csv),
        aoi_geojson = basename(aoi_geojson),
        stringsAsFactors = FALSE
      )
    }
  }
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
  ""
)

for (spec in manifest) {
  submit_lines <- c(
    submit_lines,
    paste0("echo 'Submitting ", spec$task_name, "'"),
    paste0(
      "response=\"$(curl -sS --fail-with-body -X POST \"$API_ROOT/task\" ",
      "-H \"Authorization: Bearer $APPEEARS_TOKEN\" ",
      "-H \"Content-Type: application/json\" ",
      "--data @\"",
      basename(spec$json_path),
      "\")\""
    ),
    "printf '%s\\n' \"$response\" | tee -a \"$RESPONSES\"",
    "task_id=\"$(printf '%s' \"$response\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); if (!is.null(x$task_id)) cat(x$task_id)')\"",
    "status=\"$(printf '%s' \"$response\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); if (!is.null(x$status)) cat(x$status)')\"",
    paste0(
      "printf '\"",
      spec$group, "\",\"",
      spec$task_name, "\",\"",
      spec$key, "\",\"",
      spec$dynamic_region, "\",\"",
      spec$years, "\",\"",
      spec$start, "\",\"",
      spec$end, "\",\"",
      spec$destination,
      "\",\"%s\",\"%s\"\\n' \"$task_id\" \"$status\" >> \"$TASK_CSV\""
    ),
    ""
  )
}

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
  "cat \"$TOKEN_JSON\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); print(x[names(x) != \"token\"]); cat(\"token_chars=\", nchar(x$token), \"\\n\", sep=\"\")'",
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
  "TASK_CSV_PATH=\"$TASK_CSV\" Rscript -e 'x <- read.csv(Sys.getenv(\"TASK_CSV_PATH\"), stringsAsFactors=FALSE); for (i in seq_len(nrow(x))) cat(x$task_name[i], x$task_id[i], sep=\"\\t\", \"\\n\")' |",
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
  paste0("DOWNLOAD_ROOT=\"${APPEEARS_DOWNLOAD_ROOT:-/private/tmp/appeears_downloads_", date_tag, "}\""),
  "PRODUCT_MANIFEST=\"${DOWNLOAD_ROOT}/product_raster_manifest.tsv\"",
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
  "TASK_CSV_PATH=\"$TASK_CSV\" Rscript -e 'x <- read.csv(Sys.getenv(\"TASK_CSV_PATH\"), stringsAsFactors=FALSE); for (i in seq_len(nrow(x))) cat(x$task_name[i], x$task_id[i], x$key[i], x$destination[i], sep=\"\\t\", \"\\n\")' | while IFS=$'\\t' read -r task_name task_id key destination; do",
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
  "  printf '%s' \"$bundle_json\" | Rscript -e 'x <- jsonlite::fromJSON(file(\"stdin\")); files <- x$files; if (is.null(files) || !nrow(files)) quit(status=0); for (i in seq_len(nrow(files))) cat(files$file_id[i], files$file_name[i], files$file_size[i], sep=\"\\t\", \"\\n\")' | while IFS=$'\\t' read -r file_id file_name file_size; do",
  "    case \"$key\" in",
  "      evapo) pattern='MOD16A2GF.061_ET_500m' ;;",
  "      greenup) pattern='MCD12Q2.061_Greenup_' ;;",
  "      npp) pattern='MOD17A3HGF.061_Npp_500m' ;;",
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
  "echo \"DOWNLOAD_ROOT=$DOWNLOAD_ROOT\""
)
writeLines(download_lines, file.path(out_dir, "download_product_rasters_only.sh"))
Sys.chmod(file.path(out_dir, "download_product_rasters_only.sh"), mode = "0755")

group_summary_df <- bind_rows(group_summary)
write.csv(group_summary_df, file.path(out_dir, paste0("appeears_request_groups_", date_tag, ".csv")), row.names = FALSE, na = "")

readme <- c(
  "# Final Remaining Dynamic AppEEARS Requests",
  "",
  "This folder contains the remaining AppEEARS requests from the final readiness gap table.",
  "",
  "Included only rows marked `extract_or_merge_missing` with a matched watershed polygon.",
  "Missing-polygon rows, MCM rows, and reference-only gaps are not AppEEARS requests.",
  "",
  "Run order:",
  "1. Refresh `APPEEARS_TOKEN` if `/tmp/appeears_login.json` is expired.",
  "2. `bash submit_appeears_tasks.sh`",
  "3. `bash check_task_status.sh` until all tasks are `done`.",
  "4. `bash download_product_rasters_only.sh`",
  "",
  "The download manifest records the intended Aurora raw-driver destination for each file."
)
writeLines(readme, file.path(out_dir, "README.md"))

cat("WROTE:", out_dir, "\n", sep = "")
cat("WROTE:", manifest_path, "\n", sep = "")
cat("\nRequest groups:\n")
print(group_summary_df, row.names = FALSE)
