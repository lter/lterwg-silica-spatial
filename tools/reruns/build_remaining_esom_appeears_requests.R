librarian::shelf(sf, jsonlite)

source(file.path(getwd(), "tools", "workflow_paths.R"))

args <- commandArgs(trailingOnly = TRUE)
run_date <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else format(Sys.Date(), "%Y%m%d")

out_dir <- file.path(
  getwd(),
  "generated_outputs",
  "rerun",
  paste0(run_date, "_remaining_esom_appeears")
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

old_files <- list.files(
  out_dir,
  pattern = "^(appeears_task_.*\\.json|appeears_task_manifest_.*\\.json|.*_aoi_.*\\.geojson|.*_targets_.*\\.csv|submit_appeears_tasks_template\\.sh|README\\.md)$",
  full.names = TRUE
)
if (length(old_files)) {
  unlink(old_files)
}

read_watersheds <- function(path) {
  x <- sf::st_read(path, quiet = TRUE)
  if ("shp_nm" %in% names(x)) names(x)[names(x) == "shp_nm"] <- "Shapefile_Name"
  if ("Strm_Nm" %in% names(x)) names(x)[names(x) == "Strm_Nm"] <- "Stream_Name"
  x
}

make_aoi_geojson <- function(x, group_key) {
  x <- sf::st_transform(x, 4326)
  x <- sf::st_make_valid(x)
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  if (!nrow(x)) {
    stop("No polygon geometry remained for ", group_key, call. = FALSE)
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

write_target_csv <- function(x, group_key) {
  target_cols <- intersect(c("LTER", "Stream_Name", "Shapefile_Name"), names(x))
  target_df <- sf::st_drop_geometry(x[, target_cols])
  target_csv <- file.path(out_dir, paste0(group_key, "_targets_", run_date, ".csv"))
  utils::write.csv(target_df, target_csv, row.names = FALSE, na = "")
  target_csv
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
  list(task_name = task_name, key = key, start = start, end = end, json_path = json_path)
}

cameroon_path <- "/private/tmp/final_lulc_basin_shps/silica-watersheds_hydrosheds_subset_20260517_aurora-cameroon-full-record-rerun6-20260517.shp"
australia_path <- silica_watershed_file(resolve_silica_data_root())

if (!file.exists(cameroon_path)) {
  stop("Missing Cameroon watershed file: ", cameroon_path, call. = FALSE)
}
if (!file.exists(australia_path)) {
  stop("Missing Australia watershed file: ", australia_path, call. = FALSE)
}

cameroon_targets <- c("Awout_Messam", "Nyong_Mbalmayo", "Nyong_Olama", "Soo_PontSoo")
cameroon <- read_watersheds(cameroon_path)
cameroon <- cameroon[cameroon$Shapefile_Name %in% cameroon_targets, ]
if (nrow(cameroon) != length(cameroon_targets)) {
  stop("Expected 4 Cameroon/Congo polygons, found ", nrow(cameroon), call. = FALSE)
}

australia_targets <- c("AUS_210002", "AUS_425003", "AUS_425007", "AUS_425008", "AUS_425012", "MD11", "MD14")
australia <- read_watersheds(australia_path)
australia <- australia[australia$Shapefile_Name %in% australia_targets, ]
if (nrow(australia) != length(australia_targets)) {
  stop("Expected 7 Australia/MD polygons, found ", nrow(australia), call. = FALSE)
}

groups <- list(
  list(
    group_key = "congo_cameroon_evapo",
    data = cameroon,
    windows = list(
      list(key = "evapo", window_label = "2002-2006", start = "01-01-2002", end = "12-31-2006"),
      list(key = "evapo", window_label = "2007-2011", start = "01-01-2007", end = "12-31-2011"),
      list(key = "evapo", window_label = "2012-2013", start = "01-01-2012", end = "12-31-2013"),
      list(key = "evapo", window_label = "2017", start = "01-01-2017", end = "12-31-2017"),
      list(key = "evapo", window_label = "2023", start = "01-01-2023", end = "12-31-2023")
    ),
    layers = list(list(product = "MOD16A2GF.061", layer = "ET_500m")),
    destination = "/home/shares/lter-si/si-watershed-extract/raw-driver-data/raw-evapo-v061/congo"
  ),
  list(
    group_key = "australia_md_snow",
    data = australia,
    windows = list(
      list(key = "snow", window_label = "2024-2025", start = "01-01-2024", end = "12-31-2025")
    ),
    layers = list(list(product = "MOD10A2.061", layer = "Eight_Day_Snow_Cover")),
    destination = "/home/shares/lter-si/si-watershed-extract/raw-driver-data/raw-snow-v061/australia"
  )
)

manifest <- list()

for (group in groups) {
  aoi_geojson <- make_aoi_geojson(group$data, group$group_key)
  geo_obj <- jsonlite::fromJSON(aoi_geojson, simplifyVector = FALSE)
  targets_csv <- write_target_csv(group$data, group$group_key)

  for (win in group$windows) {
    spec <- build_payload(
      group_key = group$group_key,
      key = win$key,
      window_label = win$window_label,
      start = win$start,
      end = win$end,
      layers = group$layers,
      geo_obj = geo_obj
    )
    spec$destination <- group$destination
    spec$aoi_geojson <- aoi_geojson
    spec$targets_csv <- targets_csv
    manifest[[length(manifest) + 1]] <- spec
  }
}

manifest_path <- file.path(out_dir, paste0("appeears_task_manifest_", run_date, ".json"))
jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE, pretty = TRUE)

submit_script <- file.path(out_dir, "submit_appeears_tasks_template.sh")
submit_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "APPEEARS_TOKEN=\"${APPEEARS_TOKEN:-}\"",
  "",
  "if [[ -z \"$APPEEARS_TOKEN\" ]]; then",
  "  echo \"Set APPEEARS_TOKEN before running.\" >&2",
  "  exit 1",
  "fi",
  "",
  "API_ROOT=\"https://appeears.earthdatacloud.nasa.gov/api\"",
  "RESPONSES=\"appeears_submit_responses.jsonl\"",
  ": > \"$RESPONSES\"",
  ""
)

for (spec in manifest) {
  submit_lines <- c(
    submit_lines,
    paste0("echo 'Submitting ", spec$task_name, "'"),
    paste0(
      "curl -sS --fail-with-body -X POST \"$API_ROOT/task\" ",
      "-H \"Authorization: Bearer $APPEEARS_TOKEN\" ",
      "-H \"Content-Type: application/json\" ",
      "--data @\"",
      basename(spec$json_path),
      "\" | tee -a \"$RESPONSES\""
    ),
    "echo >> \"$RESPONSES\"",
    ""
  )
}

writeLines(submit_lines, submit_script)
Sys.chmod(submit_script, mode = "0755")

readme <- c(
  "# Remaining ESOM AppEEARS Requests",
  "",
  "Purpose: submit only final ESOM-blocking raw pulls that are not already pending.",
  "",
  "Included:",
  "- Congo/Cameroon MOD16A2GF.061 ET_500m: 2002-2013, 2017, 2023, split into five tasks.",
  "- Australia/MD MOD10A2.061 Eight_Day_Snow_Cover: 2024-2025, one task.",
  "",
  "Excluded:",
  "- Puerto Rico snow: deterministic zero-fill, not an AppEEARS pull.",
  "- Germany/Seine/Elbe greenup: handled by generated_outputs/rerun/20260522_germany_seine_elbe_dynamic_appeears/submit_greenup_tasks_template.sh.",
  "",
  "Destinations after download:",
  "- Congo evapo -> /home/shares/lter-si/si-watershed-extract/raw-driver-data/raw-evapo-v061/congo",
  "- Australia snow -> /home/shares/lter-si/si-watershed-extract/raw-driver-data/raw-snow-v061/australia"
)
writeLines(readme, file.path(out_dir, "README.md"))

message("Wrote remaining ESOM AppEEARS helper files to: ", out_dir)
message("Manifest: ", manifest_path)
