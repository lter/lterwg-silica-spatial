librarian::shelf(sf, jsonlite)

source(file.path(getwd(), "tools", "workflow_paths.R"))

args <- commandArgs(trailingOnly = TRUE)
run_date <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else format(Sys.Date(), "%Y%m%d")
watershed_override <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else ""

root_path <- resolve_silica_data_root()
out_dir <- file.path(
  getwd(),
  "generated_outputs",
  "rerun",
  paste0(run_date, "_germany_seine_elbe_evapo_2023_appeears")
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

watershed_path <- if (nzchar(watershed_override)) {
  normalizePath(watershed_override, mustWork = TRUE)
} else {
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
  x
}

clean_lter <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\\s*\\([^)]*\\)", "", x)
  trimws(x)
}

make_aoi_geojson <- function(x, group_key) {
  x <- sf::st_transform(x, 4326)
  x <- sf::st_make_valid(x)
  x <- suppressWarnings(sf::st_collection_extract(x, "POLYGON"))
  x <- x[!sf::st_is_empty(x), , drop = FALSE]
  if (!nrow(x)) {
    stop("No polygon geometry remained after cleaning AOI for ", group_key, call. = FALSE)
  }

  geom <- suppressWarnings(sf::st_collection_extract(sf::st_union(x), "POLYGON"))
  geom <- suppressWarnings(sf::st_make_valid(geom))
  geom <- suppressWarnings(sf::st_collection_extract(geom, "POLYGON"))
  geom <- geom[!sf::st_is_empty(geom)]
  if (!length(geom) || any(!sf::st_is_valid(geom))) {
    stop("Invalid AOI polygon geometry for ", group_key, call. = FALSE)
  }

  aoi <- sf::st_sf(aoi_name = group_key, geometry = geom)
  out <- file.path(out_dir, paste0(group_key, "_aoi_", run_date, ".geojson"))
  sf::st_write(aoi, out, delete_dsn = TRUE, quiet = TRUE)
  out
}

sheds <- read_watersheds(watershed_path)
if (!all(c("LTER", "Shapefile_Name") %in% names(sheds))) {
  stop("Watershed file must include LTER and Shapefile_Name columns: ", watershed_path, call. = FALSE)
}

sheds$.LTER_CLEAN <- clean_lter(sheds$LTER)

seine <- sheds[
  tolower(sheds$.LTER_CLEAN) == "seine" &
    !is.na(sheds$Shapefile_Name) &
    nzchar(sheds$Shapefile_Name),
]

elbe <- sheds[
  tolower(sheds$.LTER_CLEAN) %in% c("germany", "elbe") &
    sheds$Shapefile_Name == "ElbeRiver",
]

if (nrow(seine) != 7) {
  warning("Expected 7 Seine polygons, found ", nrow(seine), call. = FALSE)
}

if (nrow(elbe) != 1) {
  warning("Expected 1 Germany ElbeRiver polygon, found ", nrow(elbe), call. = FALSE)
}

target_groups <- list(
  list(key = "seine_evapo_2023", label = "Seine", data = seine),
  list(key = "germany_elbe_evapo_2023", label = "Germany ElbeRiver", data = elbe)
)

manifest <- list()

for (group in target_groups) {
  if (!nrow(group$data)) {
    next
  }

  aoi_geojson <- make_aoi_geojson(group$data, group$key)
  geo_obj <- jsonlite::fromJSON(aoi_geojson, simplifyVector = FALSE)

  target_df <- sf::st_drop_geometry(
    group$data[, intersect(c("LTER", "Stream_Name", "Shapefile_Name"), names(group$data))]
  )
  target_csv <- file.path(out_dir, paste0(group$key, "_targets_", run_date, ".csv"))
  utils::write.csv(target_df, target_csv, row.names = FALSE, na = "")

  task_name <- paste(group$key, "evapo-v061-2023", run_date, sep = "-")
  payload <- list(
    task_type = "area",
    task_name = task_name,
    params = list(
      dates = list(
        list(startDate = "01-01-2023", endDate = "12-31-2023")
      ),
      layers = list(
        list(product = "MOD16A2GF.061", layer = "ET_500m")
      ),
      output = list(
        format = list(type = "geotiff"),
        projection = "geographic"
      ),
      geo = geo_obj
    )
  )

  json_path <- file.path(out_dir, paste0("appeears_task_", task_name, ".json"))
  jsonlite::write_json(payload, json_path, auto_unbox = TRUE, pretty = TRUE)

  manifest[[length(manifest) + 1]] <- list(
    group_key = group$key,
    group_label = group$label,
    key = "evapo",
    task_name = task_name,
    start = "01-01-2023",
    end = "12-31-2023",
    destination = "/home/shares/lter-si/si-watershed-extract/raw-driver-data/raw-evapo-v061/germany",
    aoi_geojson = aoi_geojson,
    targets_csv = target_csv,
    json_path = json_path
  )
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
  "# Germany Evapo 2023 AppEEARS Pull",
  "",
  "Purpose: backfill the only missing Germany-region evapo year found by the full coverage audit.",
  "",
  "Tasks:",
  "- Seine AOI: MOD16A2GF.061 ET_500m for 2023",
  "- Germany ElbeRiver AOI: MOD16A2GF.061 ET_500m for 2023",
  "",
  "After download, stage returned GeoTIFFs to:",
  "- /home/shares/lter-si/si-watershed-extract/raw-driver-data/raw-evapo-v061/germany",
  "",
  "Then rerun the all-year coverage check."
)
writeLines(readme, file.path(out_dir, "README.md"))

message("Wrote Germany evapo 2023 AppEEARS helper files to: ", out_dir)
message("Manifest: ", manifest_path)
