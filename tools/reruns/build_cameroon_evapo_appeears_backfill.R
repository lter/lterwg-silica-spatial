librarian::shelf(sf, jsonlite)

source(file.path(getwd(), "tools", "workflow_paths.R"))

pre_aurora_paths_file <- file.path(getwd(), "01_pre_aurora_run", "00_pre_aurora_paths.R")
if (file.exists(pre_aurora_paths_file)) {
  source(pre_aurora_paths_file)
} else {
  pre_aurora_root <- function(review_root = silica_review_root(resolve_silica_data_root())) {
    file.path(review_root, "01_pre_aurora_run")
  }
}

args <- commandArgs(trailingOnly = TRUE)
run_date <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else format(Sys.Date(), "%Y%m%d")
watershed_override <- if (length(args) >= 2 && nzchar(args[[2]])) args[[2]] else ""

root_path <- resolve_silica_data_root()
review_root <- silica_review_root(root_path)
out_dir <- file.path(pre_aurora_root(review_root), "05_appeears_backfill", paste0(run_date, "_cameroon_evapo_v061"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

target_shapes <- c("Awout_Messam", "Nyong_Mbalmayo", "Nyong_Olama", "Soo_PontSoo")
watershed_path <- if (nzchar(watershed_override)) {
  normalizePath(watershed_override, mustWork = TRUE)
} else {
  silica_watershed_file(root_path)
}

sheds <- sf::st_read(watershed_path, quiet = TRUE)
shape_field_present <- "Shapefile_Name" %in% names(sheds)
lter_field_present <- "LTER" %in% names(sheds)

cameroon <- sheds[0, ]
if (shape_field_present) {
  cameroon <- sheds[sheds$Shapefile_Name %in% target_shapes, ]
}

if (nrow(cameroon) != length(target_shapes) && lter_field_present) {
  cameroon <- sheds[tolower(trimws(sheds$LTER)) == "cameroon", ]
}

if (nrow(cameroon) != 4) {
  stop(
    "Expected 4 Cameroon watersheds but found ", nrow(cameroon),
    " in ", watershed_path,
    call. = FALSE
  )
}

if (shape_field_present && all(target_shapes %in% cameroon$Shapefile_Name)) {
  cameroon <- cameroon[match(target_shapes, cameroon$Shapefile_Name), ]
}
cameroon <- sf::st_transform(cameroon, 4326)

# AppEEARS ET backfill should use a single dissolved AOI so it returns one raster
# per date window rather than one clipped raster per watershed feature.
cameroon_dissolved <- sf::st_union(cameroon)
cameroon <- sf::st_sf(
  LTER = "Cameroon",
  geometry = cameroon_dissolved
)

aoi_geojson <- file.path(out_dir, paste0("cameroon_evapo_v061_aoi_", run_date, ".geojson"))
sf::st_write(cameroon, aoi_geojson, delete_dsn = TRUE, quiet = TRUE)

geo_obj <- jsonlite::fromJSON(aoi_geojson, simplifyVector = FALSE)

task_windows <- list(
  list(
    suffix = "2000_2012",
    start = "01-01-2000",
    end = "12-31-2012"
  ),
  list(
    suffix = "2023_only",
    start = "01-01-2023",
    end = "12-31-2023"
  )
)

layer_variants <- c("ET_500M", "ET_500m")
task_specs <- list()

for (layer_name in layer_variants) {
  for (win in task_windows) {
    task_name <- paste(
      "cameroon-evapo-v061",
      win$suffix,
      layer_name,
      run_date,
      sep = "-"
    )

    payload <- list(
      task_type = "area",
      task_name = task_name,
      params = list(
        dates = list(
          list(startDate = win$start, endDate = win$end)
        ),
        layers = list(
          list(product = "MOD16A2GF.061", layer = layer_name)
        ),
        output = list(
          format = list(type = "geotiff"),
          projection = "geographic"
        ),
        geo = geo_obj
      )
    )

    json_path <- file.path(
      out_dir,
      paste0("appeears_task_", task_name, ".json")
    )
    jsonlite::write_json(payload, json_path, auto_unbox = TRUE, pretty = TRUE)

    task_specs[[length(task_specs) + 1]] <- list(
      task_name = task_name,
      layer = layer_name,
      start = win$start,
      end = win$end,
      json_path = json_path
    )
  }
}

manifest_path <- file.path(out_dir, paste0("appeears_task_manifest_", run_date, ".json"))
jsonlite::write_json(task_specs, manifest_path, auto_unbox = TRUE, pretty = TRUE)

readme_lines <- c(
  "# Cameroon evapo v061 AppEEARS backfill",
  "",
  "This folder was generated automatically to backfill missing MOD16A2GF.061 ET files",
  "for the 4 Cameroon HydroSHEDS watersheds used in the silica workflow.",
  "",
  "AOI file:",
  paste0("- ", basename(aoi_geojson)),
  "",
  "Task payloads:",
  paste0("- ", basename(vapply(task_specs, `[[`, character(1), "json_path"))),
  "",
  "Recommended submit order:",
  "1. Try the ET_500M payload for each date window first.",
  "2. If AppEEARS rejects the layer name, retry with the ET_500m payload for that same window.",
  "",
  "Target windows:",
  "- 2000-01-01 through 2012-12-31",
  "- 2023-01-01 through 2023-12-31",
  "",
  "After download, copy returned MOD16A2GF.061_ET_500m_* files into:",
  "- /home/shares/lter-si/si-watershed-extract/raw-driver-data/raw-evapo-v061/congo",
  "",
  "Then rerun the Cameroon full-record subset on Aurora."
)
writeLines(readme_lines, con = file.path(out_dir, "README.md"))

submit_lines <- c(
  "#!/usr/bin/env bash",
  "set -euo pipefail",
  "",
  "# Fill these in after you log in to Earthdata/AppEEARS.",
  "APPEEARS_TOKEN=\"\"",
  "",
  "if [[ -z \"$APPEEARS_TOKEN\" ]]; then",
  "  echo \"Set APPEEARS_TOKEN in this script before running.\" >&2",
  "  exit 1",
  "fi",
  "",
  "API_ROOT=\"https://appeears.earthdatacloud.nasa.gov/api\"",
  ""
)

for (spec in task_specs) {
  submit_lines <- c(
    submit_lines,
    paste0("echo 'Submitting ", spec$task_name, "'"),
    paste0(
      "curl -sS -X POST \"$API_ROOT/task\" ",
      "-H \"Authorization: Bearer $APPEEARS_TOKEN\" ",
      "-H \"Content-Type: application/json\" ",
      "--data @\"",
      spec$json_path,
      "\""
    ),
    ""
  )
}

writeLines(submit_lines, con = file.path(out_dir, "submit_appeears_tasks_template.sh"))

message("Wrote AppEEARS backfill helper files to: ", out_dir)
message("AOI: ", aoi_geojson)
message("Manifest: ", manifest_path)
