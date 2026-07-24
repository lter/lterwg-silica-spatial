#!/usr/bin/env Rscript

# Build size-balanced GeoJSON payloads for Earth Engine table workflows.
#
# The source watershed layer is never modified. Simplification occurs in an
# equal-area CRS and is rejected if any polygon exceeds the requested area
# error. Generated payloads and QA tables belong under generated_outputs/.
#
# Example:
#   Rscript tools/gee_colab_helpers/build_gee_vector_payloads.R \
#     --watersheds path/to/accepted_watersheds.gpkg \
#     --output-root generated_outputs/gee/worldpop \
#     --payload-prefix worldpop \
#     --simplification-profile coarse-1km \
#     --expected-site-count 549

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
})

source(file.path("tools", "cli_helpers.R"))
source(file.path("tools", "workflow_paths.R"))

args <- commandArgs(trailingOnly = TRUE)
repo_root <- silica_find_repo_root()
watershed_path <- cli_value(args, "--watersheds", required = TRUE)
output_root <- cli_value(
  args,
  "--output-root",
  file.path(repo_root, "generated_outputs", "gee", "vector-payloads")
)
payload_prefix <- cli_value(args, "--payload-prefix", "payload")
property_prefix <- cli_value(args, "--property-prefix", "gee")
profile <- cli_value(args, "--simplification-profile", "none")
expected_site_count <- cli_integer(
  args,
  "--expected-site-count",
  NA_integer_
)
target_payload_mb <- as.numeric(cli_value(args, "--target-payload-mb", "5"))
maximum_payload_mb <- as.numeric(cli_value(args, "--maximum-payload-mb", "7.5"))
maximum_area_error_pct <- as.numeric(
  cli_value(args, "--maximum-area-error-pct", "0.5")
)
maximum_groups <- cli_integer(args, "--maximum-groups", 100L, minimum = 1L)

numeric_options <- c(
  target_payload_mb,
  maximum_payload_mb,
  maximum_area_error_pct
)
if (any(!is.finite(numeric_options)) || any(numeric_options <= 0)) {
  stop("Payload-size and area-error options must be positive numbers.", call. = FALSE)
}
if (target_payload_mb > maximum_payload_mb) {
  stop("--target-payload-mb cannot exceed --maximum-payload-mb.", call. = FALSE)
}

simplification_tolerance <- function(area_km2, selected_profile) {
  if (selected_profile == "none") {
    return(rep(0, length(area_km2)))
  }
  if (selected_profile == "fine-30m") {
    return(case_when(
      area_km2 < 100 ~ 0,
      area_km2 < 1000 ~ 5,
      area_km2 < 10000 ~ 10,
      TRUE ~ 25
    ))
  }
  if (selected_profile == "coarse-1km") {
    return(case_when(
      area_km2 < 100 ~ 0,
      area_km2 < 1000 ~ 20,
      area_km2 < 10000 ~ 75,
      TRUE ~ 250
    ))
  }
  stop(
    "--simplification-profile must be none, fine-30m, or coarse-1km.",
    call. = FALSE
  )
}

watersheds <- st_read(
  require_input_file(watershed_path, "watershed layer"),
  quiet = TRUE
) |>
  st_transform(4326)
assert_required_columns(
  watersheds,
  c("site_id", "LTER", "Stream_Name", "Shapefile_Name"),
  "watershed layer"
)
if (
  !nrow(watersheds) ||
    anyDuplicated(watersheds$site_id) ||
    any(is.na(watersheds$site_id) | !nzchar(trimws(watersheds$site_id))) ||
    any(!st_is_valid(watersheds))
) {
  stop(
    "Watersheds must contain valid geometry and distinct, nonblank site_id values.",
    call. = FALSE
  )
}
if (!is.na(expected_site_count) && nrow(watersheds) != expected_site_count) {
  stop(
    "Watershed layer has ", nrow(watersheds),
    " sites; expected ", expected_site_count, ".",
    call. = FALSE
  )
}

exact_equal_area <- st_transform(watersheds, 6933)
exact_area_km2 <- as.numeric(st_area(exact_equal_area)) / 1e6
tolerance_m <- simplification_tolerance(exact_area_km2, profile)

simplified_geometry <- st_sfc(
  lapply(seq_len(nrow(exact_equal_area)), function(index) {
    geometry <- st_geometry(exact_equal_area[index, ])
    if (tolerance_m[[index]] > 0) {
      geometry <- st_simplify(
        geometry,
        dTolerance = tolerance_m[[index]],
        preserveTopology = TRUE
      )
    }
    st_geometry(st_transform(st_sf(geometry = geometry, crs = 6933), 4326))[[1]]
  }),
  crs = 4326
)

payloads <- watersheds
st_geometry(payloads) <- simplified_geometry
simplified_area_km2 <- as.numeric(st_area(st_transform(payloads, 6933))) / 1e6
area_error_pct <- 100 * abs(simplified_area_km2 - exact_area_km2) /
  pmax(exact_area_km2, .Machine$double.eps)

if (any(!st_is_valid(payloads))) {
  stop("Simplification produced an invalid geometry.", call. = FALSE)
}
if (max(area_error_pct, na.rm = TRUE) > maximum_area_error_pct) {
  stop(
    "Maximum simplification area error is ",
    signif(max(area_error_pct, na.rm = TRUE), 5),
    "%; limit is ", maximum_area_error_pct, "%.",
    call. = FALSE
  )
}

payloads[[paste0(property_prefix, "_geometry_simplified_m")]] <- tolerance_m
payloads[[paste0(
  property_prefix,
  "_simplification_area_error_pct"
)]] <- area_error_pct

geometry_bytes <- vapply(
  st_as_binary(st_geometry(payloads)),
  length,
  integer(1)
)
feature_cost <- geometry_bytes + 1000
target_payload_bytes <- target_payload_mb * 1024^2
maximum_payload_bytes <- maximum_payload_mb * 1024^2
initial_groups <- max(1L, ceiling(sum(feature_cost) / target_payload_bytes))

assign_groups <- function(group_count) {
  order_index <- order(feature_cost + exact_area_km2 * 4, decreasing = TRUE)
  group_bytes <- rep(0, group_count)
  group_area <- rep(0, group_count)
  group_rows <- rep(0L, group_count)
  assignment <- integer(nrow(payloads))
  target_bytes <- sum(feature_cost) / group_count
  target_area <- sum(exact_area_km2) / group_count
  target_rows <- nrow(payloads) / group_count

  for (index in order_index) {
    scores <-
      (group_bytes + feature_cost[[index]]) / target_bytes +
      (group_area + exact_area_km2[[index]]) / target_area +
      (group_rows + 1) / target_rows
    selected <- which.min(scores)
    assignment[[index]] <- selected
    group_bytes[[selected]] <- group_bytes[[selected]] + feature_cost[[index]]
    group_area[[selected]] <- group_area[[selected]] + exact_area_km2[[index]]
    group_rows[[selected]] <- group_rows[[selected]] + 1L
  }
  assignment
}

payload_root <- file.path(output_root, "payloads")
prepare_output_dir(payload_root)

write_payload_set <- function(group_count) {
  old_payloads <- list.files(
    payload_root,
    pattern = "[.]geojson$",
    full.names = TRUE
  )
  if (length(old_payloads)) unlink(old_payloads)

  assignment <- assign_groups(group_count)
  records <- vector("list", group_count)
  for (group in seq_len(group_count)) {
    group_label <- sprintf("%s_%02d", payload_prefix, group)
    group_file <- paste0(group_label, ".geojson")
    group_path <- file.path(payload_root, group_file)
    selected <- payloads[assignment == group, ] |>
      arrange(site_id)
    st_write(selected, group_path, driver = "GeoJSON", quiet = TRUE)
    records[[group]] <- tibble(
      payload = group_label,
      path = file.path("payloads", group_file),
      sites = nrow(selected),
      polygon_area_km2_sum = sum(exact_area_km2[assignment == group]),
      bytes = file.info(group_path)$size
    )
  }
  bind_rows(records)
}

group_count <- initial_groups
repeat {
  manifest <- write_payload_set(group_count)
  if (max(manifest$bytes) <= maximum_payload_bytes) break
  group_count <- group_count + 1L
  if (group_count > maximum_groups) {
    stop(
      "Could not keep payloads below ", maximum_payload_mb,
      " MB using ", maximum_groups, " groups.",
      call. = FALSE
    )
  }
}

inventory <- st_drop_geometry(payloads) |>
  transmute(
    site_id,
    LTER,
    Stream_Name,
    Shapefile_Name,
    polygon_area_km2 = exact_area_km2,
    simplification_profile = profile,
    simplification_tolerance_m = tolerance_m,
    simplification_area_error_pct = area_error_pct
  ) |>
  arrange(site_id)

write_csv(manifest, file.path(output_root, "payload_manifest.csv"))
write_csv(inventory, file.path(output_root, "site_inventory.csv"))
write_csv(
  tibble(
    metric = c(
      "accepted_sites",
      "payload_groups",
      "maximum_payload_mb",
      "maximum_simplification_area_error_pct"
    ),
    value = c(
      nrow(inventory),
      nrow(manifest),
      max(manifest$bytes) / 1024^2,
      max(area_error_pct)
    )
  ),
  file.path(output_root, "build_summary.csv")
)

message("Accepted sites: ", nrow(inventory))
message("Payload groups: ", nrow(manifest))
message("Largest payload MB: ", round(max(manifest$bytes) / 1024^2, 3))
message("Maximum area error %: ", signif(max(area_error_pct), 4))
