# Build a restartable AppEEARS queue from saved requests that still match the
# current site-reference table and are not already complete locally

### Inputs

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(sf)
})

source(file.path("tools", "cli_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
site_table_path <- require_input_file(
  cli_value(args, "--site-table", required = TRUE),
  "finalized site-reference table"
)
request_root <- require_input_dir(
  cli_value(
    args,
    "--request-root",
    "generated_outputs/rerun/appeears-requests"
  ),
  "AppEEARS request root"
)
shapefile_root <- require_input_dir(
  cli_value(args, "--shapefile-root", required = TRUE),
  "versioned shapefile root"
)
current_qa_path <- cli_value(args, "--current-qa", "")
output_root <- cli_value(args, "--output-root", required = TRUE)
batch_pattern <- cli_value(args, "--batch-pattern", ".*")
shape_alias_path <- cli_value(
  args,
  "--shape-aliases",
  file.path("tools", "appeears", "config", "shapefile_aliases.tsv")
)
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

read_site_table <- function(path) {
  table <- read.delim(
    path,
    check.names = FALSE,
    quote = "",
    comment.char = "",
    stringsAsFactors = FALSE,
    fill = TRUE
  )
  table[, !is.na(names(table)) & nzchar(names(table)), drop = FALSE]
}

site_table <- read_site_table(site_table_path)
required_columns <- c(
  "LTER", "Stream_Name", "Spatial_Data_Version", "Has_Spatial_Data",
  "Shapefile_Name"
)
assert_required_columns(site_table, required_columns, "site-reference table")

final_sites <- site_table %>%
  filter(Has_Spatial_Data == "Yes", nzchar(Shapefile_Name)) %>%
  mutate(
    Spatial_Data_Version = as.integer(Spatial_Data_Version),
    source_file = file.path(
      shapefile_root,
      paste0("data_release_", Spatial_Data_Version),
      Shapefile_Name,
      paste0(Shapefile_Name, ".shp")
    )
  )

final_shapes <- final_sites %>%
  arrange(LTER, Stream_Name) %>%
  distinct(Shapefile_Name, .keep_all = TRUE)

### Request inventory

request_files <- list.files(
  request_root,
  pattern = "[.]json$",
  recursive = TRUE,
  full.names = TRUE
)
request_files <- request_files[
  grepl("/requests/", request_files) &
    grepl(batch_pattern, basename(dirname(dirname(request_files))))
]
if (!length(request_files)) stop("No matching request JSON files were found.")

parse_request <- function(path) {
  request <- fromJSON(path, simplifyVector = FALSE)
  feature <- request$params$geo$features[[1]]
  dates <- request$params$dates[[1]]
  geometry <- st_read(
    as.character(toJSON(request$params$geo, auto_unbox = TRUE)),
    quiet = TRUE
  )
  properties <- feature$properties
  list(
    record = data.frame(
      batch = basename(dirname(dirname(path))),
      task_name = request$task_name,
      aoi_name = if (is.null(properties$aoi_name)) "" else properties$aoi_name,
      site_id = if (is.null(properties$site_id)) "" else properties$site_id,
      start_year = as.integer(substr(dates$startDate, 7, 10)),
      end_year = as.integer(substr(dates$endDate, 7, 10)),
      request_file = path,
      stringsAsFactors = FALSE
    ),
    geometry = st_geometry(geometry)
  )
}

parsed_requests <- lapply(request_files, parse_request)
requests <- bind_rows(lapply(parsed_requests, `[[`, "record"))
request_geometry <- setNames(
  lapply(parsed_requests, `[[`, "geometry"),
  requests$task_name
)
if (anyDuplicated(requests$task_name)) {
  stop("Matching request files contain duplicate task names.")
}

status_files <- list.files(
  request_root,
  pattern = "appeears_task_status_.*[.]json$",
  recursive = TRUE,
  full.names = TRUE
)
status_files <- status_files[
  grepl(batch_pattern, basename(dirname(status_files)))
]
status_records <- bind_rows(lapply(status_files, function(path) {
  document <- fromJSON(path, simplifyVector = FALSE)
  if (is.null(document$tasks)) return(NULL)
  bind_rows(lapply(document$tasks, function(task) {
    data.frame(
      task_name = if (is.null(task$task_name)) "" else task$task_name,
      task_id = if (is.null(task$task_id)) "" else task$task_id,
      stringsAsFactors = FALSE
    )
  }))
})) %>%
  filter(nzchar(task_name), nzchar(task_id)) %>%
  distinct(task_name, .keep_all = TRUE)
requests <- requests %>% left_join(status_records, by = "task_name")

### Match requests to final watersheds

# Optional aliases cover request names that changed without a geometry change
shape_alias_table <- data.frame(
  Request_Shapefile_Name = character(),
  Shapefile_Name = character(),
  stringsAsFactors = FALSE
)
if (nzchar(shape_alias_path) && file.exists(shape_alias_path)) {
  shape_alias_table <- read.delim(
    shape_alias_path,
    sep = "\t",
    quote = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  assert_required_columns(
    shape_alias_table,
    c("Request_Shapefile_Name", "Shapefile_Name"),
    "shapefile alias table"
  )
  shape_alias_table <- shape_alias_table %>%
    transmute(
      Request_Shapefile_Name = trimws(Request_Shapefile_Name),
      Shapefile_Name = trimws(Shapefile_Name)
    ) %>%
    filter(nzchar(Request_Shapefile_Name), nzchar(Shapefile_Name)) %>%
    distinct()
  unknown_aliases <- setdiff(
    shape_alias_table$Shapefile_Name,
    final_sites$Shapefile_Name
  )
  if (length(unknown_aliases)) {
    stop(
      "Shapefile aliases point to names absent from the site table: ",
      paste(unknown_aliases, collapse = ", "),
      call. = FALSE
    )
  }
}

hydro_request_ids <- sub(
  "^hybas_",
  "",
  unique(requests$aoi_name[grepl("^hybas_[0-9]+$", requests$aoi_name)])
)
hydro_source <- if ("Shapefile_Source" %in% names(final_sites)) {
  grepl("hydrobasins|hydrosheds", final_sites$Shapefile_Source, ignore.case = TRUE)
} else {
  rep(TRUE, nrow(final_sites))
}
hydro_sites <- final_sites %>%
  filter(hydro_source, file.exists(source_file)) %>%
  distinct(Shapefile_Name, .keep_all = TRUE)
hydro_outlets <- bind_rows(lapply(seq_len(nrow(hydro_sites)), function(index) {
  attributes <- st_drop_geometry(st_read(hydro_sites$source_file[[index]], quiet = TRUE))
  outlet_column <- grep("outlt|outlet", names(attributes), ignore.case = TRUE)[1]
  if (is.na(outlet_column)) return(NULL)
  data.frame(
    Shapefile_Name = hydro_sites$Shapefile_Name[[index]],
    outlet_id = as.character(attributes[[outlet_column]][[1]]),
    stringsAsFactors = FALSE
  )
}))
if (nrow(hydro_outlets)) {
  hydro_outlets <- hydro_outlets %>% filter(outlet_id %in% hydro_request_ids)
} else {
  hydro_outlets <- data.frame(
    Shapefile_Name = character(),
    outlet_id = character(),
    stringsAsFactors = FALSE
  )
}

shape_aliases <- split(final_sites$Shapefile_Name, final_sites$Shapefile_Name)
for (outlet in unique(hydro_outlets$outlet_id)) {
  names_at_outlet <- sort(unique(
    hydro_outlets$Shapefile_Name[hydro_outlets$outlet_id == outlet]
  ))
  shape_aliases[[paste0("hybas_", outlet)]] <- names_at_outlet
}
for (index in seq_len(nrow(shape_alias_table))) {
  old_name <- shape_alias_table$Request_Shapefile_Name[[index]]
  shape_aliases[[old_name]] <- unique(c(
    shape_aliases[[old_name]],
    shape_alias_table$Shapefile_Name[[index]]
  ))
}

requests$mapped_shapes <- lapply(requests$aoi_name, function(name) {
  unique(shape_aliases[[name]])
})
requests$canonical_shape <- vapply(
  requests$mapped_shapes,
  function(value) if (length(value)) sort(value)[[1]] else "",
  character(1)
)

shape_metadata <- final_shapes %>%
  select(LTER, Stream_Name, Shapefile_Name, source_file)
requests <- requests %>%
  left_join(
    shape_metadata,
    by = c("canonical_shape" = "Shapefile_Name")
  )

### Geometry checks

compare_geometry <- function(task_name, source_file) {
  if (!file.exists(source_file)) {
    return(c(
      area_ratio = NA_real_, overlap = NA_real_,
      final_area_km2 = NA_real_, checked = 0
    ))
  }
  requested <- st_make_valid(st_transform(
    st_sf(geometry = request_geometry[[task_name]]),
    6933
  ))
  final <- st_make_valid(st_transform(st_read(source_file, quiet = TRUE), 6933))
  requested <- st_union(st_geometry(requested))
  final <- st_union(st_geometry(final))
  requested_area <- as.numeric(st_area(requested))
  final_area <- as.numeric(st_area(final))
  intersection_area <- tryCatch(
    as.numeric(st_area(st_intersection(requested, final))),
    error = function(error) 0
  )
  c(
    area_ratio = requested_area / final_area,
    overlap = intersection_area / final_area,
    final_area_km2 = final_area / 1e6,
    checked = 1
  )
}

geometry_checks <- t(vapply(
  seq_len(nrow(requests)),
  function(index) {
    if (!nzchar(requests$canonical_shape[[index]])) {
      return(c(
        area_ratio = NA_real_, overlap = NA_real_,
        final_area_km2 = NA_real_, checked = 0
      ))
    }
    compare_geometry(
      requests$task_name[[index]],
      requests$source_file[[index]]
    )
  },
  numeric(4)
))
requests$area_ratio <- geometry_checks[, "area_ratio"]
requests$overlap <- geometry_checks[, "overlap"]
requests$final_area_km2 <- geometry_checks[, "final_area_km2"]
requests$geometry_checked <- geometry_checks[, "checked"] == 1
requests$geometry_matches <- nzchar(requests$canonical_shape) & (
  (!requests$geometry_checked & requests$aoi_name == requests$canonical_shape) |
    (
      requests$geometry_checked &
        requests$area_ratio >= 0.98 & requests$area_ratio <= 1.02 &
        requests$overlap >= ifelse(requests$final_area_km2 < 1, 0.97, 0.98)
    )
)

### Coverage and task selection

complete_coverage <- data.frame(
  Shapefile_Name = character(),
  coverage_start_year = integer(),
  coverage_end_year = integer(),
  stringsAsFactors = FALSE
)
if (nzchar(current_qa_path) && file.exists(current_qa_path)) {
  current_qa <- readRDS(current_qa_path)
  complete_coverage <- current_qa$outputs %>%
    group_by(watershed_key) %>%
    summarize(
      complete = n() == 4L && all(status == "complete"),
      coverage_start_year = min(coverage_start_year, na.rm = TRUE),
      coverage_end_year = max(coverage_end_year, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(complete) %>%
    transmute(
      Shapefile_Name = sub("^[^_]+__", "", watershed_key),
      coverage_start_year,
      coverage_end_year
    )
}
requests$already_complete <- vapply(seq_len(nrow(requests)), function(index) {
  matching <- complete_coverage %>%
    filter(Shapefile_Name %in% requests$mapped_shapes[[index]])
  nrow(matching) > 0L && any(
    matching$coverage_start_year <= requests$start_year[[index]] &
      matching$coverage_end_year >= requests$end_year[[index]]
  )
}, logical(1))
requests$eligible <- requests$geometry_matches &
  !requests$already_complete &
  !is.na(requests$task_id) & nzchar(requests$task_id)

choose_tasks <- function(group) {
  group <- group %>%
    arrange(abs(area_ratio - 1), nchar(task_name), task_name)
  full <- group %>% filter(start_year <= 2002L, end_year >= 2025L)
  if (nrow(full)) return(full[1, , drop = FALSE])
  group %>% distinct(start_year, end_year, .keep_all = TRUE)
}

selected <- requests %>%
  filter(eligible) %>%
  group_by(canonical_shape) %>%
  group_modify(~ choose_tasks(.x)) %>%
  ungroup() %>%
  arrange(LTER, Stream_Name, start_year, task_name)

coverage_plan <- selected %>%
  group_by(canonical_shape) %>%
  summarize(
    coverage_start_year = min(start_year),
    coverage_end_year = max(end_year),
    covered_years = list(sort(unique(unlist(Map(seq, start_year, end_year))))),
    .groups = "drop"
  ) %>%
  mutate(
    contiguous = vapply(seq_len(n()), function(index) {
      identical(
        covered_years[[index]],
        seq(coverage_start_year[[index]], coverage_end_year[[index]])
      )
    }, logical(1)),
    coverage_key = paste(coverage_start_year, coverage_end_year, sep = "-")
  )
if (any(!coverage_plan$contiguous)) {
  stop(
    "Selected requests have a temporal gap for: ",
    paste(coverage_plan$canonical_shape[!coverage_plan$contiguous], collapse = ", "),
    call. = FALSE
  )
}
selected <- selected %>%
  left_join(
    coverage_plan %>% select(canonical_shape, coverage_key),
    by = "canonical_shape"
  )

selected_names <- selected$task_name
requests$selected <- requests$task_name %in% selected_names
requests$decision <- case_when(
  requests$selected ~ "download_and_extract",
  requests$already_complete ~ "already_complete_in_current_queue",
  !nzchar(requests$canonical_shape) ~ "not_used_by_finalized_table",
  !requests$geometry_matches ~ "geometry_does_not_match_finalized_watershed",
  (is.na(requests$task_id) | !nzchar(requests$task_id)) &
    requests$canonical_shape %in% selected$canonical_shape ~
      "unsubmitted_duplicate_request",
  is.na(requests$task_id) | !nzchar(requests$task_id) ~ "missing_task_id",
  requests$eligible ~ "duplicate_or_superseded_request",
  TRUE ~ "not_selected"
)

### Queue outputs

selected$watershed_key <- paste(selected$LTER, selected$canonical_shape, sep = "__")
status_table <- selected %>%
  transmute(
    task_name,
    watershed_key,
    LTER,
    Stream_Name,
    Shapefile_Name = canonical_shape,
    start_year,
    end_year
  )
write.csv(
  status_table,
  file.path(output_root, "verified_task_status.csv"),
  row.names = FALSE,
  na = ""
)

status_json <- list(
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  tasks = lapply(seq_len(nrow(selected)), function(index) {
    list(
      task_name = selected$task_name[[index]],
      task_id = selected$task_id[[index]],
      LTER = selected$LTER[[index]],
      Stream_Name = selected$Stream_Name[[index]],
      Shapefile_Name = selected$canonical_shape[[index]]
    )
  })
)
write_json(
  status_json,
  file.path(output_root, "verified_tasks.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)

selected_shapes <- selected %>%
  distinct(canonical_shape, .keep_all = TRUE)
features <- lapply(seq_len(nrow(selected_shapes)), function(index) {
  record <- selected_shapes[index, ]
  if (file.exists(record$source_file[[1]])) {
    geometry <- st_geometry(st_transform(
      st_read(record$source_file[[1]], quiet = TRUE),
      4326
    ))
    geometry <- st_union(geometry)
  } else {
    geometry <- request_geometry[[record$task_name[[1]]]]
  }
  st_sf(
    LTER = record$LTER[[1]],
    Stream_Name = record$Stream_Name[[1]],
    Shapefile_Name = record$canonical_shape[[1]],
    geometry = geometry,
    crs = 4326
  )
})
watersheds <- do.call(rbind, features)
watershed_path <- file.path(output_root, "verified_watersheds.gpkg")
if (file.exists(watershed_path)) unlink(watershed_path)
st_write(watersheds, watershed_path, quiet = TRUE)

alias_table <- bind_rows(lapply(seq_len(nrow(selected_shapes)), function(index) {
  record <- selected_shapes[index, ]
  data.frame(
    canonical_lter = record$LTER[[1]],
    canonical_shape = record$canonical_shape[[1]],
    Shapefile_Name = record$mapped_shapes[[1]],
    stringsAsFactors = FALSE
  )
})) %>%
  distinct() %>%
  left_join(
    final_sites %>%
      select(LTER, Shapefile_Name) %>%
      distinct(),
    by = "Shapefile_Name"
  )
saveRDS(alias_table, file.path(output_root, "shape_aliases.rds"))

### Coverage-specific queues

write_queue_bundle <- function(coverage_key) {
  queue_tasks <- selected %>% filter(.data$coverage_key == !!coverage_key)
  queue_shapes <- sort(unique(queue_tasks$canonical_shape))
  queue_root <- file.path(output_root, paste0("coverage-", coverage_key))
  dir.create(queue_root, recursive = TRUE, showWarnings = FALSE)

  queue_status <- queue_tasks %>%
    transmute(
      task_name,
      watershed_key,
      LTER,
      Stream_Name,
      Shapefile_Name = canonical_shape,
      start_year,
      end_year
    )
  write.csv(
    queue_status,
    file.path(queue_root, "verified_task_status.csv"),
    row.names = FALSE,
    na = ""
  )

  queue_json <- list(
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    tasks = lapply(seq_len(nrow(queue_tasks)), function(index) {
      list(
        task_name = queue_tasks$task_name[[index]],
        task_id = queue_tasks$task_id[[index]],
        LTER = queue_tasks$LTER[[index]],
        Stream_Name = queue_tasks$Stream_Name[[index]],
        Shapefile_Name = queue_tasks$canonical_shape[[index]]
      )
    })
  )
  write_json(
    queue_json,
    file.path(queue_root, "verified_tasks.json"),
    auto_unbox = TRUE,
    pretty = TRUE
  )

  queue_watershed_path <- file.path(queue_root, "verified_watersheds.gpkg")
  if (file.exists(queue_watershed_path)) unlink(queue_watershed_path)
  st_write(
    watersheds %>% filter(Shapefile_Name %in% queue_shapes),
    queue_watershed_path,
    quiet = TRUE
  )
  saveRDS(
    alias_table %>% filter(canonical_shape %in% queue_shapes),
    file.path(queue_root, "shape_aliases.rds")
  )
}
invisible(lapply(sort(unique(selected$coverage_key)), write_queue_bundle))

### Audit outputs

saveRDS(requests, file.path(output_root, "request_audit.rds"))

decision_summary <- requests %>% count(decision, name = "tasks")
batch_summary <- requests %>%
  group_by(batch) %>%
  summarize(
    requests = n(),
    selected = sum(selected),
    already_complete = sum(already_complete),
    excluded = n() - selected - sum(already_complete),
    .groups = "drop"
  )
summary_lines <- c(
  paste("Finalized table rows:", nrow(site_table)),
  paste("Request files checked:", nrow(requests)),
  paste("Tasks selected:", nrow(selected)),
  paste("Canonical watersheds selected:", nrow(selected_shapes)),
  paste(
    "Coverage queues:",
    paste(sort(unique(selected$coverage_key)), collapse = ", ")
  ),
  "",
  "Decisions:",
  capture.output(print(decision_summary, row.names = FALSE)),
  "",
  "Batches:",
  capture.output(print(batch_summary, row.names = FALSE))
)
writeLines(summary_lines, file.path(output_root, "audit_summary.txt"))
cat(paste(summary_lines, collapse = "\n"), "\n")
