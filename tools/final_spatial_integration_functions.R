# Reusable final spatial integration and coverage functions

source(file.path("tools", "identifier_helpers.R"))
source(file.path("tools", "workflow_paths.R"))

spatial_value_is_blank <- function(value) {
  is.na(value) | !nzchar(trimws(as.character(value)))
}

first_spatial_value <- function(value) {
  keep <- !spatial_value_is_blank(value)
  if (!any(keep)) {
    return(NA_character_)
  }
  as.character(value[which(keep)[[1]]])
}

driver_core_columns <- function(family, year, family_patterns, value_names) {
  switch(family,
    evapo = paste0("evapotrans_", year, "_kg_m2"),
    greenup = paste0("greenup_cycle", 0:1, "_", year, "MMDD"),
    npp = paste0("npp_", year, "_kgC_m2_year"),
    snow = paste0("snow_", year, "_num_days"),
    precip = paste0("precip_", year, "_mm_per_day"),
    airtemp = paste0("temp_", year, "_degC"),
    elevation = "elevation_mean_m",
    lithology = grep(family_patterns[[family]], value_names, value = TRUE),
    permafrost = "permafrost_mean_m",
    soil = grep(family_patterns[[family]], value_names, value = TRUE)
  )
}

build_driver_coverage <- function(
  geometry,
  value_store,
  provenance_store,
  family_patterns,
  known_unavailable = data.frame()
) {
  annual_years <- list(
    evapo = 2002:2025,
    greenup = 2002:2024,
    npp = 2002:2025,
    snow = 2002:2025,
    precip = 2002:2025,
    airtemp = 2002:2025
  )
  static_families <- c("elevation", "lithology", "permafrost", "soil")
  if (nrow(known_unavailable)) {
    known_unavailable$.shape_key <- normalize_site_key(
      known_unavailable$Shapefile_Name
    )
  }
  approved <- function(shape_key, family, year) {
    if (!nrow(known_unavailable) || is.na(year)) {
      return(FALSE)
    }
    any(
      known_unavailable$.shape_key == shape_key &
        known_unavailable$driver == family &
        known_unavailable$start_year <= year &
        known_unavailable$end_year >= year
    )
  }
  one_family <- function(family, year = NA_integer_) {
    columns <- intersect(
      driver_core_columns(family, year, family_patterns, names(value_store)),
      names(value_store)
    )
    if (!length(columns)) {
      present <- rep(FALSE, nrow(geometry))
      selected_source <- rep(NA_character_, nrow(geometry))
    } else {
      present_matrix <- do.call(cbind, lapply(columns, function(column) {
        !spatial_value_is_blank(value_store[[column]])
      }))
      provenance_matrix <- do.call(cbind, lapply(columns, function(column) {
        provenance_store[[column]]
      }))
      if (is.null(dim(present_matrix))) {
        present_matrix <- matrix(present_matrix, ncol = 1)
        provenance_matrix <- matrix(provenance_matrix, ncol = 1)
      }
      present <- rowSums(present_matrix) > 0L
      selected_source <- apply(provenance_matrix, 1, first_spatial_value)
    }
    exception <- vapply(
      geometry$.shape_key,
      approved,
      logical(1),
      family = family,
      year = year
    )
    data.frame(
      shape_key = geometry$.shape_key,
      Shapefile_Name = geometry$Shapefile_Name,
      roster_source_id = geometry$Roster_Source_ID,
      driver = family,
      year = year,
      status = ifelse(
        present,
        "covered",
        ifelse(exception, "approved_unavailable", "missing")
      ),
      selected_source_id = selected_source,
      stringsAsFactors = FALSE
    )
  }
  annual <- unlist(Map(
    function(family, years) lapply(years, function(year) one_family(family, year)),
    names(annual_years),
    annual_years
  ), recursive = FALSE)
  static <- lapply(static_families, one_family)
  do.call(rbind, c(annual, static))
}

build_final_spatial_integration <- function(
  manifest_path,
  output_root,
  allow_incomplete = FALSE
) {
  ### Paths

  data_root <- resolve_silica_data_root()
  new_sites_root <- resolve_silica_new_sites_root()
  manifest_path <- require_input_file(manifest_path, "final source manifest")
  prepare_output_dir(output_root)
  resolve_path <- function(path) {
    expand_workflow_path(
      path,
      data_root = data_root,
      new_sites_root = new_sites_root
    )
  }
  portable_path <- function(path) {
    portable_workflow_path(
      path,
      data_root = data_root,
      new_sites_root = new_sites_root
    )
  }

  write_csv <- function(data, name) {
    write.csv(
      data,
      file.path(output_root, name),
      row.names = FALSE,
      na = ""
    )
  }

  manifest <- read.delim(
    manifest_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    quote = ""
  )
  required_manifest_cols <- c(
    "source_id", "system", "family", "path", "qa_path", "qa_mode",
    "precedence", "combine", "role"
  )
  missing_manifest_cols <- setdiff(required_manifest_cols, names(manifest))
  if (length(missing_manifest_cols)) {
    stop(
      "Source manifest is missing: ",
      paste(missing_manifest_cols, collapse = ", ")
    )
  }
  if (anyDuplicated(manifest$source_id)) {
    stop("Source manifest contains duplicate source_id values.")
  }

  if (any(manifest$qa_mode == "reference_final")) {
    reference_manifest_row <- manifest[manifest$qa_mode == "reference_final", , drop = FALSE]
    if (nrow(reference_manifest_row) != 1L) {
      stop("Expected one final reference source in the manifest.")
    }
    reference_output_root <- dirname(resolve_path(reference_manifest_row$path[[1]]))
    reference_status <- system2(
      Sys.which("Rscript"),
      c(
        "tools/build_final_spatial_reference.R",
        "--outdir", reference_output_root
      )
    )
    if (!identical(reference_status, 0L)) {
      stop("The final spatial reference build failed.")
    }
  }

  manifest$resolved_path <- vapply(manifest$path, resolve_path, character(1))
  manifest$resolved_qa_path <- vapply(manifest$qa_path, resolve_path, character(1))
  manifest$precedence <- suppressWarnings(as.integer(manifest$precedence))
  manifest$combine <- vapply(manifest$combine, parse_boolean, logical(1))
  manifest$path_exists <- file.exists(manifest$resolved_path)

  ### Source QA

  qa_result <- function(mode, source_path, qa_path) {
    if (!file.exists(source_path)) {
      return(c(status = "missing", detail = "source path is missing"))
    }
    if (mode == "accepted_legacy") {
      return(c(status = "waived", detail = "accepted legacy baseline"))
    }
    if (mode == "presence") {
      return(c(status = "passed", detail = "source path exists"))
    }
    if (!nzchar(qa_path) || !file.exists(qa_path)) {
      return(c(status = "missing", detail = "QA record is missing"))
    }
    if (mode == "reference_final") {
      qa <- read.csv(qa_path, stringsAsFactors = FALSE, check.names = FALSE)
      passed <- nrow(qa) > 0L && all(qa$status == "PASS")
      values <- setNames(qa$value, qa$check)
      detail <- paste0(
        values[["final_spatial_site_rows"]], " spatial site rows; ",
        values[["distinct_watershed_geometries"]], " watershed geometries"
      )
      return(c(status = if (passed) "passed" else "failed", detail = detail))
    }
    if (mode %in% c("appeears", "appeears_final_441")) {
      qa <- readRDS(qa_path)
      summary <- qa$summary
      batch_passed <- nrow(summary) == 1L &&
        summary$outputs_failed[[1]] == 0L &&
        summary$outputs_complete[[1]] == summary$outputs_expected[[1]]
      complete_scope <- mode != "appeears_final_441" ||
        summary$watersheds[[1]] == 441L
      status <- if (batch_passed && complete_scope) {
        "passed"
      } else if (batch_passed && mode == "appeears_final_441") {
        "in_progress"
      } else {
        "failed"
      }
      detail <- paste0(
        summary$watersheds[[1]], " watersheds; ",
        summary$outputs_complete[[1]], "/",
        summary$outputs_expected[[1]], " outputs complete"
      )
      return(c(status = status, detail = detail))
    }
    if (mode == "mcm") {
      qa <- readRDS(qa_path)
      passed <- length(qa$checks) && all(qa$checks)
      detail <- paste0(sum(qa$checks), "/", length(qa$checks), " checks passed")
      return(c(status = if (passed) "passed" else "failed", detail = detail))
    }
    if (mode == "aurora") {
      qa <- readRDS(qa_path)
      passed <- nrow(qa$issues) == 0L && all(qa$drivers$status == "passed")
      detail <- paste0(
        nrow(qa$drivers), " drivers passed; ", nrow(qa$issues), " issues"
      )
      return(c(status = if (passed) "passed" else "failed", detail = detail))
    }
    if (mode == "gee_summary") {
      qa <- read.csv(qa_path, stringsAsFactors = FALSE, check.names = FALSE)
      failures <- sum(toupper(trimws(qa$status)) == "FAIL")
      detail <- paste0(failures, " failed GEE QA checks")
      return(c(status = if (!failures) "passed" else "failed", detail = detail))
    }
    if (mode == "gee_input") {
      qa <- read.csv(qa_path, stringsAsFactors = FALSE, check.names = FALSE)
      values <- setNames(qa$value, qa$check)
      passed <- identical(
        as.integer(values[["valid_nonempty_geometries"]]),
        as.integer(values[["distinct_watershed_geometries"]])
      ) &&
        identical(as.integer(values[["reference_area_unresolved_mismatch"]]), 0L)
      detail <- paste0(
        values[["valid_nonempty_geometries"]], "/",
        values[["distinct_watershed_geometries"]], " valid geometries"
      )
      return(c(status = if (passed) "passed" else "failed", detail = detail))
    }
    if (mode == "human_impacts") {
      qa <- read.csv(qa_path, stringsAsFactors = FALSE, check.names = FALSE)
      passed <- nrow(qa) > 0L && all(qa$missing_metric_cells == 0L)
      detail <- paste0(nrow(qa), " dataset-year QA rows")
      return(c(status = if (passed) "passed" else "failed", detail = detail))
    }
    c(status = "failed", detail = paste0("unknown QA mode: ", mode))
  }

  qa_cache <- new.env(parent = emptyenv())
  source_qa <- lapply(seq_len(nrow(manifest)), function(index) {
    row <- manifest[index, , drop = FALSE]
    cache_key <- paste(row$qa_mode, row$resolved_path, row$resolved_qa_path)
    if (!exists(cache_key, envir = qa_cache, inherits = FALSE)) {
      assign(
        cache_key,
        qa_result(row$qa_mode, row$resolved_path, row$resolved_qa_path),
        envir = qa_cache
      )
    }
    get(cache_key, envir = qa_cache, inherits = FALSE)
  })
  manifest$qa_status <- vapply(source_qa, `[[`, character(1), "status")
  manifest$qa_detail <- vapply(source_qa, `[[`, character(1), "detail")
  manifest$bytes <- vapply(manifest$resolved_path, function(path) {
    if (!file.exists(path) || dir.exists(path)) {
      return(NA_real_)
    }
    as.numeric(file.info(path)$size)
  }, numeric(1))
  manifest$md5 <- vapply(manifest$resolved_path, function(path) {
    if (!file.exists(path) || dir.exists(path)) {
      return(NA_character_)
    }
    unname(tools::md5sum(path))
  }, character(1))
  manifest$modified_at <- vapply(manifest$resolved_path, function(path) {
    if (!file.exists(path)) {
      return(NA_character_)
    }
    format(file.info(path)$mtime, "%Y-%m-%d %H:%M:%S %Z")
  }, character(1))

  source_registry <- manifest
  source_registry$path <- vapply(manifest$resolved_path, portable_path, character(1))
  source_registry$qa_path <- vapply(
    manifest$resolved_qa_path,
    portable_path,
    character(1)
  )
  source_registry$resolved_path <- NULL
  source_registry$resolved_qa_path <- NULL

  ### Canonical watershed roster

  reference_row <- manifest[manifest$family == "reference", , drop = FALSE]
  if (nrow(reference_row) != 1L || reference_row$qa_status != "passed") {
    stop("The source manifest must contain one available canonical reference.")
  }
  reference <- read.csv(
    reference_row$resolved_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  reference <- reference[
    trimws(reference$Has_Spatial_Data) == "Yes" &
      nzchar(trimws(reference$Shapefile_Name)), ,
    drop = FALSE
  ]
  reference$.shape_key <- normalize_site_key(reference$Shapefile_Name)
  reference$.site_key <- paste(
    normalize_lter_key(reference$LTER),
    normalize_stream_key(reference$Stream_Name),
    normalize_site_key(reference$Discharge_File_Name),
    reference$.shape_key,
    sep = "__"
  )
  reference$canonical_row_id <- sprintf("site_%04d", seq_len(nrow(reference)))

  reference_qa <- read.csv(
    reference_row$resolved_qa_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  reference_counts <- setNames(reference_qa$value, reference_qa$check)
  expected_site_rows <- as.integer(reference_counts[["final_spatial_site_rows"]])
  expected_geometry_rows <- as.integer(
    reference_counts[["distinct_watershed_geometries"]]
  )
  if (nrow(reference) != expected_site_rows) {
    stop(
      "Expected ", expected_site_rows,
      " canonical spatial site rows; found ", nrow(reference)
    )
  }
  geometry <- reference[!duplicated(reference$.shape_key), , drop = FALSE]
  geometry <- geometry[, c(
    ".shape_key", "Shapefile_Name", "Roster_Source_ID"
  ), drop = FALSE]
  rownames(geometry) <- NULL
  if (nrow(geometry) != expected_geometry_rows) {
    stop(
      "Expected ", expected_geometry_rows,
      " canonical watershed geometries; found ", nrow(geometry)
    )
  }

  ### Driver assembly

  family_patterns <- c(
    evapo = "^evapotrans_",
    greenup = "^greenup_",
    npp = "^npp_",
    snow = "^snow_",
    precip = "^precip_",
    airtemp = "^temp_",
    elevation = "^(elevation_|basin_slope_)",
    lithology = "^(major_rock$|rocks_)",
    permafrost = "^permafrost_",
    soil = "^(major_soil$|soil_)"
  )

  csv_cache <- new.env(parent = emptyenv())
  read_source <- function(path) {
    if (!exists(path, envir = csv_cache, inherits = FALSE)) {
      assign(
        path,
        read.csv(
          path,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          colClasses = "character"
        ),
        envir = csv_cache
      )
    }
    get(path, envir = csv_cache, inherits = FALSE)
  }

  alias_path <- file.path(
    "generated_outputs", "coverage", "final-spatial-20260803",
    "modis_geometry_aliases.csv"
  )
  aliases <- if (file.exists(alias_path)) {
    read.csv(alias_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    data.frame(Shapefile_Name = character(), source_shape = character())
  }
  aliases$.target_key <- normalize_site_key(aliases$Shapefile_Name)
  aliases$.source_key <- normalize_site_key(aliases$source_shape)

  value_store <- list()
  provenance_store <- list()
  metric_family <- character()
  conflicts <- list()

  collapse_source <- function(data, columns, source_id) {
    if (!"Shapefile_Name" %in% names(data)) {
      stop("Source lacks Shapefile_Name: ", source_id)
    }
    data$.shape_key <- normalize_site_key(data$Shapefile_Name)
    data <- data[!is.na(data$.shape_key) & nzchar(data$.shape_key), , drop = FALSE]
    groups <- split(seq_len(nrow(data)), data$.shape_key)
    out <- data.frame(.shape_key = names(groups), stringsAsFactors = FALSE)
    for (column in columns) {
      values <- lapply(groups, function(index) data[[column]][index])
      conflict <- vapply(values, function(value) {
        length(unique(as.character(value[!spatial_value_is_blank(value)]))) > 1L
      }, logical(1))
      if (any(conflict)) {
        conflicts[[length(conflicts) + 1L]] <<- data.frame(
          source_id = source_id,
          shape_key = names(groups)[conflict],
          column = column,
          stringsAsFactors = FALSE
        )
      }
      out[[column]] <- vapply(values, first_spatial_value, character(1))
    }

    alias_rows <- aliases[aliases$.source_key %in% out$.shape_key, , drop = FALSE]
    if (nrow(alias_rows)) {
      additions <- out[match(alias_rows$.source_key, out$.shape_key), , drop = FALSE]
      additions$.shape_key <- alias_rows$.target_key
      out <- rbind(out, additions)
    }
    out[!duplicated(out$.shape_key), , drop = FALSE]
  }

  combine_sources <- manifest[
    manifest$combine & manifest$qa_status %in% c(
      "passed", "waived", if (allow_incomplete) "in_progress" else ""
    ), ,
    drop = FALSE
  ]
  combine_sources <- combine_sources[
    order(-combine_sources$precedence, combine_sources$source_id), ,
    drop = FALSE
  ]

  for (index in seq_len(nrow(combine_sources))) {
    source <- combine_sources[index, , drop = FALSE]
    family <- source$family[[1]]
    pattern <- family_patterns[[family]]
    data <- read_source(source$resolved_path[[1]])
    columns <- grep(pattern, names(data), value = TRUE)
    if (!length(columns)) {
      stop("No ", family, " columns found in ", source$source_id)
    }
    collapsed <- collapse_source(data, columns, source$source_id)
    row_index <- match(collapsed$.shape_key, geometry$.shape_key)
    valid_rows <- !is.na(row_index)

    for (column in columns) {
      if (is.null(value_store[[column]])) {
        value_store[[column]] <- rep(NA_character_, nrow(geometry))
        provenance_store[[column]] <- rep(NA_character_, nrow(geometry))
        metric_family[[column]] <- family
      }
      candidate <- collapsed[[column]][valid_rows]
      target <- row_index[valid_rows]
      fill <- spatial_value_is_blank(value_store[[column]][target]) &
        !spatial_value_is_blank(candidate)
      if (any(fill)) {
        value_store[[column]][target[fill]] <- candidate[fill]
        provenance_store[[column]][target[fill]] <- source$source_id
      }
    }
  }

  if (!length(value_store)) stop("No driver values were assembled.")

  driver_values <- as.data.frame(value_store, stringsAsFactors = FALSE)
  driver_values$.shape_key <- geometry$.shape_key
  driver_values <- driver_values[, c(".shape_key", setdiff(names(driver_values), ".shape_key"))]

  combined <- merge(
    reference,
    driver_values,
    by = ".shape_key",
    all.x = TRUE,
    sort = FALSE
  )
  combined <- combined[match(reference$canonical_row_id, combined$canonical_row_id), ]
  combined$.site_key <- NULL
  combined$.shape_key <- NULL

  ### Required coverage

  known_unavailable_path <- file.path(
    "generated_outputs", "coverage", "final-spatial-20260803",
    "known_data_unavailable.csv"
  )
  known_unavailable <- if (file.exists(known_unavailable_path)) {
    read.csv(
      known_unavailable_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    data.frame()
  }
  coverage <- build_driver_coverage(
    geometry,
    value_store,
    provenance_store,
    family_patterns,
    known_unavailable
  )
  missing_coverage <- coverage[coverage$status == "missing", , drop = FALSE]

  conflict_table <- if (length(conflicts)) {
    do.call(rbind, conflicts)
  } else {
    data.frame(
      source_id = character(),
      shape_key = character(),
      column = character()
    )
  }
  if (nrow(conflict_table)) {
    conflict_table$selected_source_id <- mapply(
      function(shape_key, column) {
        row <- match(shape_key, geometry$.shape_key)
        if (is.na(row) || is.null(provenance_store[[column]])) {
          return(NA_character_)
        }
        provenance_store[[column]][[row]]
      },
      conflict_table$shape_key,
      conflict_table$column,
      USE.NAMES = FALSE
    )
    conflict_table$affects_selected_value <-
      conflict_table$source_id == conflict_table$selected_source_id
  } else {
    conflict_table$selected_source_id <- character()
    conflict_table$affects_selected_value <- logical()
  }

  ### GEE coverage

  gee_rows <- manifest[manifest$system == "gee", , drop = FALSE]
  gee_input <- gee_rows[gee_rows$family == "watershed_geometry", , drop = FALSE]
  gee_input_keys <- character()
  gee_input_area <- numeric()
  gee_input_data <- data.frame()
  if (nrow(gee_input) == 1L && gee_input$path_exists) {
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("Install the sf package to audit GEE watershed coverage.")
    }
    old_gee <- sf::st_read(gee_input$resolved_path, quiet = TRUE)
    old_gee_data <- sf::st_drop_geometry(old_gee)
    gee_input_data <- old_gee_data
    gee_input_keys <- normalize_site_key(old_gee_data$Shapefile_Name)
    gee_input_area <- suppressWarnings(as.numeric(old_gee_data$polygon_area_km2))
  }

  input_match <- match(geometry$.shape_key, gee_input_keys)
  gee_watershed_coverage <- data.frame(
    shape_key = geometry$.shape_key,
    Shapefile_Name = geometry$Shapefile_Name,
    roster_source_id = geometry$Roster_Source_ID,
    in_current_gee_input = !is.na(input_match),
    gee_polygon_area_km2 = gee_input_area[input_match],
    stringsAsFactors = FALSE
  )

  if (nrow(gee_input_data)) {
    combined_gee_match <- match(
      normalize_site_key(combined$Shapefile_Name),
      gee_input_keys
    )
    gee_fields <- intersect(
      c(
        "watershed_id", "polygon_area_km2", "expected_area_km2",
        "area_percent_difference", "area_qa_status", "spatial_release",
        "canonical_site_count"
      ),
      names(gee_input_data)
    )
    for (field in gee_fields) {
      combined[[paste0("gee_", field)]] <- gee_input_data[[field]][combined_gee_match]
    }
  }
  write_csv(combined, "final_combined_spatial_drivers.csv")

  crosswalk_path <- file.path(
    "generated_outputs", "gee", "final-watersheds-20260805",
    "gee_watershed_site_crosswalk.csv"
  )
  gee_crosswalk <- if (file.exists(crosswalk_path)) {
    read.csv(crosswalk_path, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    data.frame()
  }
  watershed_shape_map <- if (nrow(gee_crosswalk)) {
    unique(data.frame(
      watershed_id = gee_crosswalk$watershed_id,
      shape_key = normalize_site_key(gee_crosswalk$Shapefile_Name),
      stringsAsFactors = FALSE
    ))
  } else {
    data.frame(watershed_id = character(), shape_key = character())
  }

  gee_source_shape_keys <- function(data, shape_column) {
    if ("watershed_id" %in% names(data) && nrow(watershed_shape_map)) {
      return(watershed_shape_map$shape_key[
        match(data$watershed_id, watershed_shape_map$watershed_id)
      ])
    }
    if (!shape_column %in% names(data)) {
      return(rep(NA_character_, nrow(data)))
    }
    normalize_site_key(data[[shape_column]])
  }

  era_row <- gee_rows[gee_rows$family == "era5_annual", , drop = FALSE]
  era_coverage <- list()
  if (nrow(era_row) == 1L && era_row$path_exists) {
    era_files <- list.files(
      era_row$resolved_path,
      pattern = "^era5_land_[0-9]{4}_.*[.]csv$",
      full.names = TRUE
    )
    for (path in era_files) {
      year <- suppressWarnings(as.integer(sub(
        "^era5_land_([0-9]{4})_.*$", "\\1", basename(path)
      )))
      data <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
      keys <- gee_source_shape_keys(data, "shapefile_name")
      value_cols <- intersect(
        c(
          "precip_mm", "temp_degC", "evapotrans_mm", "potential_evap_mm",
          "snow_cover_fraction", "snow_water_equiv_mm"
        ),
        names(data)
      )
      complete <- if (length(value_cols)) {
        rowSums(!is.na(data[, value_cols, drop = FALSE])) == length(value_cols)
      } else {
        rep(FALSE, nrow(data))
      }
      era_coverage[[as.character(year)]] <- unique(keys[complete])
    }
  }

  gee_era_rows <- do.call(rbind, lapply(2000:2025, function(year) {
    keys <- era_coverage[[as.character(year)]]
    present <- geometry$.shape_key %in% keys
    data.frame(
      shape_key = geometry$.shape_key,
      Shapefile_Name = geometry$Shapefile_Name,
      roster_source_id = geometry$Roster_Source_ID,
      year = year,
      status = ifelse(present, "covered", "missing"),
      stringsAsFactors = FALSE
    )
  }))

  land_row <- gee_rows[gee_rows$family == "land_cover", , drop = FALSE]
  land_keys <- character()
  land_shape_keys <- character()
  if (nrow(land_row) == 1L && land_row$path_exists) {
    land <- read.csv(
      land_row$resolved_path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character"
    )
    stream_column <- intersect(c("Stream_Name", "stream_name"), names(land))
    if (length(stream_column)) {
      land_keys <- unique(normalize_stream_key(land[[stream_column[[1]]]]))
    }
    land_shape_keys <- unique(gee_source_shape_keys(land, "Shapefile_Name"))
  }
  geometry_stream_keys <- split(
    normalize_stream_key(reference$Stream_Name),
    reference$.shape_key
  )
  land_present <- vapply(geometry$.shape_key, function(key) {
    key %in% land_shape_keys || any(geometry_stream_keys[[key]] %in% land_keys)
  }, logical(1))

  human_row <- gee_rows[gee_rows$family == "human_impacts", , drop = FALSE]
  human_keys <- character()
  if (nrow(human_row) == 1L && human_row$path_exists) {
    human <- read.csv(
      human_row$resolved_path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      colClasses = "character"
    )
    human_keys <- unique(gee_source_shape_keys(human, "shapefile_name"))
  }
  human_present <- geometry$.shape_key %in% human_keys

  gee_aux_coverage <- data.frame(
    shape_key = geometry$.shape_key,
    Shapefile_Name = geometry$Shapefile_Name,
    roster_source_id = geometry$Roster_Source_ID,
    land_cover_present = land_present,
    human_impacts_present = human_present,
    stringsAsFactors = FALSE
  )
  ### Integration gate

  core_sources <- manifest[manifest$combine, , drop = FALSE]
  core_qa_failures <- sum(!core_sources$qa_status %in% c("passed", "waived"))
  gee_era_missing <- sum(gee_era_rows$status == "missing")
  selected_conflicts <- sum(conflict_table$affects_selected_value, na.rm = TRUE)

  summary <- data.frame(
    check = c(
      "canonical_site_rows",
      "canonical_watershed_geometries",
      "core_source_paths_and_qa",
      "required_aurora_appeears_driver_cells",
      "within_source_geometry_conflicts",
      "combined_site_rows",
      "gee_watershed_input_geometries",
      "gee_era5_watershed_year_cells",
      "gee_land_cover_geometries",
      "gee_human_impact_geometries"
    ),
    status = c(
      if (nrow(reference) == expected_site_rows) "PASS" else "FAIL",
      if (nrow(geometry) == expected_geometry_rows) "PASS" else "FAIL",
      if (!core_qa_failures) "PASS" else "FAIL",
      if (!nrow(missing_coverage)) "PASS" else "FAIL",
      if (!selected_conflicts) "PASS" else "FAIL",
      if (nrow(combined) == nrow(reference)) "PASS" else "FAIL",
      if (all(gee_watershed_coverage$in_current_gee_input)) "PASS" else "FAIL",
      if (!gee_era_missing) "PASS" else "FAIL",
      if (all(land_present)) "PASS" else "FAIL",
      if (all(human_present)) "PASS" else "FAIL"
    ),
    result = c(
      paste0(nrow(reference), " site rows"),
      paste0(nrow(geometry), " distinct geometries"),
      paste0(core_qa_failures, " failed or missing sources"),
      paste0(nrow(missing_coverage), " missing required cells"),
      paste0(
        selected_conflicts, " selected conflicts; ", nrow(conflict_table),
        " total source conflicts"
      ),
      paste0(nrow(combined), " rows written"),
      paste0(sum(gee_watershed_coverage$in_current_gee_input), "/", nrow(geometry)),
      paste0(nrow(gee_era_rows) - gee_era_missing, "/", nrow(gee_era_rows)),
      paste0(sum(land_present), "/", nrow(geometry)),
      paste0(sum(human_present), "/", nrow(geometry))
    ),
    stringsAsFactors = FALSE
  )
  write_csv(summary, "integration_gate_summary.csv")

  saveRDS(
    list(
      generated_at = Sys.time(),
      manifest = source_registry,
      summary = summary,
      coverage = coverage,
      missing_coverage = missing_coverage,
      conflicts = conflict_table,
      gee_watershed_coverage = gee_watershed_coverage,
      gee_era5_coverage = gee_era_rows,
      gee_auxiliary_coverage = gee_aux_coverage
    ),
    file.path(output_root, "integration_audit.rds")
  )

  print(summary, row.names = FALSE)
  cat("\nFinal combined drivers: ", file.path(output_root, "final_combined_spatial_drivers.csv"), "\n", sep = "")
  cat("Detailed audit: ", file.path(output_root, "integration_audit.rds"), "\n", sep = "")

  if (any(summary$status == "FAIL") && !allow_incomplete) {
    stop(
      "Final spatial integration gate is not complete. Review ",
      file.path(output_root, "integration_gate_summary.csv"),
      call. = FALSE
    )
  }

  invisible(list(
    summary = summary,
    combined_path = file.path(output_root, "final_combined_spatial_drivers.csv"),
    audit_path = file.path(output_root, "integration_audit.rds")
  ))
}
