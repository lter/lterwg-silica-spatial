#!/usr/bin/env Rscript

# Preflight the release-aware watershed library against one frozen published
# GlASS-version site-reference table. Each row independently selects its
# spatial-geometry cohort.

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1]]) else ""
script_dir <- if (nzchar(script_path)) dirname(normalizePath(script_path)) else getwd()
source(file.path(script_dir, "workflow_paths.R"))

args <- commandArgs(trailingOnly = TRUE)
release_default <- trimws(Sys.getenv("SILICA_REFERENCE_RELEASE", unset = ""))
release_snapshot <- cli_integer(
  args,
  "--release",
  default = if (nzchar(release_default)) release_default else NULL,
  required = !nzchar(release_default),
  minimum = 1L,
  maximum = 3L
)
reference_path <- cli_value(args, "--input")
shape_root <- cli_value(args, "--shape-root")
if (is.null(reference_path) || is.null(shape_root)) {
  root_path <- resolve_silica_data_root()
  if (is.null(reference_path)) {
    reference_path <- silica_release_reference_file(
      root_path,
      release = release_snapshot
    )
  }
  if (is.null(shape_root)) {
    shape_root <- silica_shape_library_root(root_path)
  }
}
reference_path <- require_input_file(reference_path, "site-reference table")
shape_root <- require_input_dir(shape_root, "canonical shapefile library")
release_dirs <- stats::setNames(
  file.path(shape_root, paste0("data_release_", 1:3)),
  as.character(1:3)
)
missing_release_dirs <- release_dirs[!dir.exists(release_dirs)]
if (length(missing_release_dirs)) {
  stop(
    "Missing spatial-version director",
    if (length(missing_release_dirs) == 1L) "y: " else "ies: ",
    paste(missing_release_dirs, collapse = ", "),
    call. = FALSE
  )
}

reference <- utils::read.csv(
  reference_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)
required_columns <- c("LTER", "Stream_Name", "Shapefile_Name")
if (!all(required_columns %in% names(reference)) ||
  !"Spatial_Data_Version" %in% names(reference)) {
  stop(
    "Reference table must contain LTER, Stream_Name, Shapefile_Name, and ",
    "Spatial_Data_Version.",
    call. = FALSE
  )
}

reference$.__release <- suppressWarnings(
  as.integer(reference[["Spatial_Data_Version"]])
)
reference$.__shape <- trimws(as.character(reference$Shapefile_Name))
reference$.__shape[reference$.__shape == ""] <- NA_character_

if ("Published_Spatial_Data_Available" %in% names(reference)) {
  has_spatial <- tolower(trimws(as.character(
    reference$Published_Spatial_Data_Available
  ))) %in% c("yes", "true", "1", "y")
  spatial_rows <- reference[
    has_spatial & !is.na(reference$.__shape), ,
    drop = FALSE
  ]
} else if ("Has_Spatial_Data" %in% names(reference)) {
  has_spatial <- tolower(trimws(as.character(reference$Has_Spatial_Data))) %in%
    c("yes", "true", "1", "y")
  spatial_rows <- reference[has_spatial & !is.na(reference$.__shape), , drop = FALSE]
} else {
  spatial_rows <- reference[
    !is.na(reference$.__release) &
      !is.na(reference$.__shape), ,
    drop = FALSE
  ]
}

bad_release <- is.na(spatial_rows$.__release) |
  !spatial_rows$.__release %in% 1:3
if (any(bad_release)) {
  stop(
    "Spatial rows contain invalid or missing release values:\n- ",
    paste(
      unique(paste(
        spatial_rows$LTER[bad_release],
        spatial_rows$Stream_Name[bad_release],
        spatial_rows$.__shape[bad_release],
        sep = " / "
      )),
      collapse = "\n- "
    ),
    call. = FALSE
  )
}

required_extensions <- c("shp", "shx", "dbf", "prj")
expected_paths <- lapply(seq_len(nrow(spatial_rows)), function(i) {
  release <- as.character(spatial_rows$.__release[i])
  stem <- spatial_rows$.__shape[i]
  directory <- file.path(release_dirs[[release]], stem)
  files <- file.path(directory, paste0(stem, ".", required_extensions))
  data.frame(
    LTER = spatial_rows$LTER[i],
    Stream_Name = spatial_rows$Stream_Name[i],
    release = release,
    Shapefile_Name = stem,
    directory = directory,
    complete = all(file.exists(files)),
    stringsAsFactors = FALSE
  )
})
expected <- do.call(rbind, expected_paths)
expected <- unique(expected)

missing <- expected[!expected$complete, , drop = FALSE]
if (nrow(missing)) {
  stop(
    "Referenced watershed bundles are missing or incomplete:\n- ",
    paste(
      paste(
        missing$release,
        missing$Shapefile_Name,
        missing$LTER,
        missing$Stream_Name,
        sep = " / "
      ),
      collapse = "\n- "
    ),
    call. = FALSE
  )
}

# Semantic QA: historical code wrote an area-difference diagnostic but did not
# enforce it, and rows sharing one LTER/release/Shapefile_Name were extracted
# over the same geometry. Reproduce historical Releases 1/2 with warnings, but
# require explicit living-table documentation before a Release 3 full run.
strict_semantic_qa <- cli_has_flag(args, "--strict") ||
  release_snapshot == 3L ||
  tolower(Sys.getenv(
    "SILICA_STRICT_SPATIAL_SEMANTICS",
    unset = "false"
  )) %in% c("1", "true", "yes", "y")

spatial_rows$.__bundle_key <- paste(
  spatial_rows$.__release,
  tolower(spatial_rows$.__shape),
  sep = "::"
)
spatial_rows$.__shared_count <- ave(
  spatial_rows$.__bundle_key,
  spatial_rows$.__bundle_key,
  FUN = length
)

note_columns <- intersect(
  c("Spatial_Notes", "CQ_Notes"),
  names(spatial_rows)
)
if (length(note_columns)) {
  note_text <- apply(
    spatial_rows[, note_columns, drop = FALSE],
    1,
    function(x) paste(x[!is.na(x)], collapse = " ")
  )
} else {
  note_text <- rep("", nrow(spatial_rows))
}
note_text <- tolower(note_text)
spatial_rows$.__note_text <- note_text

if (!requireNamespace("sf", quietly = TRUE)) {
  stop(
    "Package sf is required for release semantic preflight.",
    call. = FALSE
  )
}

bundle_area <- lapply(seq_len(nrow(expected)), function(i) {
  stem <- expected$Shapefile_Name[i]
  path <- file.path(expected$directory[i], paste0(stem, ".shp"))
  bundle_key <- paste(
    expected$release[i],
    tolower(stem),
    sep = "::"
  )
  tryCatch(
    {
      geometry <- suppressWarnings(sf::st_read(path, quiet = TRUE))
      geometry <- suppressWarnings(sf::st_make_valid(geometry))
      area_km2 <- suppressWarnings(
        sum(as.numeric(sf::st_area(sf::st_transform(geometry, 6933)))) / 1e6
      )
      data.frame(
        .__bundle_key = bundle_key,
        .__polygon_area_km2 = area_km2,
        .__geometry_error = NA_character_,
        stringsAsFactors = FALSE
      )
    },
    error = function(error) {
      data.frame(
        .__bundle_key = bundle_key,
        .__polygon_area_km2 = NA_real_,
        .__geometry_error = conditionMessage(error),
        stringsAsFactors = FALSE
      )
    }
  )
})
bundle_area <- unique(do.call(rbind, bundle_area))
spatial_rows <- merge(
  spatial_rows,
  bundle_area,
  by = ".__bundle_key",
  all.x = TRUE,
  sort = FALSE
)

shared_documented <- grepl(
  "shared|alias|same physical|duplicate|one spatial unit|same watershed",
  spatial_rows$.__note_text
)
unresolved_shared <- spatial_rows$.__shared_count > 1 &
  !shared_documented

reported_area <- if ("drainSqKm" %in% names(spatial_rows)) {
  suppressWarnings(as.numeric(spatial_rows$drainSqKm))
} else {
  rep(NA_real_, nrow(spatial_rows))
}
spatial_rows$.__area_difference_percent <- ifelse(
  is.na(reported_area) |
    reported_area <= 0 |
    is.na(spatial_rows$.__polygon_area_km2),
  NA_real_,
  100 * (
    spatial_rows$.__polygon_area_km2 - reported_area
  ) / reported_area
)

# Documentation must explicitly indicate that polygon/report-area disagreement
# was considered. This does not require overwriting the reported area.
area_documented <- grepl(
  "polygon area|geometry area|area mismatch|area difference|qa only|retain reported|preserve reported",
  spatial_rows$.__note_text
)
unresolved_area <- !is.na(spatial_rows$.__area_difference_percent) &
  abs(spatial_rows$.__area_difference_percent) > 15 &
  !area_documented
unresolved_geometry <- !is.na(spatial_rows$.__geometry_error) &
  nzchar(spatial_rows$.__geometry_error)

semantic_messages <- character()
if (any(unresolved_geometry)) {
  semantic_messages <- c(
    semantic_messages,
    paste0(
      "Bundles whose geometry could not be measured: ",
      length(unique(spatial_rows$.__bundle_key[unresolved_geometry]))
    )
  )
}
if (any(unresolved_shared)) {
  semantic_messages <- c(
    semantic_messages,
    paste0(
      "Undocumented shared-polygon rows: ",
      sum(unresolved_shared),
      " (",
      length(unique(spatial_rows$.__bundle_key[unresolved_shared])),
      " geometry groups)"
    )
  )
}
if (any(unresolved_area)) {
  semantic_messages <- c(
    semantic_messages,
    paste0(
      "Undocumented >15% polygon/reported-area mismatches: ",
      sum(unresolved_area)
    )
  )
}

if (length(semantic_messages) && strict_semantic_qa) {
  geometry_examples <- if (any(unresolved_geometry)) {
    unique(paste(
      spatial_rows$LTER[unresolved_geometry],
      spatial_rows$Stream_Name[unresolved_geometry],
      spatial_rows$.__shape[unresolved_geometry],
      spatial_rows$.__geometry_error[unresolved_geometry],
      sep = " / "
    ))
  } else {
    character()
  }
  shared_examples <- if (any(unresolved_shared)) {
    unique(paste(
      spatial_rows$LTER[unresolved_shared],
      spatial_rows$Stream_Name[unresolved_shared],
      spatial_rows$.__shape[unresolved_shared],
      sep = " / "
    ))
  } else {
    character()
  }
  area_examples <- if (any(unresolved_area)) {
    unique(paste0(
      spatial_rows$LTER[unresolved_area],
      " / ",
      spatial_rows$Stream_Name[unresolved_area],
      " / ",
      spatial_rows$.__shape[unresolved_area],
      " / ",
      sprintf(
        "%+.1f%%",
        spatial_rows$.__area_difference_percent[unresolved_area]
      )
    ))
  } else {
    character()
  }
  stop(
    paste(semantic_messages, collapse = "\n"),
    if (length(geometry_examples)) {
      paste0(
        "\nGeometry examples:\n- ",
        paste(utils::head(geometry_examples, 20), collapse = "\n- ")
      )
    } else {
      ""
    },
    if (length(shared_examples)) {
      paste0(
        "\nShared examples:\n- ",
        paste(utils::head(shared_examples, 20), collapse = "\n- ")
      )
    } else {
      ""
    },
    if (length(area_examples)) {
      paste0(
        "\nArea-mismatch examples:\n- ",
        paste(utils::head(area_examples, 20), collapse = "\n- ")
      )
    } else {
      ""
    },
    "\nDocument an accepted alias/shared-unit decision or the reviewed ",
    "polygon/report-area mismatch in Spatial_Notes/CQ_Notes before running.",
    call. = FALSE
  )
}

cat("Published GlASS version:", release_snapshot, "\n")
cat("Reference table:", reference_path, "\n")
cat("Spatial rows:", nrow(spatial_rows), "\n")
cat("Distinct release/bundle pairs:", nrow(unique(expected[, c(
  "release",
  "Shapefile_Name"
)])), "\n")
cat("All referenced bundles are complete and in the declared release folder.\n")
if (length(semantic_messages)) {
  cat(
    "Historical semantic-QA warning (non-blocking for Release 1/2):\n- ",
    paste(semantic_messages, collapse = "\n- "),
    "\n",
    sep = ""
  )
} else {
  cat("All shared polygons and >15% area mismatches are explicitly documented.\n")
}
