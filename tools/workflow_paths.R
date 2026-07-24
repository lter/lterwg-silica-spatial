# Shared path and package helpers for the numbered workflow.
#
# Publication-facing scripts must not depend on a contributor's home
# directory or current working directory. Set `SILICA_DATA_ROOT`, or keep the
# data directory beside the repository under a conventional name.

workflow_paths_source <- tryCatch(
  {
    source_files <- vapply(
      sys.frames(),
      function(frame) {
        if (is.null(frame$ofile)) "" else as.character(frame$ofile)
      },
      character(1)
    )
    source_files <- source_files[nzchar(source_files)]
    normalizePath(tail(source_files, 1), mustWork = TRUE)
  },
  error = function(...) ""
)
workflow_tools_dir <- if (nzchar(workflow_paths_source)) {
  dirname(workflow_paths_source)
} else {
  file.path(getwd(), "tools")
}
source(file.path(workflow_tools_dir, "subset_and_output_helpers.R"))
source(file.path(workflow_tools_dir, "cli_helpers.R"))

latest_existing_paths <- function(pattern) {
  hits <- Sys.glob(pattern)
  if (!length(hits)) {
    return(character())
  }
  hits[order(file.info(hits)$mtime, decreasing = TRUE)]
}

resolve_workflow_input <- function(
  cli_path = NULL,
  env_var,
  candidates = character(),
  label = "input",
  cli_flag = "--input"
) {
  if (!is.null(cli_path) && nzchar(trimws(cli_path))) {
    return(require_input_file(cli_path, label))
  }

  configured <- env_value(env_var, default = "")
  if (nzchar(configured)) {
    return(require_input_file(configured, paste0(label, " from ", env_var)))
  }

  existing <- candidates[file.exists(candidates)]
  if (length(existing)) {
    return(normalizePath(existing[[1]], mustWork = TRUE))
  }

  stop(
    "Could not locate ",
    label,
    ". Pass ",
    cli_flag,
    " or set ",
    env_var,
    if (length(candidates)) {
      paste0(". Checked:\n- ", paste(candidates, collapse = "\n- "))
    } else {
      "."
    },
    call. = FALSE
  )
}

use_project_rlibs <- function() {
  project_lib <- file.path(getwd(), ".Rlibs")
  if (dir.exists(project_lib)) {
    .libPaths(unique(c(normalizePath(project_lib, mustWork = TRUE), .libPaths())))
  }
}

silica_find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, mustWork = TRUE)
  repeat {
    markers <- c(
      file.path(current, ".git"),
      file.path(current, "01_run_config.R")
    )
    if (any(file.exists(markers) | dir.exists(markers))) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      stop(
        "Could not locate the repository root from ",
        start,
        ". Run the command from the repository or pass an explicit path.",
        call. = FALSE
      )
    }
    current <- parent
  }
}

load_workflow_packages <- function(packages) {
  use_project_rlibs()
  for (pkg in packages) {
    librarian::shelf(pkg, quiet = TRUE)
  }
}

resolve_silica_data_root <- function() {
  env_root <- env_value("SILICA_DATA_ROOT", default = "")
  if (nzchar(env_root) && dir.exists(env_root)) {
    return(normalizePath(env_root, mustWork = TRUE))
  }

  repo_root <- silica_find_repo_root()
  candidates <- c(
    file.path(repo_root, "spatial-data-extractions"),
    file.path(dirname(repo_root), "spatial-data-extractions"),
    file.path(repo_root, "spatial_data_extractions"),
    file.path(dirname(repo_root), "spatial_data_extractions"),
    file.path(repo_root, "si-watershed-extract"),
    file.path(dirname(repo_root), "si-watershed-extract"),
    "/home/shares/lter-si/si-watershed-extract"
  )

  hit <- candidates[dir.exists(candidates)]
  if (!length(hit)) {
    stop(
      "Could not find the silica data root. Set SILICA_DATA_ROOT to the ",
      "directory containing `silica-shapefiles`, `spatial-data-files`, ",
      "and `master-datasets`.",
      call. = FALSE
    )
  }

  normalizePath(hit[[1]], mustWork = TRUE)
}

silica_use_canonical_release_library <- function() {
  tolower(Sys.getenv(
    "SILICA_USE_CANONICAL_RELEASE_LIBRARY",
    unset = "false"
  )) %in% c("1", "true", "yes", "y")
}

silica_reference_release <- function(required = FALSE) {
  value <- trimws(Sys.getenv("SILICA_REFERENCE_RELEASE", unset = ""))
  if (!nzchar(value)) {
    if (required) {
      stop(
        "Set SILICA_REFERENCE_RELEASE to 1, 2, or 3 when using the canonical ",
        "published GlASS reference table.",
        call. = FALSE
      )
    }
    return(NA_integer_)
  }
  release <- suppressWarnings(as.integer(value))
  if (is.na(release) || !release %in% 1:3) {
    stop("SILICA_REFERENCE_RELEASE must be 1, 2, or 3.", call. = FALSE)
  }
  release
}

silica_shape_library_root <- function(root_path) {
  env_root <- trimws(Sys.getenv("SILICA_SHAPE_LIBRARY_ROOT", unset = ""))
  candidates <- c(
    env_root,
    file.path(root_path, "silica-shapefiles")
  )
  candidates <- candidates[nzchar(candidates) & dir.exists(candidates)]
  if (!length(candidates)) {
    stop(
      "Could not locate the canonical shapefile library. Set ",
      "SILICA_SHAPE_LIBRARY_ROOT.",
      call. = FALSE
    )
  }
  normalizePath(candidates[[1]], mustWork = TRUE)
}

silica_release_reference_file <- function(root_path, release = NULL) {
  env_ref <- trimws(Sys.getenv("SILICA_SITE_REF_FILE", unset = ""))
  if (!nzchar(env_ref)) {
    env_ref <- trimws(Sys.getenv("SILICA_BASE_FILE", unset = ""))
  }
  if (nzchar(env_ref)) {
    if (!file.exists(env_ref)) {
      stop("Configured site-reference table does not exist: ", env_ref, call. = FALSE)
    }
    return(normalizePath(env_ref, mustWork = TRUE))
  }

  if (is.null(release)) {
    release <- silica_reference_release(required = TRUE)
  }
  candidate <- file.path(
    silica_shape_library_root(root_path),
    "reference_tables",
    "published_glass_versions",
    paste0("glass_v", release),
    "site_reference_table.csv"
  )
  if (!file.exists(candidate)) {
    stop(
      "No installed site-reference table for published GlASS version ",
      release,
      ": ",
      candidate,
      if (release == 3L) {
        paste0(
          "\nThe living-table review is complete. Export/install the final ",
          "CSV with tools/install_release_reference_table.R ",
          "and resolve every referenced watershed bundle."
        )
      } else {
        ""
      },
      call. = FALSE
    )
  }
  normalizePath(candidate, mustWork = TRUE)
}

silica_release_shapefile_dirs <- function(root_path) {
  library_root <- silica_shape_library_root(root_path)
  dirs <- setNames(
    file.path(library_root, paste0("data_release_", 1:3)),
    as.character(1:3)
  )
  missing <- dirs[!dir.exists(dirs)]
  if (length(missing)) {
    stop(
      "Missing canonical shapefile release director",
      if (length(missing) == 1L) "y: " else "ies: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  stats::setNames(
    normalizePath(unname(dirs), mustWork = TRUE),
    names(dirs)
  )
}

read_silica_site_reference <- function(site_coord_dir = NULL) {
  env_ref <- Sys.getenv("SILICA_BASE_FILE", unset = "")
  if (!nzchar(env_ref)) {
    env_ref <- Sys.getenv("SILICA_SITE_REF_FILE", unset = "")
  }

  canonical_ref <- character()
  if (silica_use_canonical_release_library()) {
    canonical_ref <- silica_release_reference_file(
      resolve_silica_data_root(),
      silica_reference_release(required = TRUE)
    )
  }
  candidates <- c(
    env_ref,
    canonical_ref,
    if (!is.null(site_coord_dir)) file.path(site_coord_dir, "silica-coords_RAW.xlsx") else character()
  )
  candidates <- candidates[nzchar(candidates)]
  ref_path <- candidates[file.exists(candidates)][1]

  if (is.na(ref_path) || !nzchar(ref_path)) {
    stop(
      "Could not locate site reference table. Set SILICA_BASE_FILE or provide ",
      "silica-coords_RAW.xlsx in the site-coordinate directory.",
      call. = FALSE
    )
  }

  ext <- tolower(tools::file_ext(ref_path))
  if (ext == "csv") {
    return(clean_lter_column(utils::read.csv(ref_path, stringsAsFactors = FALSE, check.names = TRUE)))
  }

  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("Package readxl is required to read site reference workbook: ", ref_path, call. = FALSE)
  }
  clean_lter_column(readxl::read_excel(path = ref_path))
}

silica_site_coordinates_dir <- function(root_path) {
  silica_output_dir_from_root(
    root_path = root_path,
    env_var = "SILICA_SITE_COORD_DIR",
    path_candidates = c(
      file.path(root_path, "site-coordinates"),
      file.path(root_path, "silica-shapefiles", "site-coordinates")
    )
  )
}

silica_raw_driver_data_dir <- function(root_path) {
  dir <- Sys.getenv("SILICA_RAW_DRIVER_DIR", unset = file.path(root_path, "raw-driver-data"))
  if (!dir.exists(dir)) {
    stop("Missing raw-driver-data directory: ", dir, call. = FALSE)
  }
  normalizePath(dir, mustWork = TRUE)
}

silica_extracted_data_dir <- function(root_path) {
  silica_output_dir_from_root(
    root_path = root_path,
    env_var = "SILICA_EXTRACTED_DIR",
    path_candidates = c(
      file.path(root_path, "extracted-data"),
      file.path(root_path, "silica-shapefiles", "extracted-data")
    )
  )
}

silica_hydrosheds_raw_dir <- function(root_path) {
  dir <- Sys.getenv("SILICA_HYDROSHEDS_RAW_DIR", unset = file.path(root_path, "hydrosheds-raw"))
  if (!dir.exists(dir)) {
    stop("Missing hydrosheds-raw directory: ", dir, call. = FALSE)
  }
  normalizePath(dir, mustWork = TRUE)
}

silica_watershed_file <- function(root_path) {
  watershed_file <- Sys.getenv("SILICA_WATERSHED_FILE", unset = "")
  if (nzchar(watershed_file)) {
    if (!file.exists(watershed_file)) {
      stop("SILICA_WATERSHED_FILE does not exist: ", watershed_file, call. = FALSE)
    }
    return(normalizePath(watershed_file, mustWork = TRUE))
  }

  silica_sitecoord_existing_file(root_path, "silica-watersheds", "shp")
}
