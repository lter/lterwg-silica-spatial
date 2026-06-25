source(file.path(getwd(), "tools", "workflow_paths.R"))

run_list_root <- function(review_root = silica_review_root(resolve_silica_data_root())) {
  file.path(review_root, "run-lists")
}

run_list_candidates_dir <- function(review_root = silica_review_root(resolve_silica_data_root())) {
  file.path(run_list_root(review_root), "01_candidates")
}

run_list_approvals_dir <- function(review_root = silica_review_root(resolve_silica_data_root())) {
  file.path(run_list_root(review_root), "02_approvals")
}

run_list_aurora_subsets_dir <- function(review_root = silica_review_root(resolve_silica_data_root())) {
  file.path(run_list_root(review_root), "03_aurora_subsets")
}

run_list_reports_dir <- function(review_root = silica_review_root(resolve_silica_data_root())) {
  file.path(run_list_root(review_root), "04_reports")
}

ensure_run_list_dirs <- function(review_root = silica_review_root(resolve_silica_data_root())) {
  dirs <- c(
    run_list_root(review_root),
    run_list_candidates_dir(review_root),
    run_list_approvals_dir(review_root),
    run_list_aurora_subsets_dir(review_root),
    run_list_reports_dir(review_root)
  )
  for (dir_i in dirs) {
    dir.create(dir_i, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(dirs)
}

latest_run_list_file <- function(dir_path, pattern) {
  hits <- Sys.glob(file.path(dir_path, pattern))
  if (!length(hits)) {
    return(NA_character_)
  }
  hits[which.max(file.info(hits)$mtime)]
}
