source(file.path(getwd(), "tools", "workflow_paths.R"))

pre_aurora_root <- function(review_root = silica_review_root(resolve_silica_data_root())) {
  file.path(review_root, "01_pre_aurora_run")
}

pre_aurora_candidates_dir <- function(review_root = silica_review_root(resolve_silica_data_root())) {
  file.path(pre_aurora_root(review_root), "01_candidates")
}

pre_aurora_approvals_dir <- function(review_root = silica_review_root(resolve_silica_data_root())) {
  file.path(pre_aurora_root(review_root), "02_approvals")
}

pre_aurora_aurora_subsets_dir <- function(review_root = silica_review_root(resolve_silica_data_root())) {
  file.path(pre_aurora_root(review_root), "03_aurora_subsets")
}

pre_aurora_reports_dir <- function(review_root = silica_review_root(resolve_silica_data_root())) {
  file.path(pre_aurora_root(review_root), "04_reports")
}

ensure_pre_aurora_dirs <- function(review_root = silica_review_root(resolve_silica_data_root())) {
  dirs <- c(
    pre_aurora_root(review_root),
    pre_aurora_candidates_dir(review_root),
    pre_aurora_approvals_dir(review_root),
    pre_aurora_aurora_subsets_dir(review_root),
    pre_aurora_reports_dir(review_root)
  )
  for (dir_i in dirs) {
    dir.create(dir_i, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(dirs)
}

latest_pre_aurora_file <- function(dir_path, pattern) {
  hits <- Sys.glob(file.path(dir_path, pattern))
  if (!length(hits)) {
    return(NA_character_)
  }
  hits[which.max(file.info(hits)$mtime)]
}
