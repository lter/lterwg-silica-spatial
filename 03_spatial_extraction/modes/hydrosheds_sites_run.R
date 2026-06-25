args <- commandArgs(trailingOnly = TRUE)
source(file.path("tools", "workflow_paths.R"))

get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit)) return(default)
  args[hit[1] + 1]
}

combine_full <- tolower(get_arg("--combine-full", "false"))
qa_root <- get_arg("--outdir", silica_review_root(resolve_silica_data_root()))
subset_path <- get_arg("--subset", Sys.getenv("SILICA_SITE_SUBSET_FILE", unset = ""))

message("Step 01: build the mixed run-list review files")
status_run_list <- system2(
  "Rscript",
  c("03_spatial_extraction/run-lists/01_build_run_candidates.R", "--outdir", qa_root)
)

if (!identical(status_run_list, 0L)) {
  stop("run-list workflow stopped while building the review files.", call. = FALSE)
}

if (is.null(subset_path) || !nzchar(subset_path)) {
  approval_candidates <- Sys.glob(file.path(qa_root, "run-lists", "02_approvals", "run_list_approval_mixed_*.csv"))
  holdout_review <- Sys.glob(file.path(qa_root, "run-lists", "02_approvals", "run_list_holdouts_review_*.csv"))

  if (length(approval_candidates)) {
    approval_candidates <- approval_candidates[which.max(file.info(approval_candidates)$mtime)]
    message("Mixed run-list approval list written to: ", approval_candidates)
  }

  if (length(holdout_review)) {
    holdout_review <- holdout_review[which.max(file.info(holdout_review)$mtime)]
    message("run-list holdout review written to: ", holdout_review)
  }

  message("Stopping before extraction so you can review the mixed approval file.")
  message("Next steps:")
  message("  1. Review the mixed approval CSV.")
  message("  2. Run: Rscript 03_spatial_extraction/run-lists/02_split_approved_runs.R")
  message("  3. Run: Rscript 03_spatial_extraction/run-lists/03_print_aurora_handoff.R")
} else {
  subset_path <- normalizePath(subset_path, mustWork = TRUE)
  message("Run reviewed run-list subset for ", basename(subset_path))

  Sys.setenv(
    SILICA_REBUILD_ARTISANAL = "FALSE",
    SILICA_REBUILD_HYDROSHEDS = "TRUE"
  )

  status_run <- system2(
    "Rscript",
    c(
      "03_spatial_extraction/wrappers/run-targeted-subset-workflow.R",
      "--subset", subset_path,
      "--combine-full", combine_full
    )
  )

  if (!identical(status_run, 0L)) {
    stop("run-list workflow stopped during the targeted run.", call. = FALSE)
  }
}
