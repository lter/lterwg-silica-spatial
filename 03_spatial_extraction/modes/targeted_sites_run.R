subset_path <- Sys.getenv("SILICA_SITE_SUBSET_FILE", unset = "")
if (!nzchar(subset_path)) {
  stop("Targeted site mode needs SILICA_SITE_SUBSET_FILE.", call. = FALSE)
}

combine_full <- tolower(Sys.getenv("SILICA_COMBINE_FULL", unset = "false"))

message("Run targeted site workflow")
status <- system2(
  "Rscript",
  c(
    file.path("03_spatial_extraction", "wrappers", "run-targeted-subset-workflow.R"),
    "--subset", subset_path,
    "--combine-full", combine_full
  )
)

if (!identical(status, 0L)) {
  stop("Targeted site workflow did not finish cleanly.", call. = FALSE)
}
