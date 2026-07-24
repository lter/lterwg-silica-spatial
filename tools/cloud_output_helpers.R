# Optional cloud-upload helpers for spatial outputs.

upload_spatial_output <- function(path) {
  skip_upload <- tolower(Sys.getenv(
    "SILICA_SKIP_DRIVE_UPLOAD",
    unset = "false"
  )) %in% c("true", "t", "1", "yes", "y")
  if (skip_upload) {
    message("Skipping Google Drive upload: ", basename(path))
    return(invisible(FALSE))
  }

  folder_id <- Sys.getenv(
    "SILICA_SPATIAL_OUTPUT_DRIVE_FOLDER_ID",
    unset = ""
  )
  if (!nzchar(folder_id)) {
    stop(
      "Set SILICA_SPATIAL_OUTPUT_DRIVE_FOLDER_ID or set ",
      "SILICA_SKIP_DRIVE_UPLOAD=TRUE.",
      call. = FALSE
    )
  }
  if (!file.exists(path)) {
    stop("Cannot upload missing output: ", path, call. = FALSE)
  }
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Install the googledrive package before enabling upload.", call. = FALSE)
  }

  overwrite <- tolower(Sys.getenv(
    "SILICA_GOOGLE_DRIVE_OVERWRITE",
    unset = "true"
  )) %in% c("true", "t", "1", "yes", "y")
  uploaded <- googledrive::drive_upload(
    media = path,
    overwrite = overwrite,
    path = googledrive::as_id(folder_id)
  )
  message("Uploaded spatial output: ", uploaded$name)
  invisible(TRUE)
}
