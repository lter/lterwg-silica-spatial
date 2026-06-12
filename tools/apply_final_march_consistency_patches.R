#!/usr/bin/env Rscript

source("tools/subset_and_output_helpers.R")

arg_or_default <- function(args, i, default) {
  if (length(args) >= i && nzchar(args[[i]])) args[[i]] else default
}

read_csv_clean <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- make.unique(ifelse(is.na(names(x)) | names(x) == "", "blank_col", names(x)))
  x
}

norm_blank <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x %in% c("", "NA", "NaN", "nan", "NULL", "null")] <- NA_character_
  x
}

norm_key_part <- function(x) {
  x <- norm_blank(x)
  x[is.na(x)] <- ""
  tolower(x)
}

row_key <- function(df) {
  paste(
    normalize_lter_key(df$LTER),
    normalize_stream_key(df$Stream_Name),
    norm_key_part(df$Discharge_File_Name),
    norm_key_part(df$Shapefile_Name),
    sep = "||"
  )
}

as_key <- function(lter, stream_name, discharge_file_name, shapefile_name) {
  paste(
    normalize_lter_key(lter),
    normalize_stream_key(stream_name),
    norm_key_part(discharge_file_name),
    norm_key_part(shapefile_name),
    sep = "||"
  )
}

set_value <- function(df, row_index, col, new_value, reason, source_file, patch_log) {
  if (!col %in% names(df)) {
    return(list(df = df, patch_log = patch_log))
  }

  old_value <- df[[col]][row_index]
  old_blank <- norm_blank(old_value)
  new_blank <- norm_blank(new_value)
  if ((is.na(old_blank) && is.na(new_blank)) || (!is.na(old_blank) && !is.na(new_blank) && old_blank == new_blank)) {
    return(list(df = df, patch_log = patch_log))
  }

  df[[col]][row_index] <- if (is.na(new_blank)) NA_character_ else as.character(new_value)
  patch_log[[length(patch_log) + 1L]] <- data.frame(
    LTER = df$LTER[row_index],
    Stream_Name = df$Stream_Name[row_index],
    Discharge_File_Name = df$Discharge_File_Name[row_index],
    Shapefile_Name = df$Shapefile_Name[row_index],
    column = col,
    old_value = old_value,
    new_value = if (is.na(new_blank)) NA_character_ else as.character(new_value),
    reason = reason,
    source_file = source_file,
    stringsAsFactors = FALSE
  )

  list(df = df, patch_log = patch_log)
}

args <- commandArgs(trailingOnly = TRUE)
date_tag <- Sys.getenv("SILICA_AUDIT_DATE", unset = "20260606")
data_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions"
target_file <- arg_or_default(
  args,
  1,
  file.path(
    data_root,
    "final-data",
    "full-dataset",
    "all-data_si-extract_4_20260526_final-extract-merge-v4-remaining-dynamic-domain-spatial-data-extractions.csv"
  )
)
march_file <- arg_or_default(
  args,
  2,
  file.path(data_root, "final-data", "full-dataset", "all-data_si-extract_2_20250325.csv")
)
out_file <- arg_or_default(args, 3, target_file)
log_file <- arg_or_default(
  args,
  4,
  file.path(
    getwd(),
    "generated_outputs",
    "review",
    "harmonization",
    paste0("final-", date_tag),
    paste0("final_march_consistency_patch_log_", date_tag, ".csv")
  )
)

if (!file.exists(target_file)) {
  stop("Missing target file: ", target_file, call. = FALSE)
}
if (!file.exists(march_file)) {
  stop("Missing March source file: ", march_file, call. = FALSE)
}

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)

target <- read_csv_clean(target_file)
march <- read_csv_clean(march_file)
target$.row_key <- row_key(target)
march$.row_key <- row_key(march)

target_keys <- target$.row_key
march_keys <- march$.row_key
patch_log <- list()

snow_restore_sites <- data.frame(
  LTER = c(rep("GRO", 6), "LMP"),
  Stream_Name = c("Kolyma", "Lena", "Mackenzie", "Ob", "Yenisey", "Yukon", "LMP73"),
  Discharge_File_Name = c(
    "GRO_Kolyma_Q",
    "GRO_Lena_Q",
    "GRO_Mackenzie_Q",
    "GRO_Ob_Q",
    "GRO_Yenisey_Q",
    "GRO_Yukon_Q",
    "LMP_LMP73_Q"
  ),
  Shapefile_Name = c("GRO_Kolyma", "GRO_Lena", "GRO_Mackenzie", "GRO_Ob", "GRO_Yenisey", "GRO_Yukon", "LMP"),
  stringsAsFactors = FALSE
)
snow_restore_sites$.row_key <- as_key(
  snow_restore_sites$LTER,
  snow_restore_sites$Stream_Name,
  snow_restore_sites$Discharge_File_Name,
  snow_restore_sites$Shapefile_Name
)

snow_cols_from_march <- intersect(
  grep("^snow_20(0[2-9]|1[0-9]|2[0-4])_(num_days|max_prop_area)$", names(march), value = TRUE),
  names(target)
)
snow_cols_unverified <- grep("^snow_20(0[0-1]|25)_(num_days|max_prop_area)$", names(target), value = TRUE)

for (key in snow_restore_sites$.row_key) {
  target_row <- match(key, target_keys)
  march_row <- match(key, march_keys)
  if (is.na(target_row) || is.na(march_row)) {
    warning("Could not match snow patch key in target or March file: ", key)
    next
  }

  for (col in snow_cols_from_march) {
    new_value <- march[[col]][march_row]
    result <- set_value(
      target,
      target_row,
      col,
      new_value,
      "restore March 2025 snow value after erroneous domain zeroing",
      march_file,
      patch_log
    )
    target <- result$df
    patch_log <- result$patch_log
  }

  for (col in snow_cols_unverified) {
    result <- set_value(
      target,
      target_row,
      col,
      NA_character_,
      "clear unverified snow value; no March source value available",
      "",
      patch_log
    )
    target <- result$df
    patch_log <- result$patch_log
  }
}

bad_canada_evapo <- data.frame(
  LTER = c("Canada", "Canada"),
  Stream_Name = c(
    "KOOTENAY RIVER ABOVE HIGHWAY 93 IN KOOTENAY NATIONAL PARK",
    "SKEENA RIVER AT USK"
  ),
  Discharge_File_Name = c("Kootenay_Q", "Skeena_Q"),
  Shapefile_Name = c("Canada4", "Canada6"),
  stringsAsFactors = FALSE
)
bad_canada_evapo$.row_key <- as_key(
  bad_canada_evapo$LTER,
  bad_canada_evapo$Stream_Name,
  bad_canada_evapo$Discharge_File_Name,
  bad_canada_evapo$Shapefile_Name
)

for (key in bad_canada_evapo$.row_key) {
  target_row <- match(key, target_keys)
  if (is.na(target_row)) {
    warning("Could not match Canada evapo patch key in target file: ", key)
    next
  }

  result <- set_value(
    target,
    target_row,
    "evapotrans_2023_kg_m2",
    NA_character_,
    "clear implausible near-zero 2023 evapotranspiration; needs real rerun value",
    "",
    patch_log
  )
  target <- result$df
  patch_log <- result$patch_log
}

target$.row_key <- NULL
write.csv(target, out_file, row.names = FALSE, na = "")

patch_log_df <- if (length(patch_log)) {
  do.call(rbind, patch_log)
} else {
  data.frame(
    LTER = character(),
    Stream_Name = character(),
    Discharge_File_Name = character(),
    Shapefile_Name = character(),
    column = character(),
    old_value = character(),
    new_value = character(),
    reason = character(),
    source_file = character(),
    stringsAsFactors = FALSE
  )
}
write.csv(patch_log_df, log_file, row.names = FALSE, na = "")

cat("WROTE:", out_file, "\n", sep = "")
cat("WROTE:", log_file, "\n", sep = "")
cat("patched_cells=", nrow(patch_log_df), "\n", sep = "")
if (nrow(patch_log_df)) {
  print(
    aggregate(
      column ~ LTER + Stream_Name + reason,
      data = patch_log_df,
      FUN = length
    ),
    row.names = FALSE
  )
}
