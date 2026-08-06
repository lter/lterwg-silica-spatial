# Build one canonical reference from the frozen base, corrections, and additions

source(file.path("tools", "identifier_helpers.R"))
source(file.path("tools", "workflow_paths.R"))

### Arguments

args <- commandArgs(trailingOnly = TRUE)
manifest_path <- cli_value(
  args,
  "--manifest",
  file.path(
    "04_combine_qaqc", "config",
    "final_spatial_roster_sources_20260805.tsv"
  )
)
output_root <- cli_value(
  args,
  "--outdir",
  file.path("generated_outputs", "final-integration", "final-spatial-20260805")
)
reference_path <- file.path(
  output_root,
  "site_reference_table_final_candidate_20260805.csv"
)
summary_path <- file.path(output_root, "site_reference_build_summary.csv")

manifest_path <- require_input_file(manifest_path, "roster manifest")
prepare_output_dir(output_root)
new_sites_root <- resolve_silica_new_sites_root()

manifest <- read.delim(
  manifest_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  quote = ""
)
required_columns <- c("source_id", "action", "path", "expected_rows", "role")
missing_columns <- setdiff(required_columns, names(manifest))
if (length(missing_columns)) {
  stop("Roster manifest is missing: ", paste(missing_columns, collapse = ", "))
}
if (anyDuplicated(manifest$source_id)) stop("Roster source IDs are not unique.")
if (sum(manifest$action == "base") != 1L) {
  stop("Roster manifest must contain exactly one base source.")
}
manifest$resolved_path <- vapply(
  manifest$path,
  expand_workflow_path,
  character(1),
  new_sites_root = new_sites_root
)
missing_paths <- manifest$resolved_path[!file.exists(manifest$resolved_path)]
if (length(missing_paths)) {
  stop("Missing roster source:\n- ", paste(missing_paths, collapse = "\n- "))
}

read_reference <- function(path) {
  read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character()
  )
}

tables <- lapply(manifest$resolved_path, read_reference)
names(tables) <- manifest$source_id
row_counts <- vapply(tables, nrow, integer(1))
if (any(row_counts != manifest$expected_rows)) {
  bad <- which(row_counts != manifest$expected_rows)
  stop(
    "Roster source row counts differ from the manifest:\n- ",
    paste0(
      manifest$source_id[bad], ": ", row_counts[bad], " observed, ",
      manifest$expected_rows[bad], " expected",
      collapse = "\n- "
    )
  )
}

site_key <- function(data) {
  paste(
    normalize_lter_key(data$LTER),
    normalize_stream_key(data$Stream_Name),
    normalize_site_key(data$Discharge_File_Name),
    sep = "__"
  )
}

patch_key <- function(data) {
  paste(
    normalize_lter_key(data$LTER),
    normalize_stream_key(data$Stream_Name),
    sep = "__"
  )
}

### Assemble reference

base_index <- which(manifest$action == "base")
reference <- tables[[base_index]]
reference$.reference_source_id <- manifest$source_id[[base_index]]

patch_indices <- which(manifest$action == "patch")
for (index in patch_indices) {
  patch <- tables[[index]]
  patch_keys <- patch_key(patch)
  reference_keys <- patch_key(reference)
  if (anyDuplicated(patch_keys)) {
    stop("Patch source contains duplicate site keys: ", manifest$source_id[[index]])
  }
  target <- match(patch_keys, reference_keys)
  if (anyNA(target)) {
    stop(
      "Patch rows do not match the base reference: ",
      paste(patch$Stream_Name[is.na(target)], collapse = ", ")
    )
  }
  shared_columns <- intersect(names(patch), names(reference))
  reference[target, shared_columns] <- patch[, shared_columns, drop = FALSE]
  reference$.reference_source_id[target] <- manifest$source_id[[index]]
}

append_indices <- which(manifest$action == "append")
accepted_duplicate_keys <- unique(site_key(reference)[
  duplicated(site_key(reference)) | duplicated(site_key(reference), fromLast = TRUE)
])
for (index in append_indices) {
  additions <- tables[[index]]
  overlap <- site_key(additions) %in% site_key(reference)
  if (any(overlap)) {
    stop(
      "Addition rows already exist in the reference: ",
      paste(additions$Stream_Name[overlap], collapse = ", ")
    )
  }
  additions$.reference_source_id <- manifest$source_id[[index]]
  all_columns <- union(names(reference), names(additions))
  for (column in setdiff(all_columns, names(reference))) reference[[column]] <- ""
  for (column in setdiff(all_columns, names(additions))) additions[[column]] <- ""
  reference <- rbind(
    reference[, all_columns, drop = FALSE],
    additions[, all_columns, drop = FALSE]
  )
}

final_keys <- site_key(reference)
final_duplicate_keys <- unique(final_keys[
  duplicated(final_keys) | duplicated(final_keys, fromLast = TRUE)
])
unexpected_duplicate_keys <- setdiff(
  final_duplicate_keys,
  accepted_duplicate_keys
)
if (length(unexpected_duplicate_keys)) {
  stop("Final reference contains new duplicate site keys.")
}

spatial <- trimws(reference$Has_Spatial_Data) == "Yes" &
  nzchar(trimws(reference$Shapefile_Name))
shape_keys <- normalize_site_key(reference$Shapefile_Name[spatial])
if (any(is.na(shape_keys) | !nzchar(shape_keys))) {
  stop("Final spatial rows contain invalid watershed names.")
}

guadeloupe_shapes <- c(
  "vieuxhabitantsbarthole_rgealti5m",
  "vieuxhabitantssavanne_rgealti5m"
)
guadeloupe_present <- sum(shape_keys %in% guadeloupe_shapes)
addition_rows <- sum(
  reference$.reference_source_id %in% manifest$source_id[append_indices]
)
addition_geometries <- length(unique(shape_keys[
  reference$.reference_source_id[spatial] %in% manifest$source_id[append_indices]
]))

summary <- data.frame(
  check = c(
    "roster_sources", "base_rows", "patch_rows", "addition_rows",
    "final_all_rows", "final_spatial_site_rows",
    "distinct_watershed_geometries", "guadeloupe_corrected_spatial_rows",
    "new_intake_spatial_rows", "new_intake_watershed_geometries",
    "unexpected_duplicate_site_keys"
  ),
  value = c(
    nrow(manifest),
    nrow(tables[[base_index]]),
    sum(vapply(tables[patch_indices], nrow, integer(1))),
    sum(vapply(tables[append_indices], nrow, integer(1))),
    nrow(reference),
    sum(spatial),
    length(unique(shape_keys)),
    guadeloupe_present,
    addition_rows,
    addition_geometries,
    length(unexpected_duplicate_keys)
  ),
  expected = c(3L, 1215L, 6L, 154L, 1369L, 1196L, 1146L, 2L, 154L, 143L, 0L),
  stringsAsFactors = FALSE
)
summary$status <- ifelse(summary$value == summary$expected, "PASS", "FAIL")

if (any(summary$status == "FAIL")) {
  print(summary, row.names = FALSE)
  stop("Final reference build failed its locked row-count checks.")
}

reference$Roster_Source_ID <- reference$.reference_source_id
write.csv(
  reference[, setdiff(names(reference), ".reference_source_id"), drop = FALSE],
  reference_path,
  row.names = FALSE,
  na = ""
)
write.csv(summary, summary_path, row.names = FALSE)
unlink(file.path(output_root, "site_reference_row_provenance.csv"))

print(summary, row.names = FALSE)
cat("Final reference: ", reference_path, "\n", sep = "")
