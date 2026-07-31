# Copy one extraction to every finalized site-reference name that uses the same
# watershed geometry.

suppressPackageStartupMessages(library(dplyr))

source(file.path("tools", "cli_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
output_root <- require_input_dir(
  cli_value(args, "--output-root", required = TRUE),
  "extraction output root"
)
alias_file <- require_input_file(
  cli_value(args, "--alias-file", required = TRUE),
  "watershed alias file"
)

aliases <- readRDS(alias_file)
required <- c("canonical_lter", "canonical_shape", "LTER", "Shapefile_Name")
assert_required_columns(aliases, required, "watershed alias table")
aliases <- aliases %>%
  filter(
    nzchar(canonical_lter), nzchar(canonical_shape),
    nzchar(LTER), nzchar(Shapefile_Name)
  ) %>%
  distinct()

data_files <- list.files(
  file.path(output_root, "extracted-data"),
  pattern = "^si-extract_.*[.]csv$",
  full.names = TRUE
)
if (!length(data_files)) stop("No extraction tables were found.", call. = FALSE)

write_csv_atomic <- function(data, path) {
  temporary <- paste0(path, ".tmp")
  write.csv(data, temporary, row.names = FALSE, na = "")
  if (!file.rename(temporary, path)) {
    stop("Could not replace output table: ", path, call. = FALSE)
  }
}

for (path in data_files) {
  data <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  assert_required_columns(data, c("LTER", "Shapefile_Name"), basename(path))
  additions <- bind_rows(lapply(seq_len(nrow(aliases)), function(index) {
    alias <- aliases[index, ]
    source <- data %>%
      filter(
        LTER == alias$canonical_lter[[1]],
        Shapefile_Name == alias$canonical_shape[[1]]
      )
    if (!nrow(source)) return(NULL)
    source$LTER <- alias$LTER[[1]]
    source$Shapefile_Name <- alias$Shapefile_Name[[1]]
    source
  }))
  if (!nrow(additions)) next

  replacement_keys <- paste(additions$LTER, additions$Shapefile_Name, sep = "__")
  data_keys <- paste(data$LTER, data$Shapefile_Name, sep = "__")
  combined <- bind_rows(
    data[!data_keys %in% replacement_keys, , drop = FALSE],
    additions
  ) %>%
    arrange(LTER, Shapefile_Name)
  write_csv_atomic(combined, path)
}

qa_path <- file.path(output_root, "standard_modis_extraction_qa.rds")
if (file.exists(qa_path)) {
  qa <- readRDS(qa_path)
  additions <- bind_rows(lapply(seq_len(nrow(aliases)), function(index) {
    alias <- aliases[index, ]
    source_key <- paste(
      alias$canonical_lter[[1]], alias$canonical_shape[[1]], sep = "__"
    )
    source <- qa$outputs %>% filter(watershed_key == source_key)
    if (!nrow(source)) return(NULL)
    source$watershed_key <- paste(
      alias$LTER[[1]], alias$Shapefile_Name[[1]], sep = "__"
    )
    source
  }))
  if (nrow(additions)) {
    replacement_keys <- paste(additions$watershed_key, additions$driver)
    existing_keys <- paste(qa$outputs$watershed_key, qa$outputs$driver)
    qa$outputs <- bind_rows(
      qa$outputs[!existing_keys %in% replacement_keys, , drop = FALSE],
      additions
    ) %>%
      arrange(watershed_key, driver)
    qa$summary <- data.frame(
      checked_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      watersheds = n_distinct(qa$outputs$watershed_key),
      drivers = n_distinct(qa$outputs$driver),
      outputs_expected = nrow(qa$outputs),
      outputs_complete = sum(qa$outputs$status == "complete"),
      outputs_failed = sum(qa$outputs$status != "complete")
    )
    saveRDS(qa, qa_path)
  }
}

cat("Expanded extractions to ", n_distinct(aliases$LTER, aliases$Shapefile_Name),
    " finalized watershed names.\n", sep = "")
