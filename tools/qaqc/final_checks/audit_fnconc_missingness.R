librarian::shelf(dplyr)

box_root <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn"
data_root <- file.path(box_root, "spatial-data-extractions")
date_tag <- Sys.getenv("SILICA_FINAL_HARMONIZED_DATE", unset = "20260608")

final_file <- file.path(data_root, "final-data", "full-dataset", paste0("final_full_harmonized_annual_", date_tag, ".csv"))
wrtds_file <- file.path(data_root, "master", "Full_Results_WRTDS_kalman_annual.csv")
site_ref_file <- file.path(data_root, "master", "Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv")
audit_dir <- file.path(data_root, "final-data", "audit-summaries")
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

read_csv_clean <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  blank_names <- is.na(names(x)) | names(x) == ""
  names(x)[blank_names] <- paste0("source_col_", seq_len(sum(blank_names)))
  names(x) <- make.unique(names(x))
  x
}

norm_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  gsub("_+", "_", x)
}

read_lter_aliases <- function() {
  path <- "tools/lter_name_aliases.csv"
  if (!file.exists(path)) {
    return(data.frame(source = character(), target = character()))
  }
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  x <- x[x$field == "LTER", , drop = FALSE]
  data.frame(
    source = norm_key(x$source_value),
    target = norm_key(x$target_value),
    stringsAsFactors = FALSE
  )
}

read_stream_aliases <- function() {
  path <- "tools/stream_name_aliases.csv"
  if (!file.exists(path)) {
    return(data.frame(lter = character(), source = character(), target = character()))
  }
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  data.frame(
    lter = norm_key(x$LTER),
    source = norm_key(x$alias_stream_name),
    target = norm_key(x$canonical_stream_name),
    stringsAsFactors = FALSE
  )
}

lter_aliases <- read_lter_aliases()
stream_aliases <- read_stream_aliases()

canonical_lter_key <- function(lter) {
  out <- norm_key(lter)
  hit <- match(out, lter_aliases$source)
  out[!is.na(hit)] <- lter_aliases$target[hit[!is.na(hit)]]
  out
}

canonical_stream_key <- function(lter_key, stream_name) {
  out <- norm_key(stream_name)
  if (!nrow(stream_aliases)) {
    return(out)
  }

  for (i in seq_len(nrow(stream_aliases))) {
    hit <- lter_key == stream_aliases$lter[[i]] &
      (out == stream_aliases$source[[i]] | out == stream_aliases$target[[i]])
    out[hit] <- stream_aliases$target[[i]]
  }
  out
}

split_stream_id <- function(stream_id) {
  stream_id <- as.character(stream_id)
  has_sep <- grepl("__", stream_id, fixed = TRUE)
  data.frame(
    LTER = ifelse(has_sep, sub("__.*$", "", stream_id), NA_character_),
    Stream_Name = ifelse(has_sep, sub("^[^_]*__", "", stream_id), stream_id),
    stringsAsFactors = FALSE
  )
}

site_key_from_parts <- function(lter, stream_name) {
  lter_key <- canonical_lter_key(lter)
  stream_key <- canonical_stream_key(lter_key, stream_name)
  paste(lter_key, stream_key, sep = "||")
}

site_key_from_stream_id <- function(stream_id) {
  parts <- split_stream_id(stream_id)
  site_key_from_parts(parts$LTER, parts$Stream_Name)
}

first_present <- function(x) {
  v <- x[!is.na(x) & !(is.character(x) & trimws(x) == "")]
  if (length(v)) v[[1]] else NA
}

final <- read_csv_clean(final_file)
wrtds <- read_csv_clean(wrtds_file)
site_ref <- read_csv_clean(site_ref_file)

final$.site_key <- site_key_from_stream_id(final$Stream_ID)
wrtds_dsi <- wrtds %>%
  filter(chemical == "DSi") %>%
  mutate(.site_key = site_key_from_parts(LTER, Stream_Name)) %>%
  filter(!is.na(.site_key), .site_key != "||")

site_ref_one <- site_ref %>%
  mutate(.site_key = site_key_from_parts(LTER, Stream_Name)) %>%
  filter(!is.na(.site_key), .site_key != "||") %>%
  group_by(.site_key) %>%
  summarise(
    Use_WRTDS = first_present(Use_WRTDS),
    WRTDS_notes = first_present(Notes),
    .groups = "drop"
  )

wrtds_site <- wrtds_dsi %>%
  group_by(.site_key) %>%
  summarise(
    wrtds_dsi_years = n_distinct(Year),
    wrtds_first_year = min(Year, na.rm = TRUE),
    wrtds_last_year = max(Year, na.rm = TRUE),
    .groups = "drop"
  )

site_audit <- final %>%
  group_by(.site_key, Stream_ID) %>%
  summarise(
    output_first_year = min(Year, na.rm = TRUE),
    output_last_year = max(Year, na.rm = TRUE),
    output_years = n_distinct(Year),
    FNConc_years = sum(!is.na(FNConc)),
    FNConc_missing_years = sum(is.na(FNConc)),
    missing_year_list = paste(Year[is.na(FNConc)], collapse = ";"),
    .groups = "drop"
  ) %>%
  left_join(wrtds_site, by = ".site_key") %>%
  left_join(site_ref_one, by = ".site_key") %>%
  mutate(
    Use_WRTDS = ifelse(is.na(Use_WRTDS) | trimws(Use_WRTDS) == "", "not_in_site_ref", Use_WRTDS),
    missing_reason = case_when(
      FNConc_missing_years == 0 ~ "no_missing_FNConc",
      is.na(wrtds_dsi_years) ~ "no_DSi_WRTDS_for_site",
      wrtds_last_year < output_first_year ~ "DSi_WRTDS_record_before_output_window",
      FNConc_years > 0 & wrtds_last_year < output_last_year ~ "partial_WRTDS_record_or_WRTDS_ends_before_2025",
      FNConc_years > 0 ~ "partial_WRTDS_record",
      TRUE ~ "needs_review"
    )
  ) %>%
  arrange(desc(FNConc_missing_years), Stream_ID)

year_audit <- final %>%
  group_by(Year) %>%
  summarise(
    output_rows = dplyr::n(),
    FNConc_rows = sum(!is.na(FNConc)),
    FNConc_missing_rows = sum(is.na(FNConc)),
    .groups = "drop"
  )

unmatched_wrtds <- wrtds_dsi %>%
  filter(Year >= min(final$Year), Year <= max(final$Year), !.site_key %in% unique(final$.site_key)) %>%
  distinct(LTER, Stream_Name, .site_key) %>%
  arrange(LTER, Stream_Name)

site_file <- file.path(audit_dir, paste0("final_full_harmonized_annual_FNConc_missingness_by_site_", date_tag, ".csv"))
year_file <- file.path(audit_dir, paste0("final_full_harmonized_annual_FNConc_missingness_by_year_", date_tag, ".csv"))
unmatched_file <- file.path(audit_dir, paste0("final_full_harmonized_annual_unmatched_WRTDS_DSi_sites_", date_tag, ".csv"))

write.csv(site_audit, site_file, row.names = FALSE, na = "")
write.csv(year_audit, year_file, row.names = FALSE, na = "")
write.csv(unmatched_wrtds, unmatched_file, row.names = FALSE, na = "")

cat("WROTE:", site_file, "\n", sep = "")
cat("WROTE:", year_file, "\n", sep = "")
cat("WROTE:", unmatched_file, "\n", sep = "")
cat("site_audit_rows=", nrow(site_audit), "\n", sep = "")
cat("year_audit_rows=", nrow(year_audit), "\n", sep = "")
