# Shared identifier normalization and alias-table helpers.

identifier_helpers_source <- tryCatch({
  source_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) "" else as.character(frame$ofile)
    },
    character(1)
  )
  source_files <- source_files[nzchar(source_files)]
  normalizePath(tail(source_files, 1), mustWork = TRUE)
}, error = function(...) "")
identifier_helpers_tools_dir <- if (nzchar(identifier_helpers_source)) {
  dirname(identifier_helpers_source)
} else {
  file.path(getwd(), "tools")
}

normalize_site_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[x %in% c("", "na")] <- NA_character_
  x
}

clean_lter_label <- function(x) {
  x <- trimws(as.character(x))
  x <- trimws(gsub("\\s*\\([^)]*\\)", "", x))
  x[x %in% c("", "na")] <- NA_character_
  x
}

identifier_alias_path <- function() {
  configured <- Sys.getenv("SILICA_IDENTIFIER_ALIASES", unset = "")
  if (nzchar(configured)) {
    return(configured)
  }
  file.path(
    dirname(identifier_helpers_tools_dir),
    "config",
    "identifier_aliases.tsv"
  )
}

load_identifier_aliases <- function(path = identifier_alias_path()) {
  required <- c("alias_type", "source", "canonical", "canonical_label")
  if (!file.exists(path)) {
    stop(
      "Identifier alias table not found: ", path,
      ". Set SILICA_IDENTIFIER_ALIASES or restore config/identifier_aliases.tsv.",
      call. = FALSE
    )
  }
  aliases <- read.delim(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  missing <- setdiff(required, names(aliases))
  if (length(missing)) {
    stop(
      "Identifier alias table is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  aliases
}

apply_identifier_aliases <- function(x, alias_type) {
  aliases <- load_identifier_aliases()
  aliases <- aliases[aliases$alias_type == alias_type, , drop = FALSE]
  if (!nrow(aliases)) {
    return(x)
  }
  source_key <- normalize_site_key(aliases$source)
  canonical_key <- normalize_site_key(aliases$canonical)
  hit <- match(x, source_key)
  x[!is.na(hit)] <- canonical_key[hit[!is.na(hit)]]
  x
}

canonical_lter_label <- function(x) {
  labels <- clean_lter_label(x)
  aliases <- load_identifier_aliases()
  aliases <- aliases[aliases$alias_type == "lter", , drop = FALSE]
  hit <- match(normalize_site_key(labels), normalize_site_key(aliases$source))
  labels[!is.na(hit)] <- aliases$canonical_label[hit[!is.na(hit)]]
  labels
}

load_lter_exclusion_list <- function(path) {
  if (!file.exists(path)) {
    stop("LTER exclusion table not found: ", path, call. = FALSE)
  }
  exclusions <- read.delim(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  if (!"LTER" %in% names(exclusions)) {
    stop("LTER exclusion table must contain an LTER column.", call. = FALSE)
  }
  unique(exclusions$LTER[!is.na(exclusions$LTER)])
}

clean_lter_column <- function(df, drop_duplicates = TRUE) {
  if ("LTER" %in% names(df)) {
    df$LTER <- clean_lter_label(df$LTER)
  }
  if (drop_duplicates) {
    df <- df[!duplicated(df), , drop = FALSE]
  }
  df
}

normalize_lter_key <- function(x) {
  x <- normalize_site_key(clean_lter_label(x))
  apply_identifier_aliases(x, "lter")
}

normalize_region_key <- function(x) {
  x <- normalize_site_key(x)
  gsub("[[:space:]_]+", "-", x)
}

normalize_stream_key <- function(x) {
  x <- normalize_site_key(x)
  apply_identifier_aliases(x, "stream")
}
