# This script rebuilds one combined local spatial table from the current
# driver CSVs plus the site reference table. It also writes a few simple
# review files under the configured review root so we can see what is still missing.
suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(stringr)
  library(sf)
})

source(file.path(getwd(), "tools", "workflow_paths.R"))
source(file.path("04_combine_qaqc", "00_qaqc_functions.R"))

keys <- c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")
join_keys <- c(".LTER_KEY", ".STREAM_KEY", ".DISCHARGE_KEY", ".SHP_KEY")

driver_family_patterns <- c(
  evapo = "^evapotrans_",
  greenup = "^greenup_",
  precip = "^precip_",
  airtemp = "^temp_",
  snow = "^snow_",
  npp = "^npp_",
  elev = "^elevation_|^basin_slope_",
  lith = "^major_rock$|^rocks_",
  permafrost = "^permafrost_",
  soil = "^major_soil$|^soil_"
)

# Keep the key fields consistent before joining any driver tables.
norm_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

norm_lter <- function(x) {
  x <- clean_lter_label(norm_chr(x))
  dplyr::recode(
    x,
    "Swedish Goverment" = "Sweden",
    "Swedish Government" = "Sweden",
    "Carey" = "PIE",
    "Cameroon" = "Congo Basin",
    "Cameroon Site" = "Congo Basin",
    "Cameroon Sites" = "Congo Basin",
    .default = x
  )
}

join_chr <- function(x) {
  x <- norm_chr(x)
  x <- tolower(x)
  x
}

join_lter <- function(x) {
  x <- join_chr(clean_lter_label(x))
  dplyr::recode(
    x,
    "swedish goverment" = "sweden",
    "swedish government" = "sweden",
    "carey" = "pie",
    "cameroon" = "congo basin",
    "cameroon site" = "congo basin",
    "cameroon sites" = "congo basin",
    .default = x
  )
}

add_join_keys <- function(df) {
  df %>% mutate(
    .LTER_KEY = join_lter(LTER),
    .STREAM_KEY = join_chr(Stream_Name),
    .DISCHARGE_KEY = join_chr(Discharge_File_Name),
    .SHP_KEY = join_chr(Shapefile_Name)
  )
}

build_site_identity <- function(df) {
  paste(
    join_lter(df$LTER),
    join_chr(df$Stream_Name),
    join_chr(df$Discharge_File_Name),
    sep = "||"
  )
}

first_non_missing <- function(x) {
  keep <- !(is.na(x) | trimws(as.character(x)) == "")
  if (!any(keep)) {
    if (is.numeric(x)) return(NA_real_)
    return(NA)
  }
  x[which(keep)[1]]
}

collapse_duplicate_sites <- function(df) {
  annual_cols <- grep("^(evapotrans|greenup_cycle[01]|precip_|temp_|snow_|npp_)", names(df), value = TRUE)
  non_key_cols <- setdiff(names(df), c(keys, join_keys))

  df <- df %>%
    mutate(
      .site_id = build_site_identity(pick(LTER, Stream_Name, Discharge_File_Name)),
      .has_named_shapefile = !is.na(Shapefile_Name) & Shapefile_Name != "",
      .annual_non_na = if (length(annual_cols)) rowSums(!is.na(across(all_of(annual_cols)))) else 0L,
      .non_key_non_na = if (length(non_key_cols)) rowSums(!is.na(across(all_of(non_key_cols))) & across(all_of(non_key_cols)) != "") else 0L
    ) %>%
    arrange(.site_id, desc(.annual_non_na), desc(.has_named_shapefile), desc(.non_key_non_na))

  dup_summary <- df %>%
    count(.site_id, name = "n") %>%
    filter(n > 1)

  collapsed <- df %>%
    group_by(.site_id) %>%
    summarise(
      across(
        .cols = -c(.annual_non_na, .non_key_non_na, .has_named_shapefile),
        .fns = first_non_missing
      ),
      .groups = "drop"
    ) %>%
    select(-.site_id)

  attr(collapsed, "duplicate_site_groups") <- dup_summary
  collapsed
}

# Used to tell whether a site actually has data for a driver family, rather
# than just having a matching row.
row_has_any_values <- function(df, cols) {
  if (length(cols) == 0) {
    return(rep(FALSE, nrow(df)))
  }
  vals <- df[, cols, drop = FALSE]
  rowSums(!is.na(vals) & vals != "") > 0
}

fill_major_rock_from_percentages <- function(df) {
  rock_cols <- grep("^rocks_", names(df), value = TRUE)
  if (!length(rock_cols)) {
    return(df)
  }

  if (!"major_rock" %in% names(df)) {
    df$major_rock <- NA_character_
  }

  rock_vals <- as.data.frame(lapply(df[, rock_cols, drop = FALSE], function(x) {
    suppressWarnings(as.numeric(x))
  }))
  names(rock_vals) <- rock_cols

  needs_major <- is.na(df$major_rock) | trimws(as.character(df$major_rock)) == ""
  has_rock_values <- rowSums(!is.na(rock_vals)) > 0
  fill_rows <- which(needs_major & has_rock_values)

  if (length(fill_rows)) {
    df$major_rock[fill_rows] <- vapply(fill_rows, function(i) {
      vals <- unlist(rock_vals[i, , drop = TRUE], use.names = TRUE)
      vals <- vals[!is.na(vals)]
      if (!length(vals)) {
        return(NA_character_)
      }
      winners <- names(vals)[vals == max(vals, na.rm = TRUE)]
      paste(gsub("^rocks_", "", winners), collapse = "; ")
    }, character(1))
  }

  df
}

first_existing <- function(paths) {
  paths <- paths[nzchar(paths)]
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) return("")
  hit[[1]]
}

clean_tag <- function(x) {
  x <- trimws(x)
  if (!nzchar(x)) {
    return("")
  }
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("(^-+|-+$)", "", x)
}

find_previous_combined_file <- function(extracted_dir, current_output = "") {
  pats <- c(
    "^all-data_si-extract_.*combinedLocal\\.csv$",
    "^all-data_si-extract_.*recombinedLocal_obidosDedup\\.csv$",
    "^all-data_si-extract_.*fromSiteRef_obidosDedup\\.csv$",
    "^all-data_si-extract_.*fromSiteRef_full_canonical\\.csv$",
    "^all-data_si-extract_.*canonical\\.csv$",
    "^all-data_si-extract_.*\\.csv$"
  )
  cand <- unique(unlist(lapply(pats, function(pat) list.files(extracted_dir, pattern = pat, full.names = TRUE))))
  cand <- cand[file.exists(cand)]
  if (nzchar(current_output)) {
    cand <- setdiff(normalizePath(cand, mustWork = FALSE), normalizePath(current_output, mustWork = FALSE))
  }
  if (!length(cand)) return("")
  cand[which.max(file.info(cand)$mtime)]
}

detect_data_root <- function() {
  resolve_silica_data_root()
}

max_year <- function(nms, rx) {
  y <- as.integer(stringr::str_match(nms, rx)[, 2])
  y <- y[!is.na(y)]
  if (length(y) == 0) return(NA_integer_)
  max(y)
}

driver_family_from_file <- function(path) {
  x <- basename(path)
  dplyr::case_when(
    grepl("evapo", x, ignore.case = TRUE) ~ "evapo",
    grepl("greenup", x, ignore.case = TRUE) ~ "greenup",
    grepl("precip", x, ignore.case = TRUE) ~ "precip",
    grepl("air-temp", x, ignore.case = TRUE) ~ "airtemp",
    grepl("snow", x, ignore.case = TRUE) ~ "snow",
    grepl("npp", x, ignore.case = TRUE) ~ "npp",
    grepl("elevation", x, ignore.case = TRUE) ~ "elev",
    grepl("lithology", x, ignore.case = TRUE) ~ "lith",
    grepl("permafrost", x, ignore.case = TRUE) ~ "permafrost",
    grepl("soil", x, ignore.case = TRUE) ~ "soil",
    TRUE ~ NA_character_
  )
}

extract_date_token <- function(x) {
  vapply(basename(x), function(one) {
    hits <- stringr::str_extract_all(one, "[0-9]{8}")[[1]]
    if (!length(hits)) return(NA_character_)
    max(hits, na.rm = TRUE)
  }, character(1))
}

order_driver_candidates <- function(files) {
  if (!length(files)) return(files)
  stamps <- extract_date_token(files)
  stamp_rank <- suppressWarnings(as.integer(stamps))
  stamp_rank[is.na(stamp_rank)] <- -Inf
  files[order(is.na(stamps), -stamp_rank, files)]
}

resolve_driver_candidates <- function(family, extracted_dir) {
  src <- default_driver_sources[[family]]
  if (is.null(src)) return(character(0))

  files <- list.files(extracted_dir, full.names = FALSE)
  dated <- if (!is.null(src$dated) && nzchar(src$dated)) grep(src$dated, files, value = TRUE) else character(0)
  if (length(dated) > 0) {
    dated <- order_driver_candidates(dated)
  }
  legacy <- src$legacy[src$legacy %in% files]
  unique(c(dated, legacy))
}

merge_family_rows <- function(primary, fallback) {
  if (is.null(primary) || nrow(primary) == 0) return(fallback)
  if (is.null(fallback) || nrow(fallback) == 0) return(primary)

  all_cols <- union(names(primary), names(fallback))
  for (nm in setdiff(all_cols, names(primary))) primary[[nm]] <- NA
  for (nm in setdiff(all_cols, names(fallback))) fallback[[nm]] <- NA
  primary <- primary[, all_cols, drop = FALSE]
  fallback <- fallback[, all_cols, drop = FALSE]

  merged <- full_join(primary, fallback, by = join_keys, suffix = c(".new", ".old"))
  non_key <- setdiff(all_cols, join_keys)

  out <- merged[, join_keys, drop = FALSE]
  for (nm in non_key) {
    new_nm <- paste0(nm, ".new")
    old_nm <- paste0(nm, ".old")
    if (new_nm %in% names(merged) && old_nm %in% names(merged)) {
      out[[nm]] <- dplyr::coalesce(merged[[new_nm]], merged[[old_nm]])
    } else if (new_nm %in% names(merged)) {
      out[[nm]] <- merged[[new_nm]]
    } else if (old_nm %in% names(merged)) {
      out[[nm]] <- merged[[old_nm]]
    } else if (nm %in% names(merged)) {
      out[[nm]] <- merged[[nm]]
    } else {
      out[[nm]] <- NA
    }
  }

  out
}

# Read one driver family, fill in missing discharge names from the reference
# table when possible, then keep one normalized row per site key.
read_driver_with_fallbacks <- function(family, extracted_dir, base_keys, explicit_files = NULL) {
  candidates <- if (!is.null(explicit_files)) explicit_files else resolve_driver_candidates(family, extracted_dir)
  candidates <- candidates[nzchar(candidates)]
  candidates <- order_driver_candidates(candidates)
  if (!length(candidates)) return(NULL)

  family_df <- NULL
  used_files <- character(0)

  base_lookup <- base_keys %>%
    mutate(
      .LTER_KEY = join_lter(LTER),
      .STREAM_KEY = join_chr(Stream_Name),
      .SHP_KEY = join_chr(Shapefile_Name)
    ) %>%
    select(.LTER_KEY, .STREAM_KEY, .SHP_KEY, ref_Discharge_File_Name = Discharge_File_Name) %>%
    distinct()

  for (f in candidates) {
    p <- file.path(extracted_dir, f)
    if (!file.exists(p)) next

    d <- read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)

    had_discharge_file_name <- "Discharge_File_Name" %in% names(d)
    if (!had_discharge_file_name) {
      d$Discharge_File_Name <- NA_character_
    }
    for (k in keys) if (!k %in% names(d)) d[[k]] <- NA_character_

    d <- d %>%
      mutate(
        LTER = norm_lter(LTER),
        Stream_Name = norm_chr(Stream_Name),
        Discharge_File_Name = norm_chr(Discharge_File_Name),
        Shapefile_Name = norm_chr(Shapefile_Name)
      )

    d <- d %>%
      mutate(
        .LTER_KEY = join_lter(LTER),
        .STREAM_KEY = join_chr(Stream_Name),
        .SHP_KEY = join_chr(Shapefile_Name)
      ) %>%
      left_join(base_lookup, by = c(".LTER_KEY", ".STREAM_KEY", ".SHP_KEY")) %>%
      mutate(
        Discharge_File_Name = if (had_discharge_file_name) {
          dplyr::coalesce(Discharge_File_Name, ref_Discharge_File_Name)
        } else {
          dplyr::coalesce(ref_Discharge_File_Name, Discharge_File_Name)
        }
      ) %>%
      select(-ref_Discharge_File_Name)

    d <- add_join_keys(d)

    dup_n <- d %>%
      count(across(all_of(join_keys)), name = "n") %>%
      filter(n > 1)
    if (nrow(dup_n) > 0) {
      warning("Driver has duplicate normalized key rows; keeping first row per key: ", f, call. = FALSE)
    }
    d <- d %>% distinct(across(all_of(join_keys)), .keep_all = TRUE)

    keep <- setdiff(names(d), c(keys, join_keys))
    d <- d[, c(join_keys, keep), drop = FALSE]

    family_df <- merge_family_rows(family_df, d)
    used_files <- c(used_files, f)
  }

  if (is.null(family_df)) return(NULL)
  attr(family_df, "used_files") <- used_files
  family_df
}

region_from_lter <- function(x) {
  x <- join_lter(x)
  dplyr::case_when(
    x %in% c("amazon", "hybam", "gro") ~ "amazon",
    x == "westernaustralia" ~ "australia",
    x == "seine" ~ "europe-france",
    x %in% c("congo basin", "congo_basin") ~ "africa-central",
    TRUE ~ NA_character_
  )
}

data_root <- Sys.getenv("SILICA_DATA_ROOT", "")
site_coord_dir <- Sys.getenv("SILICA_SITE_COORD_DIR", "")
extracted_dir <- Sys.getenv("SILICA_EXTRACTED_DIR", "")

if (!nzchar(data_root) && (!nzchar(site_coord_dir) || !nzchar(extracted_dir))) {
  data_root <- detect_data_root()
}

if (nzchar(data_root) && dir.exists(data_root)) {
  data_root <- normalizePath(data_root, mustWork = TRUE)
} else if (nzchar(data_root) && !dir.exists(data_root)) {
  stop("SILICA_DATA_ROOT does not exist: ", data_root, call. = FALSE)
}

if (!nzchar(site_coord_dir)) {
  if (!nzchar(data_root)) {
    stop("Set SILICA_SITE_COORD_DIR (or SILICA_DATA_ROOT).", call. = FALSE)
  }
  site_coord_candidates <- c(
    file.path(data_root, "site-coordinates"),
    file.path(data_root, "silica-shapefiles", "site-coordinates")
  )
  site_coord_dir <- site_coord_candidates[dir.exists(site_coord_candidates)][1]
}
if (!nzchar(extracted_dir)) {
  if (!nzchar(data_root)) {
    stop("Set SILICA_EXTRACTED_DIR (or SILICA_DATA_ROOT).", call. = FALSE)
  }
  extracted_candidates <- c(
    file.path(data_root, "extracted-data"),
    file.path(data_root, "si-extracted-data"),
    file.path(data_root, "silica-shapefiles", "extracted-data"),
    file.path(data_root, "silica-shapefiles", "si-extracted-data")
  )
  extracted_dir <- extracted_candidates[dir.exists(extracted_candidates)][1]
}

sc <- normalizePath(site_coord_dir, mustWork = FALSE)
ex <- normalizePath(extracted_dir, mustWork = FALSE)

if (!dir.exists(sc)) {
  stop("Missing site-coordinates directory: ", sc, call. = FALSE)
}
if (!dir.exists(ex)) {
  stop("Missing extracted-data directory: ", ex, call. = FALSE)
}

base_file <- Sys.getenv("SILICA_BASE_FILE", "")
if (nzchar(base_file) && !file.exists(base_file)) {
  stop("Missing base key file: ", base_file, call. = FALSE)
}

site_file <- file.path(sc, "silica-coords_RAW.xlsx")
if (!nzchar(base_file) && !file.exists(site_file)) {
  stop("Missing site table: ", site_file, call. = FALSE)
}

default_driver_sources <- list(
  evapo = list(dated = "^si-extract_evapo(?:_v[0-9]+)?_[0-9]{8}(?:_.*)?\\.csv$", legacy = c("si-extract_evapo_2-v061.csv", "si-extract_evapo.csv")),
  greenup = list(dated = "^si-extract_greenup(?:_v[0-9]+)?_[0-9]{8}(?:_.*)?\\.csv$", legacy = c("si-extract_greenup_2_v061.csv", "si-extract_greenup.csv")),
  precip = list(dated = "^si-extract_precip_[0-9]{8}(?:_.*)?\\.csv$", legacy = c("si-extract_precip_2.csv", "si-extract_precip.csv")),
  airtemp = list(dated = "^si-extract_air-temp_[0-9]{8}(?:_.*)?\\.csv$", legacy = c("si-extract_air-temp_2.csv", "si-extract_air-temp.csv")),
  snow = list(dated = "^si-extract_snow(?:_v[0-9]+)?_[0-9]{8}(?:_.*)?\\.csv$", legacy = c("si-extract_snow_2_v061.csv", "si-extract_snow.csv")),
  npp = list(dated = "^si-extract_npp(?:_v[0-9]+)?_[0-9]{8}(?:_.*)?\\.csv$", legacy = c("si-extract_npp_2_v061.csv", "si-extract_npp.csv")),
  elev = list(dated = "^si-extract_elevation_[0-9]{8}(?:_.*)?\\.csv$", legacy = c("si-extract_elevation_2.csv", "si-extract_elevation.csv")),
  lith = list(dated = "^si-extract_lithology_[0-9]{8}(?:_.*)?\\.csv$", legacy = c("si-extract_lithology_2.csv", "si-extract_lithology.csv")),
  permafrost = list(dated = "^si-extract_permafrost_[0-9]{8}(?:_.*)?\\.csv$", legacy = c("si-extract_permafrost_2.csv", "si-extract_permafrost.csv")),
  soil = list(dated = "^si-extract_soil_[0-9]{8}(?:_.*)?\\.csv$", legacy = c("si-extract_soil_2.csv", "si-extract_soil.csv"))
)

drivers_env <- Sys.getenv("SILICA_DRIVER_FILES", "")
driver_files <- if (nzchar(drivers_env)) {
  trimws(unlist(strsplit(drivers_env, ",", fixed = TRUE)))
} else {
  character(0)
}
driver_files <- driver_files[nzchar(driver_files)]
canonicalize_obidos <- tolower(Sys.getenv("SILICA_CANONICALIZE_OBIDOS", "true")) == "true"

is_noncanonical_obidos <- function(df) {
  lter <- norm_lter(df$LTER)
  stream <- norm_chr(df$Stream_Name)
  discharge <- norm_chr(df$Discharge_File_Name)
  shp <- norm_chr(df$Shapefile_Name)

  (lter == "Amazon" & shp == "Amazon_Obidos" & discharge == "Obidos_Q") |
    (lter == "HYBAM" & stream == "Obidos")
}

ref_raw <- if (nzchar(base_file)) {
  read.csv(base_file, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  readxl::read_excel(site_file)
}

base <- ref_raw %>%
  transmute(
    LTER = norm_lter(LTER),
    Stream_Name = norm_chr(Stream_Name),
    Discharge_File_Name = norm_chr(Discharge_File_Name),
    Shapefile_Name = norm_chr(Shapefile_Name)
  ) %>%
  distinct()

glimpse_dir <- file.path(silica_review_root(detect_data_root()), "glimpse")
glimpse_checkpoint(ref_raw, "raw site reference workbook", glimpse_dir)
glimpse_checkpoint(base, "normalized site reference keys", glimpse_dir)

dropped_obidos <- base[0, , drop = FALSE]
if (canonicalize_obidos) {
  obidos_drop_idx <- is_noncanonical_obidos(base)
  dropped_obidos <- base[obidos_drop_idx, , drop = FALSE]
  base <- base[!obidos_drop_idx, , drop = FALSE]
}

if (nzchar(data_root)) cat("data_root=", data_root, "\n", sep = "")
cat("site_coord_dir=", sc, "\n", sep = "")
cat("extracted_dir=", ex, "\n", sep = "")
cat("ref_raw_rows=", nrow(ref_raw), "\n", sep = "")
cat("ref_distinct_key_rows=", nrow(base), "\n", sep = "")

# Start from the reference table, then add one driver family at a time.
out <- add_join_keys(base)
driver_summary <- list()
source_status_tables <- list()
missing_driver_files <- character(0)

families_to_add <- if (nzchar(drivers_env)) {
  unique(na.omit(vapply(driver_files, driver_family_from_file, character(1))))
} else {
  names(default_driver_sources)
}

for (family in families_to_add) {
  explicit_files <- if (nzchar(drivers_env)) {
    driver_files[vapply(driver_files, driver_family_from_file, character(1)) == family]
  } else {
    resolve_driver_candidates(family, ex)
  }
  d <- read_driver_with_fallbacks(family, ex, base_keys = base, explicit_files = explicit_files)

  if (is.null(d)) {
    missing_driver_files <- c(missing_driver_files, explicit_files)
    message("Skipping missing family: ", family)
    next
  }

  glimpse_checkpoint(d, paste("driver family", family), glimpse_dir)

  keep <- setdiff(names(d), join_keys)

  status_df <- d[, join_keys, drop = FALSE]
  status_df[[paste0("source_row_", family)]] <- TRUE
  status_df[[paste0("source_data_", family)]] <- row_has_any_values(d, keep)
  source_status_tables[[family]] <- status_df

  out <- out %>% left_join(d %>% select(all_of(c(join_keys, keep))), by = join_keys)

  ref_keys <- add_join_keys(base) %>% distinct(.LTER_KEY, .SHP_KEY)
  drv_keys <- d %>% distinct(.LTER_KEY, .SHP_KEY)
  used_files <- paste(attr(d, "used_files"), collapse = ";")
  driver_summary[[family]] <- data.frame(
    driver = family,
    files_used = used_files,
    ref_keys = nrow(ref_keys),
    driver_keys = nrow(drv_keys),
    matched = nrow(inner_join(ref_keys, drv_keys, by = c(".LTER_KEY", ".SHP_KEY"))),
    unmatched = nrow(anti_join(ref_keys, drv_keys, by = c(".LTER_KEY", ".SHP_KEY"))),
    stringsAsFactors = FALSE
  )

  message("Adding ", family, " from ", used_files)
}

if (length(missing_driver_files) > 0) {
  message("Missing driver files: ", paste(unique(missing_driver_files), collapse = ", "))
}

if (length(source_status_tables) > 0) {
  for (family in names(source_status_tables)) {
    out <- out %>% left_join(source_status_tables[[family]], by = join_keys)
  }
}

missing_audit <- out %>%
  dplyr::select(all_of(keys)) %>%
  dplyr::mutate(region = region_from_lter(LTER))

for (family in names(driver_family_patterns)) {
  fam_cols <- grep(driver_family_patterns[[family]], names(out), value = TRUE)
  final_col <- paste0("has_", family, "_in_final")
  row_col <- paste0("source_row_", family)
  data_col <- paste0("source_data_", family)
  missing_col <- paste0("missing_", family)

  missing_audit[[final_col]] <- row_has_any_values(out, fam_cols)
  missing_audit[[row_col]] <- if (row_col %in% names(out)) {
    !is.na(out[[row_col]]) & out[[row_col]]
  } else {
    FALSE
  }
  missing_audit[[data_col]] <- if (data_col %in% names(out)) {
    !is.na(out[[data_col]]) & out[[data_col]]
  } else {
    FALSE
  }
  missing_audit[[missing_col]] <- !missing_audit[[final_col]]
}

missing_cols <- paste0("missing_", names(driver_family_patterns))
missing_audit$n_missing_driver_families <- rowSums(missing_audit[, missing_cols, drop = FALSE])
missing_audit$missing_driver_families <- apply(
  missing_audit[, missing_cols, drop = FALSE],
  1,
  function(x) paste(names(driver_family_patterns)[as.logical(x)], collapse = ";")
)
missing_audit$all_missing <- missing_audit$n_missing_driver_families == length(driver_family_patterns)
missing_audit$dynamic_missing_only <- rowSums(
  missing_audit[, paste0("missing_", c("evapo", "greenup", "snow", "npp")), drop = FALSE]
) == 4 &
  rowSums(
    !missing_audit[, paste0("missing_", c("precip", "airtemp", "elev", "lith", "soil")), drop = FALSE]
  ) >= 1
missing_audit$likely_issue <- dplyr::case_when(
  missing_audit$all_missing & (is.na(missing_audit$Shapefile_Name) | missing_audit$Shapefile_Name == "") ~
    "missing_polygon_or_unrecoverable_site_keys",
  missing_audit$dynamic_missing_only &
    rowSums(missing_audit[, paste0("source_row_", c("evapo", "greenup", "snow", "npp")), drop = FALSE]) == 4 &
    rowSums(!missing_audit[, paste0("source_data_", c("evapo", "greenup", "snow", "npp")), drop = FALSE]) == 4 ~
    "driver_rows_present_but_values_blank_upstream",
  missing_audit$dynamic_missing_only ~
    "dynamic_driver_gap",
  missing_audit$n_missing_driver_families > 0 &
    ((is.na(missing_audit$Shapefile_Name) | missing_audit$Shapefile_Name == "") |
       (is.na(missing_audit$Discharge_File_Name) | missing_audit$Discharge_File_Name == "")) ~
    "reference_keys_incomplete_or_driver_row_absent",
  missing_audit$n_missing_driver_families > 0 ~
    "targeted_driver_gap",
  TRUE ~ "complete"
)
missing_audit$preferred_action <- dplyr::case_when(
  missing_audit$likely_issue == "missing_polygon_or_unrecoverable_site_keys" ~
    "recover_polygon_or_hydrosheds_site_and_rerun_all_missing_drivers",
  missing_audit$likely_issue == "driver_rows_present_but_values_blank_upstream" ~
    "rerun_missing_dynamic_drivers_from_source_rasters",
  missing_audit$likely_issue == "dynamic_driver_gap" ~
    "rerun_missing_dynamic_drivers",
  missing_audit$likely_issue == "reference_keys_incomplete_or_driver_row_absent" ~
    "complete_reference_keys_then_rerun_missing_drivers",
  missing_audit$likely_issue == "targeted_driver_gap" ~
    "rerun_missing_driver_families_only",
  TRUE ~ "none"
)
missing_audit$priority <- dplyr::case_when(
  missing_audit$all_missing ~ 1L,
  missing_audit$dynamic_missing_only ~ 2L,
  missing_audit$n_missing_driver_families > 0 ~ 3L,
  TRUE ~ 9L
)

out_final <- out %>%
  dplyr::select(-all_of(join_keys), -starts_with("source_row_"), -starts_with("source_data_"))

# Driver CSVs can carry shapefile metadata placeholders (`geom`, `Shpfl_N`) from
# each extraction. Drop the suffixed join artifacts before publishing the final
# flat table.
join_artifact_cols <- grep("^(geom|Shpfl_N)(\\.|$)", names(out_final), value = TRUE)
if (length(join_artifact_cols) > 0) {
  out_final <- out_final %>% dplyr::select(-all_of(join_artifact_cols))
}

out_final <- collapse_duplicate_sites(out_final)
out_final <- fill_major_rock_from_percentages(out_final)
duplicate_site_groups <- attr(out_final, "duplicate_site_groups")
glimpse_checkpoint(out_final, "combined local output", glimpse_dir)

run_label_tag <- clean_tag(Sys.getenv("SILICA_RUN_LABEL", unset = ""))
if (!nzchar(run_label_tag)) {
  run_label_tag <- clean_tag(basename(dirname(ex)))
}
default_out_name <- if (nzchar(run_label_tag)) {
  paste0("all-data_si-extract_3_", format(Sys.Date(), "%Y%m%d"), "_", run_label_tag, ".csv")
} else {
  paste0("all-data_si-extract_3_", format(Sys.Date(), "%Y%m%d"), "_combinedLocal.csv")
}

out_file <- Sys.getenv(
  "SILICA_OUTPUT_FILE",
  file.path(ex, default_out_name)
)
write.csv(out_final, out_file, row.names = FALSE, na = "")
cat("WROTE:", out_file, "\n", sep = "")
cat("out_rows=", nrow(out_final), "\n", sep = "")
cat("duplicate_site_groups_collapsed=", if (is.null(duplicate_site_groups)) 0 else nrow(duplicate_site_groups), "\n", sep = "")

n <- names(out_final)
cat("evapo=", max_year(n, "^evapotrans_([0-9]{4})_kg_m2$"), "\n", sep = "")
cat("greenup=", max_year(n, "^greenup_cycle[01]_([0-9]{4})MMDD$"), "\n", sep = "")
cat("precip=", max_year(n, "^precip_([0-9]{4})_mm_per_day$"), "\n", sep = "")
cat("airtemp=", max_year(n, "^temp_([0-9]{4})_degC$"), "\n", sep = "")
cat("snow=", max_year(n, "^snow_([0-9]{4})_num_days$"), "\n", sep = "")
cat("npp=", max_year(n, "^npp_([0-9]{4})_kgC_m2_year$"), "\n", sep = "")

qa_root <- Sys.getenv("SILICA_QA_ROOT", unset = silica_review_root(detect_data_root()))
harmonization_dir <- file.path(qa_root, "harmonization")
rerun_dir <- file.path(qa_root, "rerun")
dir.create(harmonization_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(rerun_dir, recursive = TRUE, showWarnings = FALSE)

if (canonicalize_obidos) {
  kept_obidos <- out_final %>%
    filter(LTER == "GRO", Discharge_File_Name == "GRO_Obidos_Q") %>%
    mutate(harmonization_role = "kept_canonical")

  obidos_audit <- bind_rows(
    dropped_obidos %>% mutate(harmonization_role = "dropped_noncanonical"),
    kept_obidos
  )

  obidos_file <- file.path(
    harmonization_dir,
    paste0("obidos_canonicalization_", format(Sys.Date(), "%Y%m%d"), ".csv")
  )
  write.csv(obidos_audit, obidos_file, row.names = FALSE)
  cat("WROTE:", obidos_file, "\n", sep = "")
}

if (length(driver_summary) > 0) {
  sumtab <- bind_rows(driver_summary)
  sumtab_file <- file.path(harmonization_dir, paste0("key_match_summary_local_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  write.csv(sumtab, sumtab_file, row.names = FALSE)
  cat("WROTE:", sumtab_file, "\n", sep = "")
  glimpse_checkpoint(sumtab, "driver key match summary", glimpse_dir)
}

missing_file <- file.path(harmonization_dir, paste0("site_driver_missingness_", format(Sys.Date(), "%Y%m%d"), ".csv"))
write.csv(missing_audit, missing_file, row.names = FALSE, na = "")
cat("WROTE:", missing_file, "\n", sep = "")
cat("sites_with_any_missing=", sum(missing_audit$n_missing_driver_families > 0), "\n", sep = "")
glimpse_checkpoint(missing_audit, "site driver missingness audit", glimpse_dir)

if (!is.null(duplicate_site_groups) && nrow(duplicate_site_groups) > 0) {
  duplicate_file <- file.path(harmonization_dir, paste0("collapsed_duplicate_site_groups_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  write.csv(duplicate_site_groups, duplicate_file, row.names = FALSE, na = "")
  cat("WROTE:", duplicate_file, "\n", sep = "")
}

rerun_manifest <- missing_audit %>%
  dplyr::filter(n_missing_driver_families > 0) %>%
  dplyr::arrange(priority, LTER, Stream_Name) %>%
  dplyr::select(
    LTER, Stream_Name, Discharge_File_Name, Shapefile_Name, region,
    n_missing_driver_families, missing_driver_families,
    likely_issue, preferred_action, priority
  )
manifest_file <- file.path(rerun_dir, paste0("targeted_rerun_manifest_from_missingness_", format(Sys.Date(), "%Y%m%d"), ".csv"))
write.csv(rerun_manifest, manifest_file, row.names = FALSE, na = "")
cat("WROTE:", manifest_file, "\n", sep = "")

subset_file <- file.path(rerun_dir, paste0("targeted_rerun_subset_from_missingness_", format(Sys.Date(), "%Y%m%d"), ".csv"))
write.csv(
  rerun_manifest %>% dplyr::select(LTER, Stream_Name, Shapefile_Name, region) %>% dplyr::distinct(),
  subset_file,
  row.names = FALSE,
  na = ""
)
cat("WROTE:", subset_file, "\n", sep = "")

watershed_file <- file.path(sc, "silica-watersheds.shp")
if (file.exists(watershed_file)) {
  sheds <- sf::st_read(watershed_file, quiet = TRUE)
  if ("shp_nm" %in% names(sheds)) names(sheds)[names(sheds) == "shp_nm"] <- "Shapefile_Name"
  if (!("LTER" %in% names(sheds))) sheds$LTER <- NA_character_

  shed_keys <- sheds %>%
    sf::st_drop_geometry() %>%
    transmute(
      .LTER_KEY = join_lter(LTER),
      .SHP_KEY = join_chr(Shapefile_Name)
    ) %>%
    distinct()

  named_ref <- base %>%
    filter(!is.na(Shapefile_Name), !Shapefile_Name %in% c("?", "MISSING")) %>%
    transmute(
      LTER,
      Shapefile_Name,
      .LTER_KEY = join_lter(LTER),
      .SHP_KEY = join_chr(Shapefile_Name)
    ) %>%
    distinct()

  miss_poly <- anti_join(named_ref, shed_keys, by = c(".LTER_KEY", ".SHP_KEY")) %>%
    select(LTER, Shapefile_Name)
  miss_poly_file <- file.path(harmonization_dir, paste0("sites_true_missing_polygon_keys_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  write.csv(miss_poly, miss_poly_file, row.names = FALSE)
  cat("true_missing_polygon_keys=", nrow(miss_poly), "\n", sep = "")
  cat("WROTE:", miss_poly_file, "\n", sep = "")
}

old_file <- first_existing(c(
  Sys.getenv("SILICA_OLD_COMBINED", ""),
  find_previous_combined_file(ex, current_output = out_file)
))

if (nzchar(old_file) && file.exists(old_file)) {
  old <- read.csv(old_file, stringsAsFactors = FALSE, check.names = FALSE)

  key_fn <- function(d) {
    paste(join_lter(d$LTER), join_chr(d$Shapefile_Name), sep = "||")
  }

  oldk <- unique(key_fn(old))
  newk <- unique(key_fn(out_final))
  oldk <- oldk[!is.na(oldk)]
  newk <- newk[!is.na(newk)]

  comp <- data.frame(
    old_rows = nrow(old),
    new_rows = nrow(out_final),
    old_unique_site_keys = length(oldk),
    new_unique_site_keys = length(newk),
    dropped_from_old = length(setdiff(oldk, newk)),
    added_vs_old = length(setdiff(newk, oldk)),
    stringsAsFactors = FALSE
  )
  comp_file <- file.path(harmonization_dir, paste0("old_vs_new_site_counts_", format(Sys.Date(), "%Y%m%d"), ".csv"))
  write.csv(comp, comp_file, row.names = FALSE)
  cat("old_unique_site_keys=", comp$old_unique_site_keys, "\n", sep = "")
  cat("new_unique_site_keys=", comp$new_unique_site_keys, "\n", sep = "")
  cat("dropped_from_old=", comp$dropped_from_old, "\n", sep = "")
  cat("added_vs_old=", comp$added_vs_old, "\n", sep = "")
  cat("WROTE:", comp_file, "\n", sep = "")
}
