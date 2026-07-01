# Rebuild one combined local spatial table from the current driver CSVs plus
# the site reference table
librarian::shelf(dplyr, readxl, stringr, quiet = TRUE)

source(file.path(getwd(), "tools", "workflow_paths.R"))

keys <- c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")
join_keys <- c(".LTER_KEY", ".STREAM_KEY", ".DISCHARGE_KEY", ".SHP_KEY")
upload_to_google_drive <- toupper(Sys.getenv("SILICA_UPLOAD_TO_GOOGLE_DRIVE", unset = "FALSE")) == "TRUE"
google_drive_folder_id <- Sys.getenv(
  "SILICA_GOOGLE_DRIVE_FOLDER_ID",
  unset = "1zF_Itljwn0bUWSTHEkwkMDyNOiKPXRF1"
)
google_drive_account <- Sys.getenv("SILICA_GOOGLE_DRIVE_ACCOUNT", unset = "")
google_drive_overwrite <- toupper(Sys.getenv("SILICA_GOOGLE_DRIVE_OVERWRITE", unset = "TRUE")) == "TRUE"

log_table_summary <- function(df, label) {
  cat(label, ": rows=", nrow(df), " cols=", ncol(df), "\n", sep = "")
}

upload_file_to_google_drive <- function(path) {
  if (!upload_to_google_drive) {
    return(invisible(FALSE))
  }

  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop(
      "Install the googledrive package or set SILICA_UPLOAD_TO_GOOGLE_DRIVE=FALSE.",
      call. = FALSE
    )
  }

  tryCatch(
    {
      if (nzchar(google_drive_account)) {
        googledrive::drive_auth(email = google_drive_account)
      } else {
        googledrive::drive_auth()
      }
    },
    error = function(e) {
      stop(
        "Google Drive auth failed. Run googledrive::drive_auth() once from R or RStudio, then rerun this script.",
        call. = FALSE
      )
    }
  )

  uploaded <- googledrive::drive_upload(
    media = path,
    path = googledrive::as_id(google_drive_folder_id),
    name = basename(path),
    overwrite = google_drive_overwrite
  )
  cat("UPLOADED_TO_GOOGLE_DRIVE:", uploaded$name, "\n", sep = "")
  invisible(TRUE)
}

# Keep the key fields consistent before joining any driver tables
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

clean_tag <- function(x) {
  x <- trimws(x)
  if (!nzchar(x)) {
    return("")
  }
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("(^-+|-+$)", "", x)
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
# table when possible, then keep one normalized row per site key
read_driver_with_fallbacks <- function(family, extracted_dir, base_keys, explicit_files = NULL) {
  candidates <- if (!is.null(explicit_files)) explicit_files else resolve_driver_candidates(family, extracted_dir)
  candidates <- candidates[nzchar(candidates)]
  candidates <- order_driver_candidates(candidates)
  if (!length(candidates)) return(NULL)

  family_df <- NULL
  key_display_df <- NULL
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

    key_display_df <- bind_rows(
      key_display_df,
      d %>% select(all_of(c(keys, join_keys)))
    ) %>%
      distinct(across(all_of(join_keys)), .keep_all = TRUE)

    keep <- setdiff(names(d), c(keys, join_keys))
    d <- d[, c(join_keys, keep), drop = FALSE]

    family_df <- merge_family_rows(family_df, d)
    used_files <- c(used_files, f)
  }

  if (is.null(family_df)) return(NULL)
  attr(family_df, "used_files") <- used_files
  attr(family_df, "key_display") <- key_display_df
  family_df
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
    file.path(data_root, "silica-shapefiles", "extracted-data")
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
clean_obidos_names <- tolower(Sys.getenv("SILICA_CLEAN_OBIDOS_NAMES", "true")) == "true"

is_duplicate_obidos_row <- function(df) {
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

log_table_summary(ref_raw, "raw_site_reference")
log_table_summary(base, "normalized_site_reference_keys")

if (clean_obidos_names) {
  obidos_drop_idx <- is_duplicate_obidos_row(base)
  base <- base[!obidos_drop_idx, , drop = FALSE]
}

if (nzchar(data_root)) cat("data_root=", data_root, "\n", sep = "")
cat("site_coord_dir=", sc, "\n", sep = "")
cat("extracted_dir=", ex, "\n", sep = "")
cat("ref_raw_rows=", nrow(ref_raw), "\n", sep = "")
cat("ref_distinct_key_rows=", nrow(base), "\n", sep = "")

# Start from the reference table, then add one driver family at a time
out <- add_join_keys(base)

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
    message("Skipping missing family: ", family)
    next
  }

  driver_key_display <- attr(d, "key_display")
  if (!is.null(driver_key_display) && nrow(driver_key_display) > 0) {
    extra_driver_rows <- anti_join(
      driver_key_display,
      out %>% select(all_of(c(keys, join_keys))),
      by = join_keys
    )

    if (nrow(extra_driver_rows) > 0) {
      out <- bind_rows(
        out,
        extra_driver_rows %>% select(all_of(c(keys, join_keys)))
      )
    }
  }

  log_table_summary(d, paste("driver_family", family))

  keep <- setdiff(names(d), join_keys)

  out <- out %>% left_join(d %>% select(all_of(c(join_keys, keep))), by = join_keys)
  used_files <- paste(attr(d, "used_files"), collapse = ";")

  message("Adding ", family, " from ", used_files)
}

out_final <- out %>%
  dplyr::select(-all_of(join_keys), -starts_with("source_row_"), -starts_with("source_data_"))

# Driver CSVs can carry shapefile metadata placeholders (`geom`, `Shpfl_N`) from
# each extraction. Drop the suffixed join artifacts before publishing the final
# flat table
join_artifact_cols <- grep("^(geom|Shpfl_N)(\\.|$)", names(out_final), value = TRUE)
if (length(join_artifact_cols) > 0) {
  out_final <- out_final %>% dplyr::select(-all_of(join_artifact_cols))
}

out_final <- collapse_duplicate_sites(out_final)
out_final <- fill_major_rock_from_percentages(out_final)
duplicate_site_groups <- attr(out_final, "duplicate_site_groups")
log_table_summary(out_final, "combined_local_output")

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
upload_file_to_google_drive(out_file)
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
