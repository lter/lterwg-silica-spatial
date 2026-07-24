normalize_domain_key <- function(x) tolower(trimws(as.character(x)))

domain_filter_source <- tryCatch(
  {
    source_files <- vapply(
      sys.frames(),
      function(frame) {
        if (is.null(frame$ofile)) "" else as.character(frame$ofile)
      },
      character(1)
    )
    source_files <- source_files[nzchar(source_files)]
    normalizePath(tail(source_files, 1), mustWork = TRUE)
  },
  error = function(...) ""
)
domain_filter_repo_root <- if (nzchar(domain_filter_source)) {
  dirname(dirname(domain_filter_source))
} else {
  getwd()
}
default_domain_zero_rules <- file.path(
  domain_filter_repo_root,
  "04_combine_qaqc",
  "config",
  "spatial_domain_zero_rules.tsv"
)

domain_site_key <- function(lter, stream) {
  paste(normalize_domain_key(lter), normalize_domain_key(stream), sep = "||")
}

domain_is_missing <- function(x) {
  is.na(x) | (is.character(x) & trimws(x) == "")
}

first_domain_reason <- function(reason_checks) {
  out <- rep(NA_character_, length(reason_checks[[1]]))
  for (reason in names(reason_checks)) {
    rows <- is.na(out) & reason_checks[[reason]]
    out[rows] <- reason
  }
  out
}

load_domain_zero_rules <- function(path = default_domain_zero_rules) {
  rules <- read.delim(
    path,
    sep = "\t",
    quote = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  required <- c("Variable", "LTER")
  missing <- setdiff(required, names(rules))
  if (length(missing)) {
    stop(
      "Spatial-domain zero-rule table is missing columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  rules$Variable <- tolower(trimws(rules$Variable))
  rules$LTER <- trimws(rules$LTER)
  rules
}

apply_spatial_domain_filters <- function(
  data,
  evidence,
  zero_snow_lters = NULL,
  zero_permafrost_lters = NULL,
  zero_rules_path = Sys.getenv(
    "SILICA_SPATIAL_DOMAIN_ZERO_RULES",
    unset = default_domain_zero_rules
  )
) {
  if (is.null(zero_snow_lters) || is.null(zero_permafrost_lters)) {
    zero_rules <- load_domain_zero_rules(zero_rules_path)
    if (is.null(zero_snow_lters)) {
      zero_snow_lters <- zero_rules$LTER[zero_rules$Variable == "snow"]
    }
    if (is.null(zero_permafrost_lters)) {
      zero_permafrost_lters <- zero_rules$LTER[
        zero_rules$Variable == "permafrost"
      ]
    }
  }
  id_columns <- c("LTER", "Stream_Name", "Shapefile_Name")
  evidence_columns <- c(
    id_columns, "latitude", "mean_temp_degC", "elevation_max_m",
    "temperature_source", "elevation_source"
  )
  if (length(setdiff(id_columns, names(data)))) {
    stop("The spatial table lacks required site identifiers.", call. = FALSE)
  }
  if (length(setdiff(evidence_columns, names(evidence)))) {
    stop("The domain evidence lacks required fields.", call. = FALSE)
  }

  data_key <- domain_site_key(data$LTER, data$Stream_Name)
  evidence_key <- domain_site_key(evidence$LTER, evidence$Stream_Name)
  if (anyDuplicated(data_key) || anyDuplicated(evidence_key)) {
    stop("The spatial table or domain evidence has duplicate site keys.", call. = FALSE)
  }
  evidence_match <- match(data_key, evidence_key)
  if (sum(!is.na(evidence_match)) != nrow(evidence)) {
    stop("At least one domain-evidence row did not match the spatial table.", call. = FALSE)
  }

  latitude <- mean_temp <- coldest_month_temp <- elevation_max <-
    rep(NA_real_, nrow(data))
  matched_rows <- which(!is.na(evidence_match))
  matched_evidence <- evidence_match[matched_rows]
  latitude[matched_rows] <- as.numeric(evidence$latitude[matched_evidence])
  mean_temp[matched_rows] <- as.numeric(evidence$mean_temp_degC[matched_evidence])
  elevation_max[matched_rows] <- as.numeric(evidence$elevation_max_m[matched_evidence])
  if ("coldest_month_temp_degC" %in% names(evidence)) {
    coldest_month_temp[matched_rows] <- as.numeric(
      evidence$coldest_month_temp_degC[matched_evidence]
    )
  }

  has_shape <- !domain_is_missing(data$Shapefile_Name)
  permafrost_columns <- grep("^permafrost_", names(data), value = TRUE)
  snow_columns <- grep("^snow_", names(data), value = TRUE)
  if (!length(permafrost_columns) || !length(snow_columns)) {
    stop("The spatial table lacks snow or permafrost fields.", call. = FALSE)
  }

  permafrost_present <- rowSums(vapply(
    data[permafrost_columns],
    function(column) !domain_is_missing(column),
    logical(nrow(data))
  )) > 0L
  permafrost_reason <- first_domain_reason(list(
    "listed non-permafrost domain for this data product" =
      data$LTER %in% zero_permafrost_lters,
    "mean annual temperature above permafrost threshold" =
      !is.na(mean_temp) & mean_temp >= 2,
    "all monthly mean temperatures above freezing" =
      !is.na(coldest_month_temp) & coldest_month_temp > 0,
    "low latitude and not a high alpine basin" =
      !is.na(latitude) & abs(latitude) < 45 &
        (is.na(elevation_max) | elevation_max < 2500)
  ))
  permafrost_rows <- which(
    !is.na(evidence_match) & has_shape & !permafrost_present &
      !is.na(permafrost_reason)
  )
  for (column in permafrost_columns) data[[column]][permafrost_rows] <- 0

  snow_reason <- first_domain_reason(list(
    "listed no-snow domain for this data product" =
      data$LTER %in% zero_snow_lters,
    "tropical lowland basin with warm annual temperature" =
      !is.na(latitude) & abs(latitude) < 23.5 &
        !is.na(elevation_max) & elevation_max < 1500 &
        !is.na(mean_temp) & mean_temp >= 10,
    "frost-free warm lowland basin" =
      !is.na(latitude) & abs(latitude) < 35 &
        !is.na(elevation_max) & elevation_max < 800 &
        !is.na(mean_temp) & mean_temp >= 18 &
        !is.na(coldest_month_temp) & coldest_month_temp >= 10,
    "all monthly mean temperatures warm and basin is low elevation" =
      !is.na(coldest_month_temp) & coldest_month_temp >= 12 &
        !is.na(mean_temp) & mean_temp >= 20 &
        (is.na(elevation_max) | elevation_max < 1000)
  ))
  snow_rows <- which(
    !is.na(evidence_match) & has_shape & !is.na(snow_reason)
  )
  for (column in snow_columns) data[[column]][snow_rows] <- 0

  make_log <- function(rows, family, reasons) {
    if (!length(rows)) {
      return(data.frame())
    }
    evidence_rows <- evidence_match[rows]
    data.frame(
      LTER = data$LTER[rows],
      Stream_Name = data$Stream_Name[rows],
      Shapefile_Name = data$Shapefile_Name[rows],
      fix_family = family,
      reason = reasons[rows],
      latitude = latitude[rows],
      mean_temp_degC = mean_temp[rows],
      elevation_max_m = elevation_max[rows],
      temperature_source = evidence$temperature_source[evidence_rows],
      elevation_source = evidence$elevation_source[evidence_rows],
      stringsAsFactors = FALSE
    )
  }
  fix_log <- rbind(
    make_log(permafrost_rows, "permafrost_zero", permafrost_reason),
    make_log(snow_rows, "snow_zero", snow_reason)
  )

  list(
    data = data,
    fix_log = fix_log,
    permafrost_rows = permafrost_rows,
    snow_rows = snow_rows
  )
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 4L) {
    stop(
      "Usage: Rscript tools/apply_spatial_domain_filters.R INPUT.csv EVIDENCE.csv OUTPUT.csv FIX_LOG.csv",
      call. = FALSE
    )
  }
  input <- read.csv(args[[1]], stringsAsFactors = FALSE, check.names = FALSE)
  evidence <- read.csv(args[[2]], stringsAsFactors = FALSE, check.names = FALSE)
  result <- apply_spatial_domain_filters(input, evidence)
  write.csv(result$data, args[[3]], row.names = FALSE, na = "")
  write.csv(result$fix_log, args[[4]], row.names = FALSE, na = "")
  cat("Saved the filtered spatial table: ", args[[3]], "\n", sep = "")
  cat("Saved the filter log: ", args[[4]], "\n", sep = "")
}
