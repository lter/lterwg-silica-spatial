# Validate one returned Aurora full non-GEE extraction and create analysis copies

source(file.path("tools", "cli_helpers.R"))
source(file.path("tools", "identifier_helpers.R"))

args <- commandArgs(trailingOnly = TRUE)
run_root <- require_input_dir(
  cli_value(args, "--run-root", required = TRUE),
  "returned Aurora run"
)
expected_path <- require_input_file(
  cli_value(args, "--expected", required = TRUE),
  "expected Aurora roster"
)
output_root <- cli_value(args, "--output-root", required = TRUE)
nohup_log <- cli_value(args, "--nohup-log", "")
coordinator_root <- cli_value(args, "--coordinator-root", "")
drivers <- strsplit(
  cli_value(
    args,
    "--drivers",
    "soil,lithology,elevation,permafrost,precip,air-temp"
  ),
  ",",
  fixed = TRUE
)[[1]]
drivers <- trimws(drivers)
drivers <- drivers[nzchar(drivers)]
parse_years <- function(value) {
  value <- trimws(value)
  if (!nzchar(value)) {
    return(integer())
  }
  if (grepl("^[0-9]{4}:[0-9]{4}$", value)) {
    bounds <- as.integer(strsplit(value, ":", fixed = TRUE)[[1]])
    return(seq.int(bounds[[1]], bounds[[2]]))
  }
  years <- suppressWarnings(as.integer(
    trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  ))
  if (anyNA(years)) {
    stop("Year arguments must be YYYY:YYYY or comma-separated years.", call. = FALSE)
  }
  years
}
required_years <- parse_years(cli_value(args, "--required-years", "2002:2025"))
expected_precip_years <- parse_years(
  cli_value(args, "--expected-precip-years", "1979:2025")
)
expected_airtemp_years <- parse_years(
  cli_value(args, "--expected-airtemp-years", "1948:2026")
)
drop_airtemp_years <- parse_years(
  cli_value(args, "--drop-airtemp-years", "2026")
)
completion_pattern <- cli_value(
  args,
  "--completion-pattern",
  "^Completed final backlog at "
)
analysis_max_year <- suppressWarnings(as.integer(
  cli_value(args, "--analysis-max-year", "2025")
))
if (!length(drivers)) stop("At least one driver is required.", call. = FALSE)
valid_drivers <- c(
  "soil", "lithology", "elevation", "permafrost", "precip", "air-temp"
)
invalid_drivers <- setdiff(drivers, valid_drivers)
if (length(invalid_drivers)) {
  stop(
    "Unsupported drivers: ", paste(invalid_drivers, collapse = ", "),
    call. = FALSE
  )
}
if (is.na(analysis_max_year)) {
  stop("--analysis-max-year must be an integer.", call. = FALSE)
}
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)
clean_root <- file.path(output_root, "cleaned-extracted-data")
dir.create(clean_root, recursive = TRUE, showWarnings = FALSE)

id_columns <- c(
  "LTER", "Shapefile_Name", "Discharge_File_Name", "Stream_Name"
)
read_character_csv <- function(path) {
  read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character()
  )
}

site_key <- function(data) {
  paste(
    normalize_lter_key(data$LTER),
    normalize_stream_key(data$Stream_Name),
    normalize_site_key(data$Shapefile_Name),
    normalize_site_key(data$Discharge_File_Name),
    sep = "__"
  )
}

geometry_key <- function(data) {
  paste(
    normalize_lter_key(data$LTER),
    normalize_site_key(data$Shapefile_Name),
    sep = "__"
  )
}

expected <- read_character_csv(expected_path)
assert_required_columns(expected, id_columns, "expected Aurora roster")
expected$.site_key <- site_key(expected)
expected$.geometry_key <- geometry_key(expected)
duplicated_site_keys <- unique(expected$.site_key[
  duplicated(expected$.site_key) |
    duplicated(expected$.site_key, fromLast = TRUE)
])
ambiguous_site_keys <- duplicated_site_keys[vapply(
  duplicated_site_keys,
  function(key) {
    length(unique(expected$.geometry_key[expected$.site_key == key])) > 1L
  },
  logical(1)
)]
if (length(ambiguous_site_keys)) {
  stop(
    "Normalized site aliases point to different watershed geometries: ",
    paste(ambiguous_site_keys, collapse = ", "),
    call. = FALSE
  )
}
expected_keys <- expected$.site_key
expected_geometry_keys <- unique(expected$.geometry_key)

issues <- list()
add_issue <- function(driver, check, detail, count = 1L) {
  issues[[length(issues) + 1L]] <<- data.frame(
    driver = driver,
    check = check,
    count = as.integer(count),
    detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

missingness <- list()
summaries <- list()
key_mismatches <- list()

numeric_bounds <- function(driver, column) {
  if (driver %in% c("soil", "lithology")) {
    return(c(0, 100))
  }
  if (driver == "permafrost") {
    return(c(0, 1))
  }
  if (driver == "precip") {
    return(c(0, 100))
  }
  if (driver == "air-temp") {
    return(c(-90, 60))
  }
  if (startsWith(column, "basin_slope_")) {
    return(c(0, 90))
  }
  c(-500, 9000)
}

expected_numeric_columns <- function(driver, names_in_file) {
  switch(driver,
    soil = grep("^soil_", names_in_file, value = TRUE),
    lithology = grep("^rocks_", names_in_file, value = TRUE),
    elevation = grep("^(elevation_|basin_slope_)", names_in_file, value = TRUE),
    permafrost = grep("^permafrost_", names_in_file, value = TRUE),
    precip = grep("^precip_", names_in_file, value = TRUE),
    `air-temp` = grep("^temp_", names_in_file, value = TRUE)
  )
}

for (driver in drivers) {
  pattern <- paste0("^si-extract_", driver, "_.*[.]csv$")
  paths <- list.files(
    file.path(run_root, "extracted-data"),
    pattern = pattern,
    full.names = TRUE
  )
  if (length(paths) != 1L) {
    add_issue(
      driver,
      "file_count",
      paste("Expected one extraction file; found", length(paths)),
      abs(length(paths) - 1L)
    )
    next
  }

  path <- paths[[1]]
  data <- read_character_csv(path)
  missing_ids <- setdiff(id_columns, names(data))
  if (length(missing_ids)) {
    add_issue(
      driver,
      "missing_identifier_columns",
      paste(missing_ids, collapse = ", "),
      length(missing_ids)
    )
    next
  }

  raw_rows <- nrow(data)
  data$.geometry_key <- geometry_key(data)
  raw_geometry_keys <- unique(data$.geometry_key)
  duplicate_geometry_keys <- unique(
    data$.geometry_key[
      duplicated(data$.geometry_key) |
        duplicated(data$.geometry_key, fromLast = TRUE)
    ]
  )
  value_columns <- setdiff(names(data), c(id_columns, ".geometry_key"))
  for (key in duplicate_geometry_keys) {
    group <- data[data$.geometry_key == key, value_columns, drop = FALSE]
    inconsistent <- names(group)[vapply(
      group,
      function(value) length(unique(value)) > 1L,
      logical(1)
    )]
    if (length(inconsistent)) {
      add_issue(
        driver,
        "inconsistent_geometry_alias_values",
        paste(key, paste(inconsistent, collapse = ", "), sep = ": "),
        length(inconsistent)
      )
    }
  }

  missing_keys <- setdiff(expected_geometry_keys, raw_geometry_keys)
  extra_keys <- setdiff(raw_geometry_keys, expected_geometry_keys)
  if (length(missing_keys)) {
    add_issue(
      driver,
      "missing_geometry_keys",
      "Expected geometry keys are absent",
      length(missing_keys)
    )
  }
  if (length(extra_keys)) {
    add_issue(
      driver,
      "extra_geometry_keys",
      "Unexpected geometry keys are present",
      length(extra_keys)
    )
  }
  if (length(missing_keys) || length(extra_keys)) {
    key_mismatches[[length(key_mismatches) + 1L]] <- rbind(
      data.frame(driver = driver, mismatch = "missing", site_key = missing_keys),
      data.frame(driver = driver, mismatch = "extra", site_key = extra_keys)
    )
    next
  }

  geometry_rows <- data[!duplicated(data$.geometry_key), , drop = FALSE]
  data <- geometry_rows[
    match(expected$.geometry_key, geometry_rows$.geometry_key), ,
    drop = FALSE
  ]
  data[id_columns] <- expected[id_columns]
  data$.site_key <- site_key(data)
  if (!identical(data$.site_key, expected_keys)) {
    add_issue(
      driver,
      "alias_expansion_site_keys",
      "Expanded site keys do not match the audited roster"
    )
  }

  numeric_columns <- expected_numeric_columns(driver, names(data))
  categorical_columns <- intersect(c("major_soil", "major_rock"), names(data))
  if (!length(numeric_columns)) {
    add_issue(driver, "numeric_schema", "No expected numeric columns were found")
  }

  invalid_numeric <- 0L
  nonfinite <- 0L
  out_of_range <- 0L
  missing_required_years <- 0L
  for (column in numeric_columns) {
    raw <- trimws(data[[column]])
    blank <- !nzchar(raw) | tolower(raw) %in% c("na", "nan")
    value <- suppressWarnings(as.numeric(raw))
    invalid <- !blank & is.na(value)
    infinite <- !blank & !is.na(value) & !is.finite(value)
    bounds <- numeric_bounds(driver, column)
    outside <- !blank & is.finite(value) &
      (value < bounds[[1]] | value > bounds[[2]])
    invalid_numeric <- invalid_numeric + sum(invalid)
    nonfinite <- nonfinite + sum(infinite)
    out_of_range <- out_of_range + sum(outside)

    year_match <- regmatches(column, regexpr("[0-9]{4}", column))
    year <- suppressWarnings(as.integer(year_match))
    required_year <- length(year) == 1L &&
      !is.na(year) &&
      year %in% required_years
    if (required_year) missing_required_years <- missing_required_years + sum(blank)

    missingness[[length(missingness) + 1L]] <- data.frame(
      driver = driver,
      column = column,
      missing = sum(blank),
      present = sum(!blank),
      minimum = if (any(is.finite(value))) min(value[is.finite(value)]) else NA_real_,
      maximum = if (any(is.finite(value))) max(value[is.finite(value)]) else NA_real_,
      stringsAsFactors = FALSE
    )
  }
  for (column in categorical_columns) {
    raw <- trimws(data[[column]])
    blank <- !nzchar(raw) | tolower(raw) %in% c("na", "nan")
    missingness[[length(missingness) + 1L]] <- data.frame(
      driver = driver,
      column = column,
      missing = sum(blank),
      present = sum(!blank),
      minimum = NA_real_,
      maximum = NA_real_,
      stringsAsFactors = FALSE
    )
  }

  if (invalid_numeric) {
    add_issue(driver, "invalid_numeric", "Nonblank values failed numeric parsing", invalid_numeric)
  }
  if (nonfinite) {
    add_issue(driver, "nonfinite_numeric", "Infinite numeric values", nonfinite)
  }
  if (out_of_range) {
    add_issue(driver, "plausibility_range", "Values outside broad physical bounds", out_of_range)
  }
  if (missing_required_years) {
    add_issue(
      driver,
      "missing_required_year_values",
      paste0(
        "Missing values in required annual columns for ",
        paste(required_years, collapse = ",")
      ),
      missing_required_years
    )
  }

  if (driver == "precip") {
    annual <- grep("^precip_[0-9]{4}_mm_per_day$", names(data), value = TRUE)
    expected_annual <- paste0(
      "precip_", expected_precip_years, "_mm_per_day"
    )
    if (!setequal(annual, expected_annual)) {
      add_issue(
        driver,
        "annual_schema",
        paste0(
          "Precipitation years are not exactly ",
          paste(expected_precip_years, collapse = ",")
        )
      )
    }
  }
  if (driver == "air-temp") {
    annual <- grep("^temp_[0-9]{4}_degC$", names(data), value = TRUE)
    expected_annual <- paste0(
      "temp_", expected_airtemp_years, "_degC"
    )
    if (!setequal(annual, expected_annual)) {
      add_issue(
        driver,
        "annual_schema",
        paste0(
          "Raw temperature years are not exactly ",
          paste(expected_airtemp_years, collapse = ",")
        )
      )
    }
  }

  clean <- data[, setdiff(names(data), c(".site_key", ".geometry_key")), drop = FALSE]
  if (driver == "air-temp" && length(drop_airtemp_years)) {
    clean <- clean[
      ,
      setdiff(names(clean), paste0("temp_", drop_airtemp_years, "_degC")),
      drop = FALSE
    ]
  }
  clean_path <- file.path(clean_root, basename(path))
  write.csv(clean, clean_path, row.names = FALSE, na = "")

  summaries[[length(summaries) + 1L]] <- data.frame(
    driver = driver,
    file = path,
    raw_rows = raw_rows,
    expanded_rows = nrow(data),
    expected_rows = nrow(expected),
    distinct_site_keys = length(unique(data$.site_key)),
    distinct_geometries = length(unique(data$.geometry_key)),
    numeric_columns = length(numeric_columns),
    status = if (any(vapply(issues, function(issue) issue$driver[[1]] == driver, logical(1)))) {
      "failed"
    } else {
      "passed"
    },
    stringsAsFactors = FALSE
  )
}

log_checks <- data.frame(
  check = character(),
  status = character(),
  detail = character(),
  stringsAsFactors = FALSE
)
if (nzchar(nohup_log)) {
  if (!file.exists(nohup_log)) {
    add_issue("logs", "missing_nohup_log", nohup_log)
  } else {
    lines <- readLines(nohup_log, warn = FALSE)
    completed <- any(grepl(completion_pattern, lines))
    fatal <- any(grepl("Execution halted|^Error|STOP:", lines))
    log_checks <- rbind(
      log_checks,
      data.frame(
        check = c("completion_marker", "fatal_error_text"),
        status = c(if (completed) "passed" else "failed", if (!fatal) "passed" else "failed"),
        detail = c(
          if (completed) tail(lines[grepl(completion_pattern, lines)], 1) else "Missing completion marker",
          if (fatal) "Fatal error text found" else "No fatal error text found"
        ),
        stringsAsFactors = FALSE
      )
    )
    if (!completed) add_issue("logs", "completion_marker", "Missing completion marker")
    if (fatal) add_issue("logs", "fatal_error_text", "Fatal error text found")
  }
}
if (nzchar(coordinator_root) && !dir.exists(coordinator_root)) {
  add_issue("logs", "missing_coordinator_root", coordinator_root)
}

summary_table <- if (length(summaries)) do.call(rbind, summaries) else data.frame()
issue_table <- if (length(issues)) {
  do.call(rbind, issues)
} else {
  data.frame(
    driver = character(),
    check = character(),
    count = integer(),
    detail = character(),
    stringsAsFactors = FALSE
  )
}
missingness_table <- if (length(missingness)) do.call(rbind, missingness) else data.frame()
key_table <- if (length(key_mismatches)) {
  do.call(rbind, key_mismatches)
} else {
  data.frame(
    driver = character(),
    mismatch = character(),
    site_key = character(),
    stringsAsFactors = FALSE
  )
}

write.csv(summary_table, file.path(output_root, "driver_qa_summary.csv"), row.names = FALSE, na = "")
write.csv(issue_table, file.path(output_root, "qa_issues.csv"), row.names = FALSE, na = "")
saveRDS(
  list(
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    expected_rows = nrow(expected),
    expected_geometries = length(expected_geometry_keys),
    normalized_site_alias_keys = duplicated_site_keys,
    drivers = summary_table,
    issues = issue_table,
    missingness = missingness_table,
    key_mismatches = key_table,
    log_checks = log_checks,
    analysis_max_year = analysis_max_year,
    raw_temperature_years_excluded = drop_airtemp_years
  ),
  file.path(output_root, "aurora_full_non_gee_qa.rds")
)
unlink(file.path(
  output_root,
  c("missingness_by_column.csv", "site_key_mismatches.csv", "log_qa.csv")
))

if (nrow(issue_table)) {
  stop(
    "Aurora QA failed with ", nrow(issue_table),
    " issue categories. See ", file.path(output_root, "qa_issues.csv"),
    call. = FALSE
  )
}

cat("Aurora QA passed for", nrow(expected), "site rows across", length(drivers), "drivers.\n")
cat("Clean analysis files:", clean_root, "\n")
