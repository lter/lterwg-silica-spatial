librarian::shelf(dplyr, sf, terra)

sf::sf_use_s2(FALSE)

root <- Sys.getenv(
  "SILICA_DATA_ROOT",
  unset = "/home/shares/lter-si/si-watershed-extract"
)
site_coord_dir <- Sys.getenv(
  "SILICA_SITE_COORD_DIR",
  unset = file.path(root, "site-coordinates")
)
watershed_file <- Sys.getenv(
  "SILICA_WATERSHED_FILE",
  unset = file.path(site_coord_dir, "silica-watersheds.shp")
)
raw_driver_dir <- Sys.getenv(
  "SILICA_RAW_DRIVER_DIR",
  unset = file.path(root, "raw-driver-data")
)
region <- Sys.getenv("SILICA_COVERAGE_REGION", unset = "germany")
coverage_label <- Sys.getenv("SILICA_COVERAGE_LABEL", unset = "")
lter_values <- strsplit(Sys.getenv("SILICA_COVERAGE_LTERS", unset = "Seine"), ",", fixed = TRUE)[[1]]
lter_values <- trimws(lter_values[nzchar(trimws(lter_values))])
lter_values <- tolower(lter_values)
stream_regex <- Sys.getenv("SILICA_COVERAGE_STREAM_REGEX", unset = "")
shape_regex <- Sys.getenv("SILICA_COVERAGE_SHAPE_REGEX", unset = "")
summary_csv <- Sys.getenv("SILICA_COVERAGE_SUMMARY_CSV", unset = "")
extract_values <- tolower(Sys.getenv("SILICA_COVERAGE_EXTRACT_VALUES", unset = "true")) == "true"
years_env <- trimws(Sys.getenv("SILICA_TARGET_YEARS", unset = "all"))

if (extract_values && !requireNamespace("exactextractr", quietly = TRUE)) {
  stop("Package exactextractr is required when SILICA_COVERAGE_EXTRACT_VALUES=TRUE.", call. = FALSE)
}

parse_years <- function(driver_name) {
  if (!nzchar(years_env) || tolower(years_env) %in% c("all", "full", "full-record", "full_record")) {
    return(switch(
      driver_name,
      evapo = as.character(2000:2025),
      greenup = as.character(2001:2024),
      npp = as.character(2001:2025),
      snow = as.character(2000:2025),
      as.character(2000:2025)
    ))
  }

  years <- strsplit(years_env, ",", fixed = TRUE)[[1]]
  years <- trimws(years[nzchar(trimws(years))])
  years
}

if (!file.exists(watershed_file)) {
  stop("Missing watershed file: ", watershed_file, call. = FALSE)
}

read_sheds <- function(path) {
  x <- st_read(path, quiet = TRUE)
  if ("shp_nm" %in% names(x)) {
    x <- rename(x, Shapefile_Name = shp_nm)
  }
  if ("Strm_Nm" %in% names(x)) {
    x <- rename(x, Stream_Name = Strm_Nm)
  }
  x
}

clean_lter <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("\\s*\\([^)]*\\)", "", x)
  trimws(x)
}

target <- read_sheds(watershed_file) %>%
  mutate(.LTER_CLEAN = tolower(clean_lter(LTER))) %>%
  filter(.LTER_CLEAN %in% lter_values, !is.na(Shapefile_Name), Shapefile_Name != "")

if (nzchar(stream_regex) || nzchar(shape_regex)) {
  keep <- rep(FALSE, nrow(target))
  if (nzchar(stream_regex) && "Stream_Name" %in% names(target)) {
    keep <- keep | grepl(stream_regex, as.character(target$Stream_Name), ignore.case = TRUE)
  }
  if (nzchar(shape_regex) && "Shapefile_Name" %in% names(target)) {
    keep <- keep | grepl(shape_regex, as.character(target$Shapefile_Name), ignore.case = TRUE)
  }
  target <- target[keep, , drop = FALSE]
}

if (!nrow(target)) {
  stop("No target polygons with Shapefile_Name found in: ", watershed_file, call. = FALSE)
}

cat("watershed_file=", watershed_file, "\n", sep = "")
cat("region=", region, "\n", sep = "")
cat("coverage_label=", coverage_label, "\n", sep = "")
cat("target_lters=", paste(lter_values, collapse = ","), "\n", sep = "")
cat("stream_regex=", stream_regex, "\n", sep = "")
cat("shape_regex=", shape_regex, "\n", sep = "")
cat("extract_values=", extract_values, "\n", sep = "")
cat("target_polygon_rows=", nrow(target), "\n", sep = "")

find_driver_files <- function(driver, product_pattern, year) {
  d <- file.path(raw_driver_dir, driver, region)
  if (!dir.exists(d)) {
    return(character())
  }
  files <- list.files(
    d,
    full.names = TRUE,
    recursive = TRUE
  )
  files[
    grepl(product_pattern, basename(files)) &
      grepl(paste0("doy", year), basename(files))
  ]
}

drivers <- list(
  evapo = list(dir = "raw-evapo-v061", pattern = "MOD16A2GF\\.061_ET_500m"),
  greenup = list(dir = "raw-greenup-v061", pattern = "MCD12Q2.*Greenup"),
  npp = list(dir = "raw-npp-v061", pattern = "MOD17A3HGF.*Npp"),
  snow = list(dir = "raw-snow-v061", pattern = "MOD10A2.*Snow")
)

check_one <- function(driver_name, driver_dir, product_pattern, year) {
  files <- find_driver_files(driver_dir, product_pattern, year)
  cat("\n== ", driver_name, " ", year, " ==\n", sep = "")
  cat("file_count=", length(files), "\n", sep = "")

  if (!length(files)) {
    return(data.frame(
      driver = driver_name,
      year = year,
      file_count = 0L,
      files_intersecting_target_bbox = 0L,
      target_rows_with_non_na_values = NA_integer_,
      non_na_extracted_cells = NA_integer_
    ))
  }

  target_vect <- vect(target)
  target_bbox_vect <- as.polygons(ext(target_vect), crs = crs(target_vect))

  intersecting <- logical(length(files))
  non_na_cells <- rep(NA_integer_, length(files))
  rows_with_values <- rep(NA_integer_, length(files))

  for (i in seq_along(files)) {
    r <- rast(files[[i]])
    r_ext_poly <- as.polygons(ext(r), crs = crs(r))
    r_ext_poly <- project(r_ext_poly, crs(target_bbox_vect))
    intersecting[[i]] <- length(relate(target_bbox_vect, r_ext_poly, "intersects")) > 0

    if (intersecting[[i]] && extract_values) {
      non_na_cells[[i]] <- 0L
      rows_with_values[[i]] <- 0L
      ex <- exactextractr::exact_extract(r, target, include_cols = "Shapefile_Name", progress = FALSE)
      ex <- bind_rows(ex)
      if (nrow(ex) && "value" %in% names(ex)) {
        non_na_cells[[i]] <- sum(!is.na(ex$value))
        rows_with_values[[i]] <- n_distinct(ex$Shapefile_Name[!is.na(ex$value)])
      }
    }

    if (i <= 5 || intersecting[[i]]) {
      cat(
        basename(files[[i]]),
        " extent=", paste(as.vector(ext(r)), collapse = ","),
        " intersects=", intersecting[[i]],
        " non_na_cells=", ifelse(is.na(non_na_cells[[i]]), "not_checked", non_na_cells[[i]]),
        " rows_with_values=", ifelse(is.na(rows_with_values[[i]]), "not_checked", rows_with_values[[i]]),
        "\n",
        sep = ""
      )
    }
  }

  max_checked <- function(x) {
    if (all(is.na(x))) {
      return(NA_integer_)
    }
    as.integer(max(x, na.rm = TRUE))
  }

  sum_checked <- function(x) {
    if (all(is.na(x))) {
      return(NA_integer_)
    }
    as.integer(sum(x, na.rm = TRUE))
  }

  data.frame(
    driver = driver_name,
    year = year,
    file_count = length(files),
    files_intersecting_target_bbox = sum(intersecting),
    target_rows_with_non_na_values = if (extract_values) max_checked(rows_with_values) else NA_integer_,
    non_na_extracted_cells = if (extract_values) sum_checked(non_na_cells) else NA_integer_
  )
}

summary <- bind_rows(lapply(names(drivers), function(driver_name) {
  cfg <- drivers[[driver_name]]
  bind_rows(lapply(parse_years(driver_name), function(year) {
    check_one(driver_name, cfg$dir, cfg$pattern, year)
  }))
}))

cat("summary_rows=", nrow(summary), "\n", sep = "")

if (nzchar(summary_csv)) {
  dir.create(dirname(summary_csv), recursive = TRUE, showWarnings = FALSE)
  write.csv(summary, summary_csv, row.names = FALSE, na = "")
  cat("\nWROTE:", summary_csv, "\n", sep = "")
}

if (any(summary$file_count == 0)) {
  cat("\nMissing file groups indicate the product/year was not pulled for this region.\n")
}

if (all(summary$file_count > 0) && all(summary$files_intersecting_target_bbox == 0)) {
  cat("\nFiles exist, but none of their raster extents intersect the target polygons. That means this crop does not cover these sites for these products.\n")
}

if (extract_values && any(summary$files_intersecting_target_bbox > 0 & summary$non_na_extracted_cells == 0)) {
  cat("\nAt least one product/year intersects the target bbox but yields zero non-NA extracted cells. That suggests nodata/masked rasters rather than a combine naming issue.\n")
}
