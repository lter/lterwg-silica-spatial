librarian::shelf(dplyr, sf, terra)

clean_key <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

mean_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x) & is.finite(x)]
  if (!length(x)) {
    return(NA_real_)
  }
  mean(x)
}

buffer_fallback_m <- function() {
  raw <- Sys.getenv("SILICA_FINAL_REMAINING_BUFFER_FALLBACK_M", unset = "500,1000,2000,5000,10000")
  vals <- suppressWarnings(as.numeric(strsplit(raw, ",")[[1]]))
  vals <- vals[is.finite(vals) & vals > 0]
  sort(unique(vals))
}

use_buffer_fallback_for_file <- function(file) {
  last_gap_only <- tolower(Sys.getenv(
    "SILICA_FINAL_REMAINING_BUFFER_FALLBACK_LAST_GAP_ONLY",
    unset = "true"
  )) %in% c("true", "1", "yes")

  !last_gap_only || grepl("last-gap-", basename(file), fixed = TRUE)
}

fallback_events <- list()

record_fallback <- function(driver, file, row_id, buffer_m, value) {
  fallback_events[[length(fallback_events) + 1L]] <<- tibble(
    driver = driver,
    file = basename(file),
    row_id = row_id,
    buffer_m = buffer_m,
    value = value
  )
}

sites_for_raster <- function(sites_sf, raster, buffer_m = 0) {
  target_crs <- terra::crs(raster)
  if (!nzchar(target_crs)) {
    target_crs <- sf::st_crs(sites_sf)
  }

  x <- sf::st_transform(sites_sf, target_crs)
  if (buffer_m > 0) {
    x <- sf::st_transform(x, 3857)
    x <- sf::st_set_geometry(x, sf::st_buffer(sf::st_geometry(x), buffer_m))
    x <- sf::st_transform(x, target_crs)
  }
  x
}

extract_mean_once <- function(raster, sites_sf) {
  vals <- terra::extract(
    raster,
    terra::vect(sites_sf),
    fun = mean,
    na.rm = TRUE,
    touches = TRUE
  )
  value_col <- setdiff(names(vals), "ID")[[1]]
  out <- tibble(
    row_id = sites_sf$row_id[vals$ID],
    value = suppressWarnings(as.numeric(vals[[value_col]]))
  )
  out$value[!is.finite(out$value)] <- NA_real_
  out
}

extract_mean_by_site <- function(files, sites_sf, driver = "unknown") {
  if (!length(files) || !nrow(sites_sf)) {
    return(tibble(row_id = integer(), value = numeric()))
  }

  buffer_m <- buffer_fallback_m()
  out <- vector("list", length(files))

  for (i in seq_along(files)) {
    r <- terra::rast(files[[i]])
    sites_exact <- sites_for_raster(sites_sf, r)
    vals <- extract_mean_once(r, sites_exact)

    missing_rows <- vals$row_id[is.na(vals$value)]
    if (length(missing_rows) && length(buffer_m) && use_buffer_fallback_for_file(files[[i]])) {
      for (dist_m in buffer_m) {
        still_missing <- vals$row_id[is.na(vals$value)]
        if (!length(still_missing)) {
          break
        }

        missing_sites <- sites_sf[sites_sf$row_id %in% still_missing, , drop = FALSE]
        buffered_sites <- sites_for_raster(missing_sites, r, buffer_m = dist_m)
        buffered_vals <- extract_mean_once(r, buffered_sites)
        buffered_vals <- buffered_vals[!is.na(buffered_vals$value), , drop = FALSE]
        if (!nrow(buffered_vals)) {
          next
        }

        fill_idx <- match(buffered_vals$row_id, vals$row_id)
        vals$value[fill_idx] <- buffered_vals$value
        for (j in seq_len(nrow(buffered_vals))) {
          record_fallback(driver, files[[i]], buffered_vals$row_id[[j]], dist_m, buffered_vals$value[[j]])
        }
      }
    }

    out[[i]] <- vals
  }

  bind_rows(out)
}

read_run_sheds <- function(shapefile) {
  x <- sf::st_read(shapefile, quiet = TRUE)

  if (!"shp_nm" %in% names(x) && "Shapefile_Name" %in% names(x)) {
    names(x)[names(x) == "Shapefile_Name"] <- "shp_nm"
  }

  needed <- c("LTER", "shp_nm")
  missing <- setdiff(needed, names(x))
  if (length(missing)) {
    stop("Missing watershed fields: ", paste(missing, collapse = ", "), call. = FALSE)
  }

  x %>%
    mutate(
      .LTER_KEY = clean_key(LTER),
      .SHP_KEY = clean_key(shp_nm)
    )
}

prepare_sites <- function(current_file, subset_file, shapefile) {
  current <- read.csv(current_file, stringsAsFactors = FALSE, check.names = FALSE)
  subset <- read.csv(subset_file, stringsAsFactors = FALSE, check.names = FALSE)
  sheds <- read_run_sheds(shapefile)

  if (!"Active_Shapefile_Name" %in% names(subset)) {
    subset$Active_Shapefile_Name <- subset$Shapefile_Name
  }

  site_index <- subset %>%
    transmute(
      .row_id = row_number(),
      LTER,
      Stream_Name,
      Discharge_File_Name,
      Shapefile_Name,
      Active_Shapefile_Name,
      Region,
      .LTER_KEY = clean_key(LTER),
      .SHP_KEY = clean_key(Shapefile_Name),
      .ACTIVE_SHP_KEY = clean_key(Active_Shapefile_Name)
    )

  current_keyed <- current %>%
    mutate(
      .row_id = row_number(),
      .LTER_KEY = clean_key(LTER),
      .SHP_KEY = clean_key(Shapefile_Name)
    )

  site_index <- site_index %>%
    left_join(
      current_keyed %>% select(.current_row = .row_id, .LTER_KEY, .SHP_KEY),
      by = c(".LTER_KEY", ".SHP_KEY")
    )

  sheds_active <- sheds %>%
    left_join(
      site_index,
      by = c(".LTER_KEY", ".SHP_KEY" = ".ACTIVE_SHP_KEY")
    ) %>%
    filter(!is.na(.current_row))

  matched_rows <- unique(sheds_active$.current_row)
  sheds_original <- sheds %>%
    left_join(
      site_index %>% filter(!.current_row %in% matched_rows),
      by = c(".LTER_KEY", ".SHP_KEY")
    ) %>%
    filter(!is.na(.current_row))

  sheds_joined <- bind_rows(sheds_active, sheds_original) %>%
    mutate(row_id = .current_row)

  list(current = current, subset = site_index, sheds = sheds_joined)
}

rebuild_greenup <- function(input_dir, output_dir, raster_root) {
  current_file <- file.path(
    input_dir,
    "si-extract_greenup_v061_20260526_final-v4-remaining-dynamic-20260526.csv"
  )
  subset_file <- "generated_outputs/rerun/active-final-run/final_remaining_dynamic_greenup_subset_20260526.csv"
  shapefile <- "/private/tmp/final_remaining_dynamic_20260526/site-coordinates-greenup/silica-watersheds_20260526_final-v4-remaining-dynamic-20260526_greenup.shp"

  prepared <- prepare_sites(current_file, subset_file, shapefile)
  out <- prepared$current

  for (region in sort(unique(prepared$subset$Region))) {
    region_key <- clean_key(region)
    region_dir <- file.path(raster_root, "raw-greenup-v061", region)
    files <- Sys.glob(file.path(region_dir, "*MCD12Q2.061_Greenup_*.tif"))
    if (!length(files)) {
      next
    }

    sites_sf <- prepared$sheds %>% filter(clean_key(Region) == region_key)
    if (!nrow(sites_sf)) {
      next
    }

    parsed <- tibble(file = files) %>%
      mutate(
        year = suppressWarnings(as.integer(sub(".*_doy([0-9]{4})[0-9]{3}.*", "\\1", basename(file)))),
        cycle = suppressWarnings(as.integer(sub(".*Greenup_([0-9]+)_doy.*", "\\1", basename(file))))
      ) %>%
      filter(!is.na(year), !is.na(cycle))

    for (yr in sort(unique(parsed$year))) {
      for (cyc in sort(unique(parsed$cycle[parsed$year == yr]))) {
        use_files <- parsed$file[parsed$year == yr & parsed$cycle == cyc]
        vals <- extract_mean_by_site(use_files, sites_sf, driver = "greenup") %>%
          group_by(row_id) %>%
          summarize(value = mean_or_na(value), .groups = "drop") %>%
          mutate(value = as.character(as.Date(floor(value), origin = "1970-01-01")))

        col <- paste0("greenup_cycle", cyc, "_", yr, "MMDD")
        if (!col %in% names(out)) {
          out[[col]] <- NA_character_
        }
        rows <- vals$row_id[!is.na(vals$value)]
        out[[col]][rows] <- vals$value[match(rows, vals$row_id)]
      }
    }
  }

  write.csv(
    out,
    file.path(output_dir, basename(current_file)),
    row.names = FALSE,
    na = ""
  )
}

rebuild_npp <- function(input_dir, output_dir, raster_root) {
  current_file <- file.path(
    input_dir,
    "si-extract_npp_v061_20260526_final-v4-remaining-dynamic-20260526.csv"
  )
  subset_file <- "generated_outputs/rerun/active-final-run/final_remaining_dynamic_npp_subset_20260526.csv"
  shapefile <- "/private/tmp/final_remaining_dynamic_20260526/site-coordinates-npp/silica-watersheds_20260526_final-v4-remaining-dynamic-20260526_npp.shp"

  prepared <- prepare_sites(current_file, subset_file, shapefile)
  out <- prepared$current

  for (region in sort(unique(prepared$subset$Region))) {
    region_key <- clean_key(region)
    region_dir <- file.path(raster_root, "raw-npp-v061", region)
    files <- Sys.glob(file.path(region_dir, "*MOD17A3HGF.061_Npp_500m*.tif"))
    if (!length(files)) {
      next
    }

    sites_sf <- prepared$sheds %>% filter(clean_key(Region) == region_key)
    if (!nrow(sites_sf)) {
      next
    }

    parsed <- tibble(file = files) %>%
      mutate(year = suppressWarnings(as.integer(sub(".*_doy([0-9]{4})[0-9]{3}.*", "\\1", basename(file))))) %>%
      filter(!is.na(year))

    for (yr in sort(unique(parsed$year))) {
      use_files <- parsed$file[parsed$year == yr]
      vals <- extract_mean_by_site(use_files, sites_sf, driver = "npp") %>%
        group_by(row_id) %>%
        summarize(value = mean_or_na(value), .groups = "drop")

      col <- paste0("npp_", yr, "_kgC_m2_year")
      if (!col %in% names(out)) {
        out[[col]] <- NA_real_
      }
      rows <- vals$row_id[!is.na(vals$value)]
      out[[col]][rows] <- vals$value[match(rows, vals$row_id)]
    }
  }

  write.csv(
    out,
    file.path(output_dir, basename(current_file)),
    row.names = FALSE,
    na = ""
  )
}

rebuild_snow <- function(input_dir, output_dir, raster_root) {
  current_file <- file.path(
    input_dir,
    "si-extract_snow_v061_20260526_final-v4-remaining-dynamic-20260526.csv"
  )
  subset_file <- "generated_outputs/rerun/active-final-run/final_remaining_dynamic_snow_subset_20260526.csv"
  region_year_file <- "generated_outputs/rerun/active-final-run/final_remaining_dynamic_snow_region_years_20260526.csv"
  shapefile <- "/private/tmp/final_remaining_dynamic_20260526/site-coordinates-snow/silica-watersheds_20260526_final-v4-remaining-dynamic-20260526_snow.shp"

  prepared <- prepare_sites(current_file, subset_file, shapefile)
  region_years <- read.csv(region_year_file, stringsAsFactors = FALSE, check.names = FALSE) %>%
    mutate(
      Region = region,
      Region_key = clean_key(region),
      year = suppressWarnings(as.integer(year))
    )
  out <- prepared$current

  snow_targets <- prepared$subset %>%
    filter(!is.na(.current_row)) %>%
    mutate(Region_key = clean_key(Region)) %>%
    left_join(region_years %>% select(Region_key, year), by = "Region_key") %>%
    filter(!is.na(year))

  has_snow_value <- function(row_id, year) {
    cols <- c(
      paste0("snow_", year, "_num_days"),
      paste0("snow_", year, "_max_prop_area")
    )
    cols <- intersect(cols, names(out))
    if (!length(cols)) {
      return(FALSE)
    }
    vals <- out[row_id, cols, drop = FALSE]
    any(!is.na(vals) & trimws(as.character(as.matrix(vals))) != "")
  }

  snow_targets$already_present <- mapply(
    has_snow_value,
    snow_targets$.current_row,
    snow_targets$year
  )
  snow_targets <- snow_targets %>% filter(!already_present)
  if (!nrow(snow_targets)) {
    write.csv(
      out,
      file.path(output_dir, basename(current_file)),
      row.names = FALSE,
      na = ""
    )
    return(invisible(out))
  }

  code_days <- vapply(0:255, function(x) sum(as.integer(intToBits(x)[1:8])), integer(1))
  fallback_m <- buffer_fallback_m()
  for (region in sort(unique(prepared$subset$Region))) {
    region_key <- clean_key(region)
    region_dir <- file.path(raster_root, "raw-snow-v061", region)
    files <- Sys.glob(file.path(region_dir, "*MOD10A2.061_Eight_Day_Snow_Cover*.tif"))
    if (!length(files)) {
      next
    }

    wanted_years <- unique(snow_targets$year[snow_targets$Region_key == region_key])
    if (!length(wanted_years)) {
      next
    }

    wanted_rows <- unique(snow_targets$.current_row[snow_targets$Region_key == region_key])
    sites_sf <- prepared$sheds %>%
      filter(clean_key(Region) == region_key, row_id %in% wanted_rows)
    if (!nrow(sites_sf)) {
      next
    }

    file_base <- basename(files)
    doy_match <- regmatches(file_base, regexec("doy([0-9]{4})([0-9]{3})", file_base))
    parsed <- tibble(
      file = files,
      year = suppressWarnings(as.integer(vapply(
        doy_match,
        function(x) if (length(x) >= 3) x[[2]] else NA_character_,
        character(1)
      ))),
      doy = suppressWarnings(as.integer(vapply(
        doy_match,
        function(x) if (length(x) >= 3) x[[3]] else NA_character_,
        character(1)
      )))
    ) %>%
      filter(!is.na(year), year %in% wanted_years)

    if (!nrow(parsed)) {
      next
    }

    recode_from <- 0:255
    recode_to <- as.numeric(code_days)
    period_rows <- vector("list", nrow(parsed))

    for (i in seq_len(nrow(parsed))) {
      r <- terra::rast(parsed$file[[i]])
      sites_exact <- sites_for_raster(sites_sf, r)
      crop_sites <- if (length(fallback_m)) {
        sites_for_raster(sites_sf, r, buffer_m = max(fallback_m))
      } else {
        sites_exact
      }
      sites_ext <- terra::ext(terra::vect(crop_sites))
      r_ext <- terra::ext(r)
      no_overlap <- r_ext[2] < sites_ext[1] ||
        r_ext[1] > sites_ext[2] ||
        r_ext[4] < sites_ext[3] ||
        r_ext[3] > sites_ext[4]

      if (no_overlap) {
        period_rows[[i]] <- tibble(
          row_id = integer(),
          year = integer(),
          doy = integer(),
          snow_days_8day = numeric()
        )
        next
      }

      r <- terra::crop(r, sites_ext)
      r <- terra::subst(r, from = recode_from, to = recode_to, others = NA)
      vals <- extract_mean_once(r, sites_exact)

      if (any(is.na(vals$value)) && length(fallback_m) && use_buffer_fallback_for_file(parsed$file[[i]])) {
        for (dist_m in fallback_m) {
          still_missing <- vals$row_id[is.na(vals$value)]
          if (!length(still_missing)) {
            break
          }

          missing_sites <- sites_sf[sites_sf$row_id %in% still_missing, , drop = FALSE]
          buffered_sites <- sites_for_raster(missing_sites, r, buffer_m = dist_m)
          buffered_vals <- extract_mean_once(r, buffered_sites)
          buffered_vals <- buffered_vals[!is.na(buffered_vals$value), , drop = FALSE]
          if (!nrow(buffered_vals)) {
            next
          }

          fill_idx <- match(buffered_vals$row_id, vals$row_id)
          vals$value[fill_idx] <- buffered_vals$value
          for (j in seq_len(nrow(buffered_vals))) {
            record_fallback("snow", parsed$file[[i]], buffered_vals$row_id[[j]], dist_m, buffered_vals$value[[j]])
          }
        }
      }

      period_rows[[i]] <- tibble(
        row_id = vals$row_id,
        year = parsed$year[[i]],
        doy = parsed$doy[[i]],
        snow_days_8day = vals$value
      )
    }

    annual <- bind_rows(period_rows) %>%
      filter(!is.na(row_id), !is.na(snow_days_8day)) %>%
      mutate(snow_frac_8day = snow_days_8day / 8) %>%
      group_by(row_id, year) %>%
      summarize(
        snow_days = sum(snow_days_8day, na.rm = TRUE),
        snow_frac = max(snow_frac_8day, na.rm = TRUE),
        .groups = "drop"
      )

    for (yr in sort(unique(annual$year))) {
      rows <- annual$row_id[annual$year == yr]
      num_col <- paste0("snow_", yr, "_num_days")
      frac_col <- paste0("snow_", yr, "_max_prop_area")
      if (!num_col %in% names(out)) {
        out[[num_col]] <- NA_real_
      }
      if (!frac_col %in% names(out)) {
        out[[frac_col]] <- NA_real_
      }
      idx <- match(rows, annual$row_id[annual$year == yr])
      out[[num_col]][rows] <- annual$snow_days[annual$year == yr][idx]
      out[[frac_col]][rows] <- annual$snow_frac[annual$year == yr][idx]
    }
  }

  write.csv(
    out,
    file.path(output_dir, basename(current_file)),
    row.names = FALSE,
    na = ""
  )
}

main <- function() {
  input_dir <- Sys.getenv(
    "SILICA_FINAL_REMAINING_INPUT_DIR",
    unset = "/private/tmp/final_remaining_dynamic_20260526/extracted-data"
  )
  output_dir <- Sys.getenv(
    "SILICA_FINAL_REMAINING_OUTPUT_DIR",
    unset = "/private/tmp/final_remaining_dynamic_20260526/extracted-data-corrected"
  )
  raster_root <- Sys.getenv(
    "SILICA_FINAL_REMAINING_RASTER_ROOT",
    unset = "/private/tmp/appeears_product_rasters_flat_20260526_final_remaining"
  )

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(
    from = Sys.glob(file.path(input_dir, "si-extract_*_20260526_final-v4-remaining-dynamic-20260526.csv")),
    to = output_dir,
    overwrite = TRUE
  )

  rebuild_greenup(input_dir, output_dir, raster_root)
  rebuild_npp(input_dir, output_dir, raster_root)
  rebuild_snow(input_dir, output_dir, raster_root)

  fallback_log_file <- file.path(output_dir, "final_remaining_dynamic_buffer_fallback_log_20260605.csv")
  if (length(fallback_events)) {
    write.csv(bind_rows(fallback_events), fallback_log_file, row.names = FALSE, na = "")
  } else if (file.exists(fallback_log_file)) {
    unlink(fallback_log_file)
  }

  for (f in Sys.glob(file.path(output_dir, "si-extract_*_20260526_final-v4-remaining-dynamic-20260526.csv"))) {
    x <- read.csv(f, check.names = FALSE)
    value_cols <- grep("^(greenup_|npp_|evapotrans_|snow_)", names(x), value = TRUE)
    non_missing <- if (length(value_cols)) sum(!is.na(x[value_cols])) else 0L
    message(basename(f), ": rows=", nrow(x), ", non_missing_driver_values=", non_missing)
  }
}

if (sys.nframe() == 0) {
  main()
}
