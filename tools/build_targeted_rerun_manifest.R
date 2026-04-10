#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit)) {
    return(default)
  }
  args[hit[1] + 1]
}

ref_path <- get_arg(
  "--ref",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial_data_extractions/master/Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv"
)
combined_path <- get_arg(
  "--combined",
  "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial_data_extractions/all-data_si-extract_3_20260323.csv"
)
outdir <- get_arg("--outdir", "qa")
large_basin_km2 <- suppressWarnings(as.numeric(get_arg("--large-basin-km2", "1000")))

dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

norm_chr <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  x
}

ref <- read_csv(ref_path, show_col_types = FALSE)
combined <- read.csv(combined_path, stringsAsFactors = FALSE, check.names = FALSE)

dynamic_cols <- names(combined)[grepl("^(evapotrans|greenup_cycle[01]|precip_|temp_|snow_|npp_)", names(combined))]
if (!length(dynamic_cols)) {
  stop("No dynamic driver columns found in combined dataset: ", combined_path)
}

ref_key <- ref %>%
  transmute(
    LTER = norm_chr(LTER),
    Stream_Name = norm_chr(Stream_Name),
    Discharge_File_Name = norm_chr(Discharge_File_Name),
    Shapefile_Name = norm_chr(Shapefile_Name),
    Shapefile_Source = norm_chr(Shapefile_Source),
    Shapefile_CRS_EPSG = suppressWarnings(as.numeric(Shapefile_CRS_EPSG)),
    drainSqKm = suppressWarnings(as.numeric(drainSqKm))
  ) %>%
  distinct()

combined_key <- combined %>%
  mutate(has_dynamic_in_final = rowSums(!is.na(across(all_of(dynamic_cols)))) > 0) %>%
  transmute(
    LTER = norm_chr(LTER),
    Stream_Name = norm_chr(Stream_Name),
    Discharge_File_Name = norm_chr(Discharge_File_Name),
    Shapefile_Name = norm_chr(Shapefile_Name),
    has_dynamic_in_final
  ) %>%
  distinct()

fixed_crs_named <- tribble(
  ~LTER, ~Stream_Name, ~Discharge_File_Name, ~Shapefile_Name,
  "Amazon", "Rio Japura", "Rio Japura_Q", "Rio_Japura",
  "Amazon", "Rio Jurua", "Rio Jurua_Q", "Rio_Jurua",
  "Amazon", "Rio Jutai", "Rio Jutai_Q", "Rio_Jutai",
  "Amazon", "Amazon River at Santo Antonio do Ica", "Santo Antonio do Ica_Q", "Amazon_SantoAntonio",
  "Amazon", "Rio Ica", "Rio Ica_Q", "Rio_Ica",
  "Amazon", "Rio Negro", "Rio Negro_Q", "Rio_Negro",
  "Amazon", "Amazon River at Vargem Grande", "Vargem Grande_Q", "Amazon_VergemGrande",
  "Amazon", "Amazon River at Manacapuru", "Manacapuru_Q", "Amazon_Manacapuru",
  "Amazon", "Rio Madeira", "Rio Madeira_Q", "Rio_Madeira",
  "Amazon", "Rio Purus", "Rio Purus_Q", "Rio_Purus",
  "Amazon", "Amazon River at Itapeua", "Itapeua_Q", "Itapeua"
) %>%
  mutate(
    Region = "amazon",
    rerun_group = "fixed_crs_named_shapefile",
    preferred_action = "assign_known_crs_eckert_iv_then_reproject_and_rerun",
    reason = "Named shapefile exists but Amazon-family .prj is missing/undefined; repo reprojection log documents Eckert IV.",
    priority = 1L
  )

hydrosheds_candidates <- ref_key %>%
  filter(
    is.na(Shapefile_Name),
    (
      LTER == "WesternAustralia" |
      (!is.na(drainSqKm) & drainSqKm >= large_basin_km2) |
      str_detect(coalesce(Shapefile_Source, ""), regex("hydroshed", ignore_case = TRUE))
    )
  ) %>%
  mutate(
    Region = dplyr::case_when(
      LTER == "WesternAustralia" ~ "australia",
      LTER == "HYBAM" ~ "amazon",
      LTER == "Tanguro(Jankowski)" ~ "amazon",
      LTER == "USGS" ~ "north-america-usa",
      LTER == "Finnish Environmental Institute" ~ "scandinavia",
      TRUE ~ NA_character_
    ),
    rerun_group = "hydrosheds_candidate",
    preferred_action = "derive_or_recover_hydrosheds_polygon_then_rerun",
    reason = case_when(
      str_detect(coalesce(Shapefile_Source, ""), regex("hydroshed", ignore_case = TRUE)) &
        !is.na(drainSqKm) & drainSqKm >= large_basin_km2 ~
        "Reference table marks HydroSHEDs provenance and drainage area is large enough for a targeted HydroSHEDS rerun.",
      str_detect(coalesce(Shapefile_Source, ""), regex("hydroshed", ignore_case = TRUE)) ~
        "Reference table marks HydroSHEDs provenance but shapefile linkage is still blank.",
      TRUE ~
        "No shapefile name in reference table and drainage area is large enough for HydroSHEDS-based extraction."
    ),
    priority = 2L
  )

manifest <- bind_rows(fixed_crs_named, hydrosheds_candidates) %>%
  left_join(combined_key, by = c("LTER", "Stream_Name", "Discharge_File_Name", "Shapefile_Name")) %>%
  mutate(
    has_dynamic_in_final = ifelse(is.na(has_dynamic_in_final), FALSE, has_dynamic_in_final),
    rerun_needed = TRUE
  ) %>%
  arrange(priority, rerun_group, LTER, Stream_Name)

shared_polygons <- ref_key %>%
  filter(!is.na(Shapefile_Name)) %>%
  add_count(Shapefile_Name, name = "shared_rows") %>%
  filter(shared_rows > 1L) %>%
  arrange(Shapefile_Name, LTER, Stream_Name)

obidos_override <- tribble(
  ~LTER, ~Stream_Name, ~Discharge_File_Name, ~Shapefile_Name, ~shared_polygon, ~harmonization_role, ~canonical_site_id, ~note,
  "Amazon", "Amazon River at Obidos", "Obidos_Q", "Amazon_Obidos", "Amazon_Obidos", "shared_polygon_only", "GRO||GRO_Obidos_Q", "Polygon is shared, but this is not the canonical analytical Obidos row.",
  "GRO", "Obidos", "GRO_Obidos_Q", "Amazon_Obidos", "Amazon_Obidos", "canonical_site", "GRO||GRO_Obidos_Q", "Use this row as the canonical Obidos site in harmonization and release outputs.",
  "HYBAM", "Obidos", "Obidos_Q", NA_character_, "Amazon_Obidos", "drop_or_alias", "GRO||GRO_Obidos_Q", "Do not treat HYBAM Obidos as the canonical Obidos site."
)

date_tag <- format(Sys.Date(), "%Y%m%d")
manifest_path <- file.path(outdir, paste0("targeted_rerun_manifest_", date_tag, ".csv"))
subset_path <- file.path(outdir, paste0("targeted_rerun_subset_", date_tag, ".csv"))
focused_subset_path <- file.path(outdir, paste0("targeted_rerun_subset_amazon_hybam_westaus_", date_tag, ".csv"))
shared_path <- file.path(outdir, paste0("shared_polygon_sites_", date_tag, ".csv"))
obidos_path <- file.path(outdir, paste0("obidos_harmonization_override_", date_tag, ".csv"))

write.csv(manifest, manifest_path, row.names = FALSE, na = "")
write.csv(
  manifest %>%
    dplyr::select(LTER, Stream_Name, Shapefile_Name, Region) %>%
    dplyr::distinct(),
  subset_path,
  row.names = FALSE,
  na = ""
)
write.csv(
  manifest %>%
    dplyr::filter(LTER %in% c("Amazon", "HYBAM", "WesternAustralia")) %>%
    dplyr::select(LTER, Stream_Name, Shapefile_Name, Region) %>%
    dplyr::distinct(),
  focused_subset_path,
  row.names = FALSE,
  na = ""
)
write.csv(shared_polygons, shared_path, row.names = FALSE, na = "")
write.csv(obidos_override, obidos_path, row.names = FALSE, na = "")

cat("WROTE:", manifest_path, "\n")
cat("rows:", nrow(manifest), "\n")
cat("group_counts:\n")
print(table(manifest$rerun_group, useNA = "ifany"))
cat("\nWROTE:", subset_path, "\n")
cat("WROTE:", focused_subset_path, "\n")
cat("\nWROTE:", shared_path, "\n")
cat("shared_polygon_names:", dplyr::n_distinct(shared_polygons$Shapefile_Name), "\n")
cat("\nWROTE:", obidos_path, "\n")
