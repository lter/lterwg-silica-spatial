# Full Dataset and ESOM Harmonization

Current authoritative output and QAQC notes are in
`FINAL_QAQC_NOTES_20260610.md`. Some older paths below are retained only as
historical setup notes from the June 7 handoff.

Prepared from `lterwg-silica-spatial` after the final 20260607 spatial/harmonization run.

## Status

This bundle contains the current reusable harmonization code for both products:

1. The full harmonized spatial-driver dataset.
2. The ESOM harmonized dataset derived from the full harmonized spatial-driver dataset.

Core full-dataset harmonization files:

- `Step1_Harmonization/00_harmonization_config.R`
- `Step1_Harmonization/00_harmonization_functions.R`
- `Step1_Harmonization/01_build-harmonized-drivers.R`

ESOM subset/join builder:

- `tools/final_dataset/build_esom_harmonized_from_full_dataset.R`

The archived `silica-spatial-controls-grl` repo is not the target. That repo belongs to the published GRL article record and should not receive this workflow.

The likely target is a new or existing repository under the Global River Chemistry organization. Install only after choosing that target repository.

Suggested target layout:

```text
<target-repo>/Step1_Harmonization/current_full_and_esom_spatial_harmonization_20260608
<target-repo>/tools/final_dataset/build_esom_harmonized_from_full_dataset.R
```

The full dataset is the primary product. ESOM is a downstream filtered/joined output built from the full harmonized dataset.

## Final Outputs Used

Full harmonized spatial drivers:

```text
/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/review/harmonization/harmonized-spatial-drivers_20260607.csv
```

Annual site-year drivers:

```text
/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/review/harmonization/harmonized-spatial-drivers-annual_20260607.csv
```

Site-average drivers:

```text
/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/review/harmonization/harmonized-spatial-drivers-site-averages_20260607.csv
```

ESOM harmonized output:

```text
/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/final-data/esom/ESOM_final_combined_spatial_data_20260607.csv
```

## Variables Covered

The final harmonized wide-site table includes:

- Site identifiers: `LTER`, `Stream_Name`, `Discharge_File_Name`, `Shapefile_Name`, `Stream_ID`
- Annual and monthly snow: `snow_*_num_days`, `snow_*_max_prop_area`, monthly snow days/proportion
- Greenup phenology: `greenup_cycle0_*MMDD`, `greenup_cycle1_*MMDD`
- Evapotranspiration: annual and monthly `evapotrans_*_kg_m2`
- NPP: annual `npp_*_kgC_m2_year`
- Temperature: annual and monthly `temp_*_degC`
- Precipitation: annual and monthly `precip_*_mm_per_day`
- Topography: elevation and basin slope summaries
- Land cover: `major_land`, `land_*`
- Lithology: `major_rock`, `rocks_*`
- Permafrost: `permafrost_median_m`, `permafrost_mean_m`, `permafrost_min_m`, `permafrost_max_m`
- Soil: `major_soil`, `soil_*`
- Discharge summaries: `n_q_years`, `q_start_year`, `q_end_year`, `mean_q`, `med_q`, `sd_q`, `min_Q`, `max_Q`, `q_95`, `q_5`, `CV_Q`
- Flow-regime metrics: `n_recession_days`, `recession_slope`, `RCS`, `total_discharge`, `total_change`, `RBI`
- Climate context: `KG_class`, `KG_name`, `max_daylength`

The annual site-year table additionally carries annual `Q`, `RBI`, `RCS`, `recession_slope`, `total_discharge`, and `total_change`.

The site-average table carries site-level means/counts of annual drivers, including annual mean `RBI`, `RCS`, `recession_slope`, `Q`, snow, greenup DOY, temperature, precipitation, evapotranspiration, and NPP.

## Intentionally Excluded

- Median N and Median P are not included, per the current manuscript/data decision.
- The older Catalina/Jemez formatting step is not included here. The final spatial data already resolved those stream identifiers upstream.

## Variable Manifests

Generated manifests in this bundle:

- `final_harmonized_variable_manifest_20260607.csv`
- `final_harmonized_variable_family_summary_20260607.csv`

Use those to compare against the target repository's expected variable list before wiring this into its main workflow entry point.
