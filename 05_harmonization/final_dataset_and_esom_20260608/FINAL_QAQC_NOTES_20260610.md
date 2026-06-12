# Final Dataset and ESOM QAQC Notes

Prepared after the June 2026 final spatial extraction and harmonization pass.

No remote push has been made from this continuation. Push only after explicit user approval.

## Current Final Outputs

Box final-data root:

```text
/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/final-data
```

Full dataset:

- `full-dataset/final_full_spatial_drivers_annual_20260608.csv`
  - Spatial-only annual driver panel.
  - 23,424 rows, 58 columns, 976 sites, years 2002-2025.
- `full-dataset/final_full_harmonized_annual_20260608.csv`
  - Annual DSi WRTDS plus final spatial drivers.
  - 23,424 rows, 37 columns, 976 sites, years 2002-2025.
- `full-dataset/final_full_harmonized_annual_raw_DSi_20260608.csv`
  - Companion full annual file with raw annual DSi summaries.
  - 23,424 rows, 41 columns, 976 sites, years 2002-2025.

ESOM:

- `esom/ESOM_spatial_drivers_annual_20260608.csv`
  - ESOM spatial-only annual driver panel.
  - 7,464 rows, 58 columns, 311 sites, years 2002-2025.
- `esom/ESOM_final_harmonized_annual_20260608.csv`
  - ESOM annual DSi WRTDS plus final spatial drivers.
  - 7,464 rows, 37 columns, 311 sites, years 2002-2025.

## Included Variables

- GEE/GLC land-cover classes for both full and ESOM files, using `full-dataset/GLC_FCS30D_full_to_simple_class_translation.csv`.
- Annual `RBI` and `RCS`. The final output column is `RCS`; the older `recession_slope` name is not retained in the final CSVs.
- Full snow representation in the spatial-driver files:
  - annual `snow_cover`
  - annual `snow_num_days`
  - monthly `snow_*_avg_prop_area`
  - monthly `snow_*_num_days`
- Drainage-area source tracking in `drainage_area_source`.
- No NOx, P, Median N, or Median P in the final full or ESOM files.

## Source Coverage Notes

The output panel extends through 2025, but chemistry and some source products have their own nonmissing coverage limits.

- WRTDS DSi (`FNConc`, `FNYield`, `GenConc`, `GenYield`) has 6,506 nonmissing rows and currently ends in 2022.
- Raw annual DSi companion columns (`raw_DSi_*`) have 8,851 nonmissing rows and currently end in 2023.
- Spatial drivers have nonmissing 2025 values for snow, NPP, evapotranspiration, permafrost, RBI, RCS, precipitation, temperature, and drainage area somewhere in the full dataset.
- Greenup currently has nonmissing values through 2024.

## QAQC Files

Core output summaries:

- `audit-summaries/final_full_spatial_drivers_annual_summary_20260608.csv`
- `esom/ESOM_spatial_drivers_annual_summary_20260608.csv`
- `audit-summaries/final_full_harmonized_annual_summary_20260608.csv`
- `esom/ESOM_final_harmonized_annual_summary_20260608.csv`

Coverage and caveats:

- `audit-summaries/final_full_spatial_drivers_variable_coverage_20260608.csv`
- `audit-summaries/final_full_spatial_driver_coverage_by_site_20260608.csv`
- `audit-summaries/final_full_spatial_no_core_driver_sites_20260608.csv`
- `audit-summaries/final_full_spatial_no_core_driver_sites_by_lter_20260608.csv`
- `audit-summaries/final_known_caveats_and_followups_20260610.csv`

Drainage area:

- `audit-summaries/final_full_spatial_drivers_drainage_area_source_summary_20260608.csv`
- `audit-summaries/final_full_spatial_drivers_drainage_area_by_site_20260608.csv`
- `audit-summaries/final_full_spatial_drivers_missing_drainage_area_sites_20260608.csv`

Chemistry coverage:

- `audit-summaries/final_full_harmonized_annual_FNConc_missingness_by_year_20260608.csv`
- `audit-summaries/final_full_harmonized_annual_FNConc_missingness_by_site_20260608.csv`
- `audit-summaries/final_full_harmonized_annual_unmatched_WRTDS_DSi_sites_20260608.csv`
- `audit-summaries/final_full_harmonized_annual_raw_DSi_variable_coverage_20260608.csv`

Snow/permafrost plausibility:

- `audit-summaries/snow_permafrost_latitude_plausibility_summary_20260608.csv`
- `audit-summaries/snow_permafrost_latitude_plausibility_flags_20260608.csv`
- `audit-summaries/snow_permafrost_latitude_plausibility_by_site_20260608.csv`

March 2025 consistency checks:

- `generated_outputs/review/final_consistency_gap_20260607/final_gap_targets_vs_march2025_audit_20260607.csv`
- `generated_outputs/review/final_consistency_gap_20260607/final_gap_targets_vs_march2025_audit_summary_20260607.csv`

## Documented Caveats

- Drainage area remains missing for 404 full-dataset sites. The source summary is 538 sites from the site reference table, 22 from combined watershed polygons, 12 from individual shapefile polygons, and 404 missing.
- 426 full-dataset sites have no core extracted spatial drivers. The largest groups are PIE, ARC, Seine, BcCZO, MCM, WesternAustralia, ColoradoAlpine, LMP, Tanguro, and BNZ.
- MCM has 24 sites in the full panel. Fourteen have no core extracted spatial drivers and nine are missing drainage area.
- The latitude plausibility audit flags possible low-latitude snow/permafrost values, but the workflow does not blindly zero them because outlet latitude can misclassify large upstream basins. Puerto Rico/LUQ snow and permafrost values are zero where values exist.
- Canada 2023 evapotranspiration and most 2025 snow gap values are within March 2025 history. GRO Mackenzie and Yenisey 2025 maximum snow proportion are below March-history minima and are documented for review.
- Temporary AppEEARS and dynamic extraction folders should stay in `/private/tmp` until final cleanup is approved.

## Reproducible Entry Points

- `tools/build_final_harmonized_through_2025.R`
- `tools/build_final_full_harmonized_from_march.R`
- `tools/audit_fnconc_missingness.R`
- `tools/audit_snow_permafrost_latitude_plausibility.R`
