# Final Spatial Data

Use this folder first when looking for the current final spatial and harmonized outputs.

## Current Final CSVs

Full dataset:

- `full-dataset/final_full_spatial_drivers_annual_20260608.csv`
  - spatial-only annual driver panel
  - 23,424 rows, 58 columns, 976 sites, 2002-2025
- `full-dataset/final_full_harmonized_annual_20260608.csv`
  - annual DSi WRTDS plus final spatial drivers
  - 23,424 rows, 37 columns, 976 sites, 2002-2025
- `full-dataset/final_full_harmonized_annual_raw_DSi_20260608.csv`
  - companion annual file with raw DSi summaries
  - 23,424 rows, 41 columns, 976 sites, 2002-2025

ESOM:

- `esom/ESOM_spatial_drivers_annual_20260608.csv`
  - ESOM spatial-only annual driver panel
  - 7,464 rows, 58 columns, 311 sites, 2002-2025
- `esom/ESOM_final_harmonized_annual_20260608.csv`
  - ESOM annual DSi WRTDS plus final spatial drivers
  - 7,464 rows, 37 columns, 311 sites, 2002-2025

## Included

- GEE/GLC land-cover classes for both full and ESOM outputs.
- `RBI` and `RCS`.
- Full snow variables: annual `snow_cover`, annual `snow_num_days`, monthly `snow_*_avg_prop_area`, and monthly `snow_*_num_days`.
- `drainage_area_source`.
- No NOx, P, Median N, or Median P.

## QAQC

Main caveat register:

- `audit-summaries/final_known_caveats_and_followups_20260610.csv`

Main coverage/audit tables:

- `audit-summaries/final_full_spatial_drivers_variable_coverage_20260608.csv`
- `audit-summaries/final_full_spatial_driver_coverage_by_site_20260608.csv`
- `audit-summaries/final_full_spatial_no_core_driver_sites_20260608.csv`
- `audit-summaries/final_full_spatial_drivers_drainage_area_source_summary_20260608.csv`
- `audit-summaries/final_full_harmonized_annual_FNConc_missingness_by_year_20260608.csv`
- `audit-summaries/final_full_harmonized_annual_FNConc_missingness_by_site_20260608.csv`
- `audit-summaries/snow_permafrost_latitude_plausibility_summary_20260608.csv`
- `audit-summaries/snow_permafrost_latitude_plausibility_flags_20260608.csv`

## Important Caveats

- The annual panel goes through 2025. WRTDS DSi values currently end in 2022; raw annual DSi values currently end in 2023.
- Drainage area remains missing for 404 full-dataset sites.
- 426 full-dataset sites have no core extracted spatial drivers. This is documented in `audit-summaries/final_full_spatial_no_core_driver_sites_20260608.csv`.
- MCM has 24 sites in the full panel; 14 have no core extracted spatial drivers and 9 are missing drainage area.
- Snow/permafrost latitude flags were written for review. Values were not blindly zeroed because outlet latitude can misclassify large upstream basins.
- Keep `/private/tmp/appeears_downloads_20260526`, `/private/tmp/appeears_product_rasters_flat_20260526_final_remaining`, and related final dynamic temp folders until final cleanup is approved.
