# ESOM Files

Current ESOM final CSVs are kept in this folder root.

- `ESOM_spatial_drivers_annual_20260608.csv`
  - ESOM spatial-only annual driver panel
  - 7,464 rows, 58 columns, 311 sites, 2002-2025
- `ESOM_final_harmonized_annual_20260608.csv`
  - ESOM annual DSi WRTDS plus final spatial drivers
  - 7,464 rows, 37 columns, 311 sites, 2002-2025

The ESOM final files are derived from the full dataset outputs. They include GEE/GLC land cover, `RBI`, `RCS`, annual snow, and monthly snow. They intentionally exclude NOx, P, Median N, and Median P.

WRTDS DSi values currently end in 2022. The spatial-driver panel itself extends through 2025.

Useful ESOM QAQC files in this folder:

- `ESOM_spatial_drivers_annual_summary_20260608.csv`
- `ESOM_final_harmonized_annual_summary_20260608.csv`
- `ESOM_missing_from_final_harmonized_annual_20260608.csv`
- `ESOM_duplicate_site_keys_final_harmonized_annual_20260608.csv`

Cross-dataset QAQC files are in `../audit-summaries/`.
