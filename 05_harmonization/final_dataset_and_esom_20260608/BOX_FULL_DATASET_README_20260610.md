# Full Dataset Files

Current full-dataset final CSVs are kept in this folder root.

- `final_full_spatial_drivers_annual_20260608.csv`
  - spatial-only annual driver panel
  - 23,424 rows, 58 columns, 976 sites, 2002-2025
- `final_full_harmonized_annual_20260608.csv`
  - annual DSi WRTDS plus final spatial drivers
  - 23,424 rows, 37 columns, 976 sites, 2002-2025
- `final_full_harmonized_annual_raw_DSi_20260608.csv`
  - companion annual file with raw DSi summaries
  - 23,424 rows, 41 columns, 976 sites, 2002-2025

The final files include GEE/GLC land cover, `RBI`, `RCS`, annual snow, and monthly snow. They intentionally exclude NOx, P, Median N, and Median P.

WRTDS DSi values currently end in 2022. Raw annual DSi values currently end in 2023. The spatial-driver panel itself extends through 2025.

Useful supporting files retained here:

- `GLC_FCS30D_full_to_simple_class_translation.csv`
- `DSi_LULC_filled_interpolated_Simple_20260524_nor27.csv`

Current QAQC files are in `../audit-summaries/`.
