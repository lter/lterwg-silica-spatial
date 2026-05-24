# 05_harmonization

This stage builds harmonized driver tables after the combined spatial dataset
has been reviewed.

Main scripts:

- `00_harmonization_config.R`
- `00_harmonization_functions.R`
- `01_build-harmonized-drivers.R`
- `../tools/run_combine_and_harmonization_workflow.R`
  Recommended end-to-end runner for a new user.

Reference inputs used here:

- `Site_Reference_Table - WRTDS_Reference_Table_LTER_V2.csv`
  from the harmonization input directory configured in `00_harmonization_config.R`
- the newest vetted combined spatial dataset from one of:
  - `<data_root>/review/harmonization/combined-spatial-dataset_*.csv`
  - `<data_root>/si-extracted-data/all_data_extractions/all-data_si-extract_[34]_*.csv`
  - `<data_root>/si-extracted-data/all-data_si-extract_3_*.csv`

Run this stage only after `04_combine_qaqc` outputs are vetted.

Typical order:

1. Preferred:
   - `Rscript tools/run_combine_and_harmonization_workflow.R`
2. Harmonization only:
   - `Rscript 05_harmonization/01_build-harmonized-drivers.R`

Notes:

- `01_build-harmonized-drivers.R` is now source-safe and does not clear the
  session.
- If you need to force a specific vetted combined file, set:
  - `SILICA_HARMONIZATION_COMBINED_FILE=/abs/path/to/combined.csv`
