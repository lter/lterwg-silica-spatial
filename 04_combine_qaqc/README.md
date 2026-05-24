# 04_combine_qaqc

This stage does two related jobs:

1. rebuild one fresh local combined spatial table from the extracted driver CSVs
2. compare that fresh table against the legacy vetted combined file and write QA/QC review outputs

Main scripts:

- `00_qaqc_config.R`
  Main user-editable config for this stage.
- `01_import-and-qaqc.R`
  Entry script for combine and QA/QC.
- `combine_from_site_ref_local.R`
  Local rebuild of the combined extraction table from extracted driver files.
- `../tools/run_combine_and_harmonization_workflow.R`
  Recommended end-to-end runner for a new user. It rebuilds the local combined
  table, runs this QA/QC stage, then launches 05 harmonization.

Primary reference input for this stage:

- `<data_root>/silica-shapefiles/site-coordinates/silica-coords_RAW.xlsx`

This stage does not use the WRTDS site reference CSV as its main reference
table. It uses the site-coordinates workbook configured in `00_qaqc_config.R`.

Known LTER alias fixes that matter for old-versus-new matching are documented
in `tools/lter_name_aliases.csv`, and known stream-name aliases are documented
in `tools/stream_name_aliases.csv`. Both are applied in the combine helpers.

Supporting files:

- `templates/site_followup_notes_template.csv`
  Optional manual annotation template for site follow-up review.
- `examples/`
  Small reference outputs.

Typical order for a new user:

1. Restart the R session for a clean run.
2. Confirm your local spatial data root exists and contains:
   - `si-extracted-data/`
   - `silica-shapefiles/site-coordinates/silica-coords_RAW.xlsx`
3. Run:
   - `Rscript tools/run_combine_and_harmonization_workflow.R`
4. Inspect QA/QC outputs under `<data_root>/review/`.

If you need to run only this stage:

1. Rebuild the fresh local combined table:
   - `Rscript 04_combine_qaqc/combine_from_site_ref_local.R`
2. Run the QA/QC stage:
   - `Rscript 04_combine_qaqc/01_import-and-qaqc.R`

Notes:

- `01_import-and-qaqc.R` now auto-detects the newest candidate new combined
  file from either:
  - `<data_root>/si-extracted-data/all_data_extractions/`
  - `<data_root>/si-extracted-data/`
- The legacy comparison baseline is still controlled by
  `old_combined_filename` in `00_qaqc_config.R`.
