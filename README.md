# LTER Silica Spatial Workflow

This is the original LTER working-group repository for the silica spatial workflow.

Future Google Earth Engine workflows should stay separate from this AppEEARS/NASA-based dataset.

## Current Data File

The current shared AppEEARS/NASA-era spatial file lives in Box:

```text
/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/spatial-data-files/appeears-nasa/all-data_si-extract_3_20260629.csv
```

## Repo Folders

- `02_watershed_delineation/`
  Build or update watershed polygons.

- `03_spatial_extraction/`
  Run the driver extractions from watershed polygons.
  Run lists live in `03_spatial_extraction/run-lists/`.
  R workflow wrappers live in `03_spatial_extraction/wrappers/`.
  The Aurora launcher is kept in `03_spatial_extraction/aurora/`.

- `04_combine_qaqc/`
  Rebuild the combined spatial table and check it against older outputs.

- `05_harmonization/`
  Format checked spatial data for the final harmonized dataset.

- `tools/`
  Shared helpers, workflow entry points, and the small final QA/QC set.

`generated_outputs/` is disposable local output, not workflow code. Do not use
old generated shell scripts as documentation for Aurora or AppEEARS runs.

## Usual Commands

Build the run-list review files:

```bash
Rscript 03_spatial_extraction/run-lists/01_build_run_candidates.R
Rscript 03_spatial_extraction/run-lists/02_split_approved_runs.R
Rscript 03_spatial_extraction/run-lists/03_print_aurora_handoff.R
```

Keep this step because it records why sites were sent as full-record runs versus
new-year update runs.

Rebuild and check the combined spatial table, then run harmonization:

```bash
Rscript tools/run_combine_and_harmonization_workflow.R
```

Upload the shared wide spatial CSV to the data release Google Drive folder:

First authenticate once from R or RStudio:

```r
googledrive::drive_auth(email = "bushsi@oregonstate.edu")
```

Then run the combine script with Drive upload enabled:

```bash
SILICA_OUTPUT_FILE=/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions/spatial-data-files/appeears-nasa/all-data_si-extract_3_20260629.csv \
SILICA_UPLOAD_TO_GOOGLE_DRIVE=TRUE \
SILICA_GOOGLE_DRIVE_FOLDER_ID=1zF_Itljwn0bUWSTHEkwkMDyNOiKPXRF1 \
SILICA_GOOGLE_DRIVE_ACCOUNT=bushsi@oregonstate.edu \
Rscript 04_combine_qaqc/combine-spatial-data.R
```

Full rebuild after new extractions:

```bash
Rscript tools/run_combine_and_harmonization_workflow.R
```

## Notes

- Name cleanup tables live in `tools/name_keys.R`.
- The Aurora shell launcher is `03_spatial_extraction/aurora/run-spatial-extraction-aurora.sh`.
- Do not put generated CSVs, plots, or run debris in this repo.

## Related Repositories

- [lter/lterwg-silica-data](https://github.com/lter/lterwg-silica-data)
- [SwampThingPaul/SiSyn](https://github.com/SwampThingPaul/SiSyn)
- [lsethna/NCEAS_SiSyn_CQ](https://github.com/lsethna/NCEAS_SiSyn_CQ)
- [lter/lterwg-silica-spatial](https://github.com/lter/lterwg-silica-spatial)
- [njlyon0/lter_silica-high-latitude](https://github.com/njlyon0/lter_silica-high-latitude)
