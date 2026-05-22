# Spatial Data Root Cleanup Plan

Target root:

- `/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial_data_extractions`

## Current Canonical Workflow Paths

These are the paths the repo should treat as canonical now.

- Extracted driver CSVs:
  - `si-extracted-data/`
- Historical combined archives:
  - `si-extracted-data/all_data_extractions/`
- Site reference workbook:
  - `silica-shapefiles/site-coordinates/silica-coords_RAW.xlsx`
- Review outputs:
  - `review/`
- Harmonization master inputs:
  - `spatial_data_harmonization/master_datasets/`
- Other master inputs:
  - `master/`

## What Is Messy Right Now

- Fresh local rebuilds are written directly into `si-extracted-data/`, while older
  combined files live under `si-extracted-data/all_data_extractions/`.
- Active rerun patch CSVs are mixed into the same folder as stable driver CSVs.
- Review outputs exist both in current folders and in `_archive_current_workflow_cleanup_20260411/`.
- There are multiple historical folders with overlapping purpose:
  - `checks/`
  - `data_checking/`
  - `harmonization_current/`
  - `qa/`
  - archived cleanup folders

## Safe Changes Already Made In Repo

- `04_combine_qaqc/00_qaqc_config.R`
  - now auto-detects the newest new combined file from either:
    - `si-extracted-data/all_data_extractions/`
    - `si-extracted-data/`
- `05_harmonization/00_harmonization_config.R`
  - now prefers the vetted combined file under:
    - `review/harmonization/combined-spatial-dataset_*.csv`
  - then falls back to older combined locations
- `tools/run_combine_and_harmonization_workflow.R`
  - provides a single end-to-end runner

These changes mean the workflow is less fragile even before physical cleanup.

## Do Not Move Yet

Do not move or delete any of these until the current rerun, recombine, and
harmonization pass are complete:

- `si-extracted-data/si-extract_*20260518_aurora-final-unresolved-hydrosheds-rerun-20260518.csv`
- the newest `all-data_si-extract_3_*.csv`
- `review/harmonization/*.csv`
- `silica-shapefiles/site-coordinates/silica-coords_RAW.xlsx`
- `spatial_data_harmonization/master_datasets/DSi_LULC_filled_interpolated_Simple.csv`

## Cleanup To Do After Current Workflow Lands

1. Archive stale patch CSVs out of `si-extracted-data/`
   - move dated rerun patch files into a subfolder such as:
   - `si-extracted-data/extraction_exports/patch_runs_archive/`

2. Separate stable combined outputs from transient rebuilds
   - keep vetted combined outputs under:
   - `review/harmonization/`
   - optionally copy milestone combined outputs into:
   - `si-extracted-data/all_data_extractions/`

3. Leave only active driver files at top level in `si-extracted-data/`
   - legacy base driver CSVs
   - newest active rerun patch files only if still needed

4. Consolidate historical review folders
   - review old contents of:
   - `checks/`
   - `data_checking/`
   - `harmonization_current/`
   - `qa/`
   - move genuinely obsolete outputs into a dated archive folder

5. Permanently patch the site reference workbook inputs
   - add the missing HydroSHEDS `Shapefile_Name` values for:
   - `Fazenda Vista Alegre`
   - `Labrea`
   - `Tabatinga`
   - `S65C`
   - `NOR27`
   - `North Sylamore`

## Recommended User Workflow

After the current rerun is complete:

1. Pull patch CSVs into `si-extracted-data/`
2. Run:
   - `Rscript tools/run_combine_and_harmonization_workflow.R`
3. Review:
   - `review/harmonization/`
   - `review/year_extension/`
4. Only then do physical cleanup/archive moves
