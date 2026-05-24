# Spatial Data Root Cleanup Notes

Target root:

- `/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/SiSyn/spatial-data-extractions`

## Current Working Paths

These are the folders to use first.

- Current final outputs:
  - `final-data/`
- Current full spatial dataset:
  - `final-data/full-dataset/`
- Current ESOM spatial outputs:
  - `final-data/esom/`
- Current audit summaries:
  - `final-data/audit-summaries/`
- Current driver patch artifacts:
  - `final-data/driver-patches/`
- GEE/GLC land-cover outputs:
  - `gee-glc-lulc-outputs/`
- AppEEARS extraction outputs:
  - `appeears-spatial-extractions/`
- Source shapefiles and site reference files:
  - `silica-shapefiles/site-coordinates/silica-coords_RAW.xlsx`
- Harmonization master inputs still used by scripts:
  - `spatial_data_harmonization/master_datasets/`

## Cleanup Already Done

- Box has a `final-data/` folder with full-dataset, ESOM, audit, and driver-patch subfolders.
- Box has a `gee-glc-lulc-outputs/` folder for raw GEE exports, upload targets, and merged checkpoints.
- Box has an `appeears-spatial-extractions/` folder with current extraction outputs separated from archived tests and superseded runs.
- GitHub `generated_outputs/rerun/` is split into active final run helpers, AppEEARS helpers, GEE targets, and archived reruns.
- GitHub `generated_outputs/final_combine/` keeps only the current useful combine checkpoints plus an archive folder.
- `.DS_Store` and `__pycache__` files were removed from the repo.

## Current Final Run State

- The year-fill extraction is still in progress.
- `generated_outputs/rerun/active-final-run/` now contains only the current targeted year-fill subset files, targeted evapo/snow manifests, the fast driver script, and the year-fill merge script.
- Completed pre-year-fill scripts were moved to `generated_outputs/rerun/archived-reruns/completed-final-v4-pre-year-fill-20260522-23/`.
- The slow broad year-fill script was archived so it is not run accidentally.

## Land-Cover Files

- `DSi_LULC_filled_interpolated_Simple_20260524_nor27.csv` is the current GEE/GLC land-cover master.
- `GLC_FCS30D_full_to_simple_class_translation.csv` records how detailed GLC classes were grouped into the simple classes.
- Both files are copied to:
  - `spatial_data_harmonization/master_datasets/`
  - `gee-glc-lulc-outputs/merged-master-checkpoints/`
  - `final-data/full-dataset/`

## Do Not Move Yet

Do not move or delete these until the year-fill merge and final audit pass:

- `generated_outputs/rerun/active-final-run/`
- `final-data/full-dataset/all-data_si-extract_4_20260523_final-extract-merge-v4-airtemp2025-domain-spatial-data-extractions.csv`
- `silica-shapefiles/site-coordinates/silica-coords_RAW.xlsx`
- `spatial_data_harmonization/master_datasets/DSi_LULC_filled_interpolated_Simple_20260524_nor27.csv`
- `/private/tmp/final_v4_year_fill_20260523/`

## Cleanup After Year-Fill Lands

1. Run the year-fill merge script and copy the merged final v4 output into `final-data/full-dataset/`.

2. Move the pre-year-fill combine checkpoint into `generated_outputs/final_combine/archive/` once the year-fill checkpoint is verified.

3. Copy final audit summaries into `final-data/audit-summaries/` and archive older audit folders that are now superseded.

4. Clean Aurora only after local final audit passes:
   - old run-output folders
   - one-off review CSVs
   - downloaded AppEEARS staging folders no longer needed

5. Remove large local AppEEARS staging folders under `/private/tmp/` only after Aurora and Box copies are verified.

6. Commit the code changes and generated helper structure once final v4 is verified.

## Current Commands To Finish

After evapo finishes:

```bash
bash generated_outputs/rerun/active-final-run/run_final_v4_year_fill_driver_fast_on_aurora_20260524.sh snow
```

After snow finishes:

```bash
bash generated_outputs/rerun/active-final-run/merge_final_v4_year_fill_patch_20260523.sh
```
