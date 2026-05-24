# Pre-Aurora Run Planning

Use this folder before starting another Aurora extraction run.

The point is to decide:

- which sites need to be run
- whether they need HydroSHEDS or already have a usable shapefile
- whether they need the full record or just newer years
- which subset CSVs should be copied to Aurora

## 1. Build The Review Files

Run:

```bash
Rscript 01_pre_aurora_run/01_build_run_candidates.R
```

This makes the files you review before running anything on Aurora. It includes:

- brand-new HydroSHEDS candidates
- Canada and MD sites that already ran but need newer years
- sites that already have shapefiles and do not need HydroSHEDS
- holdout rows that do not meet the current rules

Main outputs:

- `review/01_pre_aurora_run/02_approvals/pre_aurora_approval_mixed_<date>.csv`
- `review/01_pre_aurora_run/02_approvals/pre_aurora_holdouts_review_<date>.csv`

Main inputs:

- `<data_root>/master/Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv`
- `all-data_si-extract_2_20250325.csv`
- `all-data_si-extract_3_20260323.csv`

## 2. Review The Approval File

Open the newest mixed approval file and check the run columns.

Important columns:

- `hydrosheds_applicability`
- `Watershed_Source`
- `run_type`
- `target_years`
- `recommend_run_now`

The `run_type` column should say either:

- `full_record`
- `update_years`

## 3. Split The Approved Rows

Run:

```bash
Rscript 01_pre_aurora_run/02_split_approved_runs.R
```

This writes separate subset files for:

- full-record runs
- update-year runs

Main outputs:

- `review/01_pre_aurora_run/03_aurora_subsets/aurora_handoff_full_record_subset_<date>.csv`
- `review/01_pre_aurora_run/03_aurora_subsets/aurora_handoff_update_years_subset_<date>.csv`

## 4. Print The Aurora Commands

Run:

```bash
Rscript 01_pre_aurora_run/03_print_aurora_handoff.R
```

This prints the suggested `scp` and `ssh` commands.

## Run Types

Use `update_years` when a site already has a good extraction and only needs
newer years.

Use `full_record` when a site is new, failed before, or needs a full rerun.
For the current legacy setup, full record starts in 2002.

## HydroSHEDS Column

Use `hydrosheds_applicability` to decide whether HydroSHEDS should be used:

- `required`: run the site through HydroSHEDS.
- `not_needed_named_shapefile_present`: use the shapefile that already exists.

## Notes

- This folder is only for deciding what to run.
- The actual extraction scripts are in `03_spatial_extraction/` and `tools/`.
- This step uses the WRTDS site reference CSV, not `silica-coords_RAW.xlsx`.
- Known LTER name fixes are in `tools/lter_name_aliases.csv`.
- Known stream-name fixes are in `tools/stream_name_aliases.csv`.

## Output Folders

- `01_candidates`: first-pass candidate lists.
- `02_approvals`: files to review before Aurora.
- `03_aurora_subsets`: final subset CSVs to copy to Aurora.
- `04_reports`: small summary files.
