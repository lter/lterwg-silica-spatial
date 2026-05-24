# Pre-Aurora Run Workflow

This folder holds the review and planning steps that should happen **before**
starting another Aurora extraction run.

The goal is to answer four questions in a predictable order:

1. Which sites should be rerun?
2. Does HydroSHEDS need to be applied to each site, or does the site already have a usable shapefile?
3. Should each site be a `full_record` or `update_years` rerun?
4. Which subset files should be copied to Aurora?

## Script Order

### 1. Build the pre-Aurora candidate files

Run:

```bash
Rscript 01_pre_aurora_run/01_build_run_candidates.R
```

What it does:

- builds a strict list of truly new / never-successful HydroSHEDS candidates
- carries forward the Canada and MD HydroSHEDS rerun sites that need newer years
- adds new shapefile-backed sites that should run without HydroSHEDS
- writes a mixed approval file with a `run_type` column
- writes `hydrosheds_applicability` so you can see whether HydroSHEDS is required
- writes a holdout review file for sites that do **not** meet the current rule

Main outputs:

- `review/01_pre_aurora_run/02_approvals/pre_aurora_approval_mixed_<date>.csv`
- `review/01_pre_aurora_run/02_approvals/pre_aurora_holdouts_review_<date>.csv`

Primary reference inputs for this stage:

- site reference table:
  `<data_root>/master/Site_Reference_Table - WRTDS_Reference_Table_LTER_V3.csv`
- legacy combined table:
  `all-data_si-extract_2_20250325.csv`
- current baseline combined table:
  `all-data_si-extract_3_20260323.csv`

### 2. Review and, if needed, edit the mixed approval file

This is the manual approval step.

The mixed approval file is the handoff contract for the rest of the workflow.
Each row should already be labeled as:

- `hydrosheds_applicability = required` or `not_needed_named_shapefile_present`
- `Watershed_Source = hydrosheds` or `artisanal`
- `run_type = full_record`
- `run_type = update_years`

### 3. Split the approved file into Aurora-ready subsets

Run:

```bash
Rscript 01_pre_aurora_run/02_split_approved_runs.R
```

What it does:

- reads the latest mixed approval CSV
- writes one `full_record` subset CSV
- writes one `update_years` subset CSV

Main outputs:

- `review/01_pre_aurora_run/03_aurora_subsets/aurora_handoff_full_record_subset_<date>.csv`
- `review/01_pre_aurora_run/03_aurora_subsets/aurora_handoff_update_years_subset_<date>.csv`

### 4. Print the Aurora handoff commands

Run:

```bash
Rscript 01_pre_aurora_run/03_print_aurora_handoff.R
```

What it does:

- prints the suggested `scp` and `ssh` commands for Aurora
- keeps the full-record and update-years runs separate

## Rule Summary

### `update_years`

Use for sites that were already part of a previous successful extraction and
now only need newer target years.

### `full_record`

Use for sites that are new, previously unsuccessful, or otherwise need a full
rerun from the start of record through the agreed legacy endpoint.

For the current legacy workflow, `full_record` means `2002-2024`.
This uses the first full year shared by the legacy dynamic drivers currently
in scope for the rerun workflow. Snow can include a partial 2001 year, but
2002 is the first full year and is the clean default for full-record reruns.

### `hydrosheds_applicability`

Use this column to decide whether HydroSHEDS should be applied:

- `required`
  The site should run through HydroSHEDS.
- `not_needed_named_shapefile_present`
  The site already has a named shapefile and should run without HydroSHEDS.

## Notes

- The extraction workflow itself lives under `03_spatial_extraction/` and `tools/`.
- This folder is only for deciding **what** to run and **how** to package it
  for Aurora.
- HydroSHEDS rerun planning artifacts now live here instead of in a generic
  top-level QA folder.
- This stage uses the WRTDS site reference CSV, not `silica-coords_RAW.xlsx`.
- Known V2-to-V3 LTER label aliases are documented in `tools/lter_name_aliases.csv`
  and applied during key-building.
- Known stream-name aliases are documented in `tools/stream_name_aliases.csv`
  and applied during key-building.

## Folder Layout

- `01_candidates`
  Strict candidate lists produced from the reference table and combined outputs.
- `02_approvals`
  Human-reviewed approval files. This is the main handoff point before Aurora.
- `03_aurora_subsets`
  Final subset CSVs to copy to Aurora.
- `04_reports`
  Small summaries and bookkeeping outputs.
