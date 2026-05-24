# Spatial Extraction Workflow

This repo is for building watershed polygons, extracting spatial drivers, and
checking the combined spatial dataset before it gets used downstream.

The folders are numbered in the order they usually get used.

## Folder Order

1. `01_pre_aurora_run`
   Decide what needs to be rerun before sending anything to Aurora.

2. `02_watershed_delineation`
   Build or update watershed polygons.

3. `03_spatial_extraction`
   Extract the spatial drivers from the watershed polygons.

4. `04_combine_qaqc`
   Rebuild the combined spatial table and check it against older outputs.

5. `05_harmonization`
   Format the checked spatial data for the final harmonized dataset.

## Planning A New Aurora Run

Start here if new sites need to be run or old sites need more years.

1. Make sure the Box data folder has the main inputs:
   - `silica-shapefiles/site-coordinates/`
   - `si-extracted-data/`

2. Check the setup if you are on a new machine:

```bash
Rscript 00_check-workflow-setup.R
```

3. Build the candidate list:

```bash
Rscript 01_pre_aurora_run/01_build_run_candidates.R
```

4. Review the newest approval file in:

```text
<data_root>/review/01_pre_aurora_run/02_approvals/
```

The important columns are:

- `hydrosheds_applicability`
- `Watershed_Source`
- `run_type`
- `target_years`
- `recommend_run_now`

5. Split the approved rows into Aurora subset files:

```bash
Rscript 01_pre_aurora_run/02_split_approved_runs.R
```

6. Print the copy/run commands for Aurora:

```bash
Rscript 01_pre_aurora_run/03_print_aurora_handoff.R
```

## Checking A Finished Run

Use this after Aurora finishes and the extracted driver files are back in the
Box data folder.

1. Open `04_combine_qaqc/00_qaqc_config.R`.

2. Check:
   - `data_root`
   - `old_combined_filename`, if you need to compare against a specific older file
   - `new_combined_filename`, if you need to force a specific new file

3. Restart R.

4. Run:

```bash
Rscript 04_combine_qaqc/01_import-and-qaqc.R
```

5. Look at the outputs under:
   - `<data_root>/review/glimpse/`
   - `<data_root>/review/harmonization/`
   - `<data_root>/review/year_extension/`

## Name Matching

Some site and stream names changed between older and newer files. These files
record the known name fixes:

- `tools/lter_name_aliases.csv`
- `tools/stream_name_aliases.csv`

The scripts use those tables when they compare old and new outputs.

## Optional Top-Level Helpers

These are useful on a new machine or if you want one command to run several
steps, but they are not required.

- `00_check-workflow-setup.R`
- `01_run_config.R`
- `02_run-workflow.R`

Most of the time, it is clearer to run the numbered folder scripts directly.

## Useful Notes

- Restart R before a fresh combine/check run.
- Review files are written under your data root, usually `<data_root>/review/`.
- The final spatial data files are not stored in GitHub. They live in Box.

## Related Repositories

- [lter/lterwg-silica-data](https://github.com/lter/lterwg-silica-data)
- [SwampThingPaul/SiSyn](https://github.com/SwampThingPaul/SiSyn)
- [lsethna/NCEAS_SiSyn_CQ](https://github.com/lsethna/NCEAS_SiSyn_CQ)
- [lter/lterwg-silica-spatial](https://github.com/lter/lterwg-silica-spatial)
- [njlyon0/lter_silica-high-latitude](https://github.com/njlyon0/lter_silica-high-latitude)
