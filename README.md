# Spatial Extraction Workflow

This repo is now organized by workflow stage.

## Stage Order

1. [01_pre_aurora_run](</Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/01_pre_aurora_run>)
   Decide which sites need a rerun, whether HydroSHEDS applies to each site, which years to request, and whether each site should be `full_record` or `update_years`.

2. [02_watershed_delineation](</Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/02_watershed_delineation>)
   Build or refresh the watershed layers used by extraction.

3. [03_spatial_extraction](</Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/03_spatial_extraction>)
   Run the actual driver extraction locally or on Aurora.

4. [04_combine_qaqc](</Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/04_combine_qaqc>)
   Combine old and new outputs, inspect `glimpse()` checkpoints, and review QA/QC summaries.

5. [05_harmonization](</Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/05_harmonization>)
   Build harmonized driver tables after the combined spatial table has been vetted.

## Exact Steps For A New User

### A. Plan a new Aurora run

1. Make sure your data root exists and contains at least:
   - `silica-shapefiles/site-coordinates/`
   - `si-extracted-data/`
2. Optional: run the setup checker:
```bash
Rscript 00_check-workflow-setup.R
```
3. Build the mixed pre-Aurora candidate file:
```bash
Rscript 01_pre_aurora_run/01_build_run_candidates.R
```
4. Open the newest file under:
   `<data_root>/review/01_pre_aurora_run/02_approvals/`
   Review these columns:
   - `hydrosheds_applicability`
   - `Watershed_Source`
   - `run_type`
   - `target_years`
   - `recommend_run_now`
5. Split the approved file into Aurora-ready subsets:
```bash
Rscript 01_pre_aurora_run/02_split_approved_runs.R
```
6. Print the Aurora handoff commands:
```bash
Rscript 01_pre_aurora_run/03_print_aurora_handoff.R
```

### B. Review a finished extraction run

1. Open [04_combine_qaqc/00_qaqc_config.R](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/04_combine_qaqc/00_qaqc_config.R:1)
2. Edit:
   - `data_root`
   - optionally `old_combined_filename`
   - optionally `new_combined_filename`
3. Restart the R session.
4. Run:
```bash
Rscript 04_combine_qaqc/01_import-and-qaqc.R
```
5. Inspect outputs under:
   - `<data_root>/review/glimpse/`
   - `<data_root>/review/harmonization/`
   - `<data_root>/review/year_extension/`

That pre-Aurora review is not HydroSHEDS-only anymore. The mixed approval file
also records when a site already has a usable shapefile and therefore should
run without HydroSHEDS.

Known V2-to-V3 LTER label aliases are documented in:
- [tools/lter_name_aliases.csv](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/tools/lter_name_aliases.csv:1)
- [tools/stream_name_aliases.csv](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/tools/stream_name_aliases.csv:1)

These aliases are also applied in the workflow code so old and new files can
be matched more safely during planning and combine steps.

## Optional Wrappers

These top-level files are convenience wrappers, not the canonical workflow:

- [01_run_config.R](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/01_run_config.R:1)
- [02_run-workflow.R](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/02_run-workflow.R:1)
- [00_check-workflow-setup.R](/Users/sidneybush/Documents/GitHub/lterwg-silica-spatial/00_check-workflow-setup.R:1)

Use them if you want one configurable entrypoint. Otherwise, run the numbered stage scripts directly.

You do not need these wrapper files to complete the workflow:

- `00_check-workflow-setup.R`
  Optional diagnostic for a new machine.
- `01_run_config.R`
  Optional single-file config wrapper.
- `02_run-workflow.R`
  Optional single-command launcher that reads `01_run_config.R`.

## Main Stage Contents

### `01_pre_aurora_run`

- `00_pre_aurora_paths.R`
- `01_build_run_candidates.R`
- `02_split_approved_runs.R`
- `03_print_aurora_handoff.R`

### `02_watershed_delineation`

- `01_wrangle-artisanal-watersheds.R`
- `02_wrangle-hydrosheds.R`
- `03_combine-artisanal-hydrosheds.R`

### `03_spatial_extraction`

- `modes/`
- `extraction_scripts/`
- `aurora/`
- `examples/subset_file_template.csv`

### `04_combine_qaqc`

- `00_qaqc_config.R`
- `01_import-and-qaqc.R`
- `build-combined-spatial-dataset.R`
- `qaqc-new-vs-old.R`
- `qaqc-reasonable-new-values.R`
- `combine_from_site_ref_local.R`
- `templates/site_followup_notes_template.csv`
- `examples/`

### `05_harmonization`

- `00_harmonization_config.R`
- `00_harmonization_functions.R`
- `01_build-harmonized-drivers.R`

## Notes For Non-Coders

- Restart the R session before a fresh QA/QC run instead of using `rm(list = ls())`.
- The top-level numbered folders reflect the intended order of operations.
- Review files are written under your data root, usually:
  `<data_root>/review/`
- `glimpse()` checkpoints from the QA/QC stage are written under:
  `<data_root>/review/glimpse/`

## Related Repositories

- [lter/lterwg-silica-data](https://github.com/lter/lterwg-silica-data)
- [SwampThingPaul/SiSyn](https://github.com/SwampThingPaul/SiSyn)
- [lsethna/NCEAS_SiSyn_CQ](https://github.com/lsethna/NCEAS_SiSyn_CQ)
- [lter/lterwg-silica-spatial](https://github.com/lter/lterwg-silica-spatial)
- [njlyon0/lter_silica-high-latitude](https://github.com/njlyon0/lter_silica-high-latitude)
