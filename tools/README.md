# Tools Directory

This folder keeps active shared workflow entry points at the top level and moves one-off or late-stage scripts into subfolders.

## Top-Level Files

- `workflow_paths.R`: shared path helpers used across the workflow.
- `subset_and_output_helpers.R`: shared subset/output helpers used by extraction and review scripts.
- `hydrosheds_custom_fxns.R`: HydroSHEDS helper functions.
- `lter_name_aliases.csv` and `stream_name_aliases.csv`: name cleanup lookup tables.
- `run_targeted_subset_workflow.R`: targeted spatial extraction workflow entry point.
- `run_combine_and_harmonization_workflow.R`: combine and harmonization workflow entry point.
- `build_targeted_rerun_manifest.R` and `build_hydrosheds_followup_subset.R`: active pre-Aurora workflow helpers.
- `split_approved_hydrosheds_subset.R`: helper for splitting approved HydroSHEDS subsets.

## Subfolders

- `final_dataset/`: scripts used to build, patch, or update final full and ESOM datasets.
- `qaqc/`: audit, diagnostic, and plotting scripts.
- `reruns/`: one-off rerun, AppEEARS, backfill, and gap-filling scripts.

Keep new scripts in the narrowest matching subfolder. Leave top-level `tools/` for shared helpers and active workflow entry points that other scripts call directly.
