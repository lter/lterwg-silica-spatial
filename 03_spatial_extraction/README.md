# 03_spatial_extraction

This stage runs the actual spatial extraction workflow.

Main subfolders:

- `modes/`
  User-facing execution modes such as full runs, targeted subset runs, and update-years runs.
- `extraction_scripts/`
  Driver-specific extraction scripts. This is the canonical location for
  `extract-*.R` files used by the workflow.
- `aurora/`
  Shell wrappers for running extraction on Aurora.
- `examples/`
  Small input templates such as `subset_file_template.csv`.

Typical order:

1. Finish `01_pre_aurora_run` if you need a targeted HydroSHEDS rerun.
2. Make sure `02_watershed_delineation` outputs are ready.
3. Run the extraction mode you need.
4. Then move to `04_combine_qaqc`.
