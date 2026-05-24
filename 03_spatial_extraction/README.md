# Spatial Extraction

This is where the spatial driver extraction runs.

## What Is In Here

- `modes/`
  Scripts for the different ways we run extraction: all sites, a smaller subset,
  or newer years only.

- `extraction_scripts/`
  One script per driver, like `extract-evapo.R`, `extract-snowfrac.R`, and
  `extract-airtemp.R`.

- `aurora/`
  Shell scripts for running extraction on Aurora.

- `examples/`
  Small files showing the expected input format.

## Usual Order

1. Use `01_pre_aurora_run/` if you need to decide what should be rerun.
2. Make sure the watershed files are ready.
3. Run the extraction script or mode you need.
4. Move to `04_combine_qaqc/` to rebuild and check the combined table.
