# Spatial Extraction Workflow

This repository prepares the versioned watershed library and runs the local
and Aurora extraction work used by the stream-silica workflow. Local work
includes AppEEARS requests, MODIS downloads, extraction, and QA. Aurora runs
the soil, lithology, elevation, permafrost, precipitation, and air-temperature
drivers.

Use [data-workflow_spatial](https://github.com/global-river-chem/data-workflow_spatial)
for Earth Engine, land cover, and final spatial-data assembly.

## Run the workflow

Run these commands from the repository root. Choose the mode and reference
release in [01_run_config.R](01_run_config.R). Set `SILICA_DATA_ROOT` if the
shared data library is in a different location.

```bash
Rscript tools/site_reference/validate_site_reference.R --input PATH
Rscript 02_run-workflow.R
```

The [spatial extraction guide](03_spatial_extraction/README.md) lists the
AppEEARS request, queue, and Aurora handoff commands. The
[queue retry helper](tools/appeears/run_verified_backlog_after_process.R)
is optional when extraction needs to wait for an existing process.

## Repository layout

- [Watershed delineation](02_watershed_delineation/) prepares the versioned boundaries.
- [Spatial extraction](03_spatial_extraction/) contains local and Aurora extraction and shared site rules.
- [AppEEARS tools](tools/appeears/) handle requests, downloads, extraction, and checks.

For earlier local releases, the
[combine and harmonize wrapper](tools/run_combine_and_harmonization_workflow.R)
runs the saved local combination and harmonization steps.

Keep the site-reference copy, watershed version, source checksums, request
files, task IDs, and validation results with the dated run outside Git.
Record the script version used for that run. Store credentials separately.
