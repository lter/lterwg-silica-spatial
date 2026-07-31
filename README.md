# Spatial Extraction Workflow

This repository prepares the versioned watershed library and runs the local
and Aurora extraction work used by the stream-silica workflow. Local work
includes AppEEARS requests, MODIS downloads, extraction, and QA. Aurora runs
the soil, lithology, elevation, permafrost, precipitation, and air-temperature
drivers.

Earth Engine, GLC land cover, harmonization, and final spatial-data assembly
belong in `data-workflow_spatial`.

## Run the workflow

Run commands from the repository root. The spatial-extraction README explains
the independent AppEEARS and Aurora paths.

```bash
Rscript tools/site_reference/validate_site_reference.R --input PATH
Rscript 02_run-workflow.R
```

Set `SILICA_DATA_ROOT` if the shared data library is not in its default
location.

## Repository layout

- `02_watershed_delineation/`: watershed preparation
- `03_spatial_extraction/`: local and Aurora extraction
- `tools/appeears/`: AppEEARS request, download, extraction, and QA tools

The site-reference table and configuration files define site-specific
decisions. Raw downloads, exports, plots, credentials, and temporary work do
not belong in Git.

See [spatial extraction](03_spatial_extraction/README.md) for the reusable
AppEEARS and Aurora workflows.
