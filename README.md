# GlASS Spatial Workflow

This repository prepares watershed and spatial data for the Global Analysis of
Stream Silica (GlASS).

## Run the workflow

Run commands from the repository root:

```bash
Rscript 02_run-workflow.R
Rscript tools/run_combine_and_harmonization_workflow.R
```

Set `SILICA_DATA_ROOT` if the shared data library is not in its default
location.

## Repository layout

- `02_watershed_delineation/`: watershed preparation
- `03_spatial_extraction/`: spatial extraction
- `04_combine_qaqc/`: combination and QA/QC
- `05_harmonization/`: final harmonization

The site-reference table and configuration files define site-specific
decisions. Raw downloads, exports, plots, credentials, and temporary work do
not belong in Git.

See [spatial extraction](03_spatial_extraction/README.md) and
[Earth Engine tools](tools/gee_colab_helpers/README.md) for the few
workflow-specific instructions.
