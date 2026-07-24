# Spatial Extraction

The workflow uses the accepted site-reference snapshot and the matching
watershed from the canonical release library.

The three version fields have separate meanings:

- `GlASS_First_Release`: first publication containing the site
- `CQ_Data_Version`: accepted chemistry and discharge data
- `Spatial_Data_Version`: accepted watershed and spatial extraction

Validate the reference table and watershed library before running:

```bash
Rscript tools/site_reference/validate_site_reference.R --input PATH
Rscript tools/validate_release_reference_library.R --release 3 --strict
Rscript 02_run-workflow.R
```

Set the run mode and reference release in `01_run_config.R`. Set
`SILICA_DATA_ROOT` when needed.

Regional routing and reviewed exceptions belong in
`03_spatial_extraction/config/`, not in site-specific code.
