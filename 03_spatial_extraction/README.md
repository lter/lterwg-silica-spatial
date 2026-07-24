# Spatial Extraction

The workflow reads the reviewed site-reference table and uses the watershed
version listed for each site.

The three version fields have separate meanings:

- `GlASS_First_Release`: first planned release containing the site
- `CQ_Data_Version`: chemistry and discharge data currently in use
- `Spatial_Data_Version`: watershed and spatial results currently in use

Validate the reference table and watershed library before running:

```bash
Rscript tools/site_reference/validate_site_reference.R --input PATH
Rscript tools/validate_release_reference_library.R --release 3 --strict
Rscript 02_run-workflow.R
```

Set the run mode and reference release in `01_run_config.R`. Set
`SILICA_DATA_ROOT` when needed.

Shared rules and documented exceptions belong in
`03_spatial_extraction/config/`, not in code written for individual sites.
