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

## AppEEARS and Aurora

AppEEARS and Aurora may run in parallel after the site-reference table and
versioned watershed library pass validation. Their outputs meet later during
combination and harmonization.

### Submit AppEEARS requests

`submit_saved_requests.R` checks or submits saved request JSON.

### Prepare the local MODIS queue

`prepare_verified_request_backlog.R` matches completed requests to the current
site table and watershed geometry. `build_download_lists.R` creates temporary
GeoTIFF download lists. `run_local_modis_queue.R` downloads, extracts, checks,
and removes raw files one watershed at a time.

### Prepare an Aurora handoff

Use an explicit site subset when Aurora should start independently of MODIS:

```bash
Rscript tools/appeears/build_aurora_handoff.R \
  --reference PATH/TO/SITE_REFERENCE.csv \
  --selection PATH/TO/AURORA_SITE_SUBSET.csv \
  --shape-root PATH/TO/VERSIONED_WATERSHEDS \
  --output-root PATH/TO/AURORA_HANDOFF
```

Use `--qa` with matching `--status` files instead of `--selection` only when
the Aurora roster should be limited to sites whose local MODIS extraction has
already passed QA.

Use `tools/appeears/config/shapefile_aliases.tsv` only when a request geometry
is unchanged but its final watershed filename changed. New run-specific
choices belong in small generated manifests, not new scripts.
