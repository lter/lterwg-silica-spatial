# Spatial Extraction Workflow

This repo holds the code we use to build watershed polygons, extract spatial
drivers, and combine/check those outputs before they go into the harmonized
silica dataset.

## Folder Map

- `01_pre_aurora_run/`
  Figure out what needs to be rerun before sending anything to Aurora.

- `02_watershed_delineation/`
  Build or update watershed polygons.

- `03_spatial_extraction/`
  Run the driver extractions from watershed polygons.

- `04_combine_qaqc/`
  Rebuild the combined spatial table and check it against older outputs.

- `05_harmonization/`
  Format checked spatial data for the final harmonized dataset.

- `tools/`
  Shared helper scripts and one-off audit/build scripts used by the folders
  above.

## Most Common Starting Points

- Planning a new Aurora run:
  `01_pre_aurora_run/README.md`

- Running extractions:
  `03_spatial_extraction/README.md`

- Rebuilding and checking the combined spatial table:
  `04_combine_qaqc/README.md`

- Running harmonization:
  `05_harmonization/README.md`

## Name Matching

Some site and stream names changed between older and newer files. These files
record the known fixes:

- `tools/lter_name_aliases.csv`
- `tools/stream_name_aliases.csv`

## Related Repositories

- [lter/lterwg-silica-data](https://github.com/lter/lterwg-silica-data)
- [SwampThingPaul/SiSyn](https://github.com/SwampThingPaul/SiSyn)
- [lsethna/NCEAS_SiSyn_CQ](https://github.com/lsethna/NCEAS_SiSyn_CQ)
- [lter/lterwg-silica-spatial](https://github.com/lter/lterwg-silica-spatial)
- [njlyon0/lter_silica-high-latitude](https://github.com/njlyon0/lter_silica-high-latitude)
