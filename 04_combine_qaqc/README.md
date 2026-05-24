# Combine And Check Spatial Outputs

Use this folder after the driver extraction files have been pulled back from
Aurora.

This step does two things:

- rebuilds one combined spatial table from the extracted driver CSVs
- compares that table to older combined files so gaps are easier to spot

## Main Files

- `00_qaqc_config.R`
  Paths and filenames for this step.

- `01_import-and-qaqc.R`
  Runs the combine/check step.

- `combine_from_site_ref_local.R`
  Rebuilds the combined table from the local extracted driver files.

- `../tools/run_combine_and_harmonization_workflow.R`
  Runs combine/checks and then harmonization in one pass.

## Main Input

This step uses:

```text
<data_root>/silica-shapefiles/site-coordinates/silica-coords_RAW.xlsx
```

It does not use the WRTDS reference CSV as the main site list.

Known name fixes are in:

- `tools/lter_name_aliases.csv`
- `tools/stream_name_aliases.csv`

## Usual Run

1. Restart R.

2. Make sure the Box data folder has:
   - `si-extracted-data/`
   - `silica-shapefiles/site-coordinates/silica-coords_RAW.xlsx`

3. Run:

```bash
Rscript tools/run_combine_and_harmonization_workflow.R
```

4. Check the review files under:

```text
<data_root>/review/
```

## Run Only This Step

If you only want to rebuild and check the combined spatial table:

```bash
Rscript 04_combine_qaqc/combine_from_site_ref_local.R
Rscript 04_combine_qaqc/01_import-and-qaqc.R
```

## Notes

- The older comparison file is controlled by `old_combined_filename` in
  `00_qaqc_config.R`.
- If `new_combined_filename` is blank, the script looks for the newest combined
  file in the expected data folders.
