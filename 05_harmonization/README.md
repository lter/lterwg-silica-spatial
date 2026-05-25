# Harmonization

Use this after the combined spatial table has been rebuilt and checked.

This step formats the spatial drivers for the final harmonized dataset. It now
writes three versions:

- a wide site table
- a site-by-year table
- a site-average table

## Main Files

- `00_harmonization_config.R`
- `00_harmonization_functions.R`
- `01_build-harmonized-drivers.R`
- `../tools/run_combine_and_harmonization_workflow.R`

## Inputs

This step uses the site reference table from the harmonization input folder set
in `00_harmonization_config.R`.

It also needs the newest checked combined spatial file. The script looks in the
usual locations, including:

- `<data_root>/final-data/full-dataset/all-data_si-extract_*.csv`
- `<data_root>/review/harmonization/combined-spatial-dataset_*.csv`
- `<data_root>/si-extracted-data/all_data_extractions/all-data_si-extract_[34]_*.csv`
- `<data_root>/si-extracted-data/all-data_si-extract_3_*.csv`

## Usual Run

The easiest way is to run combine/checks and harmonization together:

```bash
Rscript tools/run_combine_and_harmonization_workflow.R
```

To run only harmonization:

```bash
Rscript 05_harmonization/01_build-harmonized-drivers.R
```

## Outputs

Files are written to `<data_root>/review/harmonization/` unless
`SILICA_HARMONIZATION_OUTPUT_DIR` is set.

- `harmonized-spatial-drivers_<date>.csv`
  Wide site table.

- `harmonized-spatial-drivers-annual_<date>.csv`
  One row per site-year. This includes annual spatial drivers plus annual Q,
  RBI, and RCS where daily discharge data are available.

- `harmonized-spatial-drivers-site-averages_<date>.csv`
  One row per site. This keeps the site-level and monthly driver columns from
  the merged spatial table and adds averages across annual drivers.

## Notes

- `01_build-harmonized-drivers.R` does not clear your R session.
- To force one specific combined spatial file, set:

```bash
SILICA_HARMONIZATION_COMBINED_FILE=/abs/path/to/combined.csv
```

- To send outputs somewhere else, set:

```bash
SILICA_HARMONIZATION_OUTPUT_DIR=/abs/path/to/output-folder
```
