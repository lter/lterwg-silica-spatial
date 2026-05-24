# Harmonization

Use this after the combined spatial table has been rebuilt and checked.

This step formats the spatial drivers for the final harmonized dataset.

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

## Notes

- `01_build-harmonized-drivers.R` does not clear your R session.
- To force one specific combined spatial file, set:

```bash
SILICA_HARMONIZATION_COMBINED_FILE=/abs/path/to/combined.csv
```
