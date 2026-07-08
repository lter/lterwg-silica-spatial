# AND ERA5-Land Small-Watershed Rerun

This folder keeps the Colab notebook for the corrected AND/HJA ERA5-Land test.

Colab link:
https://colab.research.google.com/drive/1qF3e_9Q_xoX0jfYdU7HxmUXzwCgCrr6A

## Small-Watershed Decision

For small watersheds, do not fill missing values from the watershed centroid.

The corrected extraction does this instead:

1. Extract ERA5-Land values from the watershed polygon.
2. If the polygon result is blank, retry the same polygon at a finer scale.
3. Mark retry rows with `used_fine_scale_fallback`.

This keeps the value tied to the watershed area instead of replacing it with a
single centroid pixel.

## Expected Outputs

The Colab notebook writes one CSV per year for 2001-2023, named like:

`era5_land_2001_AND_fine_scale_watershed_extract.csv`

The notebook moves those CSV files into the shared Google Drive folder from the
project link. It does not upload code to the output folder.

## After The Colab Finishes

Download the corrected `AND_fine_scale` CSV files, then rerun:

```bash
Rscript tools/plot_and_era5_modis_comparison.R
```

That script makes the ERA5-Land versus driver plots and uploads only the plots
and CSV summaries to:

`small watersheds test case / Andrews sites only testing / plots`

and:

`small watersheds test case / Andrews sites only testing / csv files`
