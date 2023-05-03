# Deprecated Files

These scripts are all either deprecated (i.e., no longer in use / relevant to workflow) or superseded (i.e., replaced by a similar but improved script). This README explains each script in this folder for posterity.

## extract-method-diagnostic.R

- Compared the outputs resulting from extracting raster data with a raster, `sf` from `sf`, and `sf` from raster. Counted pixel differences and percents per category. Meant to ensure that our eventual object class choice (`sf` from raster at time of writing) wasn't affecting our results. Confirms that it doesn't matter so flagged as deprecated to keep top-level repository tidy.

## id-watershed-polygons.R

- Identified HydroSHEDS watersheds **for each focal HydroSHEDS sub-polygon**. This was an improvement on the per-river approach of `identify-hydrosheds.R` as multiple rivers fall in the same sub-polygon and effectively ask the loop to re-do a given operation multiple times. Superseded by `wrangle-watersheds.R` which uses "artisanal" shapefiles for each watershed provided by site PIs.

    - Required some custom functions included in `hydrosheds_custom_fxns.R`


## `hydrosheds_custom_fxns.R`

- Two functions from StackOverflow used to identify the next upstream HydroSHEDS polygon and all upstream polygons respectively. Flagged as deprecated when we shifted from HydroSHEDS-derived shapefiles to using shapefiles provided by site PIs.

## identify-hydrosheds.R

- Identified HydroSHEDS watersheds **for each river**. Superseded by `id-watershed-polygons.R` because that workflow loops across **focal HydroSHEDS sub-polygon** meaning that the loop only runs once for all rivers that start in the same polygon. That shift was a dramatic improvement in computational efficiency.

    - Required some custom functions included in `hydrosheds_custom_fxns.R`

## wrangle-landcover.R

- Transformed manually-downloaded NLCD land cover rasters into a CRS (coordinate reference system) that corresponded with the shapefiles we were using at the time. Superseded by `extract-landcover.R`

## extract-watershed-info.R

- Extracted NLCD land cover and global lithology data from within StreamStats / agency shapefiles and wrangled the resulting information into CSV format. Superseded by the `extract-\<driver name\>.R` suite of scripts that each extract one driver. Those scripts are faster and are more flexible about which shapefile(s) they use to extract information.

## identify-watersheds.R

- Used StreamStats watershed shapefiles for US rivers and various government/agency-provided shapefiles for non-US rivers. Flagged as deprecated after discovery that StreamStats sometimes returns only a single pixel due to a bug in the initial selection of river starting point on the map. Superseded by various HydroSHEDS workflows.
