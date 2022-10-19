# LTER Working Group - From Poles to Tropics: A Multi-Biome Synthesis Investigating the Controls on River Si Exports

## PIs: 

- Joanna Carey
- Kathi Jo Jankowski

## Project summary

https://lternet.edu/working-groups/river-si-exports/

## Info for Participants

https://www.nceas.ucsb.edu/projects/12816

## Script Explanations

### Living

- "identify-hydrosheds.R": Identifies watershed drainage basins based on provided lat/long coordinates. Uses HydroSHEDS delineations of watersheds (see [here](https://www.hydrosheds.org/page/hydrobasins))

- "extract-watershed-info.R": Extracts lithology and land cover data within watershed shapefiles (see "identify-hydrosheds.R")

### Deprecated / Superseded

- "identify-watersheds.R": **DEPRECATED** Old method of identifying watershed shapefiles. Used shapefiles from various sources for each stream/LTER site which led to dissonance in resolution, CRS, and formatting of raw files

- "extract-method-diagnostic.R": Script for checking the sensitivity of "extract-watershed-info.R" to different CRS in input files. Showed proportion / percent of output is consistent across different coordinate reference systems

- "filepath-demo.R": Demonstration script for managing file paths. Irrelevant now but was intended as a teaching tool so it will be allowed to exist here
