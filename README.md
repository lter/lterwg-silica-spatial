# LTER Working Group - From Poles to Tropics: A Multi-Biome Synthesis Investigating the Controls on River Si Exports

## PIs / Relevant Links: 

Joanna Carey & Kathi Jo Jankowski

- [Project Summary](https://lternet.edu/working-groups/river-si-exports/)
-[Participant Information](https://www.nceas.ucsb.edu/projects/12816)

## Script Explanations

- **id-watershed-polygons.R** - Identifies watershed drainage basins. Uses HydroSHEDS basin delineations (see [here](https://www.hydrosheds.org/page/hydrobasins)) unless otherwise specified

    - **hydrosheds_custom_fxns.R** - Identification of upstream HydroSHEDS polygons must be recursive and requires two custom functions found on GitHub
 
- **extract-[...].R** - These scripts are each responsible for extracting, summarizing, and exporting the spatial data type included in the script name. Each script uses the watersheds identified by "id-watershed-polygons.R" but is otherwise completely independent of other scripts in this repo
