# LTER Working Group - From Poles to Tropics: A Multi-Biome Synthesis Investigating the Controls on River Si Exports

## PIs / Relevant Links: 

Joanna Carey & Kathi Jo Jankowski

- [Project Summary](https://lternet.edu/working-groups/river-si-exports/)
- [Participant Information](https://www.nceas.ucsb.edu/projects/12816)

## Script Explanations

- **id-watershed-polygons.R** - Identifies watershed drainage basins. Uses HydroSHEDS basin delineations (see [here](https://www.hydrosheds.org/page/hydrobasins)) unless otherwise specified

    - **hydrosheds_custom_fxns.R** - Identification of upstream HydroSHEDS polygons must be recursive and requires two custom functions found on GitHub
 
- **extract-[...].R** - These scripts are each responsible for extracting, summarizing, and exporting the spatial data type included in the script name. Each script uses the watersheds identified by "id-watershed-polygons.R" but is otherwise completely independent of other scripts in this repo

- **crop-drivers.R** - For the drivers downloaded from [AppEEARS](https://appeears.earthdatacloud.nasa.gov/), data are retrieved from manually-drawn  bounding boxes. These bounding boxes intentionally overlap eachother somewhat so that no gaps exist between downloads but this does necessitate cropping those boxes to avoid "double counting" the pixels contained in two separate bounding boxes. This script does all of that cropping.

    - AppEEARS data acquisition process is as follows:
    - 1) Sign into AppEEARS portal (create account if you don't already have one)
    - 2) Click "Extract" in top left of navbar at top of screen
    - 3) Select "Area" in the resulting dropdown menu
    - 4) Either start a new request of use an existing request if you want to use the bounding box drawn for a previous request
    - 5A) Name your request informatively
    - 5B) Draw bounding box / polygon for which you want data
    - 5C) Select range of dates for which you want data
    - 5D) Search for and add the data layers you want
    - 5E) Select the output format you desire
    - 5F) Select the coordinate reference system (CRS) for the output data
    - 5G) Click "Submit"
    - 6) Await email confirming 'download ready'
    - 7) Follow instructions to download the parts of the data / QA of your request that you want
    - NOTE: It is easy to exceed the data limitation of a single request either by (i) exceeding the spatial area allowed or (ii) including too many layers in the same request. It is better to make several smaller requests to avoid this issue.
