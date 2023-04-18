## ------------------------------------------------------- ##
        # Silica WG - Wrangle Watershed Shapefiles
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Wrangle "artisanal" watershed shapefiles into a single file to extract driver data
## Artisanal = mixed provenance / provided by WG participants

## ------------------------------------------------------- ##
                      # Housekeeping -----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, magrittr, googledrive, sf, supportR, NCEAS/scicomptools, readxl)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

## ------------------------------------------------------- ##
            # Reference Table Acquisition ----
## ------------------------------------------------------- ##

# Grab ID of the GoogleSheet with site coordinates
googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/u/0/folders/0AIPkWhVuXjqFUk9PVA")) %>%
  dplyr::filter(name == "WRTDS_Reference_Table") %>%
  googledrive::drive_download(file = ., overwrite = T,
                              path = file.path(path, "site-coordinates",
                                               "silica-coords_RAW.xlsx"))

# Read in site coordinates (i.e., ref table)
coord_df <- readxl::read_excel(path = file.path(path, "site-coordinates",
                                                "silica-coords_RAW.xlsx")) %>%
  ## Pare down to minimum needed columns
  dplyr::select(LTER, Shapefile_Name, drainSqKm, Latitude, Longitude) %>%
  ## Drop duplicate rows (if any)
  dplyr::distinct() %>%
  ## Rename area column
  dplyr::rename(expert_area_km2 = drainSqKm)
  
# Glimpse this
dplyr::glimpse(coord_df)

## ------------------------------------------------------- ##
                  # Download Shapefiles ----
## ------------------------------------------------------- ##

# Grab all of their Drive IDs
shape_ids <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1TLEFKLWUpTKwxKiZv9nqhdwccflPuF9g"))

# Should we redownload?
redownload <- FALSE

# If re-download is desired...
if(redownload == TRUE){
  # Loop across shape IDs...
  for(k in 1:nrow(shape_ids)){
    # Download each file
    googledrive::drive_download(file = shape_ids[k,]$id, overwrite = T,
                                path = file.path(path, 'artisanal-shapefiles',
                                                 shape_ids[k,]$name))
  } }

## ------------------------------------------------------- ##
                # Combine Shapefiles ----
## ------------------------------------------------------- ##

# Identify shapefiles we already have
raw_sheds <- dir(path = file.path(path, 'artisanal-shapefiles'), pattern = ".shp") %>%
  ## Drop file extension to match with reference table
  gsub(pattern = "\\.shp", replacement = "", x = .)

# Compare shapefiles we have with those that are named in the reference table
supportR::diff_check(old = unique(coord_df$Shapefile_Name), 
                     new = unique(raw_sheds))
## Any 'in old but not new' = shapefiles named in reference table but not on Aurora
## Any 'in new but not old' = shapefiles in Aurora that aren't in the reference table

# Wrangle river coordinates
good_sheds <- coord_df %>%
  # Filter to only shapefiles in ref. table & on Aurora
  dplyr::filter(Shapefile_Name %in% raw_sheds) %>%
  # Drop any non-unique rows (shouldn't be any but good to double check)
  dplyr::distinct()

# Check that out
dplyr::glimpse(good_sheds)

# Double check that there are no duplicated shapefile names
good_sheds %>%
  dplyr::group_by(Shapefile_Name) %>%
  dplyr::summarize(name_ct = dplyr::n()) %>%
  dplyr::filter(name_ct != 1)

# Create an empty object to store combined shapefiles
all_shps <- NULL

# Turn off spherical processing
sf::sf_use_s2(F)

# For each shapefile we have:
for(focal_name in sort(unique(good_sheds$Shapefile_Name))){
  
  # Read in the shapefile
  focal_shp_raw <- sf::st_read(file.path(path, "artisanal-shapefiles",
                                         paste0(focal_name, ".shp")), quiet = T) %>%
    ## Make sure CRS is WGS84 (EPSG code 4326)
    sf::st_transform(crs = 4326)
  
  # Make sure the polygon geometry is consistently named
  sf::st_geometry(obj = focal_shp_raw) <- "geom"
  
  # Calculate shapefile area
  focal_area <- focal_shp_raw %>%
    sf::st_area(x = .) %>%
    units::set_units(x = ., km^2)
  
  # Identify other relevant information we'll want to attach to the shapefile
  focal_info <- good_sheds %>%
    dplyr::filter(Shapefile_Name == focal_name)
  
  # Wrangle raw shapfiles as needed
  focal_shp <- focal_shp_raw %>%
    ## Attach relevant information into this object
    dplyr::mutate(LTER = focal_info$LTER,
                  Shapefile_Name = focal_info$Shapefile_Name,
                  expert_area_km2 = focal_info$expert_area_km2,
                  shape_area_km2 = as.numeric(focal_area)) %>%
    ## Pare down to only this information (+ geometry)!
    ### This approach flexibly ignores whatever idiosyncratic columns may be in the raw object
    dplyr::select(LTER, Shapefile_Name, expert_area_km2, shape_area_km2, geom)
  
  # Attach this shape to the rest of them
  all_shps %<>%
    dplyr::bind_rows(focal_shp)
  
  # Loop end message
  message("Completed processing for shapefile: ", focal_name, " (", focal_shp$LTER, ")") }

# Exploratory plot
plot(all_shps["Shapefile_Name"], axes = T, main = NULL)

# Tidy up environment
rm(list = setdiff(ls(), c("path", "coord_df", "all_shps")))

## ------------------------------------------------------- ##
                    # Export Results ----
## ------------------------------------------------------- ##

# Export the combine shapefile for all rivers
st_write(obj = all_shps, delete_layer = T,
         dsn = file.path(path, "site-coordinates", "silica-watersheds.shp"))

# Process the shapefile object a bit to make a flat table variant
shps_df <- all_shps %>%
  # Drop geometry
  sf::st_drop_geometry(x = .) %>%
  # Calculate some area difference metrics
  dplyr::mutate(
    area_diff_km2 = round((expert_area_km2 - shape_area_km2), digits = 2),
    area_diff_perc = round(((expert_area_km2 - shape_area_km2) / expert_area_km2), digits = 2),
    area_diff_direction = dplyr::case_when(
     abs(area_diff_perc) < 5 ~ "under 5% mismatch",
     area_diff_perc <= -5 ~ "underestimated",
     area_diff_perc >= 5 ~ "overestimated"))

# Glimpse it
dplyr::glimpse(shps_df)

# Export locally
write.csv(shps_df, file = file.path(path, "artisanal_shape_area_check.csv"), 
          row.names = F, na = '')

# Upload to Drive
googledrive::drive_upload(media = file.path(path, "artisanal_shape_area_check.csv"), overwrite = T, 
                          path = googledrive::as_id("https://drive.google.com/drive/u/0/folders/1hrVN2qTzhxbcxe-XZNdRORkPfu4TQaO7"))

## ------------------------------------------------------- ##
                  # Exploratory Maps ----
## ------------------------------------------------------- ##

# Identify color palette per LTER
shed_palette <- c("aaa" = "#ffffff", # placeholder
                  ## North America (reds/oranges/yellows)
                  "AND" = "#6a040f",
                  "ARC" = "#9d0208", 
                  "BNZ" = "#d00000", 
                  "Catalina Jemez" = "#dc2f02", 
                  "Coal Creek" = "#e85d04", 
                  "HBR" = "#f48c06", 
                  "KNZ" = "#faa307", 
                  "KRR" = "#ffba08", 
                  "LMP" = "#370617", 
                  "NWT" = "#ff9b54", 
                  "Sagehen" = "#ff7f51", 
                  "UMR" = "#ce4257", 
                  "USGS" = "#720026", 
                  "Walker Branch" = "#4f000b",
                  ## Carribbean
                  "LUQ" = "#2b9348", 
                  ## Europe
                  "Finnish Environmental Institute" = "#0077b6", 
                  "Krycklan" = "#00b4d8", 
                  "NIVA" = "#ade8f4", 
                  ## Arctic / Antarctica
                  "GRO" = "#9d4edd", 
                  "MCM" = "#c77dff")

# Read in global & state borders
world <- sf::st_as_sf(maps::map(database = "world", plot = F, fill = T))
states <- sf::st_as_sf(maps::map(database = "state", plot = F, fill = T))

# Combine the two
borders <- dplyr::bind_rows(world, states) %>%
  dplyr::mutate(LTER = "aaa")

# Make empty list to store outputs in
map_list <- list()

# For each LTER
for(focal_lter in sort(unique(all_shps$LTER))){
  # focal_lter <- "HBR"
  
  # Start loop
  message("Beginning plot creation for ", focal_lter, " watershed shapes")
  
  # Filter to one "LTER" (in quotes because includes non-LTERs)
  sub_shp <- all_shps %>%
    dplyr::filter(LTER == focal_lter)
  
  # Identify boundary of object
  sub_bbox <- sf::st_bbox(obj = sub_shp)
  
  # Create (and wrangle) a dataframe version of this
  sub_bound_df <- data.frame("corner" = names(sub_bbox),
                             "lat_long" = stringr::str_sub(string = names(sub_bbox), 1, 1),
                             "end" = stringr::str_sub(string = names(sub_bbox), 2, 4),
                             "value" = as.numeric(sub_bbox)) %>%
    # Identify range
    dplyr::group_by(lat_long) %>%
    dplyr::mutate(range = abs(max(value) - min(value))) %>%
    dplyr::ungroup() %>%
    # Replace if very small
    dplyr::mutate(range = ifelse(range < 2, yes = 2, no = range)) %>%
    # Add to actual values conditionally
    dplyr::mutate(lims = dplyr::case_when(
      value >= 0 & end == "min" ~ value - range,
      value >= 0 & end == "max" ~ value + range,
      value < 0 & end == "min" ~ value + range,
      value < 0 & end == "max" ~ value - range)) %>%
    # Add axis steps
    dplyr::mutate(steps = dplyr::case_when(range <= 5 ~ 2,
                                           range <= 10 ~ 2,
                                           range <= 50 ~ 10,
                                           range > 50 ~ 20))
  
  # Strip off relevant bit for each limits
  sub_xlim <- sub_bound_df %>%
    dplyr::filter(lat_long == "x") %>%
    dplyr::pull(lims)
  sub_ylim <- sub_bound_df %>%
    dplyr::filter(lat_long == "y") %>%
    dplyr::pull(lims)
  
  # Attach sub shape to borders
  sub_all <- sub_shp %>%
    dplyr::bind_rows(borders)
  
  # Crop palette to only relevant color(s)
  sub_palette <- shed_palette[unique(sub_all$LTER)]

  # Make plot
  sub_map <- sub_all %>%
    ggplot(aes(fill = LTER)) +
    geom_sf() +
    # Set plot extents
    coord_sf(xlim = sub_xlim, ylim = sub_ylim, expand = F, crs = st_crs(x = 4326)) +
    # Customize theming / labels
    scale_fill_manual(values = sub_palette) +
    scale_x_continuous(limits = sub_xlim, breaks = seq(from = floor(min(sub_xlim)),
                                                       to = ceiling(max(sub_xlim)),
                                                       by = max(sub_bound_df$steps))) +
    scale_y_continuous(limits = sub_ylim, breaks = seq(from = floor(min(sub_ylim)),
                                                       to = ceiling(max(sub_ylim)),
                                                       by = max(sub_bound_df$steps))) +
    labs(x = "Longitude", y = "Latitude", 
         title = paste0(focal_lter, " Map")) +
    supportR::theme_lyon() +
    theme(legend.position = "none",
          axis.text.y = element_text(angle = 90, vjust = 1, hjust = 0.5))
  
  # Add to plot list
  map_list[[focal_lter]] <- sub_map
  
  # Closing message
  message("Finished with ", focal_lter) }

# Create multi-panel graph
cowplot::plot_grid(plotlist = map_list, labels = "AUTO", nrow = 2)




# Tidy up environment
rm(list = setdiff(ls(), c("path", "coord_df", "all_shps")))

# End ----
