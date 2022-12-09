## ------------------------------------------------------- ##
   # Silica WG - Extract Spatial Data - Air Temperature
## ------------------------------------------------------- ##
# Written by:
## Nick J Lyon

# Purpose:
## Using the watershed shapefiles created in "id-watershed-polygons.R"
## Extract the following data: AIR TEMPERATURE (MONTHLY)

## ------------------------------------------------------- ##
                        # Housekeeping ----
## ------------------------------------------------------- ##

# Read needed libraries
# install.packages("librarian")
librarian::shelf(tidyverse, sf, stars, terra, exactextractr, NCEAS/scicomptools, googledrive)

# Clear environment
rm(list = ls())

# Identify path to location of shared data
(path <- scicomptools::wd_loc(local = F, remote_path = file.path('/', "home", "shares", "lter-si", "si-watershed-extract")))

# Load in site names with lat/longs
sites <- read.csv(file = file.path(path, "site-coordinates", 'silica-coords_ACTUAL.csv'))

# Check it out
dplyr::glimpse(sites)

# Grab the shapefiles the previous script (see PURPOSE section) created
sheds <- sf::st_read(dsn = file.path(path, "site-coordinates", "silica-watersheds.shp"))

# Check that out
dplyr::glimpse(sheds)

# Clean up environment
rm(list = setdiff(ls(), c('path', 'sites', 'sheds')))

## ------------------------------------------------------- ##
              # Convert netCDF to Raster ----
## ------------------------------------------------------- ##

# Load needed libraries
library(ncdf4)

# Load netCDF file
air_nc <- ncdf4::nc_open(filename = file.path(path, "raw-driver-data", "raw-airtemp-monthly",
                                               "air.mon.mean.nc"))

# Look at this
print(air_nc)

# Read coordinates out of netCDF object
nc_lat <- ncdf4::ncvar_get(nc = air_nc, varid = "lat")
nc_long <- ncdf4::ncvar_get(nc = air_nc, varid = "lon")

# Tweak longitude to be from -180 to 180
long_fix <- base::ifelse(test = (as.numeric(nc_long) > 180),
                         yes = as.numeric(nc_long) - 180,
                         no = as.numeric(nc_long))
range(long_fix)

# Grab the data of interest and its attributes
## Takes a few seconds to strip the response var (~4 sec)
nc_resp <- ncdf4::ncvar_get(nc = air_nc, varid = "air")
(nc_resp_att <- ncdf4::ncatt_get(nc = air_nc, varid = "air", attname = "missing_value")$value)

# Fill the missing value with R's missing value (`NA`)
nc_resp[nc_resp == nc_resp_att] <- NA

# Grab time and check its units
nc_time <- ncdf4::ncvar_get(nc = air_nc, varid = "time")
ncdf4::ncatt_get(nc = air_nc, varid = "time", attname = "units")$value

# Convert time to a human-readable format
time_fix <- as.POSIXct("1800-01-01 00:00") + as.difftime(tim = nc_time, units = "hours")



# HERE NOW ----





# Create all combinations of variables
## Takes several seconds
# mat_data <- as.matrix(x = expand.grid(long_fix, nc_lat, time_fix))




# Create 2D matrix of coordinates + time
## May take several seconds
# df_partial <- as.matrix(x = expand.grid(nc_lat, nc_long, time_fix))
# str(df_partial)

# Create a dataframe from these objects
# air_df <- 



# 
# # Read some key pieces out of the netCDF object
# long <- ncdf4::ncvar_get(nc = data, varid = long)
# lat <- ncdf4::ncvar_get(data, varid = lat)
# arag <- ncdf4::ncvar_get(data, varid = variable_to_extract)
# 
# # Create the dataframe
# df <- base::as.data.frame(
#   base::cbind(base::as.vector(long),
#               base::as.vector(lat),
#               base::as.vector(arag[,,1])))
# 
# # Put back the longitude data from -180 to 180
# df$x <- base::ifelse(test = (df$x > 180),
#                      yes = (df$x - 360),
#                      no = df$x)
# 
# # remove filling values
# if (filling_value_flag) {
#   # Currently assuming the filling value is a large number!!!!
#   df$z <- ifelse(test = (df$z == base::max(df$z)),
#                  yes = NA, no = df$z)
# }
# 
# # project it to a regular grid
# regridded <- akima::interp(x = df$x, y = df$y, z = df$value,
#                            xo = base::seq(from = -180,
#                                           to = 180,
#                                           grid_resolution),
#                            yo = base::seq(from = -80,
#                                           to = 90,
#                                           grid_resolution),
#                            linear = TRUE, extrap = TRUE,
#                            duplicate = "mean")
# 
# # Create a rasterLayer object
# r <- raster::raster(regridded)
# 
# # Give it the correct CRS label
# raster::crs(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"






## ------------------------------------------------------- ##
                # Combine Extracted Data ----
## ------------------------------------------------------- ##
# Clear environment
rm(list = setdiff(ls(), c('path', 'sites')))

# List current extracted data
extracted_data <- googledrive::drive_ls(googledrive::as_id("https://drive.google.com/drive/folders/1-X0WjsBg-BTS_ows1jj6n_UehSVE9zwU"))

# Make an empty list
data_list <- list()

# Download these files
for(file_name in extracted_data$name){
  
  # Filter to desired filed
  wanted <- extracted_data %>%
    dplyr::filter(name == file_name)
  
  # Download
  googledrive::drive_download(file = googledrive::as_id(wanted$id),
                              path = file.path(path, "extracted-data", file_name),
                              overwrite = T)
  
  # Read the CSV and add to the list
  data_list[[file_name]] <- read.csv(file = file.path(path, "extracted-data", file_name))
  
} # End loop

# Get a duplicate of the 'sites' object
out_df <- sites

# Now loop across remaining elements and left join each
for(k in 1:length(data_list)){
  
  # Add each bit to the existing dataframe
  out_df <- out_df %>%
    # Left join by all non-data columns
    dplyr::left_join(y = data_list[[k]],
                     by = c("LTER", "Stream_Name", "Discharge_File_Name", "drainSqKm", 
                            "river_id", "lat", "long")) %>%
    # Drop duplicated columns
    unique()
}

# Check for dropped rivers (i.e., rows)
## Stream names (chemistry river names)
setdiff(x = unique(sites$Stream_Name), y = unique(out_df$Stream_Name))
setdiff(y = unique(sites$Stream_Name), x = unique(out_df$Stream_Name))
## Discharge file names (discharge river names)
setdiff(x = unique(sites$Discharge_File_Name), y = unique(out_df$Discharge_File_Name))
setdiff(y = unique(sites$Discharge_File_Name), x = unique(out_df$Discharge_File_Name))

# Take a look
dplyr::glimpse(out_df)

# Export this
write.csv(x = out_df, na = '', row.names = F,
          file = file.path(path, "extracted-data", "all-data_si-extract.csv"))

# And upload to GoogleDrive
googledrive::drive_upload(media = file.path(path, "extracted-data", "all-data_si-extract.csv"),
                          overwrite = T,
                          path = googledrive::as_id("https://drive.google.com/drive/folders/1-X0WjsBg-BTS_ows1jj6n_UehSVE9zwU"))

# End ----
