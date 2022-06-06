## ----------------------------------------------- ##
        # Universal File Path Demonstration
## ----------------------------------------------- ##



## Option 1 : Using paths

# Set path and filenames
base_path <- "/home/shares/lter-si/gdrive"
basin_path <- "Master_Dataset/BasinCharacteristics"

watershed_file <- "LongTermWatersheds_LatLong_drainArea.csv"

# read the file
test_data <- read.csv(file = file.path(base_path, basin_path, watershed_file))



## Option 2 : You can also change the working directory (if you need to read a lot of files)

# Default working directory is the folder containing the current project
getwd()

# If you think you might want to re-set back here easily, you can capture it:
myWD <- getwd()
myWD

# Can re-set to the shares folder as follows
setwd(file.path(base_path, basin_path))

# See?
getwd()

# You can then preserve that path in the same way you preserved your own
shares <- getwd()
shares

# Now you can read in (or save out) files directly from the "shares" folder
test_data <- read.csv(file = "LongTermWatersheds_LatLong_drainArea.csv")
str(test_data)

# If you scan through the above you'll see that this script could be run by anyone with access to your group's "shares" sub-folder ("lter-si" in this case)

# If needed, you can toggle back and forth between the two paths!
getwd()
setwd(myWD)
getwd()
setwd(shares)
getwd()

# End ----
