## ----------------------------------------------- ##
        # Universal File Path Demonstration
## ----------------------------------------------- ##

## Option 1 : Using Paths ----
# You can set the paths (to your home folder and the shares folder) as objects and just add them into the functions as needed

# Set path to shares folder as an object
base_path <- file.path("/", "home", "shares", "lter-si", "gdrive")

# Set the path within your folder of interest to the data folder we want to look in as a different object
basin_path <- file.path("Master_Dataset", "BasinCharacteristics")

# And identify the file as an object
watershed_file <- "LongTermWatersheds_LatLong_drainArea.csv"

# Read the file
test_data <- read.csv(file = file.path(base_path, basin_path, watershed_file))
str(test_data)

## Option 2 : Changing the Working Directory ----
# You can also change the working directory (if you need to read/write a lot of files)

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
test_data <- read.csv(file = watershed_file)
str(test_data)

# If you scan through the above you'll see that this script could be run by anyone with access to your group's "shares" sub-folder ("lter-si" in this case)

# If needed, you can toggle back and forth between the two paths!
getwd()
setwd(myWD)
getwd()
setwd(shares)
getwd()

# End ----
