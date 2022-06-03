## ----------------------------------------------- ##
        # Universal File Path Demonstration
## ----------------------------------------------- ##

# Default working directory is the folder containing the current project
getwd()

# If you think you might want to re-set back here easily, you can capture it:
myWD <- getwd()
myWD

# Can re-set to the shares folder as follows
setwd(file.path('/', "home", "shares", "lter-si", "gdrive", "BasinCharacteristics"))

# See?
getwd()

# You can then preserve that path in the same way you preserved your own
shares <- getwd()
shares

# Now you can read in (or save out) files directly from the "shares" folder
test_data <- read.csv(file = "LongTermWatersheds_LatLong_drainArea.csv")
str(test_data)

# If you scan through the above you'll see that this script could be run by anyone with access to your group's "shares" sub-folder ("lter-si" in this case)

# No more needing to deal with relative file paths differing among users!

# End ----
