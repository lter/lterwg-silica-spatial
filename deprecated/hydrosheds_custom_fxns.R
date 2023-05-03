## ------------------------------------------------------- ##
          # Load Custom HydroSHEDS Functions ----
## ------------------------------------------------------- ##

# These are modified from someone's GitHub functions to accept non-S4 objects
# Link to originals here: https://rdrr.io/github/ECCC-MSC/Basin-Delineation/

# First function finds just the next upstream polygon(s)
find_next_up <- function(HYBAS, HYBAS.ID, ignore.endorheic = F){
  
  # Process sf object into a regular dataframe
  HYBAS_df <- HYBAS %>%
    sf::st_drop_geometry()
  
  # Find next upstream polygon(s) as character
  upstream.ab <- HYBAS_df[HYBAS_df$NEXT_DOWN == HYBAS.ID, c("HYBAS_ID", "ENDO")]
  
  if (ignore.endorheic){
    upstream.ab <- upstream.ab[upstream.ab$ENDO != 2, ]
  }
  return(as.character(upstream.ab$HYBAS_ID))
}

# Second function iteratively runs through the first one to find all of the upstream polygons
find_all_up <- function(HYBAS, HYBAS.ID, ignore.endorheic = F, split = F){
  
  # make containers
  HYBAS.ID <- as.character(HYBAS.ID)
  HYBAS.ID.master <- list()
  
  #Get the possible upstream 'branches'
  direct.upstream <- find_next_up(HYBAS = HYBAS, HYBAS.ID = HYBAS.ID,
                                  ignore.endorheic = ignore.endorheic)
  
  # for each branch iterate upstream until only returning empty results
  for (i in direct.upstream){
    run <- T
    HYBAS.ID.list <- i
    sub.basins <- i # this is the object that gets passed to find_next_up in each iteration
    while (run){
      result.i <- unlist(lapply(sub.basins, find_next_up,
                                HYBAS = HYBAS, ignore.endorheic = ignore.endorheic))
      
      if (length(result.i) == 0){ run <- F } # Stopping criterion
      HYBAS.ID.list <- c(HYBAS.ID.list, result.i)
      sub.basins <- result.i
    }
    HYBAS.ID.master[[i]] <- HYBAS.ID.list
  }
  
  if (!split){ HYBAS.ID.master <- as.character(unlist(HYBAS.ID.master)) }
  
  return(HYBAS.ID.master)
}

# End ----
