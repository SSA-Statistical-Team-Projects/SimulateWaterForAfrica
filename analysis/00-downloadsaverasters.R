################################################################################
##### SCRIPT FOR DOWNLOADING AND SAVING RASTERS FROM GAEZ GOOGLE STORAGE #######
################################################################################

#### starting with potential yield 
pak_list <- c("raster", "sf", "tidyverse", "data.table")

lapply(pak_list, function(pkg) {
  suppressMessages(suppressWarnings(
    library(pkg, character.only = TRUE)
  ))
})
lapply(pak_list, library, character.only = TRUE)

### load all functions
lapply(X = list.files("R", full.names = T),
       FUN = source,
       echo = FALSE,
       verbose = FALSE)

### Agroclimatic potential yield
crop_list <- c("BARL", "MLLT", "MAIZ", "RICW", "SORG", "WHEA")

###### first download the irrigated
lapply(X = crop_list,
       FUN = get_gaez_restwo,
       var = "YLD",
       dsn = "data-raw/gaez")

##### then the non-irrigated
lapply(X = crop_list,
       FUN = get_gaez_restwo,
       var = "YLD",
       irrigated = FALSE,
       dsn = "data-raw/gaez")


