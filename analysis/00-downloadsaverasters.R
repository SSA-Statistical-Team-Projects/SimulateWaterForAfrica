################################################################################
##### SCRIPT FOR DOWNLOADING AND SAVING RASTERS FROM GAEZ GOOGLE STORAGE #######
################################################################################

#### starting with potential yield 
pacman::p_load(raster, sf, tidyverse)


### Agroclimatic potential yield
crop_list <- c("BARL", "MLLT", "MAIZ", "RICW", "SORG", "WHEA")

lapply(X = crop_list,
       FUN = get_gaez_restwo,
       var = "YLD",
       dsn = "data-raw/gaez")