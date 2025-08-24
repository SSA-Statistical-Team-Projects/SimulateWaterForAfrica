################################################################################
##### SCRIPT FOR DOWNLOADING AND SAVING RASTERS FROM GAEZ GOOGLE STORAGE #######
################################################################################

#### starting with potential yield 
pak_list <- c("raster", "sf", "tidyverse", "data.table", "jsonlite")

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

#### get HP0120 data for our crops for all water supply types
param_grid <- 
expand.grid(var = c("CYL", "ETA", "FC2", "TSC", "WDE"),
            crop = crop_list, 
            water_supply = c("H", "L"),
            irr = c(TRUE, FALSE), 
            time_period = "HP0120") 

pmap(.l = param_grid,
     .f = ~ get_gaez_restwo_v2(var = ..1,
                               crop_code = ..2,
                               irrigated = ..4,
                               water_supply = ..3,
                               dsn = "data-raw/gaez/rasters_raw",
                               time_period = ..5)) 




### download the res06 data
ind_list <- c("HAR", "YLD", "PRD")

crop_list <- c("BRL", "MZE", "RCW", "SRG", "WHE", "MLT")

type_list <- c("WSI", "WSR", "WST")

grid_dt <-
  expand.grid(crop = crop_list,
              ind = ind_list,
              type = type_list) %>%
  apply(MARGIN = 2, FUN = as.character) %>%
  as.data.frame() %>%
  mutate(row_id = row_number()) %>%
  group_split(row_id, .keep = FALSE)



lapply(X = grid_dt,
       FUN = function(x){

         tryCatch({

           get_gaez_ressix(crop_code = x[["crop"]],
                           var = x[["ind"]],
                           type = x[["type"]],
                           dsn = "data-raw/gaez/rasters_raw")

         }, error = function(e){

           message(sprintf("failed to download for indicator '%s' for crop '%s' of type '%s' : %s",
                           x[["ind"]],
                           x[["crop"]],
                           x[["type"]],
                           e$message))

           return(NULL)

         })



       })


#### add more rasters 

get_gaez_resone_ts(theme_var = "LD1", dsn = "data-raw/gaez/rasters_raw")

get_gaez_resone_ts(theme_var = "LGD", dsn = "data-raw/gaez/rasters_raw")

get_gaez_resone_ts(theme_var = "NDD", dsn = "data-raw/gaez/rasters_raw")

get_gaez_resone_ts(theme_var = "NDR", dsn = "data-raw/gaez/rasters_raw")

resone_list <- c("RFM", "RI2", "RQ1", "RQ2", "RQ3", "RQ4", 
                 "WDE", "LGP","MCI","MCR")

lapply(X = resone_list,
       FUN = function(x){
         
         get_gaez_resone_ts(theme_var = x,
                            dsn = "data-raw/gaez/rasters_raw")
         
       })


### include LR-LCC
lapply(X = 1:12,
       FUN = get_gaez_lrlcc,
       dsn = "data-raw/gaez/rasters_raw")


### download the res05 data now

crop_list <- c("MZE", "RCW", "WHE")

param_grid <- 
  expand.grid(var = c("SIX", "SUX", "SXX", "SCX"),
              crop = crop_list, 
              water_supply = c("H", "L"),
              irr = c(TRUE, FALSE), 
              time_period = "HP0120")

pmap(.l = param_grid,
     .f = ~ get_gaez_resfive_v2(var = ..1,
                                crop_code = ..2,
                                irrigated = ..4,
                                water_supply = ..3,
                                dsn = "data-raw/gaez/rasters_raw",
                                time_period = ..5)) 
