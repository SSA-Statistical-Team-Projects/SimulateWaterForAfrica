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

# ###### first download the irrigated
# lapply(X = crop_list,
#        FUN = get_gaez_restwo,
#        var = "YLD",
#        dsn = "data-raw/gaez/rasters_raw")
# 
# ##### then the non-irrigated
# lapply(X = crop_list,
#        FUN = get_gaez_restwo,
#        var = "YLD",
#        irrigated = FALSE,
#        dsn = "data-raw/gaez/rasters_raw")
# 

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



# #### return the set of files in the KG2 folder
# kg2url_list <- list_gcs_files("https://storage.cloud.google.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/MAPSET/RES01-KG2/")
# 
# ## keep only the tifs.
# kg2url_list <- kg2url_list[grepl(".tif", kg2url_list)]
# 
# ## download only focal period 2021 to 2040 projections
# kg2url_list <- kg2url_list[grepl("FP2140", kg2url_list)]
# 
# 
# lapply(X = kg2url_list,
#        FUN = function(x){
#          
#          x <- paste0("https://storage.googleapis.com/fao-gismgr-gaez-v5-data/",
#                      x)
#          
#          download.file(destfile = paste("data-raw/gaez/rasters_raw", 
#                                         basename(x), 
#                                         sep = "/"),
#                        url = x,
#                        mode = "wb")
#          
#          
#        })
# 
# ### likewise for hte PFR data
# pfrurl_list <- list_gcs_files("https://storage.cloud.google.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/MAPSET/RES01-PFR/")
# pfrurl_list <- pfrurl_list[grepl(".tif", pfrurl_list)]
# 
# 
# lapply(X = pfrurl_list,
#        FUN = function(x){
#          
#          x <- paste0("https://storage.googleapis.com/fao-gismgr-gaez-v5-data/",
#                      x)
#          
#          download.file(destfile = paste("data-raw/gaez/rasters_raw", 
#                                         basename(x), 
#                                         sep = "/"),
#                        url = x,
#                        mode = "wb")
#          
#          
#        })

# #### download all NDD, WDE files with .tif extension
# 
# download_gcsdir_tifs(folder = "RES01-NDD")
# 
# download_gcsdir_tifs(folder = "RES01-WDE")
# 
# 
# file_list <- c("LGD", "NDR", "RFM", "RI2", "RQ1", 
#                "RQ2", "RQ3", "RQ4", "LD1")
# 
# lapply(X = paste0("RES01-", file_list),
#        FUN = download_gcsdir_tifs)
# 
# 
# #### download some more RES02 data
# 
# ind_list <- c("YLD", "WDE", "ETA", "TSC", "FC2")
# 
# grid_dt <- 
#   expand.grid(crop = crop_list, 
#               ind = ind_list) %>%
#   apply(MARGIN = 2, FUN = as.character) %>%
#   as.data.frame() %>%
#   mutate(row_id = row_number()) %>%
#   group_split(row_id, .keep = FALSE)
# 
# 
# 
# ###### first download the irrigated
# lapply(X = crop_list,
#        FUN = get_gaez_restwo,
#        var = "YLD",
#        dsn = "data-raw/gaez/rasters_raw")
# 
# ##### then the non-irrigated
# lapply(X = crop_list,
#        FUN = get_gaez_restwo,
#        var = "YLD",
#        irrigated = FALSE,
#        dsn = "data-raw/gaez/rasters_raw")
# 
# 
# 
# lapply(X = grid_dt,
#        FUN = function(x){
#          
#          message(sprintf("Downloading indicator '%s' for crop '%s'...", 
#                          x[["ind"]], 
#                          x[["crop"]]))
#          
#          tryCatch({
#            
#            get_gaez_restwo(crop_code = x[["crop"]], 
#                            var = x[["ind"]], 
#                            dsn = "data-raw/gaez/rasters_raw")
#            
#          }, error = function(e){
#            
#            message(sprintf("failed to download for indicator '%s' for crop '%s' : %s", 
#                            x[["ind"]], 
#                            x[["crop"]],
#                            e$message))
#            
#            return(NULL)
#            
#          })
#          
#          
#          
#        })
# 
# 
# ind_list <- c("SCX", "SIX", "SUX", "SXX")
# 
# crop_list <- c("BRL", "MZE", "RCW", "SRG", "WHE")
# 
# grid_dt <- 
#   expand.grid(crop = crop_list,
#               ind = ind_list) %>%
#   apply(MARGIN = 2, FUN = as.character) %>%
#   as.data.frame() %>%
#   mutate(row_id = row_number()) %>%
#   group_split(row_id, .keep = FALSE)
# 
# 
# 
# lapply(X = grid_dt,
#        FUN = function(x){
#          
#          message(sprintf("Downloading indicator '%s' for crop '%s'...", 
#                          x[["ind"]], 
#                          x[["crop"]]))
#          
#          ### for the irrigated cropland
#          tryCatch({
#            
#            get_gaez_resfive(crop_code = x[["crop"]], 
#                             var = x[["ind"]], 
#                             dsn = "data-raw/gaez/rasters_raw")
#            
#          }, error = function(e){
#            
#            message(sprintf("failed to download for indicator '%s' for crop '%s' : %s", 
#                            x[["ind"]], 
#                            x[["crop"]],
#                            e$message))
#            
#            return(NULL)
#            
#          })
#          
#          ### for rainfed cropland
#          tryCatch({
#            
#            get_gaez_resfive(crop_code = x[["crop"]], 
#                             var = x[["ind"]], 
#                             dsn = "data-raw/gaez/rasters_raw",
#                             irrigated = FALSE)
#            
#          }, error = function(e){
#            
#            message(sprintf("failed to download for indicator '%s' for crop '%s' : %s", 
#                            x[["ind"]], 
#                            x[["crop"]],
#                            e$message))
#            
#            return(NULL)
#            
#          })
#          
#          
#        })
# 
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

resone_list <- c("RFM", "RI2", "RQ1", "RQ2", "RQ3", "RQ4", "WDE", "LGP")

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

crop_list <- c("BRL", "MZE", "RCW", "SRG", "WHE", "MLT")

param_grid <- 
  expand.grid(var = c("S1X", "SUX", "SXX", "SCX"),
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
 








