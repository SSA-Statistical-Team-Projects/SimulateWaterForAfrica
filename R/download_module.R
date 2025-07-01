#' A function to download GAEZ data
#' 
#' This function will be used to download GAEZ from the google cloud storage drive. 
#' 
#' @param var the name of variables of interest within a specific theme
#' @param crop_code the 4 letter crop name
#' @param irrigated a logical, if TRUE the irrigated raster will be download otherwise, it is rainfed
#' @param dsn the folder where the data will be stored
#' @param time_period the time period (e.g., HP8100 for 1981-2000, HP0120 for 2001-2020, FP2140 for 2021-2040, 
#' FP4160 for 2041-2060, FP6180 for 2061-2080 and FP8100 for 2081-2100) or the year for time-series data


get_gaez_restwo <- function(var,
                            crop_code,
                            irrigated = T,
                            dsn,
                            time_period = "FP2140"){
  
  base_link <- "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/MAPSET"
  
  theme_var <- paste0("RES02", "-", var)
  
  if (irrigated == TRUE){
    
    irr <- "HILM"
    
  } else {
    
    irr <- "HRLM"
    
  }
  
  full_link <- paste(base_link,
                     theme_var,
                     paste0("GAEZ-V5", 
                            "." , 
                            theme_var,
                            ".",
                            time_period,
                            ".GFDL-ESM4.SSP126.", 
                            crop_code, 
                            ".", 
                            irr,
                            ".tif"),
                     sep = "/")
  
  download.file(destfile = paste(dsn, basename(full_link), sep = "/"),
                url = full_link,
                mode = "wb")
  

}


get_gaez_restwo_v2 <- function(var,
                               crop_code,
                               water_supply = "H",
                               irrigated = T,
                               dsn,
                               time_period = "FP2140"){
  
  base_link <- "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/MAPSET"
  
  theme_var <- paste0("RES02", "-", var)
  
  irr_dt <- 
    tibble(ws = water_supply,
           irrigated = ifelse(isTRUE(irrigated), "I", "R"),
           ext = "LM") |>
    mutate(water_supply = paste0(ws, irrigated, ext))

  

  if (grepl(pattern = "HP", x = time_period)) {
    
    time_period <- paste0(time_period, ".AGERA5.HIST.")
    
  } else {
    
    time_period <- paste0(time_period, ".GFDL-ESM4.SSP126.")
  }
  
  full_link <- paste(base_link,
                     theme_var,
                     paste0("GAEZ-V5", 
                            "." , 
                            theme_var,
                            ".",
                            time_period,
                            crop_code, 
                            ".", 
                            irr_dt$water_supply,
                            ".tif"),
                     sep = "/")
  
  download.file(destfile = paste(dsn, basename(full_link), sep = "/"),
                url = full_link,
                mode = "wb")
  
  
}

#' Download Koeppen-Geigger (2-character) classification data
#' 
#' This function will download KP2 classification data from the google cloud storage drive
#' 
#' @param fcp the focal period options "FP2140" for focal period 2021-2040, "FP4160" for
#' 2041-2060 and likewise for "FP6180" and "FP8100
#' @param climate_model the climate model used options are IPSL-CM6A-LR, GFDL-ESM4 etc
#' @param scenario the scenario used for instance SSP126 is the shared socioeconomic pathway
#' 
#' 


get_gaez_kg2 <- function(fcp,
                         climate_model,
                         scenario){
  
  
  
}

#' Return the set of files in each GCS repository
#' 
#' A function to take in a GCS subdirector url and return the set of files in it. 
#' 
#' @param folder_url the link
#' 
#' @export

list_gcs_files <- function(folder_url) {
  if (!grepl("storage.cloud.google.com", folder_url)) {
    stop("URL must be a Google Cloud Storage folder link")
  }
  bucket <- sub("https://storage.cloud.google.com/([^/]+).*", "\\1", folder_url)
  prefix <- sub(sprintf("https://storage.cloud.google.com/%s/?", bucket), "", folder_url)
  prefix <- gsub("^/", "", prefix)  # remove leading slash
  if (!endsWith(prefix, "/")) prefix <- paste0(prefix, "/")
  all_files <- c()
  page_token <- NULL
  repeat {
    api_url <- sprintf("https://storage.googleapis.com/storage/v1/b/%s/o?prefix=%s",
                       bucket, URLencode(prefix, reserved = TRUE))
    if (!is.null(page_token)) {
      api_url <- paste0(api_url, "&pageToken=", page_token)
    }
    res <- jsonlite::fromJSON(api_url)
    if ("items" %in% names(res)) {
      all_files <- c(all_files, res$items$name)
    }
    if (!"nextPageToken" %in% names(res)) break
    page_token <- res$nextPageToken
  }
  # Filter to files in the exact folder (not subfolders)
  all_files[grepl(sprintf("^%s[^/]+$", prefix), all_files)]
}


download_gcsdir_tifs <- function(folder,
                                 dsn = "data-raw/gaez"){
  
  base_url <- "https://storage.cloud.google.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/MAPSET/"
  
  url_link <- paste0(base_url, folder, "/")
  
  url_list <- list_gcs_files(url_link)
  
  url_list <- url_list[grepl(".tif", url_list)]
  
  
  lapply(X = url_list,
         FUN = function(x){
           
           x <- paste0("https://storage.googleapis.com/fao-gismgr-gaez-v5-data/",
                       x)
           
           download.file(destfile = paste(dsn, 
                                          basename(x), 
                                          sep = "/"),
                         url = x,
                         mode = "wb")
           
           
         })
  
  
}



#' A function for downloading GAEZ RES05
#' 
#' This function will be used to download GAEZ from the google cloud storage drive. 
#' 
#' @param var the name of variables of interest within a specific theme
#' @param crop_code the 4 letter crop name
#' @param irrigated a logical, if TRUE the irrigated raster will be download otherwise, it is rainfed
#' @param dsn the folder where the data will be stored
#' 
#' 


get_gaez_resfive <- function(var,
                             crop_code,
                             dsn,
                             irrigated = T){
  
  
  base_link <- "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/MAPSET"
  
  theme_var <- paste0("RES05", "-", var)
  
  if (irrigated == TRUE){
    
    irr <- "HILM"
    
  } else {
    
    irr <- "HRLM"
    
  }
  
  full_link <- paste(base_link,
                     theme_var,
                     paste0("GAEZ-V5", 
                            "." , 
                            theme_var,
                            ".FP2140.ENSEMBLE.SSP126.", 
                            crop_code, 
                            ".", 
                            irr,
                            ".tif"),
                     sep = "/")
  
  download.file(destfile = paste(dsn, basename(full_link), sep = "/"),
                url = full_link,
                mode = "wb")
  
  
  
}

get_gaez_resfive_v2 <- function(var,
                                crop_code,
                                water_supply,
                                time_period = "FP2140",
                                dsn,
                                irrigated = T){
  
  
  base_link <- "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/MAPSET"
  
  theme_var <- paste0("RES05", "-", var)
  
  irr_dt <- 
    tibble(ws = water_supply,
           irrigated = ifelse(isTRUE(irrigated), "I", "R"),
           ext = "LM") |>
    mutate(water_supply = paste0(ws, irrigated, ext))
  
  
  
  if (grepl(pattern = "HP", x = time_period)) {
    
    time_period <- paste0(time_period, ".AGERA5.HIST.")
    
  } else {
    
    time_period <- paste0(time_period, ".ENSEMBLE.SSP126.")
  }
  
  full_link <- paste(base_link,
                     theme_var,
                     paste0("GAEZ-V5", 
                            "." , 
                            theme_var,
                            ".",
                            time_period, 
                            crop_code, 
                            ".", 
                            irr_dt$water_supply,
                            ".tif"),
                     sep = "/")
  
  download.file(destfile = paste(dsn, basename(full_link), sep = "/"),
                url = full_link,
                mode = "wb")
  
  
  
}



#' A function for downloading GAEZ RES05
#' 
#' This function will be used to download GAEZ from the google cloud storage drive. 
#' 
#' @param var the name of variables of interest within a specific theme
#' @param crop_code the 4 letter crop name
#' @param irrigated a logical, if TRUE the irrigated raster will be download otherwise, it is rainfed
#' @param dsn the folder where the data will be stored
#' @param type a character, either "WSI", "WSR" or "WST"
#' 
#' 


get_gaez_ressix <- function(var,
                            crop_code,
                            type,
                            dsn){
  
  
  base_link <- "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/MAPSET"
  
  theme_var <- paste0("RES06", "-", var)
  

  full_link <- paste(base_link,
                     theme_var,
                     paste0("GAEZ-V5", 
                            "." , 
                            theme_var,
                            ".",
                            crop_code, 
                            ".", 
                            type,
                            ".tif"),
                     sep = "/")
  
  download.file(destfile = paste(dsn, basename(full_link), sep = "/"),
                url = full_link,
                mode = "wb")
  
  
  
}


get_gaez_resone_ts <- function(theme_var,
                               year = 2020,
                               dsn){
  
  base_link <- "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/MAPSET"
  
  theme_var <- paste0("RES01-", theme_var, "-TS")
  
  full_link <- paste(base_link,
                     theme_var,
                     paste0("GAEZ-V5", 
                            "." , 
                            theme_var,
                            ".",
                            year,
                            ".tif"),
                     sep = "/")
  
  download.file(destfile = paste(dsn, basename(full_link), sep = "/"),
                url = full_link,
                mode = "wb")
  
  
}

#' Get the landcover datasets
#' 
#' @param class, an `integer`, the landcover class number 
#' @param dsn `character` the file path where the data will be stored
#' 
#' 
get_gaez_lrlcc <- function(class,
                           dsn){
  
  
  base_link <- "https://storage.googleapis.com/fao-gismgr-gaez-v5-data/DATA/GAEZ-V5/MAPSET"
  
  lcc_file <- paste0("/LR-LCC/GAEZ-V5.LR-LCC.LC",
                     sprintf("%02d", class),
                     ".tif")
  
  full_link <- paste0(base_link, lcc_file)
  
  download.file(destfile = paste(dsn, basename(full_link), sep = "/"),
                url = full_link,
                mode = "wb")
  
}



























