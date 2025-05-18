#' A function to download GAEZ data
#' 
#' This function will be used to download GAEZ from the google cloud storage drive. 
#' 
#' @param var the name of variables of interest within a specific theme
#' @param crop_code the 4 letter crop name
#' @param irrigated a logical, if TRUE the irrigated raster will be download otherwise, it is rainfed
#' @param dsn the folder where the data will be stored


get_gaez_restwo <- function(var,
                            crop_code,
                            irrigated = T,
                            dsn){
  
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
                            ".FP2140.GFDL-ESM4.SSP126.", 
                            crop_code, 
                            ".", 
                            irr,
                            ".tif"),
                     sep = "/")
  
  download.file(destfile = paste(dsn, basename(full_link), sep = "/"),
                url = full_link,
                mode = "wb")
  

}






#' A function to create dictionary of file extensions and the functions that
#' read them so that a do.call can be applied to read in the results
#'

download_dictionary <- function(){
  
  dict_dt <- data.table(file_ext = c("tiff", "tif", "gz"),
                        opener_function = c("raster", "raster", "gunzip_reader"))
  
  return(dict_dt)
}


#' A function to download and read in a file from the internet
#'
#' @param url a link URL
#' @param ... additional arguments to be used in `download.file()`
#'

download_reader <- function(dsn,
                            url,
                            ...) {
  
  ext <- paste0(".", tools::file_ext(url))
  
  # temp_file <- tempfile(fileext = ext)
  
  utils::download.file(url = url,
                       destfile = paste(dsn, basename(url), sep = "/"),
                       mode = "wb",
                       ...)
  
  dict_dt <- download_dictionary()
  
  opener_chr <- dict_dt[file_ext == tools::file_ext(url), opener_function]
  
  do.call(opener_chr, paste(dsn, basename(url), sep = "/")) ->  raster_obj
  
  # raster_obj <- crop(raster_obj, extent(shp_dt))
  
  return(raster_obj)
  
}
