################################################################################
################# FOOD PRICE ESTIMATES BY PRODUCT AND MARKET ###################
################################################################################

### query the DDH api system for food prices for millet, barley, maize, rice, sorghum,
### and wheat


library(httr)
library(jsonlite)
library(data.table)


get_fpprice_data <- function(param_dt){
  
  base_url <- "https://microdata.worldbank.org/index.php/api/tables/data/fcv/wld_2021_rtfp_v02_m"
  
  dt_list <-
    lapply(split(param_dt, 1:nrow(param_dt)),
           FUN = function(x){
             
             res <- httr::GET(paste0(base_url,
                                     "?limit=",
                                     x$limit,
                                     "&offset=",
                                     x$offset))
             
             dt <- jsonlite::fromJSON(rawToChar(res$content))$data
             
             return(dt)
             
           })
  
  dt <- Reduce(x = dt_list,
               f = "rbind")
  
  return(dt)
  
}

res <- httr::GET("https://microdata.worldbank.org/index.php/api/tables/data/fcv/wld_2021_rtfp_v02_m?limit=5000&offset=15")


dt <- jsonlite::fromJSON(rawToChar(res$content))
# dt <- jsonlite::fromJSON(httr::content(res, "text"), simplifyDataFrame = FALSE)

### determine the set of limits and offsets
param_dt <-
  data.table(limit = c(rep(5000, round(dt$total/5000))),
             count = 0:length(c(rep(5000, dt$total %/% 5000))))

param_dt[, offset := limit * count]

price_dt <- get_fpprice_data(param_dt = param_dt)

saveRDS(price_dt, "data-raw/foodprices.RDS")



