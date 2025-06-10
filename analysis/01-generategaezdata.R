#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Generating GAEZ Data
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will:
# 1. Load the GAEZ raster files and crop them for Africa.
# 2. Then, this file will load data and merge data to form two working datasets:
#   a. Crop X Year X Geo-coord level
#   b. Unique Geo-coord dataset
#   c. Controls dataset

rm(list=ls())

# Loading Packages
pak_list <- c("raster", "sf", "tidyverse", "data.table", "jsonlite","dplyr")

lapply(pak_list, function(pkg) {
  suppressMessages(suppressWarnings(
    library(pkg, character.only = TRUE)
  ))
})
lapply(pak_list, library, character.only = TRUE)

#1191775 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Loading the Shapefile
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subs_afr_shp <- st_read("data-raw/shapefile/WB_Boundaries_GeoJSON_highres/WB_countries_Admin0.geojson") %>%
  dplyr::filter(REGION_WB == "Sub-Saharan Africa")

# Matching the crs of the shapefile to those of GAEZ files
# I do this instead of the other way round in order to reduce the chances of perturbations in coordinates
# which lead to coordinate mismatches
st_crs(subs_afr_shp) <- crs(raster::stack("data-raw/gaez/rasters_raw/GAEZ-V5.RES06-YLD.MLT.WST.tif")) # or any sample GAEZ raster

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Loading and Merging / Appending Data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# I will construct a dataset at the crop x year x geo_code level

# 2.1 Crop Data
#%%%%%%%%%%%%%%%%%%%%

crop_list <- c("MZE", "RCW", "WHE")
indicators <- c("YLD", "PRD", "HAR")
types <- c("T","I", "R")  # I = irrigated, R = rainfed, T= total

## RES 06

for (crop in crop_list) {
  for (var in indicators) {
    for (typ in types) {
      
      file_pattern <- paste0("data-raw/gaez/rasters_raw/GAEZ-V5.RES06-",
                             var, ".", crop, ".WS", typ, ".tif")
      
      if (length(file_pattern) == 0) next
      ras <- raster::stack(file_pattern)
      ras <- crop(ras, extent(subs_afr_shp))
      ras <- mask(ras, subs_afr_shp)
      
      df <- as.data.frame(ras, xy = TRUE) %>%
        rename(longitude = x, latitude = y)
      
      var_name <- paste0(tolower(var), "_", 
                         ifelse(typ == "T", 
                                "tot",
                                ifelse(typ == "I", 
                                       "irri", 
                                       "rainfed")))
      
      names(df)[names(df) == paste0("GAEZ.V5.RES06.",
                                    var, ".", crop, ".WS", typ)] <- var_name
      
      
      if (identical(var, indicators[[1]]) & identical(typ,types[[1]])) {
        crop_merged <- df
      } else { 
        crop_merged = left_join(crop_merged,
                                df,
                                by = c("longitude", "latitude"))
      }
      rm(df,ras,file_pattern)
    }
  
  }
  
  crop_merged$crop_code <-  crop
  
  if (identical(crop, crop_list[[1]])) {
    fin_df <- crop_merged 
  } else { 
    fin_df = bind_rows(fin_df,
                       crop_merged)
  }
  
  rm(crop_merged)
  gc(reset = TRUE)
}

saveRDS(fin_df,
        paste0("data-clean/gaez/gaez_crop_2020.RDS"))
rm(fin_df)

# 2.2 Generate a unique dataset
#%%%%%%%%%%%%%%%%%%%%

# # Build a Unique Dataset
# # Constructing a set of unique points
# ras <- raster::stack("data-raw/gaez/rasters_raw/GAEZ-V5.RES06-PRD.SRG.WSI.tif")
# ras <- crop(ras, extent(subs_afr_shp))
# ras <- mask(ras, subs_afr_shp)
# 
# # Convert raster to points (this will retain x, y, and raster value)
# ras_points <- rasterToPoints(ras, spatial = TRUE)
# 
# # Convert raster points to `sf`
# ras_sf <- st_as_sf(ras_points)
# 
# # Spatial join to assign country name
# # Assuming the country shapefile has a field like `country_name`
# ras_joined <- st_join(ras_sf, 
#                       subs_afr_shp["ISO_A3"], 
#                       left = FALSE)
# 
# # Drop rows with no country match (if any remain)
# ras_joined <- ras_joined %>% filter(!is.na(country_name))
# 
# saveRDS(agg_unique_sf,paste0("data-clean/working_data/gaez/gaez_unique_grid_points.RDS"))
# write.csv(agg_unique_sf,paste0("data-clean/working_data/gaez/gaez_unique_grid_points.csv"))

# 2.3 Controls
#%%%%%%%%%%%%%%%%%%%%

crop_list2 <- c("MZSI","RICW","WHEA")
indicators2 <- c("CYL","WDE","ETA","TSC","FC2")



ras <- raster::stack("data-raw/gaez/rasters_raw/GAEZ-V5.RES02-FC2.FP2140.GFDL-ESM4.SSP126.MAIZ.HILM.tif")
ras <- crop(ras, extent(subs_afr_shp))
ras <- mask(ras, subs_afr_shp)
df <- as.data.frame(ras, xy = TRUE) %>%
  rename(longitude = x, latitude = y)
