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
gc(reset = TRUE)

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
subs_afr_shp <- st_transform(subs_afr_shp, 
                             crs = crs(raster::stack("data-raw/gaez/rasters_raw/GAEZ-V5.RES06-YLD.MLT.WST.tif"))) # or any sample GAEZ raster

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
        dplyr::rename(c(longitude = x, latitude = y))
      
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
  
  crop_merged$crop_code <-  tolower(crop)
  
  if (identical(crop, crop_list[[1]])) {
    fin_df <- crop_merged 
  } else { 
    fin_df = bind_rows(fin_df,
                       crop_merged)
  }
  
  rm(crop_merged,crop,var,typ)
  gc(reset = TRUE)
}

# 2.2 Generate a unique dataset & polygons
#%%%%%%%%%%%%%%%%%%%%

unique_sf <- st_as_sf(fin_df %>%
                        dplyr::distinct(longitude, latitude),
                      coords = c("longitude", "latitude"),
                      crs = st_crs(subs_afr_shp))

# Spatial join to attach country name/attributes
unique_sf <- st_join(unique_sf, subs_afr_shp, join = st_is_within_distance, dist = 0.0001) %>%
  dplyr::filter(!is.na(NAME_EN))   # keep only matched points-- dropping grids in the ocean

unique_sf$srno <- 1:nrow(unique_sf)

# Extract coordinates back into columns (for CSV export)
unique_sf <- unique_sf %>%
  dplyr::mutate(longitude = st_coordinates(.)[,1],
                latitude = st_coordinates(.)[,2]) %>%
  dplyr::select(c(longitude,latitude,srno, WB_NAME))

# save as RDS for future R use
saveRDS(unique_sf, 
        "data-clean/working_data/gaez/gaez_unique_grid_points.RDS")

# Extracting grid polygons for GAEZ
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ras <- raster::stack("data-raw/gaez/rasters_raw/GAEZ-V5.RES06-PRD.WHE.WSI.tif")
#st_crs(ras) <- st_crs(subs_afr_shp)
ras <- crop(ras, extent(subs_afr_shp))
ras <- mask(ras, subs_afr_shp)

grid_polygon <- rasterToPolygons(ras,
                                 dissolve = FALSE,
                                 na.rm = FALSE)

grid_polygon <- sf::st_as_sf(grid_polygon)
grid_polygon <- st_join(grid_polygon,
                        unique_sf)

grid_polygon <- grid_polygon %>% 
  dplyr::filter(!is.na(srno)) %>% 
  dplyr::select(srno)

# saving the file as a shapefile
st_write(grid_polygon,
         "data-clean/working_data/gaez/grid_polygon/unique_grid_polygons.shp")

rm(grid_polygon)
gc(reset = TRUE)

## Saving findf
#adding srno
fin_df <- left_join(fin_df,
                     unique_sf %>% 
                       as.data.frame() %>%
                       dplyr::select(-geometry),
                     by = c("latitude","longitude"))

fin_df <- fin_df %>%
  dplyr::filter(!is.na(srno))

saveRDS(fin_df,
        "data-clean/gaez/gaez_crop_2020.RDS")
rm(fin_df)
gc(reset = TRUE)

# 2.3 Controls
#%%%%%%%%%%%%%%%%%%%%

crop_list2 <- c("MAIZ","RICW","WHEA")

# RES02
for (crop in crop_list2) {
  for (var in c("CYL", "ETA", "FC2", "TSC", "WDE")) {
    for (typ in c("I","R")) {
      for(int in c("H","L")) {
        
        file_pattern <- paste0("data-raw/gaez/rasters_raw/GAEZ-V5.RES02-",var,
                               ".HP0120.AGERA5.HIST.",crop,".",int,typ,"LM",".tif")
        
        if (length(file_pattern) == 0) next
        ras <- raster::stack(file_pattern)
        ras <- crop(ras, extent(subs_afr_shp))
        ras <- mask(ras, subs_afr_shp)
        
        df <- as.data.frame(ras, xy = TRUE) %>%
          dplyr::rename(c(longitude = x, latitude = y))
        
        var_name <- paste0(tolower(var), "_", 
                           int, "_",
                           ifelse(typ == "I", 
                                  "irri",
                                  "rainfed"))
        
        names(df)[names(df) == paste0("GAEZ.V5.RES02.",
                                      var,".HP0120.AGERA5.HIST.",
                                      crop,".",int,typ,"LM")] <- var_name
        
        
        if (identical(var, "CYL") & identical(typ,"I") & int == "H") {
          crop_merged <- df
        } else { 
          crop_merged = left_join(crop_merged,
                                  df,
                                  by = c("longitude", "latitude"))
        }
        rm(df,ras,file_pattern)
        
      }
    }
    
  }
  
  crop_merged$crop_code <-  crop
  
  if (identical(crop, crop_list2[[1]])) {
    finc_df <- crop_merged 
  } else { 
    finc_df = bind_rows(finc_df,
                       crop_merged)
  }
  
  rm(crop_merged,crop,var,typ,int)
  gc(reset = TRUE)
}

# RES05
for (crop in crop_list) {
  
  if(crop == "RCW") { crop2 = "RICW"}
  if(crop == "MZE") { crop2 = "MAIZ"}
  if(crop == "WHE") { crop2 = "WHEA"}
  
  for (var in c("SUX","SXX","SIX","SCX")) {
    for (typ in c("I","R")) {
      for(int in c("H","L")) {
        
        file_pattern <- paste0("data-raw/gaez/rasters_raw/GAEZ-V5.RES05-",var,
                               ".HP0120.AGERA5.HIST.",crop,".",int,typ,"LM",".tif")
        
        if (length(file_pattern) == 0) next
        ras <- raster::stack(file_pattern)
        ras <- crop(ras, extent(subs_afr_shp))
        ras <- mask(ras, subs_afr_shp)
        
        df <- as.data.frame(ras, xy = TRUE) %>%
          dplyr::rename(c(longitude = x, latitude = y))
        
        var_name <- paste0(tolower(var), "_", 
                           int, "_",
                           ifelse(typ == "I", 
                                  "irri",
                                  "rainfed"))
        
        names(df)[names(df) == paste0("GAEZ.V5.RES05.",
                                      var,".HP0120.AGERA5.HIST.",
                                      crop,".",int,typ,"LM")] <- var_name
        
        
        if (identical(var, "SUX") & identical(typ,"I") & int == "H") {
          crop_merged <- df
        } else { 
          crop_merged = left_join(crop_merged,
                                  df,
                                  by = c("longitude", "latitude"))
        }
        rm(df,ras,file_pattern)
        
      }
    }
    
  }
  
  crop_merged$crop_code <-  crop2
  
  if (identical(crop, crop_list[[1]])) {
    res05_df <- crop_merged 
  } else { 
    res05_df = bind_rows(res05_df,
                         crop_merged)
  }
  
  rm(crop_merged,crop,crop2,var,typ,int)
  gc(reset = TRUE)
}

finc_df <- left_join(finc_df,
                     res05_df,
                     by = c("latitude","longitude","crop_code"))

rm(res05_df)
gc(reset = TRUE)

finc_df <- finc_df %>%
  dplyr::mutate(crop_code = case_when(crop_code == "MAIZ" ~ "mze",
                                      crop_code == "RICW" ~ "rcw",
                                      crop_code == "WHEA" ~ "whe",
                                      TRUE ~ NA_character_))

#adding srno
finc_df <- left_join(finc_df,
                     unique_sf %>% 
                       as.data.frame() %>%
                       dplyr::select(-c(geometry,WB_NAME)),
                     by = c("latitude","longitude"))

finc_df <- finc_df %>%
  dplyr::filter(!is.na(srno))

saveRDS(finc_df,
        "data-clean/gaez/gaez_controls_crop_2020.RDS")
rm(finc_df)
gc(reset = TRUE)

# RES01

for (var in c("LD1", "LGD","NDD","NDR", "RFM", "RI2", "RQ1", "RQ2", 
              "RQ3", "RQ4", "WDE", "LGP","MCI","MCR")) {
  
  file_pattern <- paste0("data-raw/gaez/rasters_raw/GAEZ-V5.RES01-",var,
                         "-TS.2020.tif")
  
  if (length(file_pattern) == 0) next
  ras <- raster::stack(file_pattern)
  ras <- crop(ras, extent(subs_afr_shp))
  ras <- mask(ras, subs_afr_shp)
  
  df <- as.data.frame(ras, xy = TRUE) %>%
    dplyr::rename(c(longitude = x, latitude = y))
  
  names(df)[names(df) == paste0("GAEZ.V5.RES01.",
                                var,".TS.2020")] <- tolower(var)
  
  
  if (identical(var, "LD1")) {
    findf_g <- df
  } else { 
    findf_g = left_join(findf_g,
                        df,
                        by = c("longitude", "latitude"))
  }
  rm(df,ras,file_pattern, var_name, var)
  gc(reset = TRUE)
}


#adding srno
findf_g <- left_join(findf_g,
                      unique_sf %>% 
                        as.data.frame() %>%
                        dplyr::select(-c(geometry,WB_NAME)),
                      by = c("latitude","longitude"))

findf_g <- findf_g %>%
  dplyr::filter(!is.na(srno))
                     
saveRDS(findf_g,
        "data-clean/gaez/gaez_controls_2020.RDS")

# LR

for (var in c(1:12)) {
  
  if(var <= 9) {
    file_pattern <- paste0("data-raw/gaez/rasters_raw/GAEZ-V5.LR-LCC.LC0",var,
                           ".tif")
    var <- paste0("0",var)
  } else {
    file_pattern <- paste0("data-raw/gaez/rasters_raw/GAEZ-V5.LR-LCC.LC",var,
                           ".tif")
  }
  
  
  if (length(file_pattern) == 0) next
  ras <- raster::stack(file_pattern)
  ras <- crop(ras, extent(subs_afr_shp))
  ras <- mask(ras, subs_afr_shp)
  
  df <- as.data.frame(ras, xy = TRUE) %>%
    dplyr::rename(c(longitude = x, latitude = y))
  
  var_name <- case_when(var == "01" ~ "land_artif_surf_perc",
                        var == "02" ~ "land_cropland_perc",
                        var == "03" ~ "land_grassland_perc",
                        var == "04" ~ "land_tree_cover_perc",
                        var == "05" ~ "land_shrub_perc",
                        var == "06" ~ "land_herbaceous_flooded_perc",
                        var == "07" ~ "land_mangrove_perc",
                        var == "08" ~ "land_sparse_veg_perc",
                        var == "09" ~ "land_bare_perc",
                        var == 10 ~ "land_perm_snow_perc",
                        var == 11 ~ "land_water_perc",
                        var == 12 ~ "land_crop_irri_equip_perc")
  
  names(df)[names(df) == paste0("GAEZ.V5.LR-LCC.LC",
                                var)] <- var_name
  
  if(var == "01") {
    findf_g2 <- df
  } else {
    findf_g2 = left_join(findf_g2,
                         df,
                         by = c("longitude", "latitude"))
  }
  
  rm(df,ras,file_pattern, var_name, var)
}

# adding soil data in
ras <- raster::stack("data-raw/gaez/rasters_raw/SOILFER.SLOPE-MED.tif")
ras <- crop(ras, extent(subs_afr_shp))
ras <- mask(ras, subs_afr_shp)

df <- as.data.frame(ras, xy = TRUE) %>%
  dplyr::rename(c(longitude = x, latitude = y))

findf_g2 <- left_join(findf_g2,
                      df,
                      by = c("latitude","longitude"))
rm(ras,df)

#adding srno
findf_sf <- st_as_sf(findf_g2, 
                     coords = c("longitude", "latitude"), 
                     crs = st_crs(unique_sf))

rm(findf_g2)
gc(reset = TRUE)

findf_joined <- st_join(findf_sf, 
                        grid_polygon)

rm(findf_sf)
gc(reset = TRUE)

# summarize by polygon
findf_summary <- findf_joined %>%
  st_drop_geometry() %>%   # drop point geometry to allow summarization
  as.data.frame() %>%
  dplyr::group_by(srno) %>%    # replace with your polygon identifier
  dplyr::summarise(across(where(is.numeric), mean, na.rm = TRUE))

findf_summary <- findf_summary %>%
  dplyr::filter(!is.na(srno))

saveRDS(findf_summary,
        "data-clean/gaez/gaez_controls_highres_2020.RDS")

rm(findf_summary, findf_joined, unique_sf)
gc(reset = TRUE)