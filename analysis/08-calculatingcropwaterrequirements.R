#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Calculating Crop Water Requirements
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will:

rm(list=ls())
gc(reset = TRUE)

pacman::p_load(
  
  ncdf4, raster, chron, tidyverse, lubridate, sf, exactextractr, dplyr,
  rgdal, haven, spatialEco, geosphere, rasterVis
  
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Loading data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unique_df <- readRDS("data-clean/working_data/gaez/gaez_unique_grid_points.RDS")

crop_df <- readRDS("data-clean/gaez/gaez_crop_2020.RDS")
control_df <- readRDS("data-clean/gaez/gaez_controls_2020.RDS")
control_crdf <- readRDS("data-clean/gaez/gaez_controls_crop_2020.RDS")
control_cr_hrdf <- readRDS("data-clean/gaez/gaez_controls_highres_2020.RDS")

sf::sf_use_s2(FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Merging data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Adding in controls

cereal_list <- c("mze","rcw","whe")

crop_df <- left_join(crop_df,
                     control_df %>%
                       as.data.frame() %>%
                       dplyr::select(-c(latitude,longitude)),
                     by=c("srno"))

rm(control_df)
gc(reset = T)

crop_df <- left_join(crop_df,
                     control_crdf %>%
                       as.data.frame() %>%
                       dplyr::select(-c(latitude,longitude)),
                     by=c("srno","crop_code"))

rm(control_crdf)
gc(reset = T)

crop_df <- left_join(crop_df,
                     control_cr_hrdf,
                     by=c("srno"))

rm(control_cr_hrdf)
gc(reset = T)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Renaming Variables
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#defining vars based on low input
crop_df <- crop_df %>%
  dplyr::rename(c(crop_actual_evopot_mm_irri = eta_L_irri,
                  crop_actual_evopot_mm_rainfed = eta_L_rainfed,
                  growing_cycle_crop_water_def_mm_irri = wde_L_irri,
                  growing_cycle_crop_water_def_mm_rainfed = wde_L_rainfed))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Imputing Crop Water Demand
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# crop water demand / need = evapotranspiration
#https://www.fao.org/3/s2022e/s2022e02.htm
# https://www.fao.org/3/s2022e/s2022e02.htm

## I will impute crop water demand by summing actual evapotranspiration and crop water deficit.
# 1. I'll construct the crop water demand (in mm)
# 2. Crop Water Demand (in mm per hectare of crop grown): constructing this to make the crop water demand more comparable across regions
# Note: This will only done for crop water deficits of rainfed areas since we are only interested in filling water gaps of rainfed areas.
crop_df <- crop_df %>%
  dplyr::rowwise() %>%
  dplyr::mutate(crop_water_demand_mm = sum(crop_actual_evopot_mm_rainfed,
                                           growing_cycle_crop_water_def_mm_rainfed,
                                           na.rm = T)) %>%
  dplyr::ungroup()

table(is.na(crop_df$crop_water_demand_mm)) #all FALSE

## This measure does not consider the fact that some values may be 0 since that crop is not grown in that specific area
crop_df <- crop_df %>%
  dplyr::mutate(crop_water_demand_mm_adj = ifelse(har_rainfed == 0, #check if this should be har_tot
                                                  NA,
                                                  crop_water_demand_mm),
                crop_actual_evopot_mm_rainfed_adj = ifelse(har_rainfed == 0, #check if this should be har_tot
                                                  NA,
                                                  crop_actual_evopot_mm_rainfed),
                har_rainfed_adj = ifelse(har_rainfed == 0, #check if this should be har_tot
                                         NA,
                                         har_rainfed))

table(is.na(crop_df$crop_water_demand_mm_adj))
#  FALSE    TRUE
#  321108 554286 

# To make the imputed means comparable, considering crop water deficit per hectare of harvest area
crop_df <- crop_df %>%
  dplyr::mutate(crop_water_demand_mm_ha_adj = crop_water_demand_mm_adj/har_rainfed_adj,
                crop_actual_evopot_mm_ha_rainfed = crop_actual_evopot_mm_rainfed_adj / har_rainfed_adj)

## If missing then fill in the values from a neighbouring grid.

unique_sf <- st_as_sf(unique_df)
unique_sf <- unique_sf %>%
  dplyr::select(-WB_NAME)
unique_sf <- st_transform(unique_sf,
                          crs = 7801) #converting CRS to make the distance in meters

# TO BE RUN ONCE
# Generated a 12 km buffer to keep a grid that helps identify immediate neighbours
# buff_12km <- st_buffer(unique_sf,
#                        dist = 12000)
# st_write(buff_12km,
#          "data-clean/working_data/gaez/buffer/buffer_12km.shp")
# 
# # Generated a 12 km buffer to keep a grid that helps identify further away neighbours
# buff_25km <- st_buffer(unique_sf,
#                        dist = 25000)
# st_write(buff_25km,
#          "data-clean/working_data/gaez/buffer/buffer_25km.shp")


df_buffer <- st_read("data-clean/working_data/gaez/buffer/buffer_12km.shp")
buff_25km <- st_read("data-clean/working_data/gaez/buffer/buffer_25km.shp")

crop_df$mean_crop_water_demand_mm_ha <- NA

crop_df$mean_yld_irri <- NA

crop_df$mean_har_rainfed_adj <- NA

# country mean
crop_df <- crop_df %>%
  dplyr::mutate(yld_irri_new = ifelse(har_irri == 0,
                                      NA,
                                      yld_irri)) %>%
  dplyr::group_by(crop_code, WB_NAME) %>%
  dplyr::mutate(mean_crop_water_demand_mm_ha_country = mean(crop_water_demand_mm_ha_adj,
                                                         na.rm = T),
                mean_yld_irri_country = mean(yld_irri_new,
                                             na.rm = T)) %>%
  dplyr::ungroup()
 
# I will create subsets of data based on crop-year and extract the buffer means for each of these subsets
# I'm doing this because the buffer data is identifiable at the srno level and subsetting by crop-year makes the level srno level.
for(crop in cereal_list) {
  
  # 1. Constructing a raster of crop water demand per hectare
  spg <- crop_df %>%
    dplyr::filter(crop_code == crop)  %>%
    dplyr::select(c(latitude,longitude,crop_water_demand_mm_ha_adj))
  
  coordinates(spg) <- ~ longitude + latitude
  # coerce to SpatialPixelsDataFrame
  gridded(spg) <- TRUE
  
  # coerce to raster
  ras <- raster(spg)
  #making the raster's CRS consistent with the buffer polygons
  crs(ras) <- crs(df_buffer)
  
  # Exporting raster plot
  rasterPlot <- levelplot(ras)
  # Save the raster plot as a PDF file
  pdf(paste0("output/graphs/crop_water_demand/",crop,"_2020_wtr_dmd.pdf"),
      width = 8, height = 6) # Adjust width and height as needed
  print(rasterPlot)
  dev.off() # Close the graphics device
  
  # 1.1 Constructing a raster of crop yield of irri land per hectare
  spg2 <- crop_df %>%
    dplyr::filter(crop_code == crop)  %>%
    dplyr::mutate(yld_irri = ifelse(har_irri == 0,
                                    NA,
                                    yld_irri)) %>%
    dplyr::select(latitude,longitude,yld_irri)
  
  coordinates(spg2) <- ~ longitude + latitude
  # coerce to SpatialPixelsDataFrame
  gridded(spg2) <- TRUE
  
  # coerce to raster
  ras2 <- raster(spg2)
  #making the raster's CRS consistent with the buffer polygons
  crs(ras2) <- crs(df_buffer)
  
  # 2. Generating a df which matches srno to the extracted crop demand means
  subset_df <- df_buffer %>%
    dplyr::select(srno) %>%
    dplyr::mutate(crop_code = crop)
  
  subset_df2 <- buff_25km %>%
    dplyr::select(srno)
  
  # Extract the mean crop water demands within each 12 km buffer of point
  subset_df$mean_crp_w_dmnd_mm_ha_neigh <- exact_extract(ras,
                                                        df_buffer,
                                                        fun="mean")
  
  subset_df2$mean_crp_w_dmnd_mm_ha_fneigh <- exact_extract(ras,
                                                          buff_25km,
                                                          fun="mean")
  
  # Extract the mean crop water demands within each 12 km buffer of point
  subset_df$mean_yld_irri_neigh <- exact_extract(ras2,
                                                 df_buffer,
                                                 fun="mean")
  
  subset_df2$mean_yld_irri_fneigh <- exact_extract(ras2,
                                                   df_buffer,
                                                   fun="mean")
  
  subset_df <- left_join(subset_df,
                         subset_df2 %>%
                           as.data.frame() %>%
                           dplyr::select(-geometry) %>%
                           dplyr::select(mean_yld_irri_fneigh,
                                         mean_crp_w_dmnd_mm_ha_fneigh, srno),
                         by = "srno")
  
  rm(subset_df2)
  
  # 3. Merging with crop_df
  crop_df <- left_join(crop_df,
                       subset_df %>%
                         as.data.frame() %>%
                         dplyr::select(-geometry),
                       by = c("srno","crop_code"))
  
  crop_df <- crop_df %>%
    dplyr::mutate(mean_crop_water_demand_mm_ha = case_when(is.na(mean_crop_water_demand_mm_ha) &
                                                          !is.na(mean_crp_w_dmnd_mm_ha_neigh) ~ mean_crp_w_dmnd_mm_ha_neigh,
                                                        is.na(mean_crop_water_demand_mm_ha) &
                                                          is.na(mean_crp_w_dmnd_mm_ha_neigh) &
                                                          !is.na(mean_crp_w_dmnd_mm_ha_fneigh) ~ mean_crp_w_dmnd_mm_ha_fneigh,
                                                        is.na(mean_crop_water_demand_mm_ha) &
                                                          is.na(mean_crp_w_dmnd_mm_ha_neigh) &
                                                          is.na(mean_crp_w_dmnd_mm_ha_fneigh) &
                                                          !is.na(mean_crop_water_demand_mm_ha_country) ~ mean_crop_water_demand_mm_ha_country,
                                                        TRUE ~ mean_crop_water_demand_mm_ha),
                  mean_yld_irri = case_when(is.na(mean_yld_irri) &
                                              !is.na(mean_yld_irri_neigh) ~ mean_yld_irri_neigh,
                                            is.na(mean_yld_irri) &
                                              is.na(mean_yld_irri_neigh) &
                                              !is.na(mean_yld_irri_fneigh) ~ mean_yld_irri_fneigh,
                                            is.na(mean_yld_irri) &
                                              is.na(mean_yld_irri_neigh) &
                                              is.na(mean_yld_irri_fneigh) &
                                              !is.na(mean_yld_irri_country) ~ mean_yld_irri_country,
                                            TRUE ~ mean_yld_irri)) %>%
    dplyr::select(-c(mean_crp_w_dmnd_mm_ha_neigh,
                     mean_crp_w_dmnd_mm_ha_fneigh,mean_yld_irri_neigh,
                     mean_yld_irri_fneigh))
  
  rm(ras,ras2, subset_df, spg,spg2, rasterPlot)
  gc(reset = TRUE)
}

## Adding in neighbouring values
crop_df <- crop_df %>%
  dplyr::mutate(crop_water_demand_mm_ha_imp = ifelse(is.na(crop_water_demand_mm_ha_adj),
                                                  mean_crop_water_demand_mm_ha,
                                                  crop_water_demand_mm_ha_adj))


table(is.na(crop_df$crop_water_demand_mm_ha_imp))
# FALSE    TRUE
# 745720 129674

# Can fill in the water demand imputations across years for the same crop-grid
#tried it earlier but it didn't help to recover any additional info

# constructing the volume of water required for rainfed areas & volume deficit
# converting units to meter cube
crop_df <- crop_df %>%
  dplyr::mutate(crop_actual_evopot_mm_ha_imp = ifelse(is.na(crop_actual_evopot_mm_ha_rainfed) &
                                                       !is.na(crop_water_demand_mm_ha_imp),
                                                     0,
                                                     crop_actual_evopot_mm_ha_rainfed),
                crop_water_demand_vol_rainfed_m3 = ifelse(har_rainfed > 0,
                                                          (crop_water_demand_mm_ha_imp/1000) * har_rainfed * 10000000, #converting mm to m and 1000 ha to m2
                                                          NA),
                crop_water_def_vol_rainfed_m3 = ifelse(har_rainfed > 0,
                                                       ((crop_water_demand_mm_ha_imp - crop_actual_evopot_mm_ha_imp)/1000) * har_rainfed * 10000000,
                                                       NA))

# Saving dataset
saveRDS(crop_df,
        "data-clean/working_data/gaez/gaez_crop_year_analysis.RDS")