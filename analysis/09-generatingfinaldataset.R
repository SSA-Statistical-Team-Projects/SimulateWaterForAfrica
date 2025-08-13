#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Merging in crop water supply
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will merge the GAEZ dataset with the aquifer dataset

rm(list=ls())
gc(reset = TRUE)

# Loading Packages
if(!require("pacman")) install.packages("pacman")

pacman::p_load(

  ncdf4, raster, chron, tidyverse, lubridate, sf, exactextractr, dplyr,
  rgdal, haven, spatialEco, exactextractr, geosphere, stars, labelled

)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Loading data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# crop data with crop water requirement
crop_df <- readRDS("data-clean/working_data/gaez/gaez_crop_year_analysis.RDS")

# merge with BGS
bgs_merge <- readRDS("data-clean/working_data/bgs/bgs_gaez_final_merged.RDS")

# merge with aquifer data
watergp_merge <- readRDS("data-clean/working_data/aquifer-gaez-merge/aq_gaez_final_merged.RDS")

# water use data
wateruse_dt <- readRDS("data-clean/working_data/wateruse.RDS")

# identifying SSA country list
intersect_dt <- readRDS("data-clean/working_data/world-bank-aquifer-data/aquifermasterpoly.RDS")
intersect_dt <- intersect_dt %>%
  dplyr::filter(REGION_WB == "Sub-Saharan Africa") %>%
  dplyr::distinct(ISO_A3, WB_NAME)

# Loading shapefile for the WB_NAME
# subs_afr_shp <- st_read("data-raw/shapefile/WB_Boundaries_GeoJSON_highres/WB_countries_Admin0.geojson")
# subs_afr_shp <- subs_afr_shp %>%
#   dplyr::filter(REGION_WB == "Sub-Saharan Africa")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Merging Water GP and BGS
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aq_merged_df <- left_join(bgs_merge %>%
                            as.data.frame() %>%
                            dplyr::select(srno,mean_aqp_litre_per_sec,
                                          mean_gwdepth_m,mean_recharge_mmyr),
                          watergp_merge %>%
                            as.data.frame() %>%
                            dplyr::select(m1_vol_int_ls_km3,m1_vol_int_ma_km3,
                                          m1_vol_int_kt_km3,m1_vol_int_cx_km3,srno),
                          by = "srno")

rm(bgs_merge, watergp_merge)
gc(reset = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Merging Water crop data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crop_df <- left_join(crop_df,
                     aq_merged_df,
                     by = "srno"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Merging water use data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wateruse_dt <- wateruse_dt %>%
  dplyr::mutate(total_use_adj = tot_use_vol * 0.675, #multiplying by share of GW use (Figure A.4.1 of Annex of Hidden Wealth of Nations)
                share_of_sanitation_cons = hhwater_use_shr)

#keeping SSA countries
wateruse_dt <- left_join(wateruse_dt %>%
                           dplyr::mutate(ISO_A3 = remove_var_label(Code)) ,
                         intersect_dt %>%
                           dplyr::mutate(is_ssa = 1) %>%
                           dplyr::mutate(ISO_A3 = remove_var_label(ISO_A3)) %>%
                           dplyr::mutate(ISO_A3 = as.character(ISO_A3)),
                         by = "ISO_A3")
wateruse_dt <- wateruse_dt %>%
  dplyr::filter(is_ssa == 1)

wateruse_dt <- wateruse_dt %>%
  dplyr::filter(Year == 2020)

# Merging with crop_df
crop_df <- left_join(crop_df,
                     wateruse_dt %>%
                       dplyr::mutate(WB_NAME = as.character(WB_NAME)) %>%
                       dplyr::select(-c(Year,is_ssa,Entity,Code)),
                     by = c("WB_NAME")
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Cleaning
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# obtaining grid area
grid_polygon <- st_read("data-clean/working_data/gaez/grid_polygon/unique_grid_polygons.shp")
grid_polygon$gaez_grid_area = st_area(grid_polygon$geometry) #m2

# The water supply measure needs to be at the crop level not the grid level
crop_df <- left_join(crop_df,
                     grid_polygon %>%
                       as.data.frame() %>%
                       dplyr::select(srno,gaez_grid_area),
                     by = c("srno"))

# computing water supply for each crop = volume of water in grid * share of crop area in grid
crop_df <- crop_df %>%
  dplyr::mutate(across(starts_with("m1_vol"),
                       ~ .x * (har_tot *10000000) / gaez_grid_area, #converting 1000 Ha to m2 so that the m2 in the numerator and denominator cancel out
                       .names = "crop_{col}"))

# The water supply measure needs to additionally subtract water for sanitation purposes

crop_df <- crop_df %>%
  dplyr::mutate(share_of_sanitation_cons = ifelse(is.na(share_of_sanitation_cons),
                                                  0,
                                                  share_of_sanitation_cons),
                across(starts_with("crop_m1_vol"),
                       ~ .x * (1-share_of_sanitation_cons),
                       .names = "{col}_adj"))

crop_df <- crop_df %>%
  dplyr::mutate(crop_m1_vol_int_ls_m3_adj = crop_m1_vol_int_ls_km3_adj * 1e9,
                crop_m1_vol_int_ma_m3_adj = crop_m1_vol_int_ma_km3_adj * 1e9,
                crop_m1_vol_int_kt_m3_adj = crop_m1_vol_int_kt_km3_adj * 1e9,
                crop_m1_vol_int_cx_m3_adj = crop_m1_vol_int_cx_km3_adj * 1e9)


# saving final data
saveRDS(crop_df,
        "data-clean/working_data/gaez/gaez_crop_year_analysis_final.RDS")
