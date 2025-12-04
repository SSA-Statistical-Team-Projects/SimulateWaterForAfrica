#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Calculating gains (Original)
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will calculate production gains by comparing crop yields within the same grid,
# or by using the mean yield from irrigation using neighbouring grids

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

crop_df <- readRDS("data-clean/working_data/gaez/gaez_crop_year_analysis_final.RDS")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculating Production Gains
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# We have yield differences calculated for two broad cases.
# Yield difference is equal to
# i) Irrigated Yield - Rainfed Yield, when a grid has both rainfed and irrigated land
# ii) Mean Irrigated Yield - Rainfed Yield, when a grid only has rainfed cropland
# (In ii, mean irrigate yield was calculated using the yield for the crop considering neighbouring grids as well as the mean of the country)

crop_df <- crop_df %>%
  dplyr::mutate(simple_yld_diff = case_when(har_rainfed > 0 & # when both irri and rainfed croplands for each crop exist
                                             har_irri > 0 &
                                             (yld_irri > yld_rainfed) ~ (yld_irri- yld_rainfed),
                                            har_rainfed > 0 & # when both irri and rainfed croplands for each crop exist
                                              har_irri > 0 &
                                              (yld_irri <= yld_rainfed) ~ 0,
                                            har_rainfed > 0 & # when just rainfed croplands for each crop exist
                                              (har_irri == 0 | is.na(har_irri)) &
                                              (mean_yld_irri > yld_rainfed) ~ (mean_yld_irri - yld_rainfed) , # considering neighbouring grids' yield for this
                                            har_rainfed > 0 &
                                              (har_irri == 0 | is.na(har_irri)) &
                                              (mean_yld_irri <= yld_rainfed) ~ 0,
                                            TRUE ~ NA_real_))

crop_df <- crop_df %>%
  dplyr::select(crop_code,srno,simple_yld_diff)

saveRDS(crop_df,
        "data-clean/working_data/estimation/simple_diff_results.RDS")
