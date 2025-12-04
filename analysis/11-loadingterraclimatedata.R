#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Loading Terra Climate Data
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will run district regressions to test effects of irrigation

rm(list=ls())

# Loading Packages
#remotes::install_github("mikejohnson51/AOI") # suggested!
#remotes::install_github("mikejohnson51/climateR")

if(!require("pacman")) install.packages("pacman")

pacman::p_load(

  ncdf4, raster, chron, tidyverse, lubridate, sf, exactextractr,
  knitr, kableExtra, lfe, stargazer, climateR,  AOI,
  rgdal, haven, spatialEco, viridis, grid, gridExtra, plyr, dplyr

)


sf::sf_use_s2(FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Loading data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

grid_polygon <- st_read("data-clean/working_data/gaez/grid_polygon/unique_grid_polygons.shp")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Loading Climate Controls
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

grid_polygon_temp <- grid_polygon

for(var in c("tmax","ppt","PDSI")) {

  tc_df = getTerraClim(grid_polygon,
                       paste0(var),
                       startDate = "2020-01-01",
                       endDate = "2020-12-01",
                       verbose = FALSE,
                       dryrun = FALSE)

  tc_df <- tc_df[[1]]


  # Convert SpatRaster to Raster
  tc_raster <- as(tc_df, "Raster")

  #need to adjust crs of grid_polygon_temp - only have to change it for the first iteration
  if(var == "tmax") {
    grid_polygon_temp <- grid_polygon_temp %>%
      st_transform(crs(tc_raster))


    st_crs(grid_polygon_temp) == st_crs(tc_raster)
  }



  # tc_raster <- projectRaster(tc_raster,
  #                            crs=crs(grid_polygon))

  if(var %in% c("tmax","PDSI")) {

    tc_df_fin <- exactextractr::exact_extract(tc_raster,
                                              grid_polygon_temp,
                                              fun = "mean")

    # renaming variables
    tc_df_fin  <- tc_df_fin %>%
      dplyr::rename_with(~gsub(paste0("mean.", var, "_(\\d{4})\\.(\\d{2})\\.\\d{2}_total"),
                               paste0("mean.", var, "_\\2_total"), .),
                         starts_with(paste0("mean.", var, "_")))

  } else {  tc_df_fin <- exactextractr::exact_extract(tc_raster,
                                                      grid_polygon_temp,
                                                      fun = "sum")

  # renaming variables
  tc_df_fin  <- tc_df_fin %>%
    dplyr::rename_with(~gsub(paste0("sum.", var, "_(\\d{4})\\.(\\d{2})\\.\\d{2}_total"),
                             paste0("sum.", var, "_\\2_total"), .),
                       starts_with(paste0("sum.", var, "_")))
  }

  tc_df_fin$srno = grid_polygon$srno

  if(var == "tmax") {
    clm_df <- tc_df_fin
  } else {clm_df <- left_join(clm_df,
                              tc_df_fin,
                              by = "srno")}

  rm(tc_df,tc_raster,tc_df_fin,var)


}

# Kala et al: We explored several alternatives but finally selected November,
# December, and January as “winter”, February through April as “spring”, May
# through July as “summer”, and August through October as “fall”

clm_df <- clm_df %>%
  as.data.frame() %>%
  dplyr::rowwise() %>%
  dplyr::mutate(temp_winter = mean(mean.tmax_01_total,mean.tmax_11_total,mean.tmax_12_total, na.rm = T),
                temp_spring = mean(mean.tmax_02_total,mean.tmax_03_total,mean.tmax_04_total, na.rm = T),
                temp_summer = mean(mean.tmax_05_total,mean.tmax_06_total,mean.tmax_07_total, na.rm = T),
                temp_fall = mean(mean.tmax_08_total,mean.tmax_09_total,mean.tmax_10_total, na.rm = T),
                prec_winter = sum(sum.ppt_01_total,sum.ppt_11_total,sum.ppt_12_total, na.rm = T),
                prec_spring = sum(sum.ppt_02_total,sum.ppt_03_total,sum.ppt_04_total, na.rm = T),
                prec_summer = sum(sum.ppt_05_total,sum.ppt_06_total,sum.ppt_07_total, na.rm = T),
                prec_fall = sum(sum.ppt_08_total,sum.ppt_09_total,sum.ppt_10_total, na.rm = T),
                pdsi_winter = mean(mean.PDSI_01_total,mean.PDSI_11_total,mean.PDSI_12_total, na.rm = T),
                pdsi_spring = mean(mean.PDSI_02_total,mean.PDSI_03_total,mean.PDSI_04_total, na.rm = T),
                pdsi_summer = mean(mean.PDSI_05_total,mean.PDSI_06_total,mean.PDSI_07_total, na.rm = T),
                pdsi_fall = mean(mean.PDSI_08_total,mean.PDSI_09_total,mean.PDSI_10_total, na.rm = T)) %>%
  dplyr::ungroup()

clm_df <- clm_df %>%
  dplyr::mutate(across(ends_with(c("_winter","_fall","_spring","_summer")),
                       ~ .x * .x,
                       .names = '{col}_sq'),
                across(ends_with(c("_winter","_fall","_spring","_summer")),
                       ~ .x * .x * .x,
                       .names = '{col}_cubed'))

# Saving the data
saveRDS(clm_df,
        "data-clean/working_data/terra-climate/climate_data_working.RDS")
