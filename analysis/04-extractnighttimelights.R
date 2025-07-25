################################################################################
############## QUICK NIGHT TIME LIGHT EXTRACTION INTO THE GRIDS ################
################################################################################
pacman::p_load(sf, raster, exactextractr)
### read in the grid polygon shapefile

grid_dt <- st_read("data-clean/working_data/gaez/grid_polygon/unique_grid_polygons.shp")

ntl_raster <- raster("data-raw/NTL/VNL_v21_npp_2021_global_vcmcfg_c202205302300.average.dat.tif")

grid_dt$ntl <- exact_extract(x = ntl_raster,
                             y = grid_dt,
                             fun = "sum")

saveRDS(grid_dt, "data-clean/working_data/grid_dt_withntl.RDS")
