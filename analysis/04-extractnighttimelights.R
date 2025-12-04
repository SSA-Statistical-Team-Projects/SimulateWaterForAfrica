################################################################################
############## QUICK NIGHT TIME LIGHT EXTRACTION INTO THE GRIDS ################
################################################################################
rm(list=ls())
gc(reset = TRUE)

pacman::p_load(sf, raster, exactextractr, dplyr)
### read in the grid polygon shapefile

### 1. Loading Data
grid_dt <- st_read("data-clean/working_data/gaez/grid_polygon/unique_grid_polygons.shp")

ntl_raster <- raster("data-raw/NTL/VNL_v21_npp_2021_global_vcmslcfg_c202205302300.average.dat.tif")
crs(ntl_raster) <- crs(grid_dt)

subs_afr_shp <- st_read("data-raw/shapefile/WB_Boundaries_GeoJSON_highres/WB_countries_Admin0.geojson") %>%
  dplyr::filter(REGION_WB == "Sub-Saharan Africa")
subs_afr_shp <- st_transform(subs_afr_shp, 
                             crs = crs(raster::stack("data-raw/gaez/rasters_raw/GAEZ-V5.RES06-YLD.MLT.WST.tif"))) # or any sample GAEZ raster

### 2. Extracting Data

ntl_raster <- crop(ntl_raster, extent(subs_afr_shp))

grid_dt$ntl <- exact_extract(x = ntl_raster,
                             y = grid_dt,
                             fun = "sum")

### 3. Saving Data
saveRDS(grid_dt %>%
          as.data.frame() %>%
          dplyr::select(-geometry), 
        "data-clean/working_data/ntl/grid_dt_withntl.RDS")
