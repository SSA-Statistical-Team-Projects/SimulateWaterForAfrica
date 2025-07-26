#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Merging Datasets
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will merge the GAEZ dataset with the aquifer dataset and the BGS dataset

rm(list=ls())

# Loading Packages
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  
  ncdf4, raster, chron, tidyverse, lubridate, sf, exactextractr, dplyr,
  rgdal, haven, spatialEco, exactextractr, geosphere, stars, lwgeom
  
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Loading data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# GAEZ Data
unique_df <- readRDS("data-clean/working_data/gaez/gaez_unique_grid_points.RDS")

# unique_df <- unique_df %>%
#   dplyr::arrange(longitude,latitude,srno)

# Aquifer Data
aq_shp <- st_read("data-raw/world-bank-aquifer-data/aqtyp_dissolved.gpkg")

# Aquifer grid polygons:
intersect_dt <- readRDS("data-clean/working_data/world-bank-aquifer-data/aquifermasterpoly.RDS")

# BGS data:
bgs_aq_df <- readRDS("data-clean/working_data/bgs/bgs_country_combined.RDS")

# Loading SSA shapefile
subs_afr_shp <- st_read("data-raw/shapefile/WB_Boundaries_GeoJSON_highres/WB_countries_Admin0.geojson")
subs_afr_shp <- subs_afr_shp %>%
  dplyr::filter(REGION_WB == "Sub-Saharan Africa")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Intersecting GAEZ with Aquifer Grids
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Constructing variables of interest
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# I'll do this for the duplicated and non-duplicated lists

# Only keeping relevant information on aquifer polygons
# This is done in order to speed up the intersection
# Note that in this data some of the grids intersect
# i.e. grid_id is not unique but intgrid_id is unique
aquifer_poly <- intersect_dt %>%
  dplyr::filter(REGION_WB == "Sub-Saharan Africa") %>%
  dplyr::select(intgrid_id,geom)

# Making the CRSs match
st_crs(aquifer_poly) <- st_crs(grid_polygon)

# saving the file as a shapefile
st_write(aquifer_poly,
         "data-clean/working_data/aquifer_data/water_gp/grid_polygons/aquifer_grid_polygons.shp")

# Calculate centroids of grid polygons
#grid_centroids <- st_centroid(grid_polygon)

# joining the gaez grid and aquifer polygons
joined_df <- st_join(aquifer_poly,grid_polygon)

# for those which have no duplicated rows, we know the area within the aquifer is 9x9km,
# for those which have duplicated rows, we will run the intersection.
joined_df <- joined_df %>%
  dplyr::group_by(srno) %>%
  dplyr::mutate(no_of_matches = n()) %>%
  dplyr::ungroup()

joined_df_nondup = joined_df %>%
  dplyr::filter(no_of_matches == 1) %>%
  dplyr::rename(geometry = geom) %>%
  dplyr::select(intgrid_id,srno, geometry)

# Create a separate data frame for non-duplicated grid polygons
# this indicates that they had a perfect match with aquifer polygons
saveRDS(joined_df_nondup,
        "data-clean/working_data/aquifer_gaez_merge/aq_gaez_not_duplicated.RDS"
)

grid_subset <- left_join(grid_polygon,
                         joined_df_nondup %>%
                           dplyr::select(srno) %>%
                           dplyr::mutate(has_no_dup = 1) %>%
                           as.data.frame(),
                         by = c("srno"))

grid_subset <- grid_subset %>%
  dplyr::filter(is.na(has_no_dup)) %>%
  dplyr::select(geometry.x,srno) %>%
  dplyr::rename(geometry = geometry.x)

# running a check:
dim(grid_subset)[1] + dim(joined_df_nondup)[1] == dim(grid_polygon)[1] # TRUE

# Perform intersection only on the subset of boundary polygons
# Iterate over subsets of gaez polygon data by breaking it into groups of 15000
# doing this in order to run the intersections faster
lapply(1:51,
       function (x) {
         
         # setting the range of IDs to extract
         if(x == 1) {
           min = 1
           max = 2000
         } else { min = ((x-1)*2000) + 1
         max = min((x*2000),dim(grid_subset)[1])}
         
         # extracting subsets of the gaez polygons
         grid_subset_sub <- grid_subset[min:max,]
         
         #merging polygons
         intersection_result <- sf::st_intersection(grid_subset_sub,
                                                    aquifer_poly)
         
         # saving results
         saveRDS(intersection_result,
                 paste0("data-clean/working_data/aquifer_gaez_merge/",
                        "aq_gaez_merge_boundary_",min,"_",max,".RDS"
                 )
         )
         
         rm(grid_subset_sub,intersection_result, min, max)
         gc(reset = T)
         
         # Sleep for 90 seconds
         Sys.sleep(90)
         
       }
)


# computing total water volume
aquifer_df <- as.data.frame(intersect_dt)

aquifer_df <- aquifer_df %>%
  dplyr::filter(REGION_WB == "Sub-Saharan Africa") %>%
  dplyr::select(starts_with(c("intgrid_id","aqtyp","grid_area",
                              "int_resource_norm"))) %>%
  dplyr::rename(aquifer_tot_area = grid_area)

# Unlabel the intgrid_id column
aquifer_df$intgrid_id <- as.numeric(as.character(aquifer_df$intgrid_id))

#computing area of grids
#aquifer_poly$aquifer_grid_area = st_area(aquifer_poly) #water GP data
joined_df_nondup$area_intersection <- st_area(joined_df_nondup) # GAEZ grid area of those which had a perfect mapping

# merge with aquifer_df to obtain fraction of total area and volume
joined_df_nondup <- left_join(joined_df_nondup,
                              aquifer_df,
                              by = c("intgrid_id"))

# imputing the water supply for the gaez-aquifer pair for each aquifer type
# Method 1: Imputing water volume by interaction frac of area intersected * volume in water grid * pct of aquifer type
joined_df_nondup <- joined_df_nondup %>%
  dplyr::mutate(m1_vol_int_ls_km3 = (area_intersection /aquifer_tot_area) *
                  int_resource_norm * aqtyp_pct_ls / 1000000000,
                m1_vol_int_ma_km3 = (area_intersection /aquifer_tot_area) *
                  int_resource_norm * aqtyp_pct_ma  / 1000000000,
                m1_vol_int_kt_km3 = (area_intersection /aquifer_tot_area) *
                  int_resource_norm * aqtyp_pct_kt / 1000000000,
                m1_vol_int_cx_km3 = (area_intersection /aquifer_tot_area) *
                  int_resource_norm * aqtyp_pct_cx  / 1000000000)

# Create a separate data frame for non-duplicated grid polygons
# this indicates that they had a perfect match with aquifer polygons
saveRDS(joined_df_nondup,
        "data-clean/working_data/aquifer_gaez_merge/aq_gaez_analysis_not_duplicated.RDS"
)


for(x in c(1:51)) {
  
  # setting the range of IDs to load
  if(x == 1) {
    min = 1
    max = 2000
  } else { min = ((x-1)*2000) + 1
  max = min((x*2000),dim(grid_subset)[1])}
  
  # loading data
  intersection_result <- readRDS(paste0("data-clean/working_data/aquifer_gaez_merge/",
                                        "aq_gaez_merge_boundary_",
                                        min,"_",max,".RDS"))
  
  # Check for invalid geometries
  invalid_geometries <- intersection_result[!st_is_valid(intersection_result$geometry), ]
  
  # Attempt to fix invalid geometries
  if(dim(invalid_geometries)[1] > 0) {
    intersection_result <- st_make_valid(intersection_result)
  }
  
  #computing intersection area
  intersection_result$area_intersection <- st_area(intersection_result)
  
  # merge with aquifer_df to obtain fraction of total area and volume
  intersection_result = left_join(intersection_result,
                                  aquifer_df,
                                  by = c("intgrid_id"))
  
  # imputing the water supply for the gaez-aquifer pair for each aquifer type
  # Method 1: Imputing water volume by interaction frac of area intersected * volume in water grid * pct of aquifer type
  intersection_result <- intersection_result %>%
    dplyr::mutate(m1_vol_int_ls_km3 = (area_intersection /aquifer_tot_area) *
                    int_resource_norm * aqtyp_pct_ls  / 1000000000,
                  m1_vol_int_ma_km3 = (area_intersection /aquifer_tot_area) *
                    int_resource_norm * aqtyp_pct_ma  / 1000000000,
                  m1_vol_int_kt_km3 = (area_intersection /aquifer_tot_area) *
                    int_resource_norm * aqtyp_pct_kt  / 1000000000,
                  m1_vol_int_cx_km3 = (area_intersection /aquifer_tot_area) *
                    int_resource_norm * aqtyp_pct_cx  / 1000000000)
  
  if(x == 1) {
    joined_df_dup <- intersection_result
  } else {joined_df_dup <- bind_rows(joined_df_dup,intersection_result)}
  
  rm(min,max,intersection_result, invalid_geometries)
  gc(reset = T)
  Sys.sleep(5)
  
}

saveRDS(joined_df_dup,
        "data-clean/working_data/aquifer_gaez_merge/aq_gaez_duplicated.RDS"
)


# Appending dup and non-dup lists
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# generating final information at the GAEZ grid level

joined_df_dup <- joined_df_dup %>%
  dplyr::select(starts_with(c("srno","m1_"))) %>%
  dplyr::mutate(across(starts_with("m1"), #summing to obtain the total volume of water in each grid
                       ~ as.numeric(as.character(.x)))) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  dplyr::group_by(srno) %>%
  dplyr::summarize(across(starts_with("m1"),
                          ~ sum(.x,na.rm = T))) %>%
  dplyr::ungroup()

#checking which grids did not have an intersection / match
grids_not_matched <- setdiff(grid_subset$srno,joined_df_dup$srno)

gaez_w_water <- bind_rows(joined_df_nondup %>%
                            dplyr::select(starts_with(c("m1","srno"))) %>%
                            dplyr::mutate(across(starts_with("m1"),
                                                 ~ as.numeric(as.character(.x)))),
                          joined_df_dup)


saveRDS(gaez_w_water,
        "data-clean/working_data/aquifer_gaez_merge/aq_gaez_final_merged.RDS"
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Intersecting GAEZ with BGS Grids
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Here we will convert BGS data to a raster format:
#   a. Will choose the lowest resolution of the BGS data for the raster
# 2. Use exactextract to obtain extract information to GAEZ.

sf::sf_use_s2(FALSE)

# Making the CRSs match
st_crs(bgs_aq_df) <- st_crs(grid_polygon)
crs(bgs_aq_df) == crs(grid_polygon)


# finding the minimum area
bgs_aq_df$poly_area <- as.numeric(as.character(st_area(bgs_aq_df)))
min_area_bgs <- min(bgs_aq_df$poly_area) # in m2

# now we need to convert the m2 to degrees

# get the centroid
bgs_aq_df <- st_centroid(bgs_aq_df)

# Extract the centroid latitude
center_latitude <- st_coordinates(bgs_aq_df)[, "Y"] %>%
  mean(na.rm = TRUE)

# Calculate the length of a degree of latitude and longitude in meters
lat_deg_length <- distVincentySphere(cbind(center_latitude, 0), cbind(center_latitude + 1, 0))
lon_deg_length <- distVincentySphere(cbind(0, center_latitude), cbind(0, center_latitude + 1))

# Convert the square root of the minimum area from m^2 to degrees^2
sqrt_min_area_deg2 <- sqrt(min_area_bgs) / (lat_deg_length * lon_deg_length)

# Now you can use sqrt_min_area_deg2 as the resolution in rasterization
resolution <- sqrt_min_area_deg2

### convert BGS to raster

# 1. Productivity
bgs_aqp <- bgs_aq_df %>%
  dplyr::select(aq_prod_numeric,geometry)

sf_extent <- st_bbox(bgs_aqp)

bgs_aqp_star <- st_rasterize(bgs_aqp,
                             ext = sf_extent,
                             resolution = resolution)

write_stars(bgs_aqp_star, "data-clean/working_data/bgs/bgs_aq_prod.tif")

# 2. Recharge
recharge_ras <- raster::raster("data-raw/BGS/recharge_maps/Africa_Recharge_Map/Africa_recharge.tif")
st_crs(recharge_ras) == st_crs(grid_polygon) #TRUE

# 3. Depth

# Load Data
depth_df <- read.table("data-raw/BGS/DepthToGroundwater_V2/xyzASCII_dtwmap_v2.txt",
                       header = TRUE,
                       sep = "",
                       dec = ".")

#using the key to add values
depth_df$bgs_depth_m = plyr::mapvalues(depth_df$DTWAFRICA_,
                                       from = c("VS","S","SM","M","D","VD"),
                                       to = c(3.5,16,37.5,75,175,300))

# matching with country data

depth_df <- sf::st_as_sf(depth_df,
                         coords = c("X", "Y"),
                         crs = 4326,
                         agr = "constant")


depth_df <- depth_df %>%
  dplyr::select(geometry,bgs_depth_m) %>%
  dplyr::mutate(bgs_depth_m = as.numeric(bgs_depth_m))

#Joining with GAEZ grids
st_crs(depth_df) <- st_crs(grid_polygon)

#depth_df <- readRDS("data-clean/working_data/bgs/bgs_gwdepth.RDS")

grid_gaez_depth_join <- st_join(grid_polygon %>%
                                  dplyr::select(srno,geometry),
                                depth_df)

grid_gaez_depth_join <- grid_gaez_depth_join %>%
  as.data.frame() %>%
  dplyr::group_by(srno) %>%
  dplyr::summarize(mean_gwdepth_m = mean(bgs_depth_m,na.rm = T)) %>%
  dplyr::ungroup()

grid_polygon <- left_join(grid_polygon,
                          grid_gaez_depth_join,
                          by = "srno")

rm(depth_df,grid_gaez_depth_join)

### Extract information to GAEZ

# we'll consider the mean value for the aquifer productivity and
# the modal value for the aquifer type

bgs_aqp_raster <- raster::raster("data-clean/working_data/bgs/bgs_aq_prod.tif")

st_crs(bgs_aqp_raster) == st_crs(grid_polygon) #TRUE
grid_polygon[["mean_aqp_litre_per_sec"]] <- exact_extract(bgs_aqp_raster,
                                                          grid_polygon,
                                                          fun="mean")

grid_polygon[["mean_recharge_mmyr"]] <- exact_extract(recharge_ras,
                                                      grid_polygon,
                                                      fun="mean")

saveRDS(grid_polygon,
        "data-clean/working_data/bgs/bgs_gaez_final_merged.RDS"
)

