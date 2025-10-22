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

sf::sf_use_s2(FALSE)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Loading data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# BGS data:
bgs_vol_df <- readRDS("data-clean/working_data/bgs/bgs_volume.RDS")

# Loading aquifer shapefile
subs_afr_shp <- st_read("data-raw/shapefile/WB_Boundaries_GeoJSON_highres/WB_countries_Admin0.geojson")
subs_afr_shp <- subs_afr_shp %>%
  dplyr::filter(REGION_WB == "Sub-Saharan Africa")

grid_polygon <- st_read("data-clean/working_data/gaez/grid_polygon/unique_grid_polygons.shp")

bgs_aq_df <- readRDS("data-clean/working_data/bgs/bgs_country_combined.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Merging BGS Volume with BGS Aquifer Type
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Adding an ID
bgs_vol_df <- bgs_vol_df %>%
  dplyr::mutate(bgs_vol_id = row_number())

# Calculating modal value of aquifer type
joined_data <- st_join(bgs_vol_df %>%
                         dplyr::select(bgs_vol_id, geometry),
                       bgs_aq_df %>%
                         dplyr::select(aq_typ, geometry))

# Calculate the modal value of aq_typ for each group in the joined data
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

modal_aq_typ <- joined_data %>%
  as.data.frame() %>%
  dplyr::mutate(aq_typ = ifelse(aq_typ == "CSIF ",
                                "CSIF",
                                aq_typ)) %>%
  dplyr::group_by(bgs_vol_id) %>%
  dplyr::summarize(aq_typ = as.character(Mode(aq_typ))) %>%
  dplyr::ungroup()

# Merge the modal values back into bgs_vol_df
bgs_vol_df <- left_join(bgs_vol_df,
                        modal_aq_typ,
                        by = c("bgs_vol_id"))

rm(joined_data, modal_aq_typ)
gc(reset = TRUE)

saveRDS(bgs_vol_df,
        "data-clean/working_data/bgs/bgs_vol_with_aq_type.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Merging BGS Volume with GAEZ
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gaez_bgs_join <- st_join(grid_polygon %>%
                           dplyr::select(srno,geometry),
                         bgs_vol_df %>%
                           dplyr::select(bgs_vol_id,aq_typ,geometry))

# Obtaining area and storage values
gaez_bgs_join <- left_join(gaez_bgs_join %>%
                             as.data.frame() %>%
                             dplyr::select(-geometry),
                           bgs_vol_df %>%
                             as.data.frame() %>%
                             dplyr::select(bgs_area_m2,bgs_storage,bgs_vol_id),
                           by = "bgs_vol_id")

# Summing volumes of the joins for each grid conditional on the aquifer being accessible
final_merged <- gaez_bgs_join %>%
  as.data.frame() %>%
  dplyr::mutate(aq_typ = ifelse(aq_typ == "CSIF ",
                                "CSIF",
                                aq_typ),
                accessible_aq = ifelse(aq_typ %in% c("B"),
                                       0,
                                       1)) %>%
  dplyr::group_by(srno) %>%
  # dplyr::summarize(aq_typ = as.character(Mode(aq_typ)),
  #                  bgs_vol_id = bgs_vol_id[which.max(tabulate(match(aq_typ, unique(aq_typ))))]) %>%
  dplyr::summarize(bgs_volume_m3 = sum(bgs_area_m2 *
                                         as.numeric(bgs_storage)/1000 * #converting mm to m, storage is in height
                                         accessible_aq,na.rm = T)) %>%
  dplyr::ungroup()


saveRDS(final_merged,
        "data-clean/working_data/bgs/gaez_bgs_vol_with_aq_type.RDS")
