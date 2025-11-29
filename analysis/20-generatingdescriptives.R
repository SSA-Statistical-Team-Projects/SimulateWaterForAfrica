#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Generate World Aquifer Maps
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will generate world aquifer maps using the water GP dataset

rm(list = ls())
pacman::p_load("sf", "data.table", "raster", "haven", "units", "expss","lwgeom",
               "exactextractr", "here",  "ggplot2","grid","gridExtra","viridis",
               "ggpubr","tidyr","stringr","plyr","dplyr")

sf::sf_use_s2(FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Spatial Distribution of Crops
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crop_df <- readRDS("data-clean/working_data/gaez/gaez_crop_year_analysis.RDS")

#keeping only cereals
cereal_list <- c("mze","rcw","whe")
cereal_name <- c("Maize","Rice","Wheat")
crop_df <- crop_df %>%
  dplyr::filter(crop_code %in% cereal_list)

gc(reset = T)

# loading the grid_polygon
grid_polygon <- st_read("data-clean/working_data/gaez/grid_polygon/unique_grid_polygons.shp")

crop_df <- crop_df %>%
  dplyr::mutate(crop_grown = ifelse(har_tot > 0,
                                    1,
                                    har_tot))

## 1.1 Cropland type for crops grown
crop_df <- crop_df %>%
  dplyr::mutate(crop_grown_ctype = case_when(har_tot > 0 & har_irri > 0 ~ "Grown-Irrigated",
                                             har_tot > 0 &
                                               (is.na(har_irri) | har_irri == 0) ~ "Grown-Rainfed",
                                             is.na(har_tot) | har_tot == 0 ~ "Not Grown",
                                             TRUE ~ NA_character_))

# Generating plots for each crop
for (crop in cereal_list[cereal_list %in% c("mze", "whe", "rcw")]) {
  
  k = which(cereal_list == crop)
  crop_name = cereal_name[k]
  
  #merging in crop specific information
  crop_subset <- left_join(grid_polygon,
                           crop_df %>%
                             dplyr::filter(crop_code == crop) %>%
                             dplyr::select(srno,crop_grown_ctype),
                           by = "srno")
  
  # Plot the distribution of crop harvest on the map for each crop
  p <- ggplot() +
    geom_sf(data = crop_subset,
            aes(fill = crop_grown_ctype),
            color = NA) +
    scale_fill_manual(values = c("Not Grown" = "gray88",
                                 "Grown-Irrigated" = "turquoise3",
                                 "Grown-Rainfed" = "yellowgreen"),
                      na.value = "white") +
    labs(x = "",
         y = "") +
    ggtitle(paste(crop_name)) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          #legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5,
                                    size = 18)) +
    coord_sf(crs = st_crs(4326),ylim = c(-36,28))
  
  # Save or print the plot for each crop
  assign(paste0("plot.",crop),p)
  
  rm(p,crop_name,crop_subset,k,crop)
}
  
  plot <- ggarrange(plot.mze,plot.rcw,plot.whe,
                    nrow = 1,
                    ncol = 3,
                    common.legend = TRUE,
                    legend = "bottom"
  )
  
  ggsave(filename = "output/graphs/crop_cropland_distribution3.png",
         plot = plot,
         width = 10, height = 8, dpi = 150, units = "in",
         device='png')
  
  rm(list = ls(pattern = "^plot"))


#%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Recharge Map
#%%%%%%%%%%%%%%%%%%%%%%%%
  
# Load Data
recharge_ras <- raster::raster("data-raw/BGS/recharge_maps/Africa_Recharge_Map/Africa_recharge.tif")
  
ssa_shp <- read_sf("data-raw/shapefile/WB_Boundaries_GeoJSON_highres/WB_countries_Admin0.geojson")
ssa_shp <- ssa_shp %>%
    dplyr::filter(REGION_WB == "Sub-Saharan Africa")
  
# Match CRSs
st_crs(ssa_shp) <- st_crs(recharge_ras)
  
# Crop the raster using the mask function
ssa_recharge_ras <- recharge_ras %>% mask(ssa_shp)
  
ssa_recharge_df <- as.data.frame(ssa_recharge_ras,
                                   xy = TRUE)
  
plot1 <- ggplot(ssa_recharge_df,
                  aes(x = x,
                      y = y,
                      fill = Africa_recharge)) +
    geom_raster() +
    scale_fill_viridis_c(direction = -1,
                         name = "Annual Recharge Rate (mm)",
                         na.value = "white",
                         option = "mako",
                         breaks = seq(0,280, by = 40)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=8, angle = 45, hjust = 0.2),
          legend.title=element_text(size=18),
          legend.key.width = unit(2, "cm"),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0))
  
ggsave(filename = "output/graphs/aquifer_maps/ssa_recharge_map.png",
         plot = plot1,
         width = 9, height = 8, dpi = 150, units = "in",
         device='png')
  
rm(plot1,ssa_recharge_ras,ssa_recharge_df, ssa_shp)
  
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. GW Depth Map
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
# Load Data
dep_df <- read.table("data-raw/BGS/DepthToGroundwater_V2/xyzASCII_dtwmap_v2.txt",
                       header = TRUE,
                       sep = "",
                       dec = ".")
  
#using the key to add values
dep_df$bgs_depth_m = plyr::mapvalues(dep_df$DTWAFRICA_,
                                       from = c("VS","S","SM","M","D","VD"),
                                       to = c(3.5,16,37.5,75,175,300))
  
dep_df$bgs_depth_m <- as.numeric(dep_df$bgs_depth_m)
  
#crop for SSA
ssa_shp <- read_sf("data-raw/shapefile/WB_Boundaries_GeoJSON_highres/WB_countries_Admin0.geojson")
  
dep_df <- sf::st_as_sf(dep_df,
                         coords = c("X", "Y"),
                         crs = crs(ssa_shp),
                         agr = "constant")
dep_df <- st_join(dep_df,ssa_shp)
  
dep_df <- dep_df %>%
    dplyr::filter(REGION_WB == "Sub-Saharan Africa") %>%
    dplyr::select(geometry,bgs_depth_m)
  
  
#### plot the categories
  
plot1 <- dep_df %>%
    ggplot() +
    geom_sf(aes(color = bgs_depth_m)) +
    scale_color_viridis_b(name = "Depth to Groundwater (m)",
                          breaks = c(0, 7, 25, 50, 100, 250,max(dep_df$bgs_depth_m)),
                          na.value = "gray88",
                          direction = -1,
                          option = "mako") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_text(size=8, angle = 45, hjust = 0.2),
          legend.title=element_text(size=18),
          legend.key.width = unit(2, "cm"),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0))
  
ggsave(filename = "output/graphs/aquifer_maps/ssa_depth_map_final.png",
         plot = plot1,
         width = 9, height = 8, dpi = 150, units = "in",
         device='png')
  
rm(plot1,depth_df)
  
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. SSA Share of LS Map
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load data
intersect_dt <- readRDS("data-clean/working_data/world-bank-aquifer-data/aquifermasterpoly.RDS")

intersect_dt <- intersect_dt %>%
  dplyr::filter(REGION_WB == "Sub-Saharan Africa") %>%
  dplyr::rename(geometry = geom) %>%
  dplyr::mutate(int_resource_norm = as.numeric(int_resource_norm),
                ls_vol_blnlitre = aqtyp_pct_ls * int_resource_norm *1000 / 1000000000,
                aqtyp_pct_ls = aqtyp_pct_ls * 100)
  
plot1 <- ggplot(data = intersect_dt) +
    aes(fill = aqtyp_pct_ls, geometry = geometry) +
    geom_sf(color = NA) +
    scale_fill_viridis_b(direction = -1,
                         name = "Share of Local / Shallow Aquifers (%)",
                         na.value = "gray88",
                         option = "mako",
                         breaks = seq(0,100, by = 25)) +
    labs(x = "",
         y = "") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          legend.margin = margin(t = 0, unit = "cm"),  # Adjust top margin of legend
          legend.spacing = unit(0, "cm"),
          legend.text=element_text(size=13),
          legend.title=element_text(size=16)) +
    coord_sf(crs = st_crs(4326),ylim = c(-36,28))
  
ggsave(filename = "output/graphs/aquifer_maps/ls_shr_ssa_map.png",
         plot = plot1,
         width = 10, height = 8, dpi = 150, units = "in",
         device='png')
  
rm(plot1)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 5. Other Descriptive Stats
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# number of croplands which are irrigated
temp <- crop_df %>%
  dplyr::group_by(srno) %>%
  dplyr::summarise(har_tot = sum(har_tot,na.rm = TRUE),
                   har_irri = sum(har_irri,na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(is_cropland = ifelse(har_tot > 0,1,0),
                is_irri = ifelse(har_irri>0,1,0)) %>%
  dplyr::filter(is_cropland == 1) %>%

mean(temp$is_irri, na.rm = TRUE)

# number of aquifer grids
intersect_dt %>%  as.data.frame() %>% dplyr::distinct(grid_id) %>% dim()
intersect_dt %>%  as.data.frame() %>% dplyr::filter(REGION_WB == "Sub-Saharan Africa") %>% dim()

# share of local shallow aquifers in SSA
intersect_dt <- readRDS(here("data-clean/working_data/world-bank-aquifer-data/aquifermasterpoly.RDS"))

intersect_dt <- as.data.table(intersect_dt)

intersect_dt[, c("local_shallow",
                 "karstic",
                 "major_alluvial",
                 "complex") := .(aqtyp_pct_ls * int_resource_norm,
                                 aqtyp_pct_kt * int_resource_norm,
                                 aqtyp_pct_ma * int_resource_norm,
                                 aqtyp_pct_cx * int_resource_norm)]

pop_dt <- st_read("data-raw/shapefile/WB_Boundaries_GeoJSON_highres/WB_countries_Admin0.geojson")

aqtype_dt <-
  intersect_dt[, .(local_shallow = sum(local_shallow, na.rm = TRUE),
                   karstic = sum(karstic, na.rm = TRUE),
                   major_alluvial = sum(major_alluvial, na.rm = TRUE),
                   complex = sum(complex, na.rm = TRUE)),
               by = c("REGION_WB")] %>%
  melt(id.vars = "REGION_WB",
       measure.vars = c("local_shallow", "karstic", "major_alluvial", "complex")) %>%
  merge(y = pop_dt[,c("POP_EST", "REGION_WB")] %>%
          st_drop_geometry() %>%
          dplyr::group_by(REGION_WB) %>%
          dplyr::summarise(population = sum(POP_EST, na.rm = TRUE)))

aqtype_dt[, aqrate := value / sum(value, na.rm = TRUE), by = REGION_WB]

aqtype_dt[, vol_liters := as.numeric(value * 1000)]
aqtype_dt[, vol_bliters := vol_liters / 1e9]

aqtype_dt <- aqtype_dt[(REGION_WB %in% "Sub-Saharan Africa"),]