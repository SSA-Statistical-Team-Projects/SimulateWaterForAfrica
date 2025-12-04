#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Loading BGS Data
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will:
# 1. Load each country's BGS data files
# 2. Extract information on productivity and aquifer type
# 3. Save an appended dataset of all countries.


rm(list = ls())

if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  
  tidyverse, lubridate, stringr, plyr, reshape2, ggplot2, units, writexl,
  raster, sf, dplyr
  
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 1. Productivity Data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generating Compiled Dataset

country_name_list <- c("Algeria","Angola","Benin","Botswana","BurkinaFaso","Burundi",
                       "Cameroon","Chad","Congo","CotedIvoire","CtrlAfricanRepublic",
                       "Djibouti","DRC","Ethiopia","Gabon","Gambia","Ghana","Guinea",
                       "GuineaBissau","Kenya","Lesotho","Liberia","Madagascar","Malawi",
                       "Mali","Mauritania","Morocco","Mozambique","Niger","Nigeria",
                       "Rwanda","Senegal","SierraLeone","Somalia","SouthSudan","Sudan",
                       "Tanzania","Togo","Tunisia","Uganda","WesternSahara","Zambia",
                       "Zimbabwe")

country_code <- c("Alg","Ang","Ben","Bots","Bur","Bdn","Cam","Chad","Con","CdI",
                  "CAR","Dji","DRC","Eth","Gab","Gam","Gha","Gui","GBis","Ken",
                  "Les","Lib","Mad","Mal","Mali","Maur","Mor","Moz","Nig","Nigia",
                  "Rwa","Sen","SL","Som","SSd","Sud","Tz","Tog","Tun","Uga",
                  "WS","Zam","Zim")

country_list <- lapply(country_name_list,
                       function(x) {
                         
                         k = which(country_name_list == x)
                         ccode = country_code[k]
                         
                         # Loading Data
                         if(x == "BurkinaFaso") {
                           bgs_shp <- st_read(paste0("data-raw/BGS/",x,"/Burkina_Faso_HG.shp"))
                         }
                         
                         if(x == "Congo") {
                           bgs_shp <- st_read(paste0("data-raw/BGS/",x,"/RepCongo_HG.shp"))
                         }
                         
                         if(x == "CotedIvoire") {
                           bgs_shp <- st_read(paste0("data-raw/BGS/",x,"/Cote_d_Ivoire_HG.shp"))
                         }
                         
                         if(x == "CtrlAfricanRepublic") {
                           bgs_shp <- st_read(paste0("data-raw/BGS/",x,"/Central_African_Republic_HG.shp"))
                         }
                         
                         if(!exists("bgs_shp")) {
                           bgs_shp <- st_read(paste0("data-raw/BGS/",x,"/",x,"_HG.shp"))
                         }
                         
                         # Extracting information
                         if(x == "Botswana") {
                           bgs_shp$aq_typ = stringr::str_extract(bgs_shp$BotsBRHGCo, "[^-]+")
                           bgs_shp$aq_productivity <- sub(".*-", "", bgs_shp$BotsBRHGCo)
                         }
                         
                         if(x == "GuineaBissau") {
                           bgs_shp$aq_typ = stringr::str_extract(bgs_shp$GBisHCComb, "[^-]+")
                           bgs_shp$aq_productivity <- sub(".*-", "", bgs_shp$GBisHCComb)
                         }
                         
                         if(x == "Nigeria") {
                           bgs_shp$aq_typ = stringr::str_extract(bgs_shp$NigiaHGCom, "[^-]+")
                           bgs_shp$aq_productivity <- sub(".*-", "", bgs_shp$NigiaHGCom)
                         }
                         
                         if (!"aq_typ" %in% names(bgs_shp)) {
                           bgs_shp$aq_typ <- stringr::str_extract(bgs_shp[[paste0(ccode, "HGComb")]], "[^-]+")
                           bgs_shp$aq_productivity <- sub(".*-", "", bgs_shp[[paste0(ccode, "HGComb")]])
                         }
                         
                         
                         bgs_shp <- bgs_shp %>%
                           dplyr::mutate(across(starts_with("aq"),
                                                ~ifelse(.x %in% c("n/a","?","Unk"),
                                                        NA,
                                                        .x)))
                         
                         bgs_shp$country = paste0(x)
                         
                         bgs_shp <- bgs_shp %>%
                           dplyr::select(geometry,country,aq_typ,aq_productivity)
                         
                         return(bgs_shp)
                         
                       }
)

# Appending all country files as one dataset
bgs_aq_df <- do.call(rbind,country_list)

rm(country_list)

# Cleaning the data

bgs_aq_df <- bgs_aq_df %>%
  dplyr::mutate(aq_prod_fin = case_when(aq_productivity %in% c(" H","H(*)",
                                                               "H (V)","H(V)") ~ "H",
                                        aq_productivity %in% c("H/VH","H/VH(V)") ~ "H (VH)",
                                        aq_productivity %in% c("L/H(V)","L/HV",
                                                               "L/M(V)") ~ "L", #adding caution towards the lower end in cases of V (variable)
                                        aq_productivity %in% c("L/M (H)","L/M(H)") ~ "L(M/H)",
                                        aq_productivity %in% c("L (LM)") ~ "L/LM",
                                        aq_productivity %in% c("L (M)") ~ "L/M",
                                        aq_productivity %in% c("M (H)","M(H)","M/H(L)",
                                                               "M/H?") ~ "M/H",
                                        aq_productivity %in% c("M (V)","M/H(V)","M/VH(V)") ~ "M",
                                        TRUE ~ aq_productivity))

#I'll consider the middle value of the range
# In the values where two categories are mentioned (such as L/VL, I will consider the first one's range as reference and then consider the lower or upper range baed on the second part)
# For eg: L/VL. Range of L is 0.1-0.5. The middle value is thus 0.3. Since VL is below L, L/VL is in the range 0.1-0.3 and the middle value of that range is 0.2.
bgs_aq_df <- bgs_aq_df %>%
  dplyr::mutate(aq_prod_numeric = case_when(aq_prod_fin == "H" ~ 12.5,
                                            aq_prod_fin == "H (VH)" ~ 16.2,
                                            aq_prod_fin == "L" ~ 0.3,
                                            aq_prod_fin == "L(M/H)" ~ 0.45, # mid of L/M and L/H
                                            aq_prod_fin == "L/H" ~ 0.48, # dividing range of 0.3-0.5 into three parts
                                            aq_prod_fin == "L/LM" ~ 0.36,
                                            aq_prod_fin == "L/M" ~ 0.42,
                                            aq_prod_fin == "L/VL" ~ 0.2,
                                            aq_prod_fin == "LM" ~ 1.25,
                                            aq_prod_fin == "LM/H" ~ 1.46, # dividing range of 1.25-2 into two parts so current range = 1.25-1.675
                                            aq_prod_fin == "LM/VH" ~ 1.89,
                                            aq_prod_fin == "M" ~ 3.5,
                                            aq_prod_fin == "M/H" ~ 4,
                                            aq_prod_fin == "M/VH" ~ 4.5,
                                            aq_prod_fin == "U" ~ NA,
                                            aq_prod_fin == "U(L)" ~ 0.3,
                                            aq_prod_fin == "VH" ~ 25,
                                            aq_prod_fin == "VL" ~ 0.05,
                                            aq_prod_fin == "VL/H" ~ 0.075,
                                            TRUE ~ NA_real_))

# adding grid ID
bgs_aq_df <- bgs_aq_df %>%
  dplyr::mutate(bgs_ID = row_number())

# Saving dataset
saveRDS(bgs_aq_df,
        "data-clean/working_data/bgs/bgs_country_combined.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 2. Storage data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

storage_df <- read.table("data-raw/BGS/xyzASCII_gwstor_v1.txt",
                         header = TRUE,
                         sep = "",
                         dec = ".")

#using the key to add values
storage_df$bgs_storage = plyr::mapvalues(storage_df$GWSTOR_V2,
                                         from = c("0","L","LM","M","H","VH"),
                                         to = c(0,500,5500,17500,32500,75000))


storage_df <- sf::st_as_sf(storage_df,
                           coords = c("X", "Y"),
                           crs = 4326,
                           agr = "constant")


storage_df <- st_buffer(storage_df, dist = sqrt(450)*1000/sqrt(1.58), endCapStyle = "SQUARE")

storage_df$area <- units::set_units(st_area(storage_df), "km^2")

saveRDS(storage_df, "data-raw/BGS/bgs_storage.RDS")
gc(reset = TRUE)
Sys.sleep(90)

ssa_shp <- read_sf("data-raw/shapefile/WB_Boundaries_GeoJSON_highres/WB_countries_Admin0.geojson")
ssa_shp <- ssa_shp %>%
  dplyr::filter(REGION_WB == "Sub-Saharan Africa")

# Match CRSs
st_crs(ssa_shp) <- st_crs(storage_df)

# matching to country names
storage_df <- st_join(storage_df,
                      ssa_shp,
                      join = st_within)

# keeping SSA countries
storage_df <- storage_df %>%
  dplyr::filter(!is.na(WB_NAME))

gc(reset = TRUE)

storage_df$bgs_area = st_area(storage_df)

storage_df <- storage_df %>%
  dplyr::mutate(bgs_area_m2 = as.numeric(bgs_area),
                bgs_volume_m3 = bgs_area_m2 * as.numeric(bgs_storage) / 1000 ) %>%
  dplyr::select(bgs_area_m2, bgs_volume_m3, WB_NAME, ISO_A3,GWSTOR_V2,
                bgs_storage,geometry)

# save BGS volume data
saveRDS(storage_df,
        "data-clean/working_data/bgs/bgs_volume.RDS")

storage_df <- storage_df %>%
  as.data.frame() %>%
  dplyr::select(bgs_volume_m3,ISO_A3,WB_NAME) %>%
  dplyr::arrange(ISO_A3) %>%
  dplyr::group_by(ISO_A3) %>%
  dplyr::summarise(bgs_volume_country_km3 = sum(bgs_volume_m3,
                                                na.rm = T) / 1000000000)

write_xlsx(storage_df,
           "data-clean/working_data/bgs/country_bgs_volume.xlsx")

rm(storage_df)
gc(reset = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 3. Recharge data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# computing mean recharge by country

# Load Data
recharge_ras <- raster::raster("data-raw/BGS/recharge_maps/Africa_Recharge_Map/Africa_recharge.tif")

# Match CRSs
st_crs(ssa_shp) <- st_crs(recharge_ras)

# Crop the raster using the mask function
ssa_recharge_ras <- recharge_ras %>% mask(ssa_shp)

ssa_recharge_df <- as.data.frame(ssa_recharge_ras,
                                 xy = TRUE)

# convert to sf
ssa_recharge_df <- sf::st_as_sf(ssa_recharge_df,
                                coords = c("x", "y"),
                                crs = st_crs(ssa_shp),
                                agr = "constant")

# matching to country names
ssa_recharge_df <- st_join(ssa_recharge_df,
                           ssa_shp,
                           join = st_within)

# taking the mean recharge by country
ssa_recharge_df <- ssa_recharge_df %>%
  dplyr::arrange(ISO_A3) %>%
  dplyr::group_by(ISO_A3) %>%
  dplyr::summarise(mean_recharge = mean(Africa_recharge,na.rm = T))


write_xlsx(ssa_recharge_df %>%
             as.data.frame() %>%
             dplyr::select(-geometry),
           "data-clean/working_data/bgs/bgs_recharge_country.xlsx")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# 4. Depth to GW data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# computing mean depth by country

# Load Data
depth_df <- read.table("data-raw/BGS/DepthToGroundwater_V2/xyzASCII_dtwmap_v2.txt",
                       header = TRUE,
                       sep = "",
                       dec = ".")

#using the key to add values
depth_df$bgs_depth = plyr::mapvalues(depth_df$DTWAFRICA_,
                                     from = c("VS","S","SM","M","D","VD"),
                                     to = c(3.5,16,37.5,75,175,300))

# matching with country data

depth_df <- sf::st_as_sf(depth_df,
                         coords = c("X", "Y"),
                         crs = 4326,
                         agr = "constant")

st_crs(ssa_shp) <- st_crs(depth_df)

# matching to country names
depth_df <- st_join(depth_df,
                    ssa_shp,
                    join = st_within)

# keeping SSA countries
depth_df <- depth_df %>%
  dplyr::filter(!is.na(WB_NAME)) %>%
  dplyr::select(DTWAFRICA_, bgs_depth,geometry,geometry,WB_NAME,ISO_A3) %>%
  dplyr::rename(bgs_depth_m = bgs_depth)


depth_df <- st_buffer(depth_df,
                      dist = 0.27,
                      endCapStyle = "SQUARE")

saveRDS(depth_df,
        "data-clean/working_data/bgs/bgs_gwdepth.RDS")

# Finding mean depth by country

depth_c <- depth_df %>%
  as.data.frame() %>%
  dplyr::ungroup() %>%
  dplyr::select(ISO_A3,bgs_depth_m) %>%
  dplyr::group_by(ISO_A3) %>%
  dplyr::summarise(mean_depth_country_m = mean(as.numeric(bgs_depth_m),
                                                na.rm = T))

write_xlsx(depth_c,
           "data-clean/working_data/bgs/bgs_country_gwdepth.xlsx")