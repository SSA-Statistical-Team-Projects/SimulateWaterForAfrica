rm(list=ls())
gc(reset = TRUE)

pacman::p_load("sf", "data.table", "raster", "haven", "units", "expss",
               "exactextractr","cropDemand", "expss")

sf::sf_use_s2(FALSE)

#### read in the full aquifer datasets (grid and typology)

aquifer_gpkg <- sf::st_layers("data-raw/world-bank-aquifer-data/aqtyp_gwresource_grid05deg.gpkg")

aqgrid_dt <- sf::st_read("data-raw/world-bank-aquifer-data/aqtyp_gwresource_grid05deg.gpkg",
                         layer = "aqtyp_gwresource_grid05deg")

aqgrid_dt$poly_area <- sf::st_area(aqgrid_dt, "m^2")

aqtyp_gpkg <- sf::st_layers("data-raw/world-bank-aquifer-data/aqtyp_dissolved.gpkg")

aqtyp_dt <- sf::st_read("data-raw/world-bank-aquifer-data/aqtyp_dissolved.gpkg",
                        layer = "aqtyp_dissolved")

#### WARNING: DO NOT RUN PAST THIS POINT YET! ##################################

### combine this with the global shapefiles with world bank country names
countryshp_dt <- sf::st_read("data-raw/shapefile/WB_Boundaries_GeoJSON_highres/WB_countries_Admin0.geojson")

intersect_dt <- st_intersection(aqgrid_dt, countryshp_dt)

intersect_dt <- intersect_dt %>% st_make_valid()

intersect_dt$intersect_area <- st_area(intersect_dt, units = "m^2")

### include information on how much water is used for agricultural and non-agricultural use in African countries

agwater_dt <- fread("data-raw/water-use/agricultural-water-withdrawals.csv")
indwater_dt <- fread("data-raw/water-use/industrial-water-withdrawal.csv")
hhwater_dt <- fread("data-raw/water-use/municipal-water-withdrawal.csv")

wateruse_dt <- Reduce(dplyr::left_join, 
                      list(hhwater_dt, agwater_dt, indwater_dt))

setnames(wateruse_dt,
         c("Municipal water withdrawal",
           "Agricultural water withdrawal",
           "Industrial water withdrawal"),
         c("hhwater_use", "agwater_use", "indwater_use"))

wateruse_dt[, total_use := hhwater_use + agwater_use + indwater_use]


### compute resource and change units for reference in aqshp_dt and intersect_dt
intersect_dt$resource <- intersect_dt$resource * 10^9
intersect_dt$resource_norm <- intersect_dt$resource_norm * 10^9


intersect_dt <- as.data.table(intersect_dt)

intersect_dt[,intgrid_id := 1:.N, by = "grid_id"]
intersect_dt[,intgrid_id := as.integer(paste0(grid_id, intgrid_id))]

intersect_dt[, country_assignment := ifelse(intersect_area == max(intersect_area),
                                            1,
                                            0),
             by = grid_id]

intersect_dt[, c("resource", "resource_norm") := list(set_units(resource, "m^3"),
                                                      set_units(resource_norm, "m^3"))]


### recalculate volume to adjust for the difference in shapefiles during the intersection
##### this will create intersected resource and the intersected resource norm variables in the data
intersect_dt[, int_resource := (intersect_area / poly_area) * resource]
intersect_dt[, int_resource_norm := (intersect_area / poly_area) * resource_norm]

#### add variable labels to the data
intersect_dt <- apply_labels(intersect_dt,
                             grid_id = "OBJECTID from 0.5 degree fishnet",
                             lat = "latitude from 0.5 degree fishnet",
                             lon = "longitude from 0.5 degree fishnet",
                             grid_area = "area of the grid cell (m2)",
                             grid_area_norm = "normalized <minmax> grid_area",
                             aqtyp_max = "Typology class with highest % grid cell (excluding NA)",
                             aqtyp_pct_ma = "share of grid cell that is `1. major alluvial`",
                             aqtyp_pct_cx = "share of grid cell that is `2. complex`",
                             aqtyp_pct_kt = "share of grid cell that is `3. karstic`",
                             aqtyp_pct_ls = "share of grid cell that is `4. local/shallow`",
                             aqtyp_pct_NA = "share of grid cell that is not covered by data for typology classification",
                             resource_pct_grid_na = "% of grid cell not covered by country-lithological-outcrop resource data",
                             resource = "sum of resource w/in grid cell (m3/yr)",
                             resource_norm = "normalized resource if grid were 100% filled (m3/yr)",
                             poly_area = "polygon area of grid_id in original aquifer dataset",
                             intersect_area = "polygon area after intersection with country shapefile",
                             intgrid_id = "polygon ID after intersection",
                             country_assignment = "1 if intgrid_id is the largest area for each grid_id, 0 otherwise",
                             int_resource = "estimated resource to adjust for intersection with country shp",
                             int_resource_norm = "estiamted resource norm to adjust for intersection with country shp",
                             WB_NAME = "World Bank Country Name",
                             WB_REGION = "World Bank Region",
                             WB_A3 = "World Bank 3-letter Country Code",
                             ISO_A3 = "Official ISO-3 Code",
                             POP_EST = "Population Estimate",
                             POP_YEAR = "Year of Population Estimate",
                             GDP_MD_EST = "GDP Estimate",
                             INCOME_GRP = "Official WB Income Group")

wateruse_dt <- apply_labels(wateruse_dt,
                            Entity = "Country Name",
                            Code = "ISO-3 Code",
                            Year = "Year",
                            hhwater_use = "2015 freshwater household consumption (m3)",
                            agwater_use = "2015 freshwater agricultural consumption (m3)",
                            indwater_use = "2015 freshwater industrial consumption (m3)",
                            total_use = "2015 aggregate (hh + ag + ind) freshwater consumption (m3)")


### include evapotranspiration and precipitation data and use it to compute recharge rates
# cropDemand::download_terraclimate(dir_out = "data-raw/TerraClimate/", 
#                                   variable = "pet", 2020, 
#                                   region = NULL, 
#                                   sub_region = NULL)
pet_raster <- raster::brick("data-raw/TerraClimate/TerraClimate_pet_2020.nc")

pet_raster <- raster::mean(pet_raster)

intersect_dt <- as.data.frame(intersect_dt)

intersect_dt[["pet2022"]] <- exact_extract(x = pet_raster,
                                           y = st_as_sf(intersect_dt,
                                                        crs = 4326,
                                                        agr = "constant"),
                                           fun = "mean")


# download precipitation raster
# cropDemand::download_terraclimate(dir_out = "data-raw/TerraClimate/", 
#                                   variable = "ppt", 2020, 
#                                   region = NULL, 
#                                   sub_region = NULL)
ppt_raster <- raster::brick("data-raw/TerraClimate/TerraClimate_ppt_2020.nc")
ppt_raster <- raster::mean(ppt_raster)

intersect_dt[["ppt2022"]] <- exact_extract(x = ppt_raster,
                                           y = st_as_sf(intersect_dt,
                                                        crs = 4326,
                                                        agr = "constant"),
                                           fun = "mean")

alpha <- 0.72
beta <- 15.1

intersect_dt <- as.data.table(intersect_dt)

intersect_dt[, aridity := ifelse(ppt2022 != 0, pet2022 / ppt2022, NA)]

intersect_dt[, afactor := log(alpha^beta + 1)]

intersect_dt[, gw_recharge := ppt2022 * alpha * (1 - (afactor / (1 + afactor)))]

intersect_dt <- apply_labels(intersect_dt,
                             gw_recharge = "2022 groundwater recharge rates",
                             ppt2022 = "2022 precipitation in mm",
                             pet2022 = "2022 potential evapotranspiration in mm")


#### estimate population from global raster
intersect_dt <-
  intersect_dt %>%
  as.data.frame() %>%
  st_as_sf(crs = 4326,
           agr = "constant")

pop_raster <- raster("data-raw/SEDAC/Global_2020_PopulationDensity30sec_GPWv4.tiff")
crs(pop_raster) <- crs(intersect_dt)
# intersect_dt[["grid_pop"]] <- exactextractr::exact_extract(x = pop_raster,
#                                                            y = intersect_dt,
#                                                            fun = "sum")

intersect_dt[["grid_pop"]] <- parallel_zonalstats(x = pop_raster,
                                                  y = intersect_dt,
                                                  fun = "sum",
                                                  numCores = 30)

intersect_dt <- apply_labels(intersect_dt,
                             grid_pop = "2020 grid population from NASA EOG")


countryshp_dt[["POP_EST_2020"]] <- exactextractr::exact_extract(x = pop_raster,
                                                                y = countryshp_dt,
                                                                fun = "sum")

### clean up intersect_dt shapefile
intersect_dt <- intersect_dt %>% st_cast("MULTIPOLYGON")


##write data to data-clean folder
saveRDS(intersect_dt[,c("intgrid_id", "country_assignment", colnames(aqshp_dt),
                        "WB_NAME", "REGION_WB", "WB_A3", "ISO_A3",
                        "POP_EST", "POP_YEAR", "GDP_MD_EST", "INCOME_GRP",
                        "int_resource", "int_resource_norm", "gw_recharge",
                        "ppt2022", "pet2022", "grid_pop"),],
        "data-clean/world-bank-aquifer-data/aquifermasterpoly.RDS")

saveRDS(wateruse_dt, 
        "data-clean/working_data/wateruse.RDS")