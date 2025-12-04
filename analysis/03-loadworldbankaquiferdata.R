################################################################################
#################### PREPARE THE AQUIFER DATA ##################################
################################################################################

## environment set up
remove(list = objects()) ## clear all objects in R workspace

options(
  stringsAsFactors = F, ## tell R to treat text as text, not factors
  width = 100, ## set the maximum width of outputs as 80 characters
  scipen = 6, ## discourage R from displaying numbers in scientific notation
  start.time= Sys.time()
)


############## -------------- read in files ------------------- ###############

directory <- "data-raw/shapefile/WB_Boundaries_GeoJSON_highres/"

afrshp_dt <-
  sf::st_read(paste0(directory,
                     "WB_countries_Admin0.geojson"))


afrshp_dt <- afrshp_dt[afrshp_dt$WB_REGION == "AFR",]

## read in the aquifer data
aquifer_dt <- haven::read_dta("data-raw/world-bank-aquifer-data/aqtyp_gwresource_grid05deg.dta")

aquifer_dt <- sf::st_as_sf(aquifer_dt,
                           crs = 4326,
                           agr = "constant",
                           coords = c("lon", "lat"))

### plot the aquifer data on top of the shapefile so we can see where the
### aquifers are with respect to the data

afrshp_dt$fill_color <- "blue"

ggplot2::ggplot() +
  ggplot2::geom_sf(data = afrshp_dt) +
  ggplot2::geom_sf(data = aquifer_dt, fill = "blue", alpha = 0.5) +
  ggplot2::theme_bw()


### merge the country information to the aquifer data

afraquifer_dt <- sf::st_join(aquifer_dt, afrshp_dt)

afraquifer_dt <- afraquifer_dt[is.na(afraquifer_dt$ISO_A3) == FALSE,]

### save the datasets
saveRDS(afraquifer_dt, "data-clean/working_data/world-bank-aquifer-data/africa_aquifer.RDS")
saveRDS(afrshp_dt, "data-raw/shapefile/africa_shapefile.RDS")












