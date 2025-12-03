#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Simulating Income Gains
# Author: Ifeanyi N. Edochie
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will simulate income gains attributable to the production gains
# and then export the results of the welfare analysis.

if (!"pacman" %in% installed.packages()[, "Package"]) {
  install.packages("pacman")
}

pacman::p_load(sf, data.table, tidyverse, readxl, ggplot2, xtable, ggrepel)

sf::sf_use_s2(FALSE)

lapply(X = list.files("R", full.names = T),
       FUN = source)


#%%%%%%%%%%%%%%%%%%%
# Loading Data
#%%%%%%%%%%%%%%%%%%%

#### read in the price data
price_dt <- readRDS("data-raw/foodprices.RDS")

### read in the productivity gains polygon grid data
grid_dt <- sf::st_read(dsn = "data-clean/working_data/gaez/grid_polygon",
                       layer = "unique_grid_polygons")

gains_dt <- readRDS("data-clean/working_data/simulation_results_dataframe_allcons_depth7m.RDS")

agpop_dt <-
  readxl::read_excel("data-raw/USDA/AgTFPInternational2022_long.xlsx", 3) |>
  dplyr::filter(Year == 2020 & Region == "SSA" & Attribute == "Labor_Q") |>
  mutate(agpop = Value * 1000) |>
  rename(country_code = "ISO3",
         country_name = "Country/territory") |>
  dplyr::select(country_code, country_name, agpop)

crop_df <- readRDS("data-clean/working_data/gaez/gaez_crop_year_analysis_final.RDS")

code_dt <- 
  crop_df |>
  st_drop_geometry() |>
  dplyr::select(WB_NAME, ISO_A3, srno) |>
  unique()

# ### read in the country shapefile
# code_dt <- sf::st_read("data-raw/shapefile/WB_Boundaries_GeoJSON_highres/WB_countries_Admin0.geojson") %>%
#   st_drop_geometry() %>%
#   select(WB_NAME, ISO_A3)

grid_dt <-
  grid_dt %>%
  merge(code_dt, on = "srno")

#%%%%%%%%%%%%%%%%%%%
# Cleaning Data
#%%%%%%%%%%%%%%%%%%%

price_dt <-
  price_dt %>%
  mutate(lat = as.numeric(lat)) %>%
  mutate(lon = as.numeric(lon)) %>%
  filter(!is.na(lat) & !is.na(lon) & year >= 2017 & year <= 2023) %>%
  st_as_sf(crs = 4326, agr = "constant", coords = c("lon", "lat")) %>%
  select(h_wheat, l_wheat, o_wheat, c_wheat,
         h_maize, l_maize, o_maize, c_maize,
         h_rice, l_rice, o_rice, c_rice,
         ISO3, country, adm1_name, adm2_name,
         mkt_name, geo_id, year, month)

### convert prices to numerics

##### if open and close exist use that otherwise, do high and low prices instead
price_dt <- as.data.table(price_dt)

price_vars <- colnames(price_dt)[grepl(pattern = "h_|l_|o_|c_",
                                       x = colnames(price_dt))]

price_dt[, (price_vars) := lapply(.SD, as.numeric), .SDcols = price_vars]

### compute average prices for wheat, maize, rice
price_dt[, whe := apply(X = .SD, MARGIN = 1, FUN = rowprice_sum),
         .SDcols = colnames(price_dt)[grepl(pattern = "_wheat",
                                            x = colnames(price_dt))]]
price_dt[, mze := apply(X = .SD, MARGIN = 1, FUN = rowprice_sum),
         .SDcols = colnames(price_dt)[grepl(pattern = "_maize",
                                            x = colnames(price_dt))]]
price_dt[, rcw := apply(X = .SD, MARGIN = 1, FUN = mean, na.rm = TRUE),
         .SDcols = colnames(price_dt)[grepl(pattern = "_rice",
                                            x = colnames(price_dt))]]

#### include the ppp and cpi data
pfw_dt <-
  readxl::read_excel(path = "data-clean/working_data/CPI_PPP_Monthly.xlsx") %>%
  as.data.table()

setnames(price_dt, "ISO3", "code")

price_dt <- pfw_dt[, c("code", "ppp",
                       "monthly_cpi",
                       "year", "month")][price_dt, on = c("code", "year", "month")]


### include the country names
price_dt <- price_dt[code %in% c("MRT", "MLI", "NER", "TCD", "SDN",
                                 "ERI", "CPV", "SEN", "BFA", "ETH",
                                 "NGA", "GMB", "CMR", "GNB", "GIN",
                                 "BEN", "SSD", "SOM", "GHA", "TGO",
                                 "CAF", "CIV", "SLE", "LBR", "COD",
                                 "KEN", "UGA", "GNQ", "COG", "GAB",
                                 "STP", "TZA", "RWA", "BDI", "AGO",
                                 "SHN", "ZMB", "MWI", "MOZ", "COM",
                                 "MDG", "ZWE", "NAM", "BWA", "MUS",
                                 "ZAF", "SWZ", "LSO"),]


### compute the real prices two sets of real prices
exchange_dt <- fread("data-clean/working_data/exchange_rates.csv", header = TRUE)

exchange_dt <-
  exchange_dt %>%
  dplyr::select(-`Indicator Name`, -`Indicator Code`) %>%
  melt(id.vars = c("Country Name", "Country Code"),
       variable.name = "year",
       value.name = "exchange_rate") %>%
  setnames(old = c("Country Name", "Country Code"),
           new = c("country", "code")) %>%
  mutate(year = as.integer(as.character(year)))

price_dt <-
  price_dt %>%
  merge(exchange_dt, on = c("code", "year"))

### conversions by ppp and cpi
price_dt[, whe_ppp := whe / ppp / monthly_cpi]
price_dt[, rcw_ppp := rcw / ppp / monthly_cpi]
price_dt[, mze_ppp := mze / ppp / monthly_cpi]
### conversions by exchange rate
price_dt[, whe_usd := whe / exchange_rate]
price_dt[, rcw_usd := rcw / exchange_rate]
price_dt[, mze_usd := mze / exchange_rate]

#### merge price data to the grid
price_dt[, price_id := 1:.N]

price_dt <-
  price_dt %>%
  st_as_sf(crs = 4326,
           agr = "constant")

##### get the unique market locations
price_dt <-
  price_dt %>%
  mutate(lat = st_coordinates(geometry)[, "Y"]) %>%
  mutate(lon = st_coordinates(geometry)[, "X"]) %>%
  st_drop_geometry()

plocs_dt <-
  price_dt %>%
  select(code, country, adm1_name, adm2_name, mkt_name, lat, lon) %>%
  unique() %>%
  st_as_sf(coords = c("lon", "lat"),
           agr = "constant",
           crs = 4326)


#### now merge with the grid
grid_dt <-
  price_dt[, lapply(.SD, mean, na.rm = TRUE),
           .SDcols = colnames(price_dt)[grepl(pattern = "whe|mze|rcw",
                                              x = colnames(price_dt))],
           by = c("code", "country", "adm1_name", "adm2_name", "mkt_name",
                  "lat", "lon")] %>%
  st_as_sf(coords = c("lon", "lat"),
           agr = "constant",
           crs = 4326) %>%
  select(code, country, adm1_name, adm2_name, mkt_name, contains("_ppp"), contains("_usd")) %>%
  st_join(x = grid_dt,
          y = .,
          join = st_nearest_feature)

#### include the nominal price data as well
grid_dt <- grid_dt %>%
  as.data.table() %>%
  .[, grid_ID := 1:.N] %>%
  st_as_sf(crs = 4326, agr = "constant")

grid_dt <-
  price_dt[, lapply(.SD, mean, na.rm = TRUE),
           .SDcols = c("whe",  "mze", "rcw"),
           by = c("code", "country", "adm1_name",
                  "adm2_name", "mkt_name",
                  "lat", "lon")] %>%
  st_as_sf(coords = c("lon", "lat"),
           agr = "constant",
           crs = 4326) %>%
  select(whe, mze, rcw) %>%
  st_join(x = grid_dt,
          y = .,
          join = st_nearest_feature) %>%
  select(whe, mze, rcw, grid_ID) %>%
  st_drop_geometry() %>%
  as.data.table() %>%
  .[grid_dt %>%
      as.data.table(), on = "grid_ID"]

### quickly change column names
setnames(grid_dt,
         old = c("whe", "mze", "rcw"),
         new = paste0(c("whe", "mze", "rcw"), "_nom"))

#### convert the grid prices to long from wide
grid_dt <-
  grid_dt %>%
  pivot_longer(cols = matches("(nom|ppp|usd)$"),
               names_to = c("crop_code", ".value"),
               names_sep = "_")

grid_dt <- as.data.table(grid_dt)
gains_dt <- as.data.table(gains_dt)

gpgrid_dt <- grid_dt[gains_dt, on = c("srno", "crop_code")]


#### compute the income gains at the country level
##### converting tonnes to kg 
gpgrid_dt[, incomegains_ppp := cland_convert_prod_gain_1000tonne * 1e6 * ppp]
gpgrid_dt[, incomegains_usd := cland_convert_prod_gain_1000tonne * 1e6 * usd]
gpgrid_dt[, incomegains_nom := cland_convert_prod_gain_1000tonne * 1e6 * nom]


income_dt <-
  gpgrid_dt[, lapply(.SD, sum, na.rm = TRUE),
            .SDcols = c("cland_convert_prod_gain_1000tonne",
                        "incomegains_ppp",
                        "incomegains_usd",
                        "incomegains_nom"),
            by = c("ISO_A3", "WB_NAME", "crop_code", "model_name",
                   "depth_constraint", "recharge_constraint",
                   "prod_constraint")]

income_dt <- income_dt[!is.na(ISO_A3), ]


### to get the hectarage go ahead and use
farm_dt <-
  crop_df %>%
  select(crop_code, har_rainfed, srno) 

gpgrid_dt <-
  farm_dt %>%
  as.data.table() %>%
  .[gpgrid_dt, on = c("srno", "crop_code")]


### compute income per hectare
gpgrid_dt[, incomegains_ppp_perhect :=
            cland_convert_prod_gain_1000tonne * 1e6 * ppp / (har_rainfed * 1000)]
gpgrid_dt[, incomegains_usd_perhect :=
            cland_convert_prod_gain_1000tonne * 1e6 * usd / (har_rainfed * 1000)]
gpgrid_dt[, incomegains_nom_perhect :=
            cland_convert_prod_gain_1000tonne * 1e6 * nom / (har_rainfed * 1000)]


add_dt <-
  gpgrid_dt[, lapply(.SD, mean, na.rm = TRUE),
            .SDcols = c("incomegains_ppp_perhect",
                        "incomegains_usd_perhect",
                        "incomegains_nom_perhect"),
            by = c("ISO_A3", "WB_NAME", "crop_code", "model_name",
                   "depth_constraint", "recharge_constraint",
                   "prod_constraint")]

income_dt <- add_dt[, c("ISO_A3", "WB_NAME",
                        "crop_code", "model_name",
                        "incomegains_ppp_perhect",
                        "incomegains_usd_perhect",
                        "incomegains_nom_perhect")][income_dt,
                                                    on = c("ISO_A3", "WB_NAME",
                                                           "crop_code", "model_name")]

### income per capita
# pop_dt <- fread("data-clean/working_data/population2010.csv")
# agpop_dt <- fread("data-clean/working_data/estimation/ag_employment.csv")
# setnames(pop_dt,
#          old = c("Country Code", "y2010"),
#          new = c("ISO_A3", "population"))
#
# income_dt <- pop_dt[, c("ISO_A3", "population")][income_dt, on = c("ISO_A3")]
# income_dt[, population := as.numeric(population)]

income_dt <-
  income_dt |>
  left_join(agpop_dt,
            by = c("ISO_A3" = "country_code",
                   "WB_NAME" = "country_name"))

income_dt[, incomegains_ppp_pcapinag := incomegains_ppp / agpop]
income_dt[, incomegains_usd_pcapinag := incomegains_usd / agpop]
income_dt[, incomegains_nom_pcapinag := incomegains_nom / agpop]

# setnames(agpop_dt,
#          old = c("ISO3", "2010ag"),
#          new = c("ISO_A3", "agemprate"))
#
# income_dt <- agpop_dt[, c("ISO_A3", "agemprate")][income_dt, on = "ISO_A3"]
# income_dt[, agemppop := 0.01 * agemprate * population ]
#
# income_dt[, incomegains_ppp_pcapinag := incomegains_ppp / agemppop]
# income_dt[, incomegains_usd_pcapinag := incomegains_usd / agemppop]
# income_dt[, incomegains_nom_pcapinag := incomegains_nom / agemppop]

income_dt[, rate_extremepov_ppp := incomegains_ppp_pcapinag / (1.90*365)]
income_dt[, rate_extremepov_usd := incomegains_usd_pcapinag / (1.90*365)]

### only keep the data with ISO3 codes since the NAs are not countries
x <- income_dt[!is.na(ISO_A3), ]


fwrite(income_dt[, c("WB_NAME", "ISO_A3", "incomegains_ppp",
                     "incomegains_usd", "incomegains_ppp_pcapinag",
                     "incomegains_usd_pcapinag", "rate_extremepov_ppp",
                     "rate_extremepov_usd")],
       "data-clean/results/income_gains.csv")

#### plot distribution of income gains percent of poverty line

crop_dt <- data.table(crop_name = c("wheat", "rice", "maize"),
                      crop_code = c("whe", "rcw", "mze"))

income_dt <- crop_dt[income_dt, on = "crop_code"]


map_dt <- expand.grid(crop_dt$crop_name,
                      c("SD", "SRHS")) %>%
  as.data.table() %>%
  setnames(old = c("Var1", "Var2"),
           new = c("crop", "sim")) %>%
  mutate(crop = as.character(crop)) %>%
  mutate(sim = as.character(sim))

income_dt[, percent_extremepov_ppp := rate_extremepov_ppp * 100]
income_dt[, percent_extremepov_usd := rate_extremepov_usd * 100]


# mapply(FUN = barplot_income_gains,
#        dt = rep(income_dt, nrow(map_dt)),
#        model_name = map_dt$crop,
#        crop_name = map_dt$sim,
#        outvar = "percent_extremepov_ppp")

### compute the expected change in nominal income per capita as a result
##### read in nominal gdp per capita data in LCU

gdppcap_dt <- fread("data-clean/gdppcaplcu.csv")

setnames(gdppcap_dt,
         old = c("Country Name", "Country Code"),
         new = c("WB_NAME", "ISO_A3"))

income_dt <- gdppcap_dt[income_dt, on = c("ISO_A3", "WB_NAME")]

income_dt[, percent_income_pcapinag_ingdplcu := (incomegains_nom_pcapinag / gdppcaplcu) * 100]


for (row in 1:nrow(map_dt)){
  
  barplot_income_gains(dt = income_dt,
                       model = map_dt$sim[[row]],
                       crop = map_dt$crop[[row]],
                       outvar = "percent_extremepov_ppp",
                       nametag = "pfwadj")
  
  
}



for (row in 1:nrow(map_dt)){
  
  barplot_income_gains(dt = income_dt,
                       model = map_dt$sim[[row]],
                       crop = map_dt$crop[[row]],
                       outvar = "percent_extremepov_usd",
                       nametag = "exchange")
  
  
}



### keep only the three crops we care about
income_dt <- income_dt[ crop_code %in% c("whe", "rcw", "mze"),]


##### compute overall changes

overall_dt <- income_dt[, lapply(.SD, sum, na.rm = TRUE),
                        .SDcols = c("percent_extremepov_ppp", "percent_extremepov_usd"),
                        by = c("WB_NAME", "ISO_A3", "model_name")]


for (i in seq_along(c("SD", "SRHS"))){
  
  barplot_totalincome_gains(dt = income_dt,
                            model = c("SD", "SRHS")[i],
                            outvar = "percent_extremepov_ppp",
                            nametag = "pfwadj")
  
}




for (i in seq_along(c("SD", "SRHS"))){
  
  barplot_totalincome_gains(dt = income_dt,
                            model = c("SD", "SRHS")[i],
                            outvar = "percent_extremepov_usd",
                            nametag = "exchange")
  
}


##### include a plot of the maps
for (i in seq_along(c("SD", "SRHS"))){
  
  barplot_totalincome_gains(dt = income_dt,
                            model = c("SD", "SRHS")[i],
                            outvar = "percent_income_pcapinag_ingdplcu",
                            nametag = "incomepergdplcu")
  
}

allnom_dt <- income_dt[, sum(percent_income_pcapinag_ingdplcu, na.rm = TRUE),
                       by = c("WB_NAME", "ISO_A3", "model_name")]

setnames(allnom_dt, "V1", "percent_income_pcapinag_ingdplcu")


for (i in seq_along(c("SD", "SRHS"))){
  
  barplot_totalincome_gains(dt = allnom_dt,
                            model = c("SD", "SRHS")[i],
                            outvar = "percent_income_pcapinag_ingdplcu",
                            nametag = "ALL_incomepergdplcu")
  
}



#### create the xtables for the paper

total_dt <- income_dt[, sum(percent_extremepov_ppp, na.rm = TRUE), by = c("WB_NAME", "ISO_A3", "model_name")]

setnames(total_dt, "V1", "overall_inc_gains_change_povline")

srhs_dt <- total_dt[model_name == "SRHS",]
sd_dt <- total_dt[model_name == "SD",]

write.csv(total_dt[order(-overall_inc_gains_change_povline) & model_name == "SRHS",
                   c("WB_NAME", "ISO_A3","overall_inc_gains_change_povline")],
          "data-clean/results/income_gains_change_SRHS.csv")

write.csv(total_dt[order(-overall_inc_gains_change_povline) & model_name == "SD",
                   c("WB_NAME", "ISO_A3","overall_inc_gains_change_povline")],
          "data-clean/results/income_gains_change_SD.csv")



### quick create .tex file
tex_dt <- read.csv("data-clean/results/income_gains_change_SRHS.csv")

tex_dt <- as.data.table(tex_dt)

tex_dt <- tex_dt[, 2:4]

# setnames(tex_dt,
#          c("overall_inc_gains_change_povline",
#            "Country.Code"),
#          c("income_gains", "code"))

colnames(tex_dt) <- c("Country", "ISO-3 Code", "Gains (%)")

latex_table <- xtable(tex_dt,
                      caption = "% Income Gains Per Capita of 1.90 USD International Poverty Line (SRHS Model)")

# writeLines(
#   xtable::xtable(latex_table, caption = "Percentage Income Gains Per Capita of International Poverty Line"),
#   "output/tables/simulation/incomegains_percapita.tex"
# )


# Create LaTeX table object
# Save LaTeX table to a .tex file
sink("output/tables/simulation/incomegains_percapita_SRHS.tex")
print(latex_table, caption.placement = "top")
sink()



tex_dt <- read.csv("data-clean/results/income_gains_change_SD.csv")

tex_dt <- as.data.table(tex_dt)

# setnames(tex_dt,
#          c("Income.Gains....of.1.90.USD.extreme.poverty.line.",
#            "Country.Code"),
#          c("income_gains", "code"))

tex_dt <- tex_dt[, 2:4]

colnames(tex_dt) <- c("Country", "ISO-3 Code", "Gains (%)")

latex_table <- xtable(tex_dt,
                      caption = "% Income Gains Per Capita of 1.90 USD International Poverty Line (SD Model)")

# writeLines(
#   xtable::xtable(latex_table, caption = "Percentage Income Gains Per Capita of International Poverty Line"),
#   "output/tables/simulation/incomegains_percapita.tex"
# )


# Create LaTeX table object
# Save LaTeX table to a .tex file
sink("output/tables/simulation/incomegains_percapita_SD.tex")
print(latex_table, caption.placement = "top")
sink()




#### draw historgram

# Sort by income gains
srhs_dt <- srhs_dt[order(overall_inc_gains_change_povline)]

# Compute ECDF manually to get proper (x, y) values for labeling
srhs_dt[, ecdf_y := ecdf(overall_inc_gains_change_povline)(overall_inc_gains_change_povline)]

# Create formatted labels: "Country Name (Gain)"
srhs_dt[, label := paste0(WB_NAME, " (", round(overall_inc_gains_change_povline, 4), ")")]

# Plot ECDF with correctly positioned labels
p <- ggplot(srhs_dt, aes(x = overall_inc_gains_change_povline)) +
  stat_ecdf(geom = "step", color = "blue", size = 1) +  # Proper ECDF calculation
  geom_point(aes(y = after_stat(y)), stat = "ecdf", color = "red", size = 2) +  # Add points on ECDF
  geom_text_repel(data = srhs_dt, aes(y = ecdf_y, label = label),  # Use precomputed ECDF values
                  size = 3,
                  nudge_y = 0.05,
                  direction = "y",
                  force = 5,
                  box.padding = 0.6,
                  point.padding = 0.3,
                  segment.color = NA) +  # Remove arrows
  scale_x_continuous(labels = scales::comma) +  # Standard x-axis tick labels
  labs(title = "Cumulative Distribution of Income Gains",
       x = "Income Gains Change (Pov Line)",
       y = "Empirical Cumulative Probability") +
  theme_minimal()

# Save the plot as a PNG
ggsave(filename = "cumulative_income_gains.png",
       plot = p,
       width = 10,
       height = 6,
       dpi = 300)