#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Visualizing Simulation Results
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will construct maps and histograms to present our results

rm(list=ls())

# Loading Packages
if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  
  ncdf4, raster, chron, tidyverse, lubridate, sf, exactextractr,
  rgdal, haven, spatialEco, viridis, grid, gridExtra, plyr, dplyr,
  data.table, ggpubr, paletteer
)

sf::sf_use_s2(FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Loading data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sim_df <- readRDS("data-clean/working_data/simulation_results_dataframe_allcons_depth7m.RDS")

crop_df <- readRDS("data-clean/working_data/gaez/gaez_crop_year_analysis_final.RDS")

grid_dt <- sf::st_read(dsn = "data-clean/working_data/gaez/grid_polygon",
                       layer = "unique_grid_polygons")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Maps
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Merging in geometries
sim_df <- left_join(sim_df,
                    crop_df %>%
                      dplyr::filter(crop_code =="mze") %>%
                      dplyr::select(srno,WB_NAME),
                    by = "srno")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# By crop
cereal_list <- c("mze","rcw","whe")
cereal_name <- c("Maize","Rice","Wheat")

# Distribution of gains
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

grid_dt <- left_join(grid_dt,
                     sim_df %>%
                       dplyr::select(c(srno,crop_code,cland_convert_prod_gain_1000tonne)),
                     by = "srno")
sfgains_dt <- 
  grid_dt |>
  mutate(
    gains_indicator = case_when(
      is.na(cland_convert_prod_gain_1000tonne) ~ "No Simulated Results",
      cland_convert_prod_gain_1000tonne > 0 ~ "Gain",
      cland_convert_prod_gain_1000tonne <= 0 ~ "No Gain"
    )
  ) |>
  mutate(gains_indicator = factor(gains_indicator, levels = c("Gain", "No Gain", "No Simulated Results"))) |>
  mutate(crop_name = case_when(
    crop_code == "mze" ~ "Maize",
    crop_code == "rcw" ~ "Rice",
    crop_code == "whe" ~ "Wheat",
    TRUE ~ NA_character_
  )) |>
  as_tibble() |>
  st_as_sf(crs = 4326, agr = "constant")

sfgains_plot <- 
  sfgains_dt |>
  ggplot() +
  geom_sf(aes(fill = gains_indicator), color = NA) +
  scale_fill_manual(values = c("Gain" = "#228B22", 
                               "No Gain" = "#800000", 
                               "No Simulated Results" = "gray88"),
                    na.value = "white") +
  facet_wrap(~crop_name, nrow = 1) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  size = 18)) +
    coord_sf(crs = st_crs(4326),
             ylim = c(-36,28),
             xlim= c(-20,55))

ggsave(filename = "output/graphs/production_gains_distribution.png",
       plot = sfgains_plot,
       width = 10, height = 4, dpi = 150, units = "in",
       device = 'png')

# Country Level Plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

country_crop_median_ci <- sim_df %>%
  dplyr::mutate(result_calculated = ifelse(!is.na(perc_gain_prod), 1, 0)) %>%
  dplyr::group_by(WB_NAME,crop_code,model_name) %>%
  dplyr::summarize(
    median_perc_gain_prod = median(perc_gain_prod, na.rm = TRUE),
    lower_ci = quantile(perc_gain_prod, 0.025, na.rm = TRUE),
    upper_ci = quantile(perc_gain_prod, 0.975, na.rm = TRUE),
    no_grids_with_results = sum(result_calculated,na.rm = TRUE)
  ) %>%
  dplyr::filter(!WB_NAME %in% c("Saint Helena, Ascension and Tristan da Cunha (UK)",
                                "Heard Island and McDonald Islands (Aus.)",
                                "French Southern and Antarctic Lands (Fr.)",
                                "British Indian Ocean Territory (UK)")) 


# For all crops in one bar graph

crop_labels <- c("mze"="Maize", "rcw"="Rice", "whe"="Wheat")

for(model in c("SRHS")) {
  
  df = country_crop_median_ci %>%
    dplyr::filter(crop_code %in% c("mze","rcw","whe") & model_name == model &
                    median_perc_gain_prod != 0 & 
                    no_grids_with_results >=5) %>%
    dplyr::arrange(WB_NAME)
  
  # Factor WB_NAME alphabetically
  df$WB_NAME <- factor(df$WB_NAME, levels = rev(sort(unique(df$WB_NAME))))
  
  plot1 <- ggplot(
    data = df,
    aes(x = median_perc_gain_prod,
        y = WB_NAME,
        fill = crop_code)
  ) +
    geom_col() +
    scale_fill_manual(
      values = c("mze" = "darkblue",
                 "rcw" = "yellowgreen",
                 "whe" = "darkgoldenrod2"),
      labels = c("mze" = "Maize",
                 "rcw" = "Rice",
                 "whe" = "Wheat"),
      name   = "Crop"
    ) +
    facet_wrap(
      ~ crop_code,
      scales  = "fixed",                 # same x-axis across facets
      labeller = labeller(crop_code = crop_labels)
    ) +
    labs(
      x = "Median Production Gain (%)",
      y = "Country"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 10, family = "sans"),
      axis.text.x = element_text(size = 10, family = "sans"),
      axis.title  = element_text(size = 14, family = "sans"),
      plot.title  = element_text(size = 16, family = "sans", hjust = 0.5),
      strip.text  = element_text(size = 12, face = "bold")
    )
  
  ggsave(filename = paste0("output/graphs/bar_country_gain_all_",
                           model,
                           "_perc.png"),
         plot = plot1,
         width = 10, height = 8, dpi = 150, units = "in",
         device='png')
  
  rm(plot1,df)
  
  
  rm(list = ls(pattern = "^plot"))
  rm(model)
  
}

# List of country-crop combinations with median gain of 0
zero_gains <- country_crop_median_ci %>%
  dplyr::filter(median_perc_gain_prod == 0 & crop_code %in% c("mze","rcw","whe") & model_name == "SRHS")
zero_gains

under5_gains <- country_crop_median_ci %>%
  dplyr::filter(no_grids_with_results <5 & crop_code %in% c("mze","rcw","whe") & model_name == "SRHS")
under5_gains %>% dplyr::group_by(WB_NAME) %>% dplyr::mutate(count = n()) %>% dplyr::ungroup() %>% dplyr::filter(count ==3) %>% dplyr::distinct(WB_NAME) %>% print()
