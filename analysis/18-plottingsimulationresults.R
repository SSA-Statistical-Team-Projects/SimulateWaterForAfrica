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
# Country Level Plot
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# By crop
cereal_list <- c("mze","rcw","whe")
cereal_name <- c("Maize","Rice","Wheat")

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
