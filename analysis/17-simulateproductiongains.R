#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Running Simulations
# Author: Ifeanyi Edochie, Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will write a function to run the simulations for different crops,
# methods of computing yield gaps between rainfed and irrigated croplands, and
# then generate a final results dataset.

rm(list=ls())

pacman::p_load(
  
  writexl, ncdf4, raster, chron, tidyverse, lubridate, sf, exactextractr,
  rgdal, haven, spatialEco, geosphere, rasterVis, units, xtable, stringr, dplyr
  
)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Loading Data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

a1_dt <- readRDS("data-clean/working_data/estimation/a1_crop_year_level_working_data.RDS")

a1_dt <- a1_dt[, c("srno", "a1_id")] %>% st_drop_geometry()

a1_dt <- unique(a1_dt)

crop_df <- readRDS("data-clean/working_data/gaez/gaez_crop_year_analysis_final.RDS")

bgs_vol_df <- readRDS("data-clean/working_data/bgs/gaez_bgs_vol_with_aq_type.RDS")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Cleaning
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### BGS Volume Merge
crop_df <- left_join(crop_df,
                     bgs_vol_df,
                     by = "srno")

rm(bgs_vol_df)
# computing water supply for each crop = volume of water in grid * share of crop area in grid
crop_df <- crop_df %>%
  dplyr::mutate(crop_bgs_volume_m3 = bgs_volume_m3 * har_tot *10000000 / gaez_grid_area,#converting 1000 Ha to m2
                crop_bgs_volume_m3_adj = crop_bgs_volume_m3 * (1-share_of_sanitation_cons))

### Merging Admin1 IDs

crop_df <- left_join(crop_df,
                     a1_dt,
                     by = "srno")

grid_df <- crop_df %>%
  dplyr::filter(crop_code == "whe") %>%
  dplyr::group_by(WB_NAME) %>%
  dplyr::mutate(country_area = sum(gaez_grid_area, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(grid_total_water_use = total_use_adj*gaez_grid_area/country_area, #sum(#grid_water_def_vol_rainfed_m3,
                grid_extrac_recharge_ratio = grid_total_water_use/(mean_recharge_mmyr*0.001*gaez_grid_area))

grid_df$grid_extrac_recharge_ratio = units::drop_units(grid_df$grid_extrac_recharge_ratio)

crop_df <- left_join(crop_df,
                     grid_df %>%
                       as.data.frame() %>%
                       dplyr::select(srno,grid_extrac_recharge_ratio),
                     by = c("srno"))

rm(grid_df)
gc(reset = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Water Simulation Function
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

water_simulation <- function(crop_df,
                             crop_name,
                             vol_var,
                             effect_dt,
                             effect_var,
                             merge_var,
                             model_name,
                             depth_constraint = max(crop_df$mean_gwdepth_m, na.rm = T) + 1,
                             recharge_constraint = max(crop_df$grid_extrac_recharge_ratio, na.rm = T) + 1,
                             prod_constraint = min(crop_df$mean_aqp_litre_per_sec, na.rm = T) - 1,
                             crop_code){
  
  # Adding a crop value for filtering purposes (otherwise the filter doesn't work)
  crop = paste0(crop_code)
  
  # Adding information on whether the grid is a cropland
  temp <- crop_df %>%
    dplyr::group_by(srno) %>%
    dplyr::summarise(har_tot = sum(har_tot,na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(is_cropland = ifelse(har_tot > 0,1,0)) %>%
    dplyr::select(srno,is_cropland)
  
  crop_df <- left_join(crop_df,
                       temp,
                       by = "srno")
  
  ### prepare effect dataset
  colnames(effect_dt)[colnames(effect_dt) %in% effect_var] <- "yld_diff_final"
  
  crop_df <- left_join(crop_df,
                       effect_dt[, c(merge_var, "crop_code", "yld_diff_final")],
                       by = c(merge_var, "crop_code"))
  
  
  crop_df <- crop_df %>%
    dplyr::mutate(grid_extrac_recharge_ratio_ind = ifelse((!is.na(grid_extrac_recharge_ratio) &
                                                             as.numeric(grid_extrac_recharge_ratio) <= recharge_constraint) | #extract_ratio
                                                            is.na(grid_extrac_recharge_ratio), #also keeping data for whom constraint values are unknown
                                                          1,
                                                          0))
  
  crop_df <- crop_df %>%
    dplyr::filter(crop_code == crop)
  
  crop_df <- crop_df %>%
    mutate(grid_constraints_met = ifelse(mean_aqp_litre_per_sec >= prod_constraint &
                                           mean_gwdepth_m <= depth_constraint &
                                           grid_extrac_recharge_ratio_ind == 1,
                                         # grid_extrac_recharge_ratio_ <= recharge_constraint,
                                         1,
                                         0))
  
  depth_constraint_ind <- ifelse(depth_constraint > max(crop_df$mean_gwdepth_m, na.rm = T),
                                 NA,
                                 depth_constraint)
  
  prod_constraint_ind <- ifelse(prod_constraint < min(crop_df$mean_aqp_litre_per_sec, na.rm = T) - 1,
                                NA,
                                prod_constraint)
  
  recharge_constraint_ind <- ifelse(recharge_constraint > max(crop_df$grid_extrac_recharge_ratio, na.rm = T) + 1,
                                    NA,
                                    recharge_constraint)
  
  if(vol_var == "crop_m1_vol_int_ls_m3_adj") {
    vol_ind = "GAEZ GW Volume"
  } else {vol_ind = "BGS GW Volume"}
  
  # Identifying how much rainfed land can be supplemented by irrigated cropland
  
  ### THE 3 VARIABLES OF INTEREST
  
  ### cland_can_be_convert_shr checks to see what fraction of rainfed cropland can be converted to irrigated land
  ### defined as volume of water available should be less than crop water demand deficit. In case it is more, we look
  ### at what share is being met
  
  ### cland_convert_prod_gain_1000tonne for all the convertable lands what are the production gains
  if(vol_var == "crop_m1_vol_int_ls_m3_adj") {
    crop_df <- crop_df %>%
      dplyr::mutate(cland_can_be_convert_shr = case_when(grid_constraints_met == 1 & (as.numeric(crop_water_def_vol_rainfed_m3) <=
                                                                                        as.numeric(crop_m1_vol_int_ls_m3_adj)) ~ 1,
                                                         grid_constraints_met == 1 &
                                                           (as.numeric(crop_water_def_vol_rainfed_m3) >
                                                              as.numeric(crop_m1_vol_int_ls_m3_adj)) ~
                                                           as.numeric(crop_m1_vol_int_ls_m3_adj)/as.numeric(crop_water_def_vol_rainfed_m3),
                                                         TRUE ~ NA_real_))
  } else {crop_df <- crop_df %>%
    dplyr::mutate(cland_can_be_convert_shr = case_when(grid_constraints_met == 1 & (as.numeric(crop_water_def_vol_rainfed_m3) <=
                                                                                      as.numeric(crop_bgs_volume_m3_adj)) ~ 1,
                                                       grid_constraints_met == 1 &
                                                         (as.numeric(crop_water_def_vol_rainfed_m3) >
                                                            as.numeric(crop_bgs_volume_m3_adj)) ~
                                                         as.numeric(crop_bgs_volume_m3_adj)/as.numeric(crop_water_def_vol_rainfed_m3),
                                                       TRUE ~ NA_real_))
  }
  
  crop_df <- crop_df %>%
    dplyr::mutate(cland_convert_prod_gain_1000tonne = case_when(grid_constraints_met == 1 & har_rainfed > 0 & # when both irri and rainfed croplands for each crop exist
                                                                  (yld_diff_final > 0) ~ yld_diff_final * har_rainfed *1000 * cland_can_be_convert_shr /1000,
                                                                grid_constraints_met == 1 & har_rainfed > 0 & # when both irri and rainfed croplands for each crop exist
                                                                  (yld_diff_final <= 0) ~ 0,
                                                                TRUE ~ NA_real_))
  
  # there are cases when prd_rainfed = 0 even when the harvest area of rainfed is > 0. This could be due to crop losses post the growing season.
  # thus, I will adjust for this.
  crop_df <- crop_df %>%
    dplyr::mutate(perc_gain_prod = ifelse((is.na(prd_rainfed) | prd_rainfed == 0) & har_rainfed > 0 &
                                            cland_convert_prod_gain_1000tonne > 0,
                                          100,
                                          cland_convert_prod_gain_1000tonne * 100 / prd_rainfed))
  
  ### GENERATING STATISTICS
  
  # Share of rainfed c-land where constraints are met
  grid_constraints_met_perc <- round(dim(crop_df[which(crop_df$grid_constraints_met == 1 & crop_df$is_cropland == 1),])[1]*100/dim(crop_df[which(crop_df$is_cropland == 1),])[1],
                                     2)
  
  # Share of grids where we observe production gains out of all grids
  grids_with_gains_overall_perc <- round(dim(crop_df[which(crop_df$perc_gain_prod >0 & crop_df$is_cropland == 1),])[1]*100/dim(crop_df[which(crop_df$is_cropland == 1),])[1],
                                         2)
  
  # Share of grids where we observe production gains out of grids where the grid constraints are met
  grids_with_gains_sub_perc <- round(dim(crop_df[which(crop_df$perc_gain_prod >0 & crop_df$is_cropland == 1),])[1]*100/dim(crop_df[which(crop_df$grid_constraints_met == 1 & crop_df$is_cropland == 1),])[1],
                                     2)
  
  # percentage of gains
  median_gains_perc <- round(median(crop_df$perc_gain_prod,
                                    na.rm = T),
                             2)
  
  mean_gains_perc <- round(mean(crop_df$perc_gain_prod,
                                na.rm = T),
                           2)
  
  cl_low_gains_perc <- round(quantile(crop_df$perc_gain_prod,
                                      0.05,
                                      na.rm = T),
                             2)
  
  cl_up_gains_perc <- round(quantile(crop_df$perc_gain_prod,
                                     0.95,
                                     na.rm = T),
                            2)
  
  
  
  ###
  
  return(list(simulated_dt = crop_df[, c("srno",
                                         "perc_gain_prod",
                                         "cland_can_be_convert_shr",
                                         "cland_convert_prod_gain_1000tonne",
                                         "crop_code")],
              table_dt = data.frame(crop_name = crop_name,
                                    model_name = model_name,
                                    vol_data = vol_ind,
                                    depth_constraint = depth_constraint_ind,
                                    recharge_constraint = recharge_constraint_ind,
                                    prod_constraint = prod_constraint_ind,
                                    constraints_met = grid_constraints_met_perc,
                                    gains_overall = grids_with_gains_overall_perc,
                                    gains_sub = grids_with_gains_sub_perc,
                                    median_gains_perc = median_gains_perc,
                                    mean_gains_perc = mean_gains_perc,
                                    cl_low_gains_perc = cl_low_gains_perc,
                                    cl_up_gains_perc = cl_up_gains_perc
              )))
  
  
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generating Results Table
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## List of Inputs
cereal_list <- c("mze","rcw","whe")
cereal_names <- c("Maize","Wetland Rice","Wheat")

effect_dt_list <- c("simple_diff_results","struc_ricard_heck_selec_results")
effect_var_list <- c("simple_yld_diff","pred_yld_diff")
merge_var_list <- c("srno","a1_id")
model_name_list <- c("SD","SRHS")

vol_var_list <- c("crop_m1_vol_int_ls_m3_adj","crop_bgs_volume_m3_adj")

# Cases:
# 1. All constraints
# 2. 2 constraints
# 3. Only 1 Constraint
# 4. No Constraints
depth_constraint_list <-    c(7, 7, 7, 500, 7, 500,500, 500)
recharge_constraint_list <- c(0.7,0.7,500, 0.7, 500,0.7,500, 500)
prod_constraint_list <-     c(0.05,-3,0.05,0.05, -3, -3,0.05,-3)

## Function

GenerateSimulationResults <- function(crop_df,
                                      vol_var_list,
                                      cereal_list,
                                      cereal_names,
                                      effect_dt_list,
                                      effect_var_list,
                                      merge_var_list,
                                      model_name_list,
                                      depth_constraint_list,
                                      recharge_constraint_list,
                                      prod_constraint_list) {
  
  all_results <- data.frame()
  
  # Iterate over volume measure
  for(v in 1:length(vol_var_list)) {
    vol_var = vol_var_list[v]
    
    # Iterate over effect datasets
    for (j in 1:length(effect_dt_list)) {
      
      effect_dt_name <- effect_dt_list[j]
      effect_var <- effect_var_list[j]
      merge_var <- merge_var_list[j]
      model_name <- model_name_list[j]
      
      
      effect_dt <- readRDS(paste0("data-clean/working_data/estimation/",
                                  effect_dt_name,
                                  ".RDS"))
      
      # Iterate over crop codes
      for (i in 1:length(cereal_list)) {
        crop_code <- cereal_list[i]
        crop_name <- cereal_names[i]  
        
        # Iterate over constraint values
        for (k in 1:length(depth_constraint_list)) {
          depth_constraint <- depth_constraint_list[k]
          recharge_constraint <- recharge_constraint_list[k]
          prod_constraint <- prod_constraint_list[k]
          
          # Call your water_simulation function
          result <- water_simulation(crop_df = crop_df,
                                     crop_name = crop_name,
                                     vol_var = vol_var,
                                     effect_dt = effect_dt,
                                     effect_var = effect_var,
                                     merge_var = merge_var,
                                     model_name = model_name,
                                     depth_constraint = depth_constraint,
                                     recharge_constraint = recharge_constraint,
                                     prod_constraint = prod_constraint,
                                     crop_code = crop_code)
          
          # Append the results to the list
          all_results <- bind_rows(all_results, result[[2]])
          
          rm(result,depth_constraint,recharge_constraint,prod_constraint)
        }
        
        rm(crop_code,crop_name)
      }
      rm(effect_dt,effect_dt_name,effect_var,merge_var,model_name)
    }
    
    rm(vol_var)
  }
  
  
  
  
  return(all_results)
}

# Run the function
result_df   <- GenerateSimulationResults(crop_df = crop_df,
                                         vol_var_list = vol_var_list,
                                         cereal_list = cereal_list,
                                         cereal_names = cereal_names,
                                         effect_dt_list = effect_dt_list,
                                         effect_var_list = effect_var_list,
                                         merge_var_list = merge_var_list,
                                         model_name_list = model_name_list,
                                         depth_constraint_list = depth_constraint_list,
                                         recharge_constraint_list = recharge_constraint_list,
                                         prod_constraint_list = prod_constraint_list)


# Save Data
saveRDS(result_df,
        "data-clean/working_data/simulation_results_depth7m.RDS")

# Save as an Excel File

result_df <- result_df %>%
  dplyr::rename(c( "Crop" = crop_name,
                   "Model" = model_name,
                   "GW Volume Data" = vol_data,
                   "Depth Constraint" = depth_constraint,
                   "Recharge Constraint" =  recharge_constraint,
                   "Aquifer Productivity Constraint" = prod_constraint,
                   "Grids Satisfying Constraints (%)" = constraints_met,
                   "Grids with Production Gains (%)" =  gains_overall,
                   "Grids with Production Gains: Constraints Satisfied (%)" = gains_sub,
                   "Median Gains in Production (%)" =  median_gains_perc,
                   "Mean Gains in Production (%)" = mean_gains_perc,
                   "Lower CI Gains in Production (%)" = cl_low_gains_perc,
                   "Upper CI Gains in Production (%)" = cl_up_gains_perc
  ))



result_df <- result_df %>%
  setNames(c(
    "Crop" = "crop_name",
    "Model" = "model_name",
    "GW Volume Data" = "vol_data",
    "Depth Constraint" = "depth_constraint",
    "Recharge Constraint" = "recharge_constraint",
    "Aquifer Productivity Constraint" = "prod_constraint",
    "Grids Satisfying Constraints (%)" = "constraints_met",
    "Grids with Production Gains (%)" = "gains_overall",
    "Grids with Production Gains: Constraints Satisfied (%)" = "gains_sub",
    "Median Gains in Production (%)" = "median_gains_perc",
    "Mean Gains in Production (%)" = "mean_gains_perc",
    "Lower CI Gains in Production (%)" = "cl_low_gains_perc",
    "Upper CI Gains in Production (%)" = "cl_up_gains_perc"
  ))

result_df <- result_df %>%
  dplyr::mutate(Crop = crop_name,
                Model = model_name,
                `GW Volume Data` = vol_data,
                `Depth Constraint` = depth_constraint,
                `Recharge Constraint` = recharge_constraint,
                `Aquifer Productivity Constraint` = prod_constraint,
                `Grids Satisfying Constraints (%)` = constraints_met,
                `Grids with Production Gains (%)` = gains_overall,
                `Grids with Production Gains: Constraints Satisfied (%)` = gains_sub,
                `Median Gains in Production (%)` = median_gains_perc,
                `Mean Gains in Production (%)` = mean_gains_perc,
                `Lower CI Gains in Production (%)` = cl_low_gains_perc,
                `Upper CI Gains in Production (%)` = cl_up_gains_perc
  ) %>%
  dplyr::select(-crop_name, -model_name, -vol_data, -depth_constraint,
                -recharge_constraint, -prod_constraint, -constraints_met,
                -gains_overall, -gains_sub, -median_gains_perc,
                -mean_gains_perc, -cl_low_gains_perc, -cl_up_gains_perc)

# Save to Excel file
write_xlsx(result_df,
           "output/tables/simulation/simulation_results_7m.xlsx")

# Exporting tex files
for(vol_typ in c("GAEZ GW Volume")) {
  for(mod in c("SRHS")) {
    
    if(vol_typ == "BGS GW Volume") {
      vol_name = "bgs"
      data_typ = "BGS"
    } else {vol_name = "ls_wgp"
    data_typ = "World Bank"
    }
    
    temp <- result_df %>%
      dplyr::filter(`GW Volume Data` == vol_typ &
                      Model == mod) %>%
      dplyr::select(-c("Model","GW Volume Data",
                       "Grids with Production Gains (%)",
                       "Lower CI Gains in Production (%)",
                       "Upper CI Gains in Production (%)")) %>%
      dplyr::rename(c(`Extraction-Recharge Ratio Constraint` = `Recharge Constraint`,
                      `Grids with Production Gains (%)` = `Grids with Production Gains: Constraints Satisfied (%)`))
    
    
    # Using xtable to convert to .tex format
    temp_xtab <- xtable(temp)
    
    # Breaking tex code line by line
    t <- unlist(strsplit(noquote(print.xtable(temp_xtab,
                                              comment = FALSE,
                                              sanitize.text.function = identity)),
                         "\\\\"))
    t <- t[t != ""]  # Remove empty lines
    
    ## Editing the table to fit custom specifications
    
    # Remove everything before the first & (Row number)
    for(i in c(6:33)) {
      t[i] <- gsub("^[^&]*&\\s*", "", t[i]) #str_replace(t[i],"5%...","")
      t[i] <- paste0(t[i]," \\\\")
    }
    
    t <- c("\\begin{table}[H] \\centering",
           paste0("\\caption{Simulation Results Using ",mod,
                  " Approach and ",data_typ," Measure of GW Volume}"),
           paste0("\\label{tab:res_",mod,"_",vol_name,"_7m}"),
           "\\small",
           "\\setlength\\extrarowheight{-10pt}",
           "\\begin{threeparttable}",
           "\\begin{tabular}{@{\\extracolsep{-10pt}}lD{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} }",
           "\\\\[-1.8ex] \\hline \\hline",
           "\\\\[-1.8ex] \\\\  \\multicolumn{1}{c}{Crop} & \\multicolumn{1}{c}{\\shortstack{GW \\\\\\ Depth \\\\\\ Constraint}} & \\multicolumn{1}{c}{\\shortstack{Extraction \\\\\\ Recharge \\\\\\ Ratio \\\\\\ Constraint}} & \\multicolumn{1}{c}{\\shortstack{Aquifer \\\\\\ Productivity \\\\\\ Constraint}} & \\multicolumn{1}{c}{\\shortstack{Grids \\\\\\ Satisfying \\\\\\ Constraints \\\\\\ (\\%)}} & \\multicolumn{1}{c}{\\shortstack{Grids with \\\\\\ Production \\\\\\ Gains (\\%)}} & \\multicolumn{1}{c}{\\shortstack{Median \\\\\\ Gains in \\\\\\ Production \\\\\\ (\\%)}} & \\multicolumn{1}{c}{\\shortstack{Mean \\\\\\ Gains in \\\\\\ Production \\\\\\ (\\%)}} \\\\",
           "\\\\[-1.8ex] \\\\  \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} & \\multicolumn{1}{c}{(4)} & \\multicolumn{1}{c}{(5)} & \\multicolumn{1}{c}{(6)} & \\multicolumn{1}{c}{(7)} & \\multicolumn{1}{c}{(8)} \\\\",
           "\\\\[-0.8ex] \\hline \\\\[-0.8ex] ",
           t[6:29],
           " \\hline \\\\",
           "\\end{tabular}",
           "\\begin{tablenotes} \\scriptsize",
           paste0("\\item Notes. In this table we present the results of the ",
                  mod,
                  " approach using the groundwater supply or volume measure using the aquifer dataset constructed by ",
                  data_typ,
                  ". Each row represents one simulation. Column (1) specifies the crop for which the analysis was run. Columns (2)-(4) specify whether or not constraints were imposed; if yes, then what the thresholds were. Column (5) presents the fraction of all croplands where the constraints were met. Column (6) presents the share of croplands where we see production gains, as compared to the all the croplands where the constraints were met. Columns (7) and (8) present the median and mean of the simulated gains in production."),
           "\\end{tablenotes}",
           "\\end{threeparttable}",
           "\\end{table}")
    
    
    writeLines(t, paste0("output/tables/simulation/Table_sim_res_",
                         vol_name,"_",mod,"_depth7m.tex"))
    
    rm(temp,t,temp_xtab)
  }
}

# Cropped version of the results

for(vol_typ in c("GAEZ GW Volume","BGS GW Volume")) {
  for(mod in c("SRHS","SD")) {
    
    if(vol_typ == "BGS GW Volume" & mod == "SD") { next}
    
    if(vol_typ == "BGS GW Volume") {
      vol_name = "bgs"
      data_typ = "BGS"
    } else {vol_name = "ls_wgp"
    data_typ = "World Bank"
    }
    
    if(vol_typ == "GAEZ GW Volume" & mod == "SRHS") {
      
      note = "\\item Notes. In this table we present the results of the simulation approach using the groundwater supply or volume measure from the aquifer dataset constructed by World Bank. Each row represents one simulation. Column (1) specifies the crop for which the analysis was run. Column (2) presents the fraction of all croplands where the constraints were met. Column (3) presents the share of croplands where we see production gains compared to all the croplands where the constraints were met. Columns (4) and (5) present the median and mean of the simulated gains in production. All the results presented imposed conditions on groundwater depth $\\leq$ 7m and an extraction recharge ratio $\\leq$ 0.7."
      out= "output/tables/simulation/Table_sim_main_res_ls_wgp_SRHS_depth7m.tex"
      
    }
    
    if(vol_typ == "GAEZ GW Volume" & mod == "SD") {
      
      note = paste0("\\item Notes. In this table we present the results of the simulation approach using the groundwater supply or volume measure from the aquifer dataset constructed by ",
                    data_typ,
                    ". A simple differences approach is used to generate these results.",
                    " Each row represents one simulation. Column (1) specifies the crop for which the analysis was run. Column (2) presents the fraction of all croplands where the constraints were met. Column (3) presents the share of croplands where we see production gains compared to all the croplands where the constraints were met. Columns (4) and (5) present the median and mean of the simulated gains in production."
      )
      out= paste0("output/tables/simulation/Table_sim_res_",vol_name,"_",mod,"_depth7m_cropped.tex")
    }
    
    if(vol_typ == "BGS GW Volume" & mod == "SRHS") {
      
      note = paste0("\\item Notes. In this table we present the results of the simulation approach using the groundwater supply or volume measure from the aquifer dataset constructed by ",
                    data_typ,
                    ". Each row represents one simulation. Column (1) specifies the crop for which the analysis was run. Column (2) presents the fraction of all croplands where the constraints were met. Column (3) presents the share of croplands where we see production gains compared to all the croplands where the constraints were met. Columns (4) and (5) present the median and mean of the simulated gains in production."
      )
      out= paste0("output/tables/simulation/Table_sim_res_",vol_name,"_",mod,"_depth7m_cropped.tex")
    }
    
    
    if(vol_typ == "GAEZ GW Volume" & mod == "SRHS") {
      label = "\\label{tab:res_main_SRHS_7m}"
    } else {
      label = paste0("\\label{tab:res_",mod,"_",vol_name,"_7m_cropped}")
    }
    
   
    
    temp <- result_df %>%
      dplyr::filter(`GW Volume Data` == vol_typ &
                      Model == mod) %>%
      dplyr::select(-c("Model","GW Volume Data",
                       "Depth Constraint",
                       "Recharge Constraint","Aquifer Productivity Constraint",
                       "Grids with Production Gains (%)",
                       "Lower CI Gains in Production (%)",
                       "Upper CI Gains in Production (%)")) %>%
      dplyr::rename(c(`Grids with Production Gains (%)` = `Grids with Production Gains: Constraints Satisfied (%)`))
    
    
    # Using xtable to convert to .tex format
    temp_xtab <- xtable(temp)
    
    # Breaking tex code line by line
    tc <- unlist(strsplit(noquote(print.xtable(temp_xtab,
                                              comment = FALSE,
                                              sanitize.text.function = identity)),
                         "\\\\"))
    tc <- tc[tc != ""]  # Remove empty lines
    
    ## Editing the table to fit custom specifications
    
    # Remove everything before the first & (Row number)
    for(i in c(6:33)) {
      tc[i] <- gsub("^[^&]*&\\s*", "", tc[i]) #str_replace(t[i],"5%...","")
      tc[i] <- paste0(tc[i]," \\\\")
    }
    
    tcf <- c("\\begin{table}[H] \\centering",
             paste0("\\caption{Simulation Results Using ",mod,
                    " Approach and ",data_typ," Measure of GW Volume}"),
             label,
           "\\small",
           "\\setlength\\extrarowheight{-10pt}",
           "\\begin{threeparttable}",
           "\\begin{tabular}{@{\\extracolsep{-10pt}}lD{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} }",
           "\\\\[-1.8ex] \\hline \\hline",
           "\\\\[-1.8ex] \\\\  \\multicolumn{1}{c}{Crop} & \\multicolumn{1}{c}{\\shortstack{Grids \\\\\\ Satisfying \\\\\\ Constraints \\\\\\ (\\%)}} & \\multicolumn{1}{c}{\\shortstack{Grids with \\\\\\ Production \\\\\\ Gains (\\%)}} & \\multicolumn{1}{c}{\\shortstack{Median \\\\\\ Gains in \\\\\\ Production \\\\\\ (\\%)}} & \\multicolumn{1}{c}{\\shortstack{Mean \\\\\\ Gains in \\\\\\ Production \\\\\\ (\\%)}} \\\\",
           "\\\\[-1.8ex] \\\\  \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} & \\multicolumn{1}{c}{(4)} & \\multicolumn{1}{c}{(5)} \\\\",
           "\\\\[-0.8ex] \\hline \\\\[-0.8ex] ",
           tc[7],
           tc[15],
           tc[23],
           " \\hline \\\\",
           "\\end{tabular}",
           "\\begin{tablenotes} \\scriptsize",
           note,
           "\\end{tablenotes}",
           "\\end{threeparttable}",
           "\\end{table}")
    
    writeLines(tcf, 
               out)
    
    rm(temp,tc,tcf,temp_xtab,data_typ,note,out,vol_name, label)
    
  }
  rm(vol_typ)
}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Saving Results Dataframe
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This dataset contains results at the grid level for the case where all constraints were imposed

for (j in 1:length(effect_dt_list)) {
  
  effect_dt_name <- effect_dt_list[j]
  effect_var <- effect_var_list[j]
  merge_var <- merge_var_list[j]
  model_name <- model_name_list[j]
  
  
  effect_dt <- readRDS(paste0("data-clean/working_data/estimation/",
                              effect_dt_name,
                              ".RDS"))
  
  for(crop_code in cereal_list) {
    
    if(crop_code == "mze") {
      cname = "Maize"
    }
    if(crop_code == "rcw") {
      cname = "Rice"
    }
    if(crop_code == "whe") {
      cname = "Wheat"
    }
    
    sim_df <- water_simulation(crop_df,
                               crop_name = cname,
                               vol_var = "crop_m1_vol_int_ls_m3_adj",
                               effect_dt = effect_dt,
                               effect_var = effect_var,
                               merge_var = merge_var,
                               model_name = model_name,
                               depth_constraint = 7,
                               recharge_constraint = 0.7,
                               prod_constraint = -3, #keeping the main specification's constraints
                               crop_code = crop_code)[[1]]
    
    sim_df <- sim_df %>%
      dplyr::mutate(model_name = model_name,
                    depth_constraint = 7,
                    recharge_constraint = 0.7,
                    prod_constraint = -3)
    
    if(j ==1 & crop_code == cereal_list[1]) {
      fin_sim_df <- sim_df
    } else { fin_sim_df <- bind_rows(fin_sim_df,sim_df)}
    
    rm(sim_df, crop_code,cname)
    
  }
  
  rm(model_name,merge_var,effect_dt_name,effect_var, effect_dt,j)
  gc(reset = TRUE)
}

saveRDS(fin_sim_df,
        "data-clean/working_data/simulation_results_dataframe_allcons_depth7m.RDS")