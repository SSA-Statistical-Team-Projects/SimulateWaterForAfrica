#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Running District Regressions
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will run district regressions to test effects of irrigation

rm(list=ls())
gc(reset = TRUE)

# Loading Packages
#remotes::install_github("mikejohnson51/AOI") # suggested!
#remotes::install_github("mikejohnson51/climateR")

if(!require("pacman")) install.packages("pacman")

pacman::p_load(
  
  ncdf4, raster, chron, tidyverse, lubridate, sf, exactextractr, stringr,
  knitr, kableExtra, lfe, stargazer, sampleSelection, ivreg, micEcon,expss,
  glmnet, rgdal, haven, spatialEco, viridis, grid, gridExtra, plyr, dplyr
  
)


sf::sf_use_s2(FALSE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Loading data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crop_df <- readRDS("data-clean/working_data/estimation/a1_crop_year_level_working_data.RDS")

int_df <- readRDS("data-clean/working_data/ntl/grid_dt_withntl.RDS")

# Merging the datasets
#%%%%%%%%%%%%%%%%%%%%%

crop_df <- left_join(crop_df,
                     int_df %>%
                       as.data.frame() %>%
                       dplyr::select(ntl,srno),
                     by = "srno")

# Use the label() function to add labels to the variables
crop_df <- apply_labels(crop_df,
                        any_irri = "Irrigated (Binary)",
                        irri_intensity = "Irrigation Intensity",
                        growp_tot_days_number = "No of Growing Season Days",
                        growp_longest_conseq_dry_days = "Growing Season: Longest Consecutive Dry Days",
                        no_of_rain_days = "No. of Rain Days",
                        crop_actual_evopot_mm = "Crop Actual Evapotranspiration (mm)",
                        length_crop_grwth_cycle_days = "Length of Growing Cycle (Days)",
                        growing_cycl_crop_acc_temp_degCday = "Growing Cycle: Accumulated Temp (C / Day)",
                        growing_cycle_crop_water_def_mm = "Growing Cycle: Crop Water Deficit (mm)" ,
                        har_tot = "Harvest Area (1000 Ha)",
                        prd_tot = "Production (1000 T)",
                        crop_yld_constraint_moisture = "Moisture Constraint in Growth",
                        temp_winter = "Temp Winter",
                        temp_spring = "Temp Spring",
                        temp_fall = "Temp Fall",
                        temp_summer = "Temp Summer",
                        prec_fall = "Prec Fall",
                        prec_spring = "Prec Spring",
                        prec_summer = "Prec Summer",
                        prec_winter = "Prec Winter",
                        pdsi_fall = "PDSI Fall",
                        pdsi_winter = "PDSI Winter",
                        pdsi_spring = "PDSI Spring" ,
                        pdsi_summer = "PDSI Summer" ,
                        terrain_median_slp = "Terrain Median Slope",
                        mod_fournier_index_mm = "Modified Fournier Index (mm)",
                        crop_suitability = "Soil Suitability (Continuous Index)",
                        multi_cropping_class_irri = "Multi-cropping Class (With Irrigation)",
                        multi_cropping_class_rainfed = "Multi-cropping Class (With Rainfed)",
                        temp_winter_sq = "Temp Winter Sq",
                        temp_spring_sq = "Temp Spring Sq",
                        temp_fall_sq = "Temp Fall Sq",
                        temp_summer_sq = "Temp Summer Sq",
                        prec_fall_sq = "Prec Fall Sq",
                        prec_spring_sq = "Prec Spring Sq",
                        prec_summer_sq = "Prec Summer Sq",
                        prec_winter_sq = "Prec Winter Sq",
                        pdsi_fall_sq = "PDSI Fall Sq",
                        pdsi_winter_sq = "PDSI Winter Sq",
                        pdsi_spring_sq = "PDSI Spring Sq",
                        pdsi_summer_sq = "PDSI Summer Sq")


est_df <- crop_df %>%
  dplyr::filter(!is.na(any_irri) & har_tot > 0)

est_df <- est_df %>%
  dplyr::mutate(ntl = ifelse(ntl < 0, 0,ntl),
                lntl = log(ntl+1))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Model 1
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Heckman selection

source("analysis/14-lassovariableselection.R")

rm(test_data,test_data2,train_data,train_data2,ls_df,ls_df2,cvfit,cvfit2)
gc(reset = TRUE)

state_list <- est_df %>%
  dplyr::distinct(a1_id)

state_list <- state_list$a1_id

crop_list <- est_df %>%
  dplyr::distinct(crop_code)

crop_list <- crop_list$crop_code

country_list <- est_df %>%
  dplyr::distinct(WB_NAME)

country_list <- country_list$WB_NAME

# adjusting a few variables in those selected in order to avoid multicollinearity and to help the first stage converge or vars with little to no variation
selected_variable_names <- selected_variable_names[!grepl("^prec_", selected_variable_names)]
selected_variable_names <- selected_variable_names[!selected_variable_names %in% c("gaez_grid_area", "har_irri")]

# using the list of variables selected through lasso
varlist <- c(selected_variable_names,irri_minus_yld)
yld_reg_vars <- setdiff(varlist,irri_minus_yld)

##### TEMPPPPPP
for(crop in crop_list) {
  
  temp_df <- est_df %>%
    dplyr::filter(crop_code == crop) %>%
    dplyr::mutate(count_irri = ifelse(any(har_irri > 0),1,0),
                  yld_irri = ifelse(is.na(har_irri) | har_irri == 0, 0,yld_irri),
                  yld_rainfed = ifelse(is.na(har_rainfed) | har_rainfed == 0, 0,yld_rainfed))
  
  # Remove observations with missing values for variables in varlist
  temp_df <- temp_df[complete.cases(temp_df[, varlist]), ]
  
  count_irri = temp_df$count_irri[1]
  
  # Since est_df has the subset of croplands which grow a certain crop, there will be some states with no observations for a certain crop
  if(dim(temp_df)[1] <= 40 | count_irri != 1) {
    next
  }
  
  
  # ### Check
  #
  # m.rainfed     <- heckit(as.formula(paste("any_irri ~ -1 + ",
  #                                          paste(
  #                                            paste(varlist,
  #                                                  collapse = " + ")
  #                                          ),
  #                                          " + as.character(WB_NAME)"
  # )
  # ),
  # as.formula(
  #   paste("yld_rainfed ~ -1 + ",
  #         paste(
  #           paste(yld_reg_vars,
  #                 collapse = " + ")
  #         ),
  #         " + as.character(WB_NAME)"
  #   )
  # ),
  # data = temp_df %>%
  #   dplyr::mutate(yld_rainfed = ifelse(any_irri == 1, 0,yld_rainfed)))
  
  myprobit    <- probit(as.formula(
    paste("any_irri ~ -1 + ",
          paste(
            paste(varlist,
                  collapse = " + ")
          ),
          "  "
          #" + as.character(WB_NAME) "
    )
  ) ,
  x = TRUE,
  iterlim = 100,
  data=temp_df )
  
  imrData     <- invMillsRatio(myprobit) # same as yours in this particular case
  temp_df$IMR1 <- imrData$IMR1
  
  m.rainfed     <- lm(as.formula(
    paste("yld_rainfed ~ -1 + ",
          paste(
            paste(yld_reg_vars,
                  collapse = " + ")
          ),
          " + IMR1 " #+ as.character(WB_NAME)"
    )
  ),
  data = temp_df,
  subset = (any_irri == 0))
  
  m.irri     <- lm(as.formula(
    paste("yld_irri ~ -1 + ",
          paste(
            paste(yld_reg_vars,
                  collapse = " + ")
          ),
          " + IMR1 " #+ as.character(WB_NAME)"
    )
  ),
  data = temp_df,
  subset = (any_irri == 1))
  
  #saving models
  assign(paste0("mf.",crop),myprobit)
  assign(paste0("mi.",crop),m.irri)
  assign(paste0("mr.",crop),m.rainfed)
  
  
  
  ### saving results
  
  temp_irri <- temp_df %>%
    dplyr::filter(any_irri == 1)
  
  temp_irri$pred_yld_irri <- m.irri$fitted.values
  
  temp_rain <- temp_df %>%
    dplyr::filter(any_irri == 0)
  
  temp_rain$pred_yld_rainfed <- m.rainfed$fitted.values
  
  # Appending datasets
  new_temp_df <- bind_rows(temp_irri,temp_rain)
  rm(temp_irri,temp_rain)
  
  # saving means
  assign(paste0("mean_",crop,"_i"), 
         round(mean(temp_df %>% 
                      dplyr::filter(any_irri == 1) %>% 
                      dplyr::select(yld_irri),
                    na.rm = TRUE),
               3))
  
  assign(paste0("mean_",crop,"_r"), 
         round(mean(temp_df %>% 
                      dplyr::filter(any_irri == 0) %>% 
                      dplyr::select(yld_rainfed),
                    na.rm = TRUE),
               3))
  
  assign(paste0("mean_anyirri_",crop), 
         round(mean(temp_df$any_irri,3)))
  
  #temp_df$predicted_yld_diff <- m.irri$fitted.values - m.rainfed$fitted.values
  if(crop == crop_list[1]) {
    results_df <- new_temp_df %>%
      dplyr::select(pred_yld_rainfed,pred_yld_irri, WB_NAME,srno,crop_code,a1_id)
  } else {new_temp_df <- new_temp_df %>%
    dplyr::select(pred_yld_rainfed,pred_yld_irri, WB_NAME,srno, crop_code,a1_id)
  results_df <- bind_rows(results_df,
                          new_temp_df)
  }
  rm(m.rainfed,m.irri,temp_df,new_temp_df,myprobit)
  gc(reset = TRUE)
}

# Saving Predictions
results_df <- results_df %>%
  dplyr::group_by(a1_id,crop_code) %>%
  dplyr::mutate(a1_pred_yld_rainfed = mean(pred_yld_rainfed,na.rm = T),
                a1_pred_yld_irri = mean(pred_yld_irri,na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(WB_NAME,crop_code) %>%
  dplyr::mutate(ctry_pred_yld_rainfed = mean(pred_yld_rainfed,na.rm = T),
                ctry_pred_yld_irri = mean(pred_yld_irri,na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(crop_code) %>%
  dplyr::mutate(ov_pred_yld_rainfed = mean(pred_yld_rainfed,na.rm = T),
                ov_pred_yld_irri = mean(pred_yld_irri,na.rm = T)) %>%
  dplyr::ungroup()

results_df <- results_df %>%
  dplyr::mutate(pred_yld_diff = case_when(!is.na(a1_pred_yld_rainfed) &
                                            !is.na(a1_pred_yld_irri) ~ a1_pred_yld_irri - a1_pred_yld_rainfed,
                                          (is.na(a1_pred_yld_rainfed) |
                                             is.na(a1_pred_yld_irri)) &
                                            !is.na(ctry_pred_yld_rainfed) &
                                            !is.na(ctry_pred_yld_irri)~ ctry_pred_yld_irri - ctry_pred_yld_rainfed,
                                          (is.na(a1_pred_yld_rainfed) |
                                             is.na(a1_pred_yld_irri)) &
                                            (is.na(ctry_pred_yld_rainfed) |
                                               is.na(ctry_pred_yld_irri)) &
                                            !is.na(ov_pred_yld_rainfed) &
                                            !is.na(ov_pred_yld_irri) ~ ov_pred_yld_irri - ov_pred_yld_rainfed,
                                          TRUE ~ NA_real_))

results_df <- results_df %>%
  dplyr::distinct(a1_id,crop_code, .keep_all = T) %>%
  dplyr::select(-srno)

saveRDS(results_df,
        "data-clean/working_data/estimation/struc_ricard_heck_selec_results.RDS")

rm(results_df)

## Exporting tables

v.whe <- names(mi.whe$coefficients)
#v.whe <- v.whe[1:67]
# v.rcw <- names(mi.rcw$coefficients)
# v.rcw <- v.rcw[1:67]
# v.mze <- names(mi.mze$coefficients)
# v.mze <- v.mze[1:67]

main.vars.order <- v.whe

lab <- c("Harvest Area (1000 Ha)", 
         "Growing Season: Longest Consecutive Dry Days",
         "Modified Fournier Index (mm)",
         "Terrain Median Slope",
         "Length of Growing Cycle (Days)",
         "Growing Cycle: Accumulated Temp (C / Day)",
         "Soil Suitability (Continuous Index)",
         "Mean Depth to Groundwater (mm)",
         "Temp Winter Squared", 
         "PDSI Winter Squared", 
         "Temp Fall Squared",
         "PDSI Fall Squared", 
         "PDSI Spring Squared",
         "Temp Summer Squared",
         "PDSI Summer Squared",
         "PDSI Winter",
         "PDSI Spring",
         "Temp Fall", 
         "PDSI Fall",
         "Temp Summer",
         "PDSI Summer",
         "Log Night Lights",
         "Inverse Mills Ratio"
)  

fs.vars.order <- names(coef(mf.whe))

fs.lab <- c("Harvest Area (1000 Ha)", 
            "Growing Season: Longest Consecutive Dry Days",
            "Modified Fournier Index (mm)",
            "Terrain Median Slope",
            "Length of Growing Cycle (Days)",
            "Growing Cycle: Accumulated Temp (C / Day)",
            "Soil Suitability (Continuous Index)",
            "Mean Depth to Groundwater (mm)",
            "Temp Winter Squared", 
            "PDSI Winter Squared", 
            "Temp Fall Squared",
            "PDSI Fall Squared",
            "Temp Spring Squared",
            "PDSI Spring Squared",
            "Temp Summer Squared",
            "PDSI Summer Squared",
            "Temp Winter",
            "PDSI Winter",
            "Temp Spring",
            "PDSI Spring",
            "Temp Fall", 
            "PDSI Fall",
            "Temp Summer",
            "PDSI Summer",
            "Log Night Lights"
)  

t.isrhs <- stargazer(mi.mze,mi.rcw,mi.whe,
                     type = "latex",
                     header = FALSE,
                     omit.stat = c("LL","ser","f","adj.rsq"),
                     font.size = "footnotesize",
                     column.sep.width = "0pt",
                     dep.var.labels.include = FALSE,
                     model.names = FALSE,
                     model.numbers = TRUE,
                     star.cutoffs = c(0.1,0.05,0.01),
                     align = TRUE,
                     add.lines = list(c("Country Controls","\\multicolumn{1}{c}{Yes}","\\multicolumn{1}{c}{Yes}","\\multicolumn{1}{c}{Yes}")),
                     table.placement = "H",
                     order = paste0("^",main.vars.order,"$"),
                     keep = main.vars.order,
                     covariate.labels = lab,
                     label=paste0("tab:srhs_est_irri"),
                     title = paste0("Estimation Results of Climatic and Soil Controls on Yields For Irrigated Grids"),
                     dep.var.caption = "Crop Yield (Kg Per Ha)")
t.rsrhs <- stargazer(mr.mze,mr.rcw,mr.whe,
                     type = "latex",
                     header = FALSE,
                     omit.stat = c("LL","ser","f","adj.rsq"),
                     font.size = "footnotesize",
                     column.sep.width = "0pt",
                     dep.var.labels.include = FALSE,
                     model.names = FALSE,
                     model.numbers = TRUE,
                     star.cutoffs = c(0.1,0.05,0.01),
                     align = TRUE,
                     add.lines = list(c("Country Controls","\\multicolumn{1}{c}{Yes}","\\multicolumn{1}{c}{Yes}","\\multicolumn{1}{c}{Yes}")),
                     table.placement = "H",
                     order = paste0("^",main.vars.order,"$"),
                     keep = main.vars.order,
                     covariate.labels = lab,
                     label=paste0("tab:srhs_est_rain"),
                     title = paste0("Estimation Results of Climatic and Soil Controls on Yields For Rainfed Grids"),
                     dep.var.caption = "Crop Yield (Kg Per Ha)")

for(typ in c("i","r")) {
  
  if(typ == "i") {
    short = "irri"
    exp = "only the grids whose some or all parts were irrigated for the specific crop."
  } else {
    short = "rain"
    exp = "only the grids which had no portion irrigated for the specific crop."
  }
  
  t <- get(paste0("t.",typ,"srhs"))
  fin <- length(t)-3
  t1 <- c("\\begin{ThreePartTable} \\centering",
          "\\scriptsize",
          "\\setlength\\extrarowheight{-4pt}",
          "\\begin{longtable}{@{\\extracolsep{0pt}}lD{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} } ",
          t[3:4],
          t[7:10],
          "\\\\[-1.8ex] & \\multicolumn{1}{c}{Maize} & \\multicolumn{1}{c}{Rice} & \\multicolumn{1}{c}{Wheat} \\\\",
          t[11:fin],
          "\\end{longtable}",
          "\\begin{tablenotes}",
          "\\scriptsize",
          paste0("\\item Notes. Estimates are from the Structural Ricardian Model with Heckman Selection. The dependent variable is the crop yield in a given GAEZ grid. Each column represents the results for each crop: namely, maize, rice, and wheat. Estimations were conducted after subsetting the dataset to include ",
                 exp,
                 " The outcome mean is presented at the bottom; however, it is worth noting that since this is a selection model zeros were assigned to the yield when the crop was grown (i.e. harvest area of the crop exceeded zero) but the yields were not reported. *p$<$0.1 **p$<$0.05 ***p$<$0.01."),
          "\\end{tablenotes}",
          "\\end{ThreePartTable}"
  )
  
  # Remove rows with "  & & & \\\\ " from t.isrhs
  #t1 <- t1[t1 != "  & & & \\\\ "]
  
  writeLines(t1,
             paste0("output/tables/regressions/Table_srhs_estimations_",short,".tex")) 
  
  rm(t,t1,fin)
}

# first stage
tf <- stargazer(mf.mze,mf.rcw,mf.whe,
                type = "latex",
                header = FALSE,
                omit.stat = c("LL","ser","f","adj.rsq"),
                font.size = "footnotesize",
                column.sep.width = "0pt",
                dep.var.labels.include = FALSE,
                model.names = FALSE,
                model.numbers = TRUE,
                star.cutoffs = c(0.1,0.05,0.01),
                align = TRUE,
                add.lines = list(c("Country Controls","\\multicolumn{1}{c}{Yes}","\\multicolumn{1}{c}{Yes}","\\multicolumn{1}{c}{Yes}")),
                table.placement = "H",
                order = paste0("^",fs.vars.order,"$"),
                keep = fs.vars.order, 
                covariate.labels = fs.lab, 
                label=paste0("tab:srhs_est_fs"),
                title = paste0("Estimation Results of Climatic and Soil Controls on Irrigation Likelihood of Grids"),
                dep.var.caption = "Whether the Grid was Irrigated")

fin1 <- length(tf)-3
tf1 <- c("\\begin{ThreePartTable} \\centering",
         "\\scriptsize",
         "\\setlength\\extrarowheight{-4pt}",
         "\\begin{longtable}{@{\\extracolsep{-4pt}}lD{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} } ",
         tf[3:4],
         tf[7:10],
         "\\\\[-1.8ex] & \\multicolumn{1}{c}{Maize} & \\multicolumn{1}{c}{Rice} & \\multicolumn{1}{c}{Wheat} \\\\",
         tf[11:fin1],
         "\\end{longtable}",
         "\\begin{tablenotes}",
         "\\scriptsize",
         paste0("\\item Notes. Estimates are from the first stage of the Structural Ricardian Model with Heckman Selection. The dependent variable is binary: it equals one if the given GAEZ grid had any level of irrigation, and is zero otherwise. Each column represents the results for each crop: namely, maize, rice, and wheat. The outcome mean is presented at the bottom. *p$<$0.1 **p$<$0.05 ***p$<$0.01."),
         "\\end{tablenotes}",
         "\\end{ThreePartTable}"
)

writeLines(tf1,
           paste0("output/tables/regressions/Table_srhs_estimations_firsstage.tex")) 