#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Lasso for Variable Selection
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will use lasso for variable selection for the following outcomes:
# i) Irrigation dummy
# ii) Yield Total
# iii) Yield of Irrigated
# iv) Yield of rainfed

# rm(list=ls())
# gc(reset = TRUE)
# 
# # Loading Packages
# pak_list <- c("raster", "sf", "tidyverse","dplyr")
# 
# lapply(pak_list, function(pkg) {
#   suppressMessages(suppressWarnings(
#     library(pkg, character.only = TRUE)
#   ))
# })
# lapply(pak_list, library, character.only = TRUE)

# 1. Irrigation Dummy
#%%%%%%%%%%%%%%%%%%%%

#encode binary variable
ls_df <- est_df %>%
  dplyr::select(-starts_with(c("crop_m1","cland","grid_extrac","ISO_A3","m1_",
                               "has_su","mean_aqp","crop_water","mean_yld",
                               "mean_crop","ccropl","grid_total_water",
                               "grid_constra","latitude","longitude","perc_gain",
                               "total_use","mean_rechar","yld_irri_new",
                               "avg_grid_act","prd_","grid_water_def","yld",
                               "terrain_slp","land_","excel_area_","ntl","thermal_zone",
                               "annual_precip_mm","mst_class","thz_class",
                               "crop_actual_evopot_mm","crop_yld_constraint_moisture","har_rainfed","growp_net_prim_prodn",
                               "no_of_rain_days","thermal_climate","growp_beg_date_long_comp_day","growp_tot_days_number",
                               "crop_a1_int","yga_","a1_id","srno","WB_NAME","total_use_adj","ISO_A3","tot_use_vol",
                               "growing_cycle_crop_water_def_mm_irri","growing_cycle_crop_water_def_mm_rainfed",
                               "growing_cycle_crop_water_def_mm", "growp_length_longest_comp_days"))) %>% #TEST
  dplyr::select(-ends_with(c("_imp","_adj","_cubed","sanitation_cons","_class3","water_use_vol","water_use_shr"))) %>%
  dplyr::select(-starts_with(c("multi_cropping_class_irri","multi_cropping_class_rainfed","har_irri","only_irri","none_irri_rain","only_rainfed"))) %>% # removing vars which are probabilitistic indices
  dplyr::select(-c(irri_intensity,both_irri_rain))
ls_df$any_irri <- as.factor(ls_df$any_irri)

# removing duplicates (these were used to create another variable)
vars <- c("cyl","fc2","tsc","sux")

ls_df <- ls_df %>%
  dplyr::select(-all_of(paste0(rep(vars, each = 2), c("_L_irri","_L_rainfed"))))
rm(vars)

# Keeping complete cases
missing_values <- colSums(is.na(ls_df))

# Display variables with missing values and their counts
print(missing_values[missing_values > 0])
ls_df <- na.omit(ls_df)

set.seed(09072025)  # For reproducibility

train_indices <- sample(1:nrow(ls_df), 0.8 * nrow(ls_df))
train_data <- ls_df[train_indices, ]
test_data <- ls_df[-train_indices, ]
#Fit Lasso Model:
#Use the cv.glmnet function to fit a Lasso regression model. Cross-validation is used to choose the optimal value of the regularization parameter (lambda) that controls the amount of shrinkage.

# Assuming you want to predict 'Outcome' using other variables
x <- model.matrix(any_irri ~ ., data = train_data)[, -1]  # Exclude the intercept column
y <- as.numeric(train_data$any_irri)

# Perform cross-validated Lasso regression
cvfit <- cv.glmnet(x, y, alpha = 1)  # alpha = 1 for Lasso, alpha = 0 for Ridge
plot(cvfit)

# Extract selected variables based on the optimal lambda
coeficients <- coef(cvfit, s = cvfit$lambda.min)
selected_indices <- which(coeficients != 0)

# Get variable names
selected_variable_names <- colnames(x)[selected_indices]
# Remove NA values from selected_variable_names

selected_variable_names <- selected_variable_names[complete.cases(selected_variable_names)]
selected_variable_names <- selected_variable_names[!selected_variable_names %in% c("crop_codemze",
                                                                                   "crop_codercw",
                                                                                   "crop_codewhe")]

# Final Dataset to run the heckman selection model on
fin_df <- est_df %>%
  dplyr::select(c("any_irri","crop_code","a1_id","WB_NAME","yld_irri","yld_rainfed","har_irri",
                  all_of(selected_variable_names)))


# 2. Yield (Total)
#%%%%%%%%%%%%%%%%%%%%

#Keeping relevant variables
ls_df2 <- est_df %>%
  dplyr::select(-starts_with(c("crop_m1","cland","grid_extrac","ISO_A3","m1_",
                               "has_su","mean_aqp","crop_water","mean_yld",
                               "mean_crop","ccropl","grid_total_water",
                               "grid_constra","latitude","longitude","perc_gain",
                               "total_use","mean_rechar","yld_irri_new",
                               "avg_grid_act","qga_","prd_","grid_water_def","yld_irri","yld_rainfed",
                               "terrain_slp","land_","excel_area_","ntl","thermal_zone",
                               "annual_precip_mm","mst_class","thz_class",
                               "crop_actual_evopot_mm","crop_yld_constraint_moisture","har_rainfed","growp_net_prim_prodn",
                               "no_of_rain_days","thermal_climate","growp_beg_date_long_comp_day","growp_tot_days_number",
                               "crop_a1_int","yga_","a1_id","srno","WB_NAME"))) %>%
  dplyr::select(-ends_with(c("_imp","_adj","_cubed","sanitation_cons","_class3"))) %>%
  dplyr::select(-starts_with(c("multi_cropping_class_irri","multi_cropping_class_rainfed","har_irri","only_irri","none_irri_rain","only_rainfed"))) %>% # removing vars which are probabilitistic indices
  dplyr::select(-c(irri_intensity,both_irri_rain))

# Keeping complete cases
missing_values2 <- colSums(is.na(ls_df2))

# Display variables with missing values and their counts
print(missing_values2[missing_values2 > 0])
ls_df2 <- na.omit(ls_df2)

set.seed(09072025)  # For reproducibility

train_indices2 <- sample(1:nrow(ls_df2), 0.8 * nrow(ls_df2))
train_data2 <- ls_df2[train_indices2, ]
test_data2 <- ls_df2[-train_indices2, ]
#Fit Lasso Model:
#Use the cv.glmnet function to fit a Lasso regression model. Cross-validation is used to choose the optimal value of the regularization parameter (lambda) that controls the amount of shrinkage.

# Assuming you want to predict 'Outcome' using other variables
x2 <- model.matrix(yld_tot ~ ., data = train_data2)[, -1]  # Exclude the intercept column
y2 <- as.numeric(train_data2$yld_tot)

# Perform cross-validated Lasso regression
cvfit2 <- cv.glmnet(x2, y2, alpha = 1)  # alpha = 1 for Lasso, alpha = 0 for Ridge
#Plot Cross-Validation Results (Optional):

#plot(cvfit2)

# Extract selected variables based on the optimal lambda
coeficients2 <- coef(cvfit2, s = cvfit2$lambda.min)
selected_indices2 <- which(coeficients2 != 0)

# Get variable names
selected_variable_names2 <- colnames(x)[selected_indices2]
# Remove NA values from selected_variable_names

selected_variable_names2 <- selected_variable_names2[complete.cases(selected_variable_names2)]
selected_variable_names2 <- selected_variable_names2[!selected_variable_names2 %in% c("crop_codemze",
                                                                                      "crop_codercw",
                                                                                      "crop_codewhe")]
#### Identifying differences between the two selected variable vectors

irri_minus_yld <- setdiff(selected_variable_names,selected_variable_names2)
yld_minus_irri <- setdiff(selected_variable_names2,selected_variable_names)

irri_minus_yld

