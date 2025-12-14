#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Project: GW Africa Project
# File: Generating Balance Table
# Author: Bhavya Srivastava
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# This file will generate a balance table

rm(list=ls())
gc(reset = TRUE)

# Loading Packages
pak_list <- c("raster", "sf", "tidyverse","dplyr","xtable","stats", "stringr",
              "knitr","kableExtra")

lapply(pak_list, function(pkg) {
  suppressMessages(suppressWarnings(
    library(pkg, character.only = TRUE)
  ))
})
lapply(pak_list, library, character.only = TRUE)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Loading data
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

crop_df <- readRDS("data-clean/working_data/estimation/a1_crop_year_level_working_data.RDS")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Balance Table
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Set up
#%%%%%%%%%%%%%%%

balance_df <- crop_df %>%
  dplyr::select(starts_with(c("none","both_","only_","yld_tot","growp_tot_d",
                              "growp_long","no_of","grid_suit_o_vs_Ms_ms_index",#"annual_prec",
                              "crop_actual_","length_","har_tot","har_irri","har_rainfed","growing_",
                              "crop_yld_","prd_tot","temp","prec","pdsi","terrain_median",
                              "crop_suit","multi_","mod_fo","mean_gwdepth_m"))) 

balance_df <- balance_df %>%
  dplyr::select(-ends_with(c("cubed","_sq","_imp")))

balance_df1 <- balance_df %>%
  dplyr::mutate(with_irri = case_when(only_irri == 1 | both_irri_rain == 1 ~ 1,
                                      only_rainfed == 1 ~ 0,
                                      TRUE ~ NA_real_),
                crop_actual_evopot_mm = case_when(with_irri == 1 ~ crop_actual_evopot_mm_irri, #diff definition from before in order to show the differences between the two types of grids
                                                  with_irri == 0 ~ crop_actual_evopot_mm_rainfed,
                                                  TRUE ~ NA_real_),
                growing_cycle_crop_water_def_mm = case_when(with_irri == 1 ~ growing_cycle_crop_water_def_mm_irri,
                                                            with_irri == 0 ~ growing_cycle_crop_water_def_mm_rainfed,
                                                            TRUE ~ NA_real_)) %>%
  dplyr::filter(!is.na(with_irri)) %>%
  dplyr::select(-c(both_irri_rain,none_irri_rain,only_irri,only_rainfed,
                   growing_cycle_crop_water_def_mm_irri,growing_cycle_crop_water_def_mm_rainfed,
                   crop_actual_evopot_mm_irri,crop_actual_evopot_mm_rainfed,
                   crop_actual_evopot_mm_rainfed_adj, 
                   har_rainfed, har_rainfed_adj))

## Renaming variables
balance_df1 <- balance_df1 %>%
  dplyr::rename(c("Total Yield (T/Ha)" = yld_tot,
                  "No of Growing Season Days" = growp_tot_days_number,
                  "Growing Season: Longest Consecutive Dry Days" = growp_longest_conseq_dry_days,
                  "No. of Rain Days" = no_of_rain_days ,
                  "Crop Actual Evapotranspiration (mm)" = crop_actual_evopot_mm,
                  "Length of Growing Cycle (Days)" = length_crop_grwth_cycle_days,
                  "Growing Cycle: Accumulated Temp (C / Day)" = growing_cycl_crop_acc_temp_degCday,
                  "Growing Cycle: Crop Water Deficit (mm)" = growing_cycle_crop_water_def_mm ,
                  "Harvest Area (1000 Ha)" = har_tot,
                  "Production (1000 T)" = prd_tot,
                  "Moisture Constraint in Growth" = crop_yld_constraint_moisture,
                  "Temp Winter" = temp_winter,
                  "Temp Spring" = temp_spring,
                  "Temp Fall" = temp_fall,
                  "Temp Summer" = temp_summer,
                  "Prec Fall" = prec_fall,
                  "Prec Spring" = prec_spring,
                  "Prec Summer" = prec_summer,
                  "Prec Winter" = prec_winter,
                  "PDSI Fall" = pdsi_fall,
                  "PDSI Winter" = pdsi_winter,
                  "PDSI Spring" = pdsi_spring,
                  "PDSI Summer" = pdsi_summer,
                  "Mean Depth to Groundwater (m)" = mean_gwdepth_m,
                  "Terrain Median Slope" = terrain_median_slp,
                  "Modified Fournier Index (mm)" = mod_fournier_index_mm,
                  "Soil Suitability (Continuous Index)" = crop_suitability,
                  "Multi-cropping Class (With Irrigation)" = multi_cropping_class_irri,
                  "Multi-cropping Class (With Rainfed)" = multi_cropping_class_rainfed))

# t-tests
#%%%%%%%%%%%%%%%

# Function to run the t-test for each var
calc_row <- function(var) {
  x  <- balance_df1[[var]]
  g1 <- balance_df1$with_irri == 1
  g0 <- balance_df1$with_irri == 0
  
  x1 <- x[g1]; x0 <- x[g0]
  x1 <- x1[is.finite(x1)]
  x0 <- x0[is.finite(x0)]
  
  n1 <- length(x1); n0 <- length(x0)
  if (n1 < 2 || n0 < 2) return(NULL)
  
  m1 <- mean(x1); m0 <- mean(x0)
  d_mean <- m1 - m0
  sg <- ifelse(d_mean > 0, "+", ifelse(d_mean < 0, "-", "0"))
  
  # Cohen's d (pooled SD, equal-variances version)
  s1 <- sd(x1); s0 <- sd(x0)
  s_pooled <- sqrt((s1^2 + s0^2) / 2)
  d_cohen <- if (s_pooled > 0) d_mean / s_pooled else NA_real_
  
  pval <- tryCatch(stats::t.test(x1, x0)$p.value, error = function(e) NA_real_)
  
  data.frame(
    Variable = var,
    Mean_0   = m0,
    Mean_1   = m1,
    Sign     = sg,
    p_value  = pval,
    cohen_d  = d_cohen,
    stringsAsFactors = FALSE
  )
}

# lapply and bind
results_list <- lapply(names(balance_df1)[!names(balance_df1) %in% "with_irri"], 
                       calc_row)
results_df <- do.call(rbind, 
                      results_list)

# Fprmatting
#%%%%%%%%%%%%%%%

# Sort column names by sign
if (!is.null(results_df) && nrow(results_df) > 0) {
  results_df <- dplyr::arrange(results_df, Sign)
}

# Add stars
fmt_p <- function(p) {
  # stars: . <0.10, * <0.05, ** <0.01, *** <0.001
  stars <- dplyr::case_when(
    is.na(p)            ~ "",
    p < 0.001           ~ "***",
    p < 0.01            ~ "**",
    p < 0.05            ~ "*",
    p < 0.10            ~ ".",
    TRUE                ~ ""
  )
  val <- ifelse(is.na(p), NA_character_,
                ifelse(p < 0.0005, "0.000", sprintf("%.3f", p)))
  paste0(val, stars)
}

crosses_for_d <- function(d) {
  ad <- abs(d)
  dplyr::case_when(
    is.na(ad)    ~ "",
    ad >= 0.8    ~ "$\\dagger \\dagger \\dagger$",  # ††† large
    ad >= 0.5    ~ "$\\dagger \\dagger$",        # †† medium
    ad >= 0.2    ~ "$\\dagger$",              # † small
    TRUE         ~ ""
  )
}

# Adding formatted columns
results_df <- results_df %>%
  dplyr::mutate(
    p_value_fmt  = fmt_p(p_value),
    cohen_d_fmt  = paste0(sprintf("%.3f", cohen_d), crosses_for_d(cohen_d))
  ) %>%
  # keep only what you want shown
  dplyr::select(Variable, Mean_0, Mean_1, Sign,
                p_value_fmt, cohen_d_fmt)

colnames(results_df) <- c("Variable", "Only Rainfed", "Any Irrigated", "Difference", "p-value", "Cohen's d")

# Round the numeric columns to three decimal places
results_df[, 2:3] <- round(results_df[, 2:3], 3)

# Adding number of Observations

obs_count_0 <- sum(!is.na(balance_df1[balance_df1$with_irri == 0, "with_irri"]))
obs_count_1 <- sum(!is.na(balance_df1[balance_df1$with_irri == 1, "with_irri"]))
results_df[nrow(results_df) + 1,] <- c("N",obs_count_0,obs_count_1,"","","")

# LaTeX
#%%%%%%%%%%%%%%%
# exporting and formatting

# Print the LaTeX table using kable
balance_table <- knitr::kable(results_df,
                              format = "latex",
                              booktabs = TRUE,
                              caption = "T-Test Results and Mean Differences") %>%
  kableExtra::kable_styling(full_width = FALSE)

writeLines(balance_table,
           con = "output/tables/balance_table/balance_table_any_irri_v2.tex")

t <- readLines("output/tables/balance_table/balance_table_any_irri_v2.tex")

# Formatting table

t <- t[!grepl("\\\\addlinespace", t)]
t <- str_replace_all(t, fixed("\\textbackslash{}dagger"), "\\dagger")
t <- str_replace_all(t,fixed("\\$"),"$")

end <- length(t)-4

t <- c(t[1:4],
       "\\label{tab:balance_table}",
       "\\small",
       "\\begin{threeparttable}",
       "\\begin{tabular}{lccccc}",
       "\\toprule",
       " & \\shortstack{Only \\\\ Rainfed} & \\shortstack{Any \\\\ Irrigated} & Difference & p-value & Cohen's d \\\\",
       " & \\multicolumn{1}{c}{(1)} & \\multicolumn{1}{c}{(2)} & \\multicolumn{1}{c}{(3)} & \\multicolumn{1}{c}{(4)} & \\multicolumn{1}{c}{(5)} \\\\",
       t[8:end],
       "\\midrule",
       paste0("\\textbf{N} & \\textbf{",obs_count_0,"} & \\textbf{",obs_count_1,"} &  &  & \\\\"),
       "\\bottomrule",
       "\\end{tabular}",
       "\\begin{tablenotes} \\footnotesize",
       "\\item Notes. In this table we compare agro-climatic factors and conditions across croplands that were solely rain-fed and croplands that practiced any level of irrigation. The analysis was conducted using data at the crop-GAEZ grid level for croplands. Columns (1) and (2) compute the averages of the outcomes for rain-fed- and irrigated- croplands, respectively. Column (3) indicates the sign difference in their means, whereas column (4) denotes whether these mean differences are statistically significant. Given that we have a large number of observations, we present Cohen's d estimates in column (5) to show the strength of the effect size. *p$<$0.1 **p$<$0.05 ***p$<$0.01. Effect size thresholds: $\\dagger$ small ($d \\geq 0.2$), $\\dagger\\dagger$ medium ($d \\geq 0.5$), $\\dagger\\dagger\\dagger$ large ($d \\geq 0.8$).",
       "\\end{tablenotes}",
       "\\end{threeparttable}",
       "\\end{table}")

writeLines(t,
           con = "output/tables/balance_table/balance_table_any_irri_v2.tex")
