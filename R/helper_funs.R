#' Helper functions that help with the cleaning process
#'

addstring_to_colname <- function(name,
                                 string_tag){
  
  y <- paste(string_tag, name, sep = "_")
  
  return(y)
  
}

pastery <- function(x1,
                    x2){
  
  y <- paste(x1, x2, sep = "/")
  
  return(y)
  
}


bgdm_parallelcrop <- function(raster_folder = "//esapov/esapov/ALL/Energy",
                              shp_dsn,
                              shp_layer,
                              numCores = 15,
                              out_folder){
  
  sf::sf_use_s2(FALSE)
  ##read in africa shapefile
  shp_dt <- sf::st_read(dsn = shp_dsn,
                        layer = shp_layer)
  
  shp_dt <- shp_dt %>% sf::st_bbox() %>% sf::st_as_sfc()
  shp_dt <- shp_dt %>% sf::st_buffer(dist = 1)
  shp_dt <- sf::as_Spatial(shp_dt)
  
  ##read in the folder to crop
  raster_list <- list.files(path = raster_folder,
                            pattern = ".tif")
  
  ### parallelization processing
  numCores <- min(numCores, parallel::detectCores())
  parallelMap::parallelLibrary("foreach")
  parallelMap::parallelLibrary("raster")
  parallelMap::parallelLibrary("sf")
  
  doParallel::registerDoParallel(cores = numCores) ##initiate the number of cores to be used
  
  foreach(i = 1:length(raster_list)) %dopar% {
    
    raster_tif <- raster::raster(paste(raster_folder, raster_list[i], sep = "/"))
    
    raster::crop(raster_tif, shp_dt, filename = paste(out_folder, raster_list[i], sep = "/"))
    
  }
  
  endCluster()
  
}

load_packages <- function(packages){
  
  # Install packages not yet installed
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
}

alphabetorder_cols <- function(X){
  
  X <- X[,order(colnames(X))]
  
  return(X)
  
}

subpply <- function(X, tag, replacement){
  
  names <- sub(pattern = tag, replacement = "", x = X)
  
  return(names)
}


namepply <- function(dt, X){
  
  colnames(dt) <- X
  
  return(dt)
  
}



grepply <- function(x, y){
  
  return(x[grepl(pattern = y, x = x)])
  
}




ols_model <- function(dt,
                      xvars,
                      y,
                      weights){
  
  dt <- as.data.table(dt)
  
  dt <- na.omit(dt[,c(y, xvars, weights), with = F])
  
  xset <- dt[, xvars, with = F]
  
  weights <- dt[, weights, with = F]
  
  y <- dt[,colnames(dt)[!(colnames(dt) %in% c(colnames(xset), names(weights)))], with = F]
  
  model_dt <- cbind(y, xset)
  
  model_formula <- formula(paste(names(y), ".", sep = " ~ "))
  
  ols_model <- lm(formula = model_formula,
                  data = model_dt,
                  weights = weights)
  
  return(ols_model)
  
  
}


saeplus_calibratepovrate <- function(hh_dt = hh.dt,
                                     pop_dt = gin_masterpoly.dt,
                                     ebp_obj = ginemdi_model2,
                                     area_id = "ADM3_CODE",
                                     pop_var = "population",
                                     harea_id = "ADM1_CODE",
                                     povline = 5006362,
                                     welfare = "pcexp",
                                     weight = "hhweight",
                                     excl_outsample = TRUE){
  
  hh_dt <- as.data.table(hh_dt)
  pop_dt <- as.data.table(pop_dt)
  
  pop_doms <- unique(pop_dt[,get(area_id)])
  survey_doms <- unique(hh_dt[,get(area_id)])
  
  if(excl_outsample == TRUE){
    
    excl_doms <- pop_doms[!(pop_doms %in% survey_doms)]
    
    pop_dt <- pop_dt[!(get(area_id) %in% excl_doms),]
  }
  #1. Generate population estimate for each SAE level ID by aggregating across grids
  pop_size <- pop_dt[,sum(get(pop_var), na.rm = TRUE),by = get(area_id)]
  # pop_size <- hh_dt[,sum(get(weight)), by = area_id]
  setnames(pop_size, colnames(pop_size), c(area_id, "population"))
  
  #### merge in h_area ID and poverty rates from EMDI object
  pop_size <- pop_dt[,c(area_id, harea_id),with=F][pop_size, on = area_id]
  pop_size <- unique(pop_size)
  povrate.dt <- as.data.table(ebp_obj$ind)
  povrate.dt[,Domain := as.integer(as.character(Domain))]
  setnames(povrate.dt, "Domain", area_id)
  pop_size <- povrate.dt[,c(area_id, "Head_Count"),with=F][pop_size, on = area_id]
  
  #2.	Use these population estimates for each sampled sub-prefecture as weights to generate small area estimates for
  #   each state. Call these the sae state estimates.
  #         a.	Take the sum of population*poverty rate / total population, by sub-prefecture
  #3. Estimate state level poverty rates
  pop_size[,harea_popsize := sum(population, na.rm = TRUE), by = harea_id]
  pop_size[,area_povrate_share := population * Head_Count / harea_popsize]
  pop_size[,harea_povrate := sum(area_povrate_share, na.rm = TRUE), by = harea_id]
  
  hh_dt[,povline := ifelse(get(welfare) < povline, 1, 0)]
  harea_povline <- hh_dt[,weighted.mean(povline, get(weight)), by = harea_id]
  setnames(harea_povline, "V1", "survey_povrate")
  harea_povline <- harea_povline[!is.na(harea_id),]
  
  #4.	Divide the vector of state survey estimates by sae estimates to get the benchmarking ratio.
  #5.	Multiply the point estimates by the benchmarking ratio.
  
  pop_size <- harea_povline[pop_size, on = harea_id]
  
  pop_size[, bmratio := survey_povrate / harea_povrate]
  
  pop_size[, BM_Head_Count := Head_Count * bmratio]
  
  return(pop_size)
  
}


saeplus_calibratepovrate2 <- function(hh_dt = hh.dt,
                                      pop_dt = gin_masterpoly.dt,
                                      ebp_obj = ginemdi_model2,
                                      area_id = "ADM3_CODE",
                                      pop_var = "population",
                                      harea_id = "ADM1_CODE",
                                      povline = 5006362,
                                      welfare = "pcexp",
                                      weight = "hhweight",
                                      excl_outsample = TRUE){
  
  hh_dt <- as.data.table(hh_dt)
  pop_dt <- as.data.table(pop_dt)
  
  pop_doms <- unique(pop_dt[,get(area_id)])
  survey_doms <- unique(hh_dt[,get(area_id)])
  
  
  if(excl_outsample == TRUE){
    
    excl_doms <- pop_doms[!(pop_doms %in% survey_doms)]
    
    pop_dt <- pop_dt[!(get(area_id) %in% excl_doms),]
  }
  #1. Generate population estimate for each SAE level ID by aggregating across grids
  pop_size <- pop_dt[,sum(get(pop_var), na.rm = TRUE),by = get(area_id)]
  # pop_size <- hh_dt[,sum(get(weight)), by = area_id]
  setnames(pop_size, colnames(pop_size), c(area_id, "population"))
  
  #### merge in h_area ID and poverty rates from EMDI object
  pop_size <- pop_dt[,c(area_id, harea_id),with=F][pop_size, on = area_id]
  pop_size <- unique(pop_size)
  povrate.dt <- as.data.table(ebp_obj$ind)
  povrate.dt[,Domain := as.integer(as.character(Domain))]
  setnames(povrate.dt, "Domain", area_id)
  pop_size <- povrate.dt[,c(area_id, "Head_Count"),with=F][pop_size, on = area_id]
  
  #2.	Use these population estimates for each sampled sub-prefecture as weights to generate small area estimates for
  #   each state. Call these the sae state estimates.
  #         a.	Take the sum of population*poverty rate / total population, by sub-prefecture
  #3. Estimate state level poverty rates
  pop_size[,harea_popsize := sum(population, na.rm = TRUE), by = harea_id]
  pop_size[,npHead_Count := 1 - Head_Count]
  pop_size[,area_nprate_share := population * npHead_Count / harea_popsize]
  pop_size[,harea_nprate := sum(area_nprate_share, na.rm = TRUE), by = harea_id]
  
  hh_dt[,povline := ifelse(get(welfare) < povline, 1, 0)]
  harea_povline <- hh_dt[,weighted.mean(povline, get(weight)), by = harea_id]
  setnames(harea_povline, "V1", "survey_povrate")
  harea_povline[,survey_nprate := 1 - survey_povrate]
  harea_povline <- harea_povline[!is.na(harea_id),]
  
  
  #4.	Divide the vector of state survey estimates by sae estimates to get the benchmarking ratio.
  #5.	Multiply the point estimates by the benchmarking ratio.
  
  pop_size <- harea_povline[pop_size, on = harea_id]
  
  pop_size[, bmratio := survey_nprate / harea_nprate]
  
  pop_size[, npBM_Head_Count := npHead_Count * bmratio]
  
  pop_size[, BM_Head_Count := 1 - npBM_Head_Count]
  
  return(pop_size)
  
}


compute_cv <- function(ebp_obj,
                       yvar,
                       domainvar,
                       designvar = NULL,
                       weights,
                       calibmatrix,
                       threshold,
                       cluster_id){
  
  ## ******************** Direct Estimate : Mean and CV ************************
  
  ## computing direct estimate using calibrated bootstrapping (EMDI + LAEKEN) - direct CV1
  direct_obj <- emdi::direct(y = yvar,
                             smp_data = ebp_obj$framework$smp_data,
                             smp_domains = domainvar,
                             weights = weights,
                             design = designvar,
                             threshold = threshold,
                             var = TRUE,
                             boot_type = "calibrate",
                             B = 50,
                             X_calib = calibmatrix,
                             totals = NULL,
                             na.rm = TRUE)
  
  direct_obj$ind$Direct_Head_Count_CV <- sqrt(direct_obj$MSE$Head_Count) / direct_obj$ind$Head_Count
  
  ## computing direct estimate using the Horowitz Thompson (HT) indicator - direct CV2
  ### first compute poverty rates
  poor <- as.integer(ebp_obj$framework$smp_data[[yvar]] < threshold)
  
  domsize_dt <- as.data.frame(tapply(ebp_obj$model$data[[weights]],
                                     ebp_obj$model$data[[domainvar]],
                                     sum,
                                     na.rm = TRUE))
  
  colnames(domsize_dt) <- "popsize"
  domsize_dt$Domain <- rownames(domsize_dt)
  
  domsize_dt <- domsize_dt[is.na(domsize_dt$popsize) == FALSE,]
  
  domsize_dt <- domsize_dt[,c("Domain", "popsize")]
  
  ### HT estimator CV for direct estimate
  directht_dt <- sae::direct(y = poor,
                             dom = ebp_obj$model$data[[domainvar]],
                             sweight = ebp_obj$model$data[[weights]],
                             domsize = domsize_dt)
  
  directht_dt$Domain <- direct_obj$ind$Domain
  ## Compute design effect controlled direct estimates and CVs. (direct CV3)
  #### first estimate naive bootstrap
  #### compute design effect
  #### include psu list into the ebp data object
  ebp_obj$model$data$cluster_id <- as.factor(cluster_id)
  
  ebp_obj$model$data$weights <- ebp_obj$model$data[[weights]]
  ebp_obj$model$data$domainvar <- ebp_obj$model$data[[domainvar]]
  
  ebp_obj$model$data$poor <- as.integer(ebp_obj$model$data[[yvar]] > log(threshold))
  
  ebpobj_svy <- survey::svydesign(ids = ~1,
                                  weights = ~weights,
                                  strata = designvar,
                                  survey.lonely.psu = "adjust",
                                  data = ebp_obj$model$data)
  
  deff_adjust <- survey::svymean(x = ~poor, ebpobj_svy, na = TRUE, deff = TRUE)
  deff_adjust <- attr(deff_adjust, "deff")[1,1]
  
  ### multiple design effect with naive calibration
  naivevar_dt <- direct(y = yvar,
                        smp_data = ebp_obj$framework$smp_data,
                        smp_domains = domainvar,
                        design = designvar,
                        weights = weights,
                        threshold = threshold,
                        var = TRUE)
  
  naivevar_dt$ind$deff_CV <- sqrt(deff_adjust) * (sqrt(naivevar_dt$MSE$Head_Count) / naivevar_dt$ind$Head_Count)
  
  ## ************************ SAE Model Estimates and CV Estimation ****************************
  
  ## compute standard CV using the EMDI package estimator function
  emdi_dt <- emdi::estimators(object = ebp_obj,
                              indicator = "Head_Count",
                              MSE = FALSE,
                              CV = TRUE)
  
  ### combine the direct CV estimates with the SAE CV estimates
  result_dt <- as.data.table(emdi_dt$ind)
  
  setnames(result_dt, c("Head_Count", "Head_Count_CV"), c("EBP_Head_Count", "EBP_Head_Count_CV"))
  
  direct_dt <- as.data.table(direct_obj$ind[,c("Domain", "Head_Count", "Direct_Head_Count_CV")])
  setnames(direct_dt, c("Head_Count", "Direct_Head_Count_CV"), c("Direct_Head_Count", "CB_Head_Count_CV"))
  
  result_dt <- direct_dt[result_dt, on = "Domain"]
  
  direct_dt <- as.data.table(directht_dt[,c("Domain", "CV")])
  direct_dt[, CV := CV/100]
  direct_dt[, Domain := as.factor(Domain)]
  setnames(direct_dt, "CV", "HT_Head_Count_CV")
  
  result_dt <- direct_dt[result_dt, on = "Domain"]
  
  direct_dt <- as.data.table(naivevar_dt$ind[,c("Domain", "deff_CV")])
  setnames(direct_dt, "deff_CV", "DesignEffect_CV")
  
  result_dt <- direct_dt[result_dt, on = "Domain"]
  
  result_dt <- result_dt[,c("Domain", "Direct_Head_Count", "EBP_Head_Count", "HT_Head_Count_CV",
                            "CB_Head_Count_CV", "DesignEffect_CV", "EBP_Head_Count_CV")]
  
  
  
  return(result_dt)
  
}

specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k)) ### round to decimal place

report_ebpcoeftable <- function(ebp_obj){
  
  options(scipen = 999) ##drop the scientific notation
  
  varname_dt <- as.data.table(rownames(coef(summary(ebp_obj$model))))
  
  coef_dt <- as.data.table(coef(summary(ebp_obj$model)))
  
  coef_dt <- cbind(varname_dt, coef_dt)
  
  setnames(coef_dt, "V1", "Variable")
  
  coef_dt[,Sig := ifelse(`p-value` < 0.001, "***",
                         ifelse(`p-value` < 0.05 & `p-value` >= 0.001, "**",
                                ifelse(`p-value` < 0.01 & `p-value` >= 0.05, "*", "")))]
  
  coef_dt[,Value := ifelse(Value < abs(0.0004999999),
                           signif(Value, 2),
                           specify_decimal(Value, 3))]
  
  coef_dt[,Value := paste0(Value, Sig)]
  
  coef_dt <- coef_dt[,c("Variable", "Value")]
  
  return(coef_dt)
  
  
}

extract_var <- function(codename = "BFA",
                        varlist = bfa_selvars_fnlist,
                        var = "psu",
                        dt = hhgeosurvey_dt){
  
  psu_list <- na.omit(dt[code == codename, c(names(varlist), var), with = F])[[var]]
  
  return(psu_list)
  
}



present_modelselect <- function(dt, xvars, y){
  
  dt <- as.data.table(dt)
  
  dt <- na.omit(dt[,c(y, xvars), with = F])
  
  xset <- dt[, xvars, with = F]
  
  y <- dt[,colnames(dt)[!(colnames(dt) %in% colnames(xset))],with = F]
  
  model_dt <- cbind(y, xset)
  
  lasso_model <- hdm::rlasso(lpcexp ~ ., data = model_dt, post = TRUE)
  
  rsq <- 1 - (lasso_model$rss / lasso_model$tss)
  
  x <- ((lasso_model$rss)/lasso_model$tss)
  
  n <- nrow(lasso_model$model)
  
  lasso_model <- cbind(names(lasso_model$coefficients), as.data.table(lasso_model$coefficients))
  
  colnames(lasso_model) <- c("variable_name", "value")
  
  lasso_model <- lasso_model[(value != 0),]
  
  lasso_model <- lasso_model[-1,]
  
  k <- nrow(lasso_model)
  
  rsq_adj <- 1 - (x*(n - 1)/(n - k))
  
  return(list(lasso_model, rsq, rsq_adj))
  
  
}

present_modelselect2 <- function(dt,
                                 xvars,
                                 y,
                                 pen_vector,
                                 weights,
                                 opt_lambda){
  
  dt <- as.data.table(dt)
  
  if(is.null(weights) == FALSE){
    
    dt <- na.omit(dt[,c(y, xvars, weights), with = F])
    
    weights <- dt[, weights, with = F]
    
    #weights <- scale(weights)
    
  } else {
    
    dt <- na.omit(dt[,c(y, xvars), with = F])
    
    weights <- 1
    
  }
  
  
  xset <- dt[, xvars, with = F]
  
  y <- dt[, y, with = F]
  
  
  glmnet_obj <- cv.glmnet(x = as.matrix(xset),
                          y = as.matrix(y),
                          type.measure  = "mse",
                          nfolds = 10,
                          alpha = 1,
                          gamma = 1,
                          penalty.factor = pen_vector,
                          weights = as.matrix(weights))
  
  indicator_dt <- data.table(variable_name = rownames(as.matrix(coef(glmnet_obj,
                                                                     s = opt_lambda))),
                             value = as.data.table(as.matrix(coef(glmnet_obj,
                                                                  s = opt_lambda))))
  
  indicator_dt <- indicator_dt[value.s1 != 0,]
  
  colnames(indicator_dt) <- c("variable_name", "value")
  
  rsq <-
    glmnet_obj$glmnet.fit$dev.ratio[which(glmnet_obj$glmnet.fit$lambda ==
                                            glmnet_obj$lambda.1se)]
  
  nobs <- gin_modelselect$glmnet.fit$nobs
  
  rsq_adj <- 1 - (((1 - rsq) * (nobs - 1)) / (nobs - nrow(indicator_dt)))
  
  return(list(indicator_dt, rsq, rsq_adj))
  
  
}
### create calibration matrix
gen_calibmatrix <- function(codename = "BFA",
                            varlist = bfa_selvars_fnlist,
                            add_var = "natrep_admin"){
  
  new_var <- na.omit(hhgeosurvey_dt[code == codename, c(names(varlist), add_var), with = F])[[add_var]]
  
  new_var <- as.factor(new_var)
  
  new_var_dt <- weights::dummify(new_var)
  
  return(new_var_dt)
  
}


### compute correlation matrix and drop highly correlated variables
drop_singular <- function(dt,
                          threshold){
  # dt <- as.data.table(dt)
  checkCor <- cor(dt, use = "complete.obs") ##ensure only complete observations are used
  checkCor[is.na(checkCor)] <- 0 ##replace missing values with 0
  highCor <- sum(abs(checkCor[upper.tri(checkCor)]) > threshold)
  highCor <- caret::findCorrelation(checkCor, cutoff = threshold)
  # Remove high correlations
  varlist <- colnames(dt[, -highCor, with = F])
  
  # cor_matrix <- cor(dt)
  # cor_matrix_rm <- cor_matrix
  # cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0
  # diag(cor_matrix_rm) <- 0
  #
  # drop_col <- function(x){
  #   any(x > threshold)
  # }
  
  
  # varlist <- colnames(dt[,!apply(cor_matrix_rm, 2, drop_col), with = F])
  
  
  return(varlist)
  
}


countrymodel_select_stata <- function(dt,
                                      xvars,
                                      y,
                                      weights,
                                      selection = "BIC"){
  
  
  dt <- as.data.table(dt)
  
  if(is.null(weights) == FALSE){
    
    dt <- na.omit(dt[,c(y, xvars, weights), with = F])
    
    weights <- dt[, weights, with = F]
    
    #weights <- scale(weights)
    
  } else {
    
    dt <- na.omit(dt[,c(y, xvars), with = F])
    
    weights <- 1
    
  }
  
  xset <- dt[, xvars, with = F]
  
  y <- dt[, y, with = F]
  
  
  stata(lasso_wrapper,y=y,xvars=x,weights=weights,selection=selection,output="lasso_model")
  #read text file into indicator_list and return model
  xxx
}


### model selection by country
countrymodel_select <- function(dt,
                                xvars,
                                y,
                                pen_vector,
                                weights,
                                opt_lambda,
                                numfolds = 10,
                                seed_number = 123){
  
  dt <- as.data.table(dt)
  
  if(is.null(weights) == FALSE){
    
    dt <- na.omit(dt[,c(y, xvars, weights), with = F])
    
    weights <- dt[, weights, with = F]
    
    #weights <- scale(weights)
    
  } else {
    
    dt <- na.omit(dt[,c(y, xvars), with = F])
    
    weights <- 1
    
  }
  
  
  xset <- dt[, xvars, with = F]
  
  y <- dt[, y, with = F]
  
  set.seed(seed_number)
  
  set_folds <- sample(1:numfolds, nrow(dt), replace = TRUE)
  
  glmnet_obj <- cv.glmnet(x = as.matrix(xset),
                          y = as.matrix(y),
                          type.measure  = "mse",
                          nfolds = numfolds,
                          alpha = 1,
                          gamma = 1,
                          penalty.factor = pen_vector,
                          weights = as.matrix(weights),
                          foldid = set_folds,
                          keep = TRUE)
  
  indicator_dt <- data.table(indicator_name = rownames(as.matrix(coef(glmnet_obj,
                                                                      s = glmnet_obj$lambda.1se))),
                             coef_value = as.data.table(as.matrix(coef(glmnet_obj,
                                                                       s = glmnet_obj$lambda.1se))))
  
  indicator_dt <- indicator_dt[coef_value.s1 != 0,]
  
  indicator_list <- indicator_dt$indicator_name
  
  indicator_list <- indicator_list[!grepl("(Intercept)", indicator_list)]
  
  return(indicator_list)
  
}

#### combines candidate variable selection and model selection in a way that will be more replicable

model_selector <- function(tag = "areadummy",
                           code = "GIN",
                           keep_var = "areadummy_gin001",
                           sing_threshold = 0.99,
                           candidate_vars = model_vars){
  
  ### the set of candidate variables in each model
  candvar_list <- c(candidate_vars[!grepl(tag, candidate_vars)],
                    candidate_vars[grepl(paste(tag, tolower(code), sep = "_"),
                                         candidate_vars)])
  
  candvar_list <- candvar_list[!(candvar_list %in% keep_var)]
  
  #### drop potential singular variables
  candvar_list <- drop_singular(dt = hhgeosurvey_dt[country_code == code,
                                                    candvar_list,
                                                    with = F],
                                threshold = sing_threshold)
  
  pen_dt <- data.table(variable = candvar_list)
  
  area_id <- paste(tag, tolower(code), sep = "_")
  
  pen_dt[,pen_value := ifelse(grepl(area_id, candvar_list) == TRUE, 0, 1)]
  
  selvars_list <- countrymodel_select(dt = hhgeosurvey_dt[country_code == code,],
                                      xvars = candvar_list,
                                      y = "lpcexp",
                                      pen_vector = pen_dt$pen_value,
                                      weights = "hhweight",
                                      opt_lambda = "lambda.1se")
  
  
  
  return(selvars_list)
  
  
}


# ### drop super low variance variables
# drop_lowvariance <- function(dt,
#                              code,
#                              var_set){
#
#   drop_vars <-
#   dt[country_code == code,
#      check_vars,
#      with = F][,as.numeric(which(apply(.SD, 2, var, na.rm = TRUE) == 0))]
#
#   # drop_vars <-
#   #   dt[code == code,
#   #      check_vars,
#   #      with = F][,as.numeric(which(apply(.SD, 2, var, na.rm = TRUE) == 0))]
#   #
#   # drop_vars <- dt[code == "GIN",
#   #                 check_vars,
#   #                 with = F][,drop_vars,
#   #                            with = F]
#   #
#   # drop_vars <- colnames(drop_vars)
#
#   return(drop_vars)
#
# }


get_fpprice_data <- function(param_dt){
  
  base_url <- "https://microdata.worldbank.org/index.php/api/tables/data/fcv/wld_2021_rtfp_v02_m"
  
  dt_list <-
    lapply(split(param_dt, 1:nrow(param_dt)),
           FUN = function(x){
             
             res <- httr::GET(paste0(base_url,
                                     "?limit=",
                                     x$limit,
                                     "&offset=",
                                     x$offset))
             
             dt <- jsonlite::fromJSON(rawToChar(res$content))$data
             
             return(dt)
             
           })
  
  dt <- Reduce(x = dt_list,
               f = "rbind")
  
  return(dt)
  
}


rowprice_sum <- function(x){
  
  ## create the price types for each product
  hvar <- names(x)[grepl("h_", names(x))]
  lvar <- names(x)[grepl("l_", names(x))]
  ovar <- names(x)[grepl("o_", names(x))]
  cvar <- names(x)[grepl("c_", names(x))]
  
  if (is.na(x[[ovar]]) == FALSE | is.na(x[[cvar]] == FALSE)){
    
    p <- mean(c(x[[ovar]], x[[cvar]]), na.rm = TRUE)
    
    return(p)
    
  } else if (is.na(x[[ovar]]) & is.na(x[[cvar]])){
    
    p <- mean(c(x[[ovar]], x[[cvar]],
                x[[hvar]], x[[lvar]]),
              na.rm = TRUE)
    
    return(p)
    
  }
  
  
}


barplot_income_gains <- function(dt,
                                 model,
                                 crop,
                                 outvar,
                                 nametag){
  
  dt$outvar <- dt[[outvar]]
  
  sub_dt <- dt[dt$model_name == model & dt$crop_name == crop,]
  
  plotobj <-
    sub_dt %>%
    ggplot() +
    aes(x = outvar,
        y = reorder(WB_NAME, -outvar)) +
    geom_bar(stat = "identity",
             fill = "yellowgreen") + #goldenrod3
    labs(x = paste0("Income Gain (%) of extreme $1.90 poverty line ",
                    crop),
         y = "Country") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 10,
                                     family = "sans"),
          axis.text.x = element_text(size = 10,
                                     family = "sans"),
          axis.title = element_text(size = 14,
                                    family = "sans",
                                    hjust = 0.5),
          plot.title = element_text(size = 16,
                                    family = "sans",
                                    hjust = 0.5))
  
  ggsave(filename = paste0("output/graphs/income_gain/bar_country_percincomegain_",
                           nametag,
                           crop,
                           "_",
                           model,
                           ".png"),
         plot = plotobj,
         width = 10,
         height = 8,
         dpi = 150,
         units = "in",
         device='png')
  
  
}




barplot_totalincome_gains <- function(dt,
                                      model,
                                      outvar,
                                      nametag){
  
  dt$outvar <- dt[[outvar]]
  
  sub_dt <- dt[dt$model_name == model,]
  
  plotobj <-
    sub_dt %>%
    ggplot() +
    aes(x = outvar,
        y = reorder(WB_NAME, -outvar)) +
    geom_bar(stat = "identity",
             fill = "yellowgreen") + #goldenrod3
    labs(x = paste0("Overall Income Gain (%) of extreme $1.90 poverty line (",
                    model,
                    " model)"),
         y = "Country") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(size = 10,
                                     family = "sans"),
          axis.text.x = element_text(size = 10,
                                     family = "sans"),
          axis.title = element_text(size = 14,
                                    family = "sans",
                                    hjust = 0.5),
          plot.title = element_text(size = 16,
                                    family = "sans",
                                    hjust = 0.5))
  
  ggsave(filename = paste0("output/graphs/income_gain/bar_country_percincomegain_ALL_",
                           nametag,
                           "_",
                           model,
                           ".png"),
         plot = plotobj,
         width = 10,
         height = 8,
         dpi = 150,
         units = "in",
         device='png')
  
  
}



























