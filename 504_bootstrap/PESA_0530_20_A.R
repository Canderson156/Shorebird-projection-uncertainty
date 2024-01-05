# Bootstrapping results for change in area

# Time to run: 


##### NOTES





##### OBJECTS CREATED IN THIS SCRIPT







##### LOAD PACKAGES

source("0001 Library.R")



##### LOAD DATA

 


species_data <- readRDS("R_objects/species_data_ch3.RDS")

scenarios <- readRDS("R_objects/scenarios_ch3.RDS")
scenarios <- lapply(scenarios, unwrap)


scenario_names <- names(scenarios)[1:18]

selected_vars <- readRDS("R_objects/selected_vars_PESA.RDS")


# create predictor lists


climate_predictors <- c("mean_annual_temp",     
                        "mean_temp_warmest_Q", "mean_temp_coldest_Q", 
                        "annual_precip",  "precip_wettest_month", 
                        "precip_driest_month", "precip_seasonality", 
                        "precip_wettest_Q",   "precip_driest_Q", 
                        "precip_warmest_Q",  "precip_coldest_Q", 
                        "mean_diurnal_range",   "isothermality", 
                        "temp_seasonality",   "max_temp_warmest_month", 
                        "min_temp_coldest_month", 
                        "mean_temp_wettest_Q",  "mean_temp_driest_Q")

climate_predictors2 <- c(climate_predictors, paste(climate_predictors, "2", sep = "")) 


lc_predictors <- c("elevation", "stdev_elevation", "d2coast", 
                   "lc_class", "substrate")



all_predictors <- c("mean_annual_temp",     
                    "mean_temp_warmest_Q", "mean_temp_coldest_Q", 
                    "annual_precip",  "precip_wettest_month", 
                    "precip_driest_month", "precip_seasonality", 
                    "precip_wettest_Q",   "precip_driest_Q", 
                    "precip_warmest_Q",  "precip_coldest_Q", 
                    "mean_diurnal_range",   "isothermality", 
                    "temp_seasonality",   "max_temp_warmest_month", 
                    "min_temp_coldest_month", 
                    "mean_temp_wettest_Q",  "mean_temp_driest_Q", 
                    "elevation", "stdev_elevation", "d2coast", 
                    "lc_class", "substrate")

all_predictors2 <- c(all_predictors, paste(all_predictors[1:21], "2", sep = "")) 


#what is the size of each cell in the raster (smaller closer to the poles)

cell_area <- cellSize(scenarios$future245_can_5k$lc_class, unit = "km")










# recurring parameters


focal_sp_data <- species_data$PESA
#selected_vars <- selected_vars$PESA

n_times <- 20

indices <- CreateSpacetimeFolds(focal_sp_data, spacevar = "Region_Code", k = 4)



#train control settings

#conventional
trControlCon <- trainControl(method = 'cv', number = 4, classProbs = TRUE,  
                            savePredictions = TRUE, summaryFunction = kappa_custom)
  
#spatial
trControlSp <- trainControl(method = 'cv', number = 4, classProbs = TRUE,
                            savePredictions = TRUE,  summaryFunction = kappa_custom, 
                            index = indices$index, indexOut = indices$indexOut)



#tune grid

tgrid <- expand.grid(
  mtry = c(2,4,6,8,10,12,14,16,18,20),
  splitrule = "extratrees",
  min.node.size = 1
)



###### GROUP 1 - GLM


# model 2
# all predictors
# climate + additional
# MaxKappa


xseed <- (sample(1:100000, 1))

focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", all_predictors2)))


vs2 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'glm', 
             family = 'binomial', 
             metric = 'Kappa_Custom',  
             data = focal_sp_data2
)

m2_samples <- bootstraps(focal_sp_data, times = n_times)


m2_samples$present <- map_vec(m2_samples$splits, area_function_glm, scenario = "present", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future245_can_5k <- map_vec(m2_samples$splits, area_function_glm, scenario = "future245_can_5k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future245_can_20k <- map_vec(m2_samples$splits, area_function_glm, scenario = "future245_can_20k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future245_can_e <- map_vec(m2_samples$splits, area_function_glm, scenario = "future245_can_e", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future245_had_5k <- map_vec(m2_samples$splits, area_function_glm, scenario = "future245_had_5k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future245_had_20k <- map_vec(m2_samples$splits, area_function_glm, scenario = "future245_had_20k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future245_had_e <- map_vec(m2_samples$splits, area_function_glm, scenario = "future245_had_e", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future245_acc_5k <- map_vec(m2_samples$splits, area_function_glm, scenario = "future245_acc_5k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future245_acc_20k <- map_vec(m2_samples$splits, area_function_glm, scenario = "future245_acc_20k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future245_acc_e <- map_vec(m2_samples$splits, area_function_glm, scenario = "future245_acc_e", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future585_can_5k <- map_vec(m2_samples$splits, area_function_glm, scenario = "future585_can_5k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future585_can_20k <- map_vec(m2_samples$splits, area_function_glm, scenario = "future585_can_20k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future585_can_e <- map_vec(m2_samples$splits, area_function_glm, scenario = "future585_can_e", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future585_had_5k <- map_vec(m2_samples$splits, area_function_glm, scenario = "future585_had_5k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future585_had_20k <- map_vec(m2_samples$splits, area_function_glm, scenario = "future585_had_20k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future585_had_e <- map_vec(m2_samples$splits, area_function_glm, scenario = "future585_had_e", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future585_acc_5k <- map_vec(m2_samples$splits, area_function_glm, scenario = "future585_acc_5k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future585_acc_20k <- map_vec(m2_samples$splits, area_function_glm, scenario = "future585_acc_20k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m2_samples$future585_acc_e <- map_vec(m2_samples$splits, area_function_glm, scenario = "future585_acc_e", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")



#summarize results

m2_change <- m2_samples %>%
  filter(present != 0) %>%
  mutate(across(any_of(scenario_names), ~ .x / present)) %>%
  select(-present, -splits, -id) 


m2_predicted <- m2_change %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "trend") %>%
  mutate(model = "climate+additional_none_glm") %>%
  separate(scenario, c( "scenario", "gcm", "dispersal")) %>%
  separate(model, c( "pool", "selection", "model"), sep = "_")

m2_predicted$Kappa <- round(vs2$results$Kappa_Custom, 3)


















# model 4
# all predictors
# climate-only
# MaxKappa


xseed <- (sample(1:100000, 1))

focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", climate_predictors2)))


vs4 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'glm', 
             family = 'binomial', 
             metric = 'Kappa_Custom',  
             data = focal_sp_data2
)



m4_samples <- bootstraps(focal_sp_data, times = n_times)

m4_samples$present <- map_vec(m4_samples$splits, area_function_glm, scenario = "present", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future245_can_5k <- map_vec(m4_samples$splits, area_function_glm, scenario = "future245_can_5k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future245_can_20k <- map_vec(m4_samples$splits, area_function_glm, scenario = "future245_can_20k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future245_can_e <- map_vec(m4_samples$splits, area_function_glm, scenario = "future245_can_e", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future245_had_5k <- map_vec(m4_samples$splits, area_function_glm, scenario = "future245_had_5k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future245_had_20k <- map_vec(m4_samples$splits, area_function_glm, scenario = "future245_had_20k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future245_had_e <- map_vec(m4_samples$splits, area_function_glm, scenario = "future245_had_e", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future245_acc_5k <- map_vec(m4_samples$splits, area_function_glm, scenario = "future245_acc_5k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future245_acc_20k <- map_vec(m4_samples$splits, area_function_glm, scenario = "future245_acc_20k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future245_acc_e <- map_vec(m4_samples$splits, area_function_glm, scenario = "future245_acc_e", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future585_can_5k <- map_vec(m4_samples$splits, area_function_glm, scenario = "future585_can_5k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future585_can_20k <- map_vec(m4_samples$splits, area_function_glm, scenario = "future585_can_20k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future585_can_e <- map_vec(m4_samples$splits, area_function_glm, scenario = "future585_can_e", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future585_had_5k <- map_vec(m4_samples$splits, area_function_glm, scenario = "future585_had_5k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future585_had_20k <- map_vec(m4_samples$splits, area_function_glm, scenario = "future585_had_20k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future585_had_e <- map_vec(m4_samples$splits, area_function_glm, scenario = "future585_had_e", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future585_acc_5k <- map_vec(m4_samples$splits, area_function_glm, scenario = "future585_acc_5k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future585_acc_20k <- map_vec(m4_samples$splits, area_function_glm, scenario = "future585_acc_20k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m4_samples$future585_acc_e <- map_vec(m4_samples$splits, area_function_glm, scenario = "future585_acc_e", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")





#summarize results

m4_change <- m4_samples %>%
  filter(present != 0) %>%
  mutate(across(any_of(scenario_names), ~ .x / present)) %>%
  select(-present, -splits, -id) 


m4_predicted <- m4_change %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "trend") %>%
  mutate(model = "climate-only_none_glm") %>%
  separate(scenario, c( "scenario", "gcm", "dispersal")) %>%
  separate(model, c( "pool", "selection", "model"), sep = "_")

m4_predicted$Kappa <- round(vs4$results$Kappa_Custom, 3)





# model 6
# conventional selection
# climate + additional
# MaxKappa

xseed <- (sample(1:100000, 1))



m6_samples <- bootstraps(focal_sp_data, times = n_times)

m6_samples$present <- map_vec(m6_samples$splits, area_function_glm, scenario = "present", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future245_can_5k <- map_vec(m6_samples$splits, area_function_glm, scenario = "future245_can_5k", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future245_can_20k <- map_vec(m6_samples$splits, area_function_glm, scenario = "future245_can_20k", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future245_can_e <- map_vec(m6_samples$splits, area_function_glm, scenario = "future245_can_e", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future245_had_5k <- map_vec(m6_samples$splits, area_function_glm, scenario = "future245_had_5k", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future245_had_20k <- map_vec(m6_samples$splits, area_function_glm, scenario = "future245_had_20k", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future245_had_e <- map_vec(m6_samples$splits, area_function_glm, scenario = "future245_had_e", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future245_acc_5k <- map_vec(m6_samples$splits, area_function_glm, scenario = "future245_acc_5k", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future245_acc_20k <- map_vec(m6_samples$splits, area_function_glm, scenario = "future245_acc_20k", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future245_acc_e <- map_vec(m6_samples$splits, area_function_glm, scenario = "future245_acc_e", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future585_can_5k <- map_vec(m6_samples$splits, area_function_glm, scenario = "future585_can_5k", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future585_can_20k <- map_vec(m6_samples$splits, area_function_glm, scenario = "future585_can_20k", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future585_can_e <- map_vec(m6_samples$splits, area_function_glm, scenario = "future585_can_e", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future585_had_5k <- map_vec(m6_samples$splits, area_function_glm, scenario = "future585_had_5k", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future585_had_20k <- map_vec(m6_samples$splits, area_function_glm, scenario = "future585_had_20k", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future585_had_e <- map_vec(m6_samples$splits, area_function_glm, scenario = "future585_had_e", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future585_acc_5k <- map_vec(m6_samples$splits, area_function_glm, scenario = "future585_acc_5k", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future585_acc_20k <- map_vec(m6_samples$splits, area_function_glm, scenario = "future585_acc_20k", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")
m6_samples$future585_acc_e <- map_vec(m6_samples$splits, area_function_glm, scenario = "future585_acc_e", model_seed = xseed, predictors = selected_vars$m6_vars, threshold = "MaxKappa")




#summarize results

m6_change <- m6_samples %>%
  filter(present != 0) %>%
  mutate(across(any_of(scenario_names), ~ .x / present)) %>%
  select(-present, -splits, -id) 


m6_predicted <- m6_change %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "trend") %>%
  mutate(model = "climate+additional_conventional_glm") %>%
  separate(scenario, c( "scenario", "gcm", "dispersal")) %>%
  separate(model, c( "pool", "selection", "model"), sep = "_")


m6_predicted$Kappa <- selected_vars$m6_Kappa







# model 8
# conventional selection
# climate-only
# MaxKappa

xseed <- (sample(1:100000, 1))



m8_samples <- bootstraps(focal_sp_data, times = n_times)

m8_samples$present <- map_vec(m8_samples$splits, area_function_glm, scenario = "present", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future245_can_5k <- map_vec(m8_samples$splits, area_function_glm, scenario = "future245_can_5k", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future245_can_20k <- map_vec(m8_samples$splits, area_function_glm, scenario = "future245_can_20k", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future245_can_e <- map_vec(m8_samples$splits, area_function_glm, scenario = "future245_can_e", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future245_had_5k <- map_vec(m8_samples$splits, area_function_glm, scenario = "future245_had_5k", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future245_had_20k <- map_vec(m8_samples$splits, area_function_glm, scenario = "future245_had_20k", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future245_had_e <- map_vec(m8_samples$splits, area_function_glm, scenario = "future245_had_e", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future245_acc_5k <- map_vec(m8_samples$splits, area_function_glm, scenario = "future245_acc_5k", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future245_acc_20k <- map_vec(m8_samples$splits, area_function_glm, scenario = "future245_acc_20k", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future245_acc_e <- map_vec(m8_samples$splits, area_function_glm, scenario = "future245_acc_e", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future585_can_5k <- map_vec(m8_samples$splits, area_function_glm, scenario = "future585_can_5k", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future585_can_20k <- map_vec(m8_samples$splits, area_function_glm, scenario = "future585_can_20k", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future585_can_e <- map_vec(m8_samples$splits, area_function_glm, scenario = "future585_can_e", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future585_had_5k <- map_vec(m8_samples$splits, area_function_glm, scenario = "future585_had_5k", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future585_had_20k <- map_vec(m8_samples$splits, area_function_glm, scenario = "future585_had_20k", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future585_had_e <- map_vec(m8_samples$splits, area_function_glm, scenario = "future585_had_e", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future585_acc_5k <- map_vec(m8_samples$splits, area_function_glm, scenario = "future585_acc_5k", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future585_acc_20k <- map_vec(m8_samples$splits, area_function_glm, scenario = "future585_acc_20k", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")
m8_samples$future585_acc_e <- map_vec(m8_samples$splits, area_function_glm, scenario = "future585_acc_e", model_seed = xseed, predictors = selected_vars$m8_vars, threshold = "MaxKappa")


#summarize results

m8_change <- m8_samples %>%
  filter(present != 0) %>%
  mutate(across(any_of(scenario_names), ~ .x / present)) %>%
  select(-present, -splits, -id) 



m8_predicted <- m8_change %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "trend") %>%
  mutate(model = "climate-only_conventional_glm") %>%
  separate(scenario, c( "scenario", "gcm", "dispersal")) %>%
  separate(model, c( "pool", "selection", "model"), sep = "_")

m8_predicted$Kappa <- selected_vars$m8_Kappa





# model 10
# spatial selection
# climate + additional
# MaxKappa


xseed <- (sample(1:100000, 1))



m10_samples <- bootstraps(focal_sp_data, times = n_times)

m10_samples$present <- map_vec(m10_samples$splits, area_function_glm, scenario = "present", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future245_can_5k <- map_vec(m10_samples$splits, area_function_glm, scenario = "future245_can_5k", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future245_can_20k <- map_vec(m10_samples$splits, area_function_glm, scenario = "future245_can_20k", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future245_can_e <- map_vec(m10_samples$splits, area_function_glm, scenario = "future245_can_e", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future245_had_5k <- map_vec(m10_samples$splits, area_function_glm, scenario = "future245_had_5k", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future245_had_20k <- map_vec(m10_samples$splits, area_function_glm, scenario = "future245_had_20k", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future245_had_e <- map_vec(m10_samples$splits, area_function_glm, scenario = "future245_had_e", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future245_acc_5k <- map_vec(m10_samples$splits, area_function_glm, scenario = "future245_acc_5k", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future245_acc_20k <- map_vec(m10_samples$splits, area_function_glm, scenario = "future245_acc_20k", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future245_acc_e <- map_vec(m10_samples$splits, area_function_glm, scenario = "future245_acc_e", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future585_can_5k <- map_vec(m10_samples$splits, area_function_glm, scenario = "future585_can_5k", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future585_can_20k <- map_vec(m10_samples$splits, area_function_glm, scenario = "future585_can_20k", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future585_can_e <- map_vec(m10_samples$splits, area_function_glm, scenario = "future585_can_e", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future585_had_5k <- map_vec(m10_samples$splits, area_function_glm, scenario = "future585_had_5k", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future585_had_20k <- map_vec(m10_samples$splits, area_function_glm, scenario = "future585_had_20k", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future585_had_e <- map_vec(m10_samples$splits, area_function_glm, scenario = "future585_had_e", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future585_acc_5k <- map_vec(m10_samples$splits, area_function_glm, scenario = "future585_acc_5k", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future585_acc_20k <- map_vec(m10_samples$splits, area_function_glm, scenario = "future585_acc_20k", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")
m10_samples$future585_acc_e <- map_vec(m10_samples$splits, area_function_glm, scenario = "future585_acc_e", model_seed = xseed, predictors = selected_vars$m10_vars, threshold = "MaxKappa")


#summarize results

m10_change <- m10_samples %>%
  filter(present != 0) %>%
  mutate(across(any_of(scenario_names), ~ .x / present)) %>%
  select(-present, -splits, -id) 


m10_predicted <- m10_change %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "trend") %>%
  mutate(model = "climate+additional_spatial_glm") %>%
  separate(scenario, c( "scenario", "gcm", "dispersal")) %>%
  separate(model, c( "pool", "selection", "model"), sep = "_")

m10_predicted$Kappa <- selected_vars$m10_Kappa







# model 12
# spatial selection
# climate-only
# MaxKappa



xseed <- (sample(1:100000, 1))



m12_samples <- bootstraps(focal_sp_data, times = n_times)

m12_samples$present <- map_vec(m12_samples$splits, area_function_glm, scenario = "present", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future245_can_5k <- map_vec(m12_samples$splits, area_function_glm, scenario = "future245_can_5k", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future245_can_20k <- map_vec(m12_samples$splits, area_function_glm, scenario = "future245_can_20k", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future245_can_e <- map_vec(m12_samples$splits, area_function_glm, scenario = "future245_can_e", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future245_had_5k <- map_vec(m12_samples$splits, area_function_glm, scenario = "future245_had_5k", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future245_had_20k <- map_vec(m12_samples$splits, area_function_glm, scenario = "future245_had_20k", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future245_had_e <- map_vec(m12_samples$splits, area_function_glm, scenario = "future245_had_e", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future245_acc_5k <- map_vec(m12_samples$splits, area_function_glm, scenario = "future245_acc_5k", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future245_acc_20k <- map_vec(m12_samples$splits, area_function_glm, scenario = "future245_acc_20k", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future245_acc_e <- map_vec(m12_samples$splits, area_function_glm, scenario = "future245_acc_e", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future585_can_5k <- map_vec(m12_samples$splits, area_function_glm, scenario = "future585_can_5k", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future585_can_20k <- map_vec(m12_samples$splits, area_function_glm, scenario = "future585_can_20k", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future585_can_e <- map_vec(m12_samples$splits, area_function_glm, scenario = "future585_can_e", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future585_had_5k <- map_vec(m12_samples$splits, area_function_glm, scenario = "future585_had_5k", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future585_had_20k <- map_vec(m12_samples$splits, area_function_glm, scenario = "future585_had_20k", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future585_had_e <- map_vec(m12_samples$splits, area_function_glm, scenario = "future585_had_e", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future585_acc_5k <- map_vec(m12_samples$splits, area_function_glm, scenario = "future585_acc_5k", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future585_acc_20k <- map_vec(m12_samples$splits, area_function_glm, scenario = "future585_acc_20k", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")
m12_samples$future585_acc_e <- map_vec(m12_samples$splits, area_function_glm, scenario = "future585_acc_e", model_seed = xseed, predictors = selected_vars$m12_vars, threshold = "MaxKappa")



#summarize results

m12_change <- m12_samples %>%
  filter(present != 0) %>%
  mutate(across(any_of(scenario_names), ~ .x / present)) %>%
  select(-present, -splits, -id) 


m12_predicted <- m12_change %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "trend") %>%
  mutate(model = "climate-only_spatial_glm") %>%
  separate(scenario, c( "scenario", "gcm", "dispersal")) %>%
  separate(model, c( "pool", "selection", "model"), sep = "_")


m12_predicted$Kappa <- selected_vars$m12_Kappa







##### GROUP 2: RANDOM FOREST




# model 1
# all predictors
# climate + additional
# MaxKappa


xseed <- (sample(1:100000, 1))

focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", all_predictors2)))


vs1 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'ranger', 
             metric = 'Kappa_Custom',  
             data = focal_sp_data2,
             replace = FALSE,
             sample.fraction = 1)


m1_samples <- bootstraps(focal_sp_data, times = n_times)


m1_samples$present <- map_vec(m1_samples$splits, area_function_rf, scenario = "present", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future245_can_5k <- map_vec(m1_samples$splits, area_function_rf, scenario = "future245_can_5k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future245_can_20k <- map_vec(m1_samples$splits, area_function_rf, scenario = "future245_can_20k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future245_can_e <- map_vec(m1_samples$splits, area_function_rf, scenario = "future245_can_e", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future245_had_5k <- map_vec(m1_samples$splits, area_function_rf, scenario = "future245_had_5k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future245_had_20k <- map_vec(m1_samples$splits, area_function_rf, scenario = "future245_had_20k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future245_had_e <- map_vec(m1_samples$splits, area_function_rf, scenario = "future245_had_e", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future245_acc_5k <- map_vec(m1_samples$splits, area_function_rf, scenario = "future245_acc_5k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future245_acc_20k <- map_vec(m1_samples$splits, area_function_rf, scenario = "future245_acc_20k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future245_acc_e <- map_vec(m1_samples$splits, area_function_rf, scenario = "future245_acc_e", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future585_can_5k <- map_vec(m1_samples$splits, area_function_rf, scenario = "future585_can_5k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future585_can_20k <- map_vec(m1_samples$splits, area_function_rf, scenario = "future585_can_20k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future585_can_e <- map_vec(m1_samples$splits, area_function_rf, scenario = "future585_can_e", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future585_had_5k <- map_vec(m1_samples$splits, area_function_rf, scenario = "future585_had_5k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future585_had_20k <- map_vec(m1_samples$splits, area_function_rf, scenario = "future585_had_20k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future585_had_e <- map_vec(m1_samples$splits, area_function_rf, scenario = "future585_had_e", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future585_acc_5k <- map_vec(m1_samples$splits, area_function_rf, scenario = "future585_acc_5k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future585_acc_20k <- map_vec(m1_samples$splits, area_function_rf, scenario = "future585_acc_20k", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")
m1_samples$future585_acc_e <- map_vec(m1_samples$splits, area_function_rf, scenario = "future585_acc_e", model_seed = xseed, predictors = all_predictors2, threshold = "MaxKappa")



#summarize results

m1_change <- m1_samples %>%
  filter(present != 0) %>%
  mutate(across(any_of(scenario_names), ~ .x / present)) %>%
  select(-present, -splits, -id) 


m1_predicted <- m1_change %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "trend") %>%
  mutate(model = "climate+additional_none_rf") %>%
  separate(scenario, c( "scenario", "gcm", "dispersal")) %>%
  separate(model, c( "pool", "selection", "model"), sep = "_")


m1_predicted$Kappa <- max(round(vs1$results$Kappa_Custom, 3))







# model 3
# all predictors
# climate-only
# MaxKappa


xseed <- (sample(1:100000, 1))


focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", climate_predictors2)))


vs3 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'ranger', 
             metric = 'Kappa_Custom',  
             data = focal_sp_data2, 
             replace = FALSE,
             sample.fraction = 1)


m3_samples <- bootstraps(focal_sp_data, times = n_times)

m3_samples$present <- map_vec(m3_samples$splits, area_function_rf, scenario = "present", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future245_can_5k <- map_vec(m3_samples$splits, area_function_rf, scenario = "future245_can_5k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future245_can_20k <- map_vec(m3_samples$splits, area_function_rf, scenario = "future245_can_20k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future245_can_e <- map_vec(m3_samples$splits, area_function_rf, scenario = "future245_can_e", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future245_had_5k <- map_vec(m3_samples$splits, area_function_rf, scenario = "future245_had_5k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future245_had_20k <- map_vec(m3_samples$splits, area_function_rf, scenario = "future245_had_20k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future245_had_e <- map_vec(m3_samples$splits, area_function_rf, scenario = "future245_had_e", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future245_acc_5k <- map_vec(m3_samples$splits, area_function_rf, scenario = "future245_acc_5k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future245_acc_20k <- map_vec(m3_samples$splits, area_function_rf, scenario = "future245_acc_20k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future245_acc_e <- map_vec(m3_samples$splits, area_function_rf, scenario = "future245_acc_e", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future585_can_5k <- map_vec(m3_samples$splits, area_function_rf, scenario = "future585_can_5k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future585_can_20k <- map_vec(m3_samples$splits, area_function_rf, scenario = "future585_can_20k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future585_can_e <- map_vec(m3_samples$splits, area_function_rf, scenario = "future585_can_e", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future585_had_5k <- map_vec(m3_samples$splits, area_function_rf, scenario = "future585_had_5k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future585_had_20k <- map_vec(m3_samples$splits, area_function_rf, scenario = "future585_had_20k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future585_had_e <- map_vec(m3_samples$splits, area_function_rf, scenario = "future585_had_e", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future585_acc_5k <- map_vec(m3_samples$splits, area_function_rf, scenario = "future585_acc_5k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future585_acc_20k <- map_vec(m3_samples$splits, area_function_rf, scenario = "future585_acc_20k", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")
m3_samples$future585_acc_e <- map_vec(m3_samples$splits, area_function_rf, scenario = "future585_acc_e", model_seed = xseed, predictors = climate_predictors2, threshold = "MaxKappa")





#summarize results

m3_change <- m3_samples %>%
  filter(present != 0) %>%
  mutate(across(any_of(scenario_names), ~ .x / present)) %>%
  select(-present, -splits, -id) 


m3_category <- m3_change %>%
  mutate(across(everything(), cut, breaks=c(-Inf, 1/2, 1/1.5, 1.5, 2, Inf), labels=c("major loss","minor loss","stable", "minor gain", "major gain")))


m3_predicted <- m3_change %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "trend") %>%
  mutate(model = "climate-only_none_rf") %>%
  separate(scenario, c( "scenario", "gcm", "dispersal")) %>%
  separate(model, c( "pool", "selection", "model"), sep = "_")



m3_predicted$Kappa <- max(round(vs3$results$Kappa_Custom, 3))








# model 5
# conventional selection
# climate + additional
# MaxKappa



xseed <- (sample(1:100000, 1))


m5_samples <- bootstraps(focal_sp_data, times = n_times)

m5_samples$present <- map_vec(m5_samples$splits, area_function_rf, scenario = "present", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future245_can_5k <- map_vec(m5_samples$splits, area_function_rf, scenario = "future245_can_5k", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future245_can_20k <- map_vec(m5_samples$splits, area_function_rf, scenario = "future245_can_20k", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future245_can_e <- map_vec(m5_samples$splits, area_function_rf, scenario = "future245_can_e", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future245_had_5k <- map_vec(m5_samples$splits, area_function_rf, scenario = "future245_had_5k", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future245_had_20k <- map_vec(m5_samples$splits, area_function_rf, scenario = "future245_had_20k", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future245_had_e <- map_vec(m5_samples$splits, area_function_rf, scenario = "future245_had_e", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future245_acc_5k <- map_vec(m5_samples$splits, area_function_rf, scenario = "future245_acc_5k", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future245_acc_20k <- map_vec(m5_samples$splits, area_function_rf, scenario = "future245_acc_20k", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future245_acc_e <- map_vec(m5_samples$splits, area_function_rf, scenario = "future245_acc_e", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future585_can_5k <- map_vec(m5_samples$splits, area_function_rf, scenario = "future585_can_5k", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future585_can_20k <- map_vec(m5_samples$splits, area_function_rf, scenario = "future585_can_20k", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future585_can_e <- map_vec(m5_samples$splits, area_function_rf, scenario = "future585_can_e", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future585_had_5k <- map_vec(m5_samples$splits, area_function_rf, scenario = "future585_had_5k", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future585_had_20k <- map_vec(m5_samples$splits, area_function_rf, scenario = "future585_had_20k", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future585_had_e <- map_vec(m5_samples$splits, area_function_rf, scenario = "future585_had_e", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future585_acc_5k <- map_vec(m5_samples$splits, area_function_rf, scenario = "future585_acc_5k", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future585_acc_20k <- map_vec(m5_samples$splits, area_function_rf, scenario = "future585_acc_20k", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")
m5_samples$future585_acc_e <- map_vec(m5_samples$splits, area_function_rf, scenario = "future585_acc_e", model_seed = xseed, predictors = selected_vars$m5_vars, threshold = "MaxKappa")




#summarize results

m5_change <- m5_samples %>%
  filter(present != 0) %>%
  mutate(across(any_of(scenario_names), ~ .x / present)) %>%
  select(-present, -splits, -id) 



m5_predicted <- m5_change %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "trend") %>%
  mutate(model = "climate+additional_conventional_rf") %>%
  separate(scenario, c( "scenario", "gcm", "dispersal")) %>%
  separate(model, c( "pool", "selection", "model"), sep = "_")


m5_predicted$Kappa <- selected_vars$m5_Kappa









# model 7
# conventional selection
# climate-only
# MaxKappa

xseed <- (sample(1:100000, 1))


m7_samples <- bootstraps(focal_sp_data, times = n_times)

m7_samples$present <- map_vec(m7_samples$splits, area_function_rf, scenario = "present", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future245_can_5k <- map_vec(m7_samples$splits, area_function_rf, scenario = "future245_can_5k", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future245_can_20k <- map_vec(m7_samples$splits, area_function_rf, scenario = "future245_can_20k", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future245_can_e <- map_vec(m7_samples$splits, area_function_rf, scenario = "future245_can_e", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future245_had_5k <- map_vec(m7_samples$splits, area_function_rf, scenario = "future245_had_5k", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future245_had_20k <- map_vec(m7_samples$splits, area_function_rf, scenario = "future245_had_20k", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future245_had_e <- map_vec(m7_samples$splits, area_function_rf, scenario = "future245_had_e", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future245_acc_5k <- map_vec(m7_samples$splits, area_function_rf, scenario = "future245_acc_5k", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future245_acc_20k <- map_vec(m7_samples$splits, area_function_rf, scenario = "future245_acc_20k", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future245_acc_e <- map_vec(m7_samples$splits, area_function_rf, scenario = "future245_acc_e", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future585_can_5k <- map_vec(m7_samples$splits, area_function_rf, scenario = "future585_can_5k", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future585_can_20k <- map_vec(m7_samples$splits, area_function_rf, scenario = "future585_can_20k", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future585_can_e <- map_vec(m7_samples$splits, area_function_rf, scenario = "future585_can_e", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future585_had_5k <- map_vec(m7_samples$splits, area_function_rf, scenario = "future585_had_5k", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future585_had_20k <- map_vec(m7_samples$splits, area_function_rf, scenario = "future585_had_20k", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future585_had_e <- map_vec(m7_samples$splits, area_function_rf, scenario = "future585_had_e", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future585_acc_5k <- map_vec(m7_samples$splits, area_function_rf, scenario = "future585_acc_5k", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future585_acc_20k <- map_vec(m7_samples$splits, area_function_rf, scenario = "future585_acc_20k", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")
m7_samples$future585_acc_e <- map_vec(m7_samples$splits, area_function_rf, scenario = "future585_acc_e", model_seed = xseed, predictors = selected_vars$m7_vars, threshold = "MaxKappa")


#summarize results

m7_change <- m7_samples %>%
  filter(present != 0) %>%
  mutate(across(any_of(scenario_names), ~ .x / present)) %>%
  select(-present, -splits, -id) 


m7_predicted <- m7_change %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "trend") %>%
  mutate(model = "climate-only_conventional_rf") %>%
  separate(scenario, c( "scenario", "gcm", "dispersal")) %>%
  separate(model, c( "pool", "selection", "model"), sep = "_")


m7_predicted$Kappa <- selected_vars$m7_Kappa








# model 9
# spatial selection
# climate + additional
# MaxKappa




xseed <- (sample(1:100000, 1))



m9_samples <- bootstraps(focal_sp_data, times = n_times)

m9_samples$present <- map_vec(m9_samples$splits, area_function_rf, scenario = "present", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future245_can_5k <- map_vec(m9_samples$splits, area_function_rf, scenario = "future245_can_5k", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future245_can_20k <- map_vec(m9_samples$splits, area_function_rf, scenario = "future245_can_20k", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future245_can_e <- map_vec(m9_samples$splits, area_function_rf, scenario = "future245_can_e", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future245_had_5k <- map_vec(m9_samples$splits, area_function_rf, scenario = "future245_had_5k", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future245_had_20k <- map_vec(m9_samples$splits, area_function_rf, scenario = "future245_had_20k", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future245_had_e <- map_vec(m9_samples$splits, area_function_rf, scenario = "future245_had_e", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future245_acc_5k <- map_vec(m9_samples$splits, area_function_rf, scenario = "future245_acc_5k", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future245_acc_20k <- map_vec(m9_samples$splits, area_function_rf, scenario = "future245_acc_20k", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future245_acc_e <- map_vec(m9_samples$splits, area_function_rf, scenario = "future245_acc_e", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future585_can_5k <- map_vec(m9_samples$splits, area_function_rf, scenario = "future585_can_5k", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future585_can_20k <- map_vec(m9_samples$splits, area_function_rf, scenario = "future585_can_20k", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future585_can_e <- map_vec(m9_samples$splits, area_function_rf, scenario = "future585_can_e", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future585_had_5k <- map_vec(m9_samples$splits, area_function_rf, scenario = "future585_had_5k", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future585_had_20k <- map_vec(m9_samples$splits, area_function_rf, scenario = "future585_had_20k", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future585_had_e <- map_vec(m9_samples$splits, area_function_rf, scenario = "future585_had_e", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future585_acc_5k <- map_vec(m9_samples$splits, area_function_rf, scenario = "future585_acc_5k", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future585_acc_20k <- map_vec(m9_samples$splits, area_function_rf, scenario = "future585_acc_20k", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")
m9_samples$future585_acc_e <- map_vec(m9_samples$splits, area_function_rf, scenario = "future585_acc_e", model_seed = xseed, predictors = selected_vars$m9_vars, threshold = "MaxKappa")


#summarize results

m9_change <- m9_samples %>%
  filter(present != 0) %>%
  mutate(across(any_of(scenario_names), ~ .x / present)) %>%
  select(-present, -splits, -id) 


m9_predicted <- m9_change %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "trend") %>%
  mutate(model = "climate+additional_spatial_rf") %>%
  separate(scenario, c( "scenario", "gcm", "dispersal")) %>%
  separate(model, c( "pool", "selection", "model"), sep = "_")


m9_predicted$Kappa <- selected_vars$m9_Kappa










# model 11
# spatial selection
# climate-only
# MaxKappa



xseed <- (sample(1:100000, 1))



m11_samples <- bootstraps(focal_sp_data, times = n_times)

m11_samples$present <- map_vec(m11_samples$splits, area_function_rf, scenario = "present", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future245_can_5k <- map_vec(m11_samples$splits, area_function_rf, scenario = "future245_can_5k", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future245_can_20k <- map_vec(m11_samples$splits, area_function_rf, scenario = "future245_can_20k", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future245_can_e <- map_vec(m11_samples$splits, area_function_rf, scenario = "future245_can_e", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future245_had_5k <- map_vec(m11_samples$splits, area_function_rf, scenario = "future245_had_5k", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future245_had_20k <- map_vec(m11_samples$splits, area_function_rf, scenario = "future245_had_20k", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future245_had_e <- map_vec(m11_samples$splits, area_function_rf, scenario = "future245_had_e", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future245_acc_5k <- map_vec(m11_samples$splits, area_function_rf, scenario = "future245_acc_5k", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future245_acc_20k <- map_vec(m11_samples$splits, area_function_rf, scenario = "future245_acc_20k", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future245_acc_e <- map_vec(m11_samples$splits, area_function_rf, scenario = "future245_acc_e", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future585_can_5k <- map_vec(m11_samples$splits, area_function_rf, scenario = "future585_can_5k", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future585_can_20k <- map_vec(m11_samples$splits, area_function_rf, scenario = "future585_can_20k", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future585_can_e <- map_vec(m11_samples$splits, area_function_rf, scenario = "future585_can_e", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future585_had_5k <- map_vec(m11_samples$splits, area_function_rf, scenario = "future585_had_5k", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future585_had_20k <- map_vec(m11_samples$splits, area_function_rf, scenario = "future585_had_20k", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future585_had_e <- map_vec(m11_samples$splits, area_function_rf, scenario = "future585_had_e", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future585_acc_5k <- map_vec(m11_samples$splits, area_function_rf, scenario = "future585_acc_5k", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future585_acc_20k <- map_vec(m11_samples$splits, area_function_rf, scenario = "future585_acc_20k", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")
m11_samples$future585_acc_e <- map_vec(m11_samples$splits, area_function_rf, scenario = "future585_acc_e", model_seed = xseed, predictors = selected_vars$m11_vars, threshold = "MaxKappa")



#summarize results

m11_change <- m11_samples %>%
  filter(present != 0) %>%
  mutate(across(any_of(scenario_names), ~ .x / present)) %>%
  select(-present, -splits, -id) 


m11_predicted <- m11_change %>%
  pivot_longer(everything(), names_to = "scenario", values_to = "trend") %>%
  mutate(model = "climate-only_spatial_rf") %>%
  separate(scenario, c( "scenario", "gcm", "dispersal")) %>%
  separate(model, c( "pool", "selection", "model"), sep = "_")



m11_predicted$Kappa <- selected_vars$m11_Kappa










 




##### Group all results together





all_predicted_PESA <- map_dfr(list(
  "m1_predicted",
  "m2_predicted",
  "m3_predicted",
  "m4_predicted",
  "m5_predicted",
  "m6_predicted",
  "m7_predicted",
  "m8_predicted",
  "m9_predicted",
  "m10_predicted",
  "m11_predicted",
  "m12_predicted"),
  ~ if (exists(.x)) {get(.x)})







##### SAVE OBJECTS


saveRDS(all_predicted_PESA, "R_objects/predicted_PESA_0530_20_A.RDS")


