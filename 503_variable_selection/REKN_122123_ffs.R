# variable selection



##### LOAD PACKAGES

source("0001 Library.R")



##### LOAD DATA




species_data <- readRDS("R_objects/species_data_ch3.RDS")

scenarios <- readRDS("R_objects/scenarios_ch3.RDS")
scenarios <- lapply(scenarios, unwrap)


scenario_names <- names(scenarios)[1:18]


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





# recurring parameters



indices <- CreateSpacetimeFolds(species_data[[1]], spacevar = "Region_Code", k = 4)



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
  splitrule = c("extratrees", "gini"),
  min.node.size = 1
)





###### GROUP 1 - GLM



# predictor sets
  
  pred_all <- species_data$REKN %>%
    select(all_of(c(all_predictors2)))
  
  pred_clim <- species_data$REKN %>%
    select(all_of(c(climate_predictors2)))
  
  


# model 6
# conventional selection
# climate + additional
# MaxKappa



vs6 <- ffs(predictors = pred_all, 
           response = species_data$REKN$presence,
           trControl = trControlCon,
           minVar = 2,
           method = 'glm', 
           family = 'binomial', 
           metric = 'Kappa_Custom'
)

k_data <- species_data$REKN %>%
  select(any_of(c("presence", vs6$selectedvars)))


vs6_k <- train(presence ~ .,
               trControl = trControlSp, 
               method = 'glm', 
               family = 'binomial', 
               metric = 'Kappa_Custom',  
               data = k_data
)




# model 8
# conventional selection
# climate-only
# MaxKappa


vs8 <- ffs(predictors = pred_clim, 
           response = species_data$REKN$presence,
           trControl = trControlCon,
           minVar = 2,
           method = 'glm', 
           family = 'binomial', 
           metric = 'Kappa_Custom'
)

k_data <- species_data$REKN %>%
  select(any_of(c("presence", vs8$selectedvars)))


vs8_k <- train(presence ~ .,
               trControl = trControlSp, 
               method = 'glm', 
               family = 'binomial', 
               metric = 'Kappa_Custom',  
               data = k_data
)



# model 10
# spatial selection
# climate + additional
# MaxKappa


vs10 <- ffs(predictors = pred_all, 
            response = species_data$REKN$presence,
            trControl = trControlSp,
            minVar = 2,
            method = 'glm', 
            family = 'binomial', 
            metric = 'Kappa_Custom'
)

k_data <- species_data$REKN %>%
  select(any_of(c("presence", vs10$selectedvars)))


vs10_k <- train(presence ~ .,
               trControl = trControlSp, 
               method = 'glm', 
               family = 'binomial', 
               metric = 'Kappa_Custom',  
               data = k_data
)



# model 12
# spatial selection
# climate-only
# MaxKappa


vs12 <- ffs(predictors = pred_clim, 
            response = species_data$REKN$presence,
            trControl = trControlSp,
            minVar = 2,
            method = 'glm', 
            family = 'binomial', 
            metric = 'Kappa_Custom'
)

k_data <- species_data$REKN %>%
  select(any_of(c("presence", vs12$selectedvars)))


vs12_k <- train(presence ~ .,
               trControl = trControlSp, 
               method = 'glm', 
               family = 'binomial', 
               metric = 'Kappa_Custom',  
               data = k_data
)



##### GROUP 2: RANDOM FOREST






# model 5
# conventional selection
# climate + additional
# MaxKappa


vs5 <- ffs(predictors = pred_all, 
           response = species_data$REKN$presence,
           trControl = trControlCon,
           minVar = 2,
           method = 'ranger', 
           metric = 'Kappa_Custom', 
           tuneGrid = tgrid, 
           replace = FALSE,
           sample.fraction = 1
)


k_data <- species_data$REKN %>%
  select(any_of(c("presence", vs5$selectedvars)))


vs5_k <- train(presence ~ .,
                trControl = trControlSp, 
                method = 'ranger', 
                metric = 'Kappa_Custom',
                replace = FALSE,
                sample.fraction = 1,  
                data = k_data
)


# model 7
# conventional selection
# climate-only
# MaxKappa


vs7 <- ffs(predictors = pred_clim, 
           response = species_data$REKN$presence,
           trControl = trControlCon,
           minVar = 2,
           method = 'ranger', 
           metric = 'Kappa_Custom', 
           tuneGrid = tgrid,
           replace = FALSE,
           sample.fraction = 1
)


k_data <- species_data$REKN %>%
  select(any_of(c("presence", vs7$selectedvars)))


vs7_k <- train(presence ~ .,
               trControl = trControlSp, 
               method = 'ranger', 
               metric = 'Kappa_Custom',
               replace = FALSE,
               sample.fraction = 1,  
               data = k_data
)



# model 9
# spatial selection
# climate + additional
# MaxKappa



vs9 <- ffs(predictors = pred_all, 
           response = species_data$REKN$presence,
           trControl = trControlSp,
           minVar = 2,
           method = 'ranger', 
           metric = 'Kappa_Custom', 
           tuneGrid = tgrid,
           replace = FALSE,
           sample.fraction = 1
)

k_data <- species_data$REKN %>%
  select(any_of(c("presence", vs9$selectedvars)))


vs9_k <- train(presence ~ .,
                trControl = trControlSp, 
                method = 'ranger', 
                metric = 'Kappa_Custom',
                replace = FALSE,
                sample.fraction = 1,  
                data = k_data
)



# model 11
# spatial selection
# climate-only
# MaxKappa



vs11 <- ffs(predictors = pred_clim, 
            response = species_data$REKN$presence,
            trControl = trControlSp,
            minVar = 2,
            method = 'ranger', 
            metric = 'Kappa_Custom', 
            tuneGrid = tgrid,
            replace = FALSE,
            sample.fraction = 1
)

k_data <- species_data$REKN %>%
  select(any_of(c("presence", vs11$selectedvars)))


vs11_k <- train(presence ~ .,
               trControl = trControlSp, 
               method = 'ranger', 
               metric = 'Kappa_Custom',
               replace = FALSE,
               sample.fraction = 1,  
               data = k_data
)





#all selected vars

selected_vars <- list(m5_vars = vs5$selectedvars,
               m6_vars = vs6$selectedvars,
               m7_vars = vs7$selectedvars,
               m8_vars = vs8$selectedvars,
               m9_vars = vs9$selectedvars,
               m10_vars = vs10$selectedvars,
               m11_vars = vs11$selectedvars,
               m12_vars = vs12$selectedvars,
               m6_Kappa = round(vs6_k$results$Kappa_Custom, 3),
               m8_Kappa = round(vs8_k$results$Kappa_Custom, 3),
               m10_Kappa = round(vs10_k$results$Kappa_Custom, 3),
               m12_Kappa = round(vs12_k$results$Kappa_Custom, 3),
               m5_Kappa = max(round(vs5_k$results$Kappa_Custom, 3)),
               m7_Kappa = max(round(vs7_k$results$Kappa_Custom, 3)),
               m9_Kappa = max(round(vs9_k$results$Kappa_Custom, 3)),
               m11_Kappa = max(round(vs11_k$results$Kappa_Custom, 3))
               )





##### SAVE OBJECTS


saveRDS(selected_vars, "R_objects/selected_vars_REKN.RDS")


