# Consensus maps


#### need to set this up so that it doesn't include removed species



##### LOAD OBJECTS 


model_names <- readRDS("R_objects/model_names.RDS")
model_names <- model_names[c(2,1,4,3,6,5,8,7,10,9,12,11)]


selected_files <- list.files("R_objects/ffs/") %>%
  str_prefix("R_objects/ffs/")
selected_vars <- lapply(selected_files, readRDS)

names(selected_vars) <- substr(selected_files, 29,32)


species_data <- readRDS("R_objects/species_data.RDS")

scenarios <- readRDS("R_objects/scenarios.RDS")
scenarios <- lapply(scenarios, unwrap)


coastline <- readRDS("R_objects/coastline.RDS")

study_area <- readRDS("R_objects/study_area.RDS")



##### SPECIES MODELS

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




#train control settings


indices <- CreateSpacetimeFolds(species_data$AMGP, spacevar = "Region_Code", k = 4)

#conventional
trControlCon <- trainControl(method = 'cv', number = 4, classProbs = TRUE,  
                             savePredictions = TRUE, summaryFunction = kappa_custom)

#spatial
trControlSp <- trainControl(method = 'cv', number = 4, classProbs = TRUE,
                            savePredictions = TRUE,  summaryFunction = kappa_custom, 
                            index = indices$index, indexOut = indices$indexOut)


#make the models for each species


# 20 mins


models <- rep(list(list()), length(species_data))

tic()

for(i in 1:length(species_data)){
  


focal_sp_data <- species_data[[i]]


# model 2
# all predictors
# climate + additional

focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", all_predictors2)))


vs2 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'glm', 
             family = 'binomial', 
             metric = 'Kappa_Custom',  
             data = focal_sp_data2
)


# model 4
# all predictors
# climate-only

focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", climate_predictors2)))


vs4 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'glm', 
             family = 'binomial', 
             metric = 'Kappa_Custom',  
             data = focal_sp_data2
)



# model 6
# conventional selection
# climate + additional

focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", selected_vars[[i]]$m6_vars)))


vs6 <- train(presence ~ .,
               trControl = trControlSp, 
               method = 'glm', 
               family = 'binomial', 
               metric = 'Kappa_Custom',  
               data = focal_sp_data2
)



# model 8
# conventional selection
# climate-only

focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", selected_vars[[i]]$m8_vars)))


vs8 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'glm', 
             family = 'binomial', 
             metric = 'Kappa_Custom',  
             data = focal_sp_data2
)


# model 10
# spatial selection
# climate + additional

focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", selected_vars[[i]]$m10_vars)))


vs10 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'glm', 
             family = 'binomial', 
             metric = 'Kappa_Custom',  
             data = focal_sp_data2
)

# model 12
# spatial selection
# climate-only

focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", selected_vars[[i]]$m12_vars)))


vs12 <- train(presence ~ .,
              trControl = trControlSp, 
              method = 'glm', 
              family = 'binomial', 
              metric = 'Kappa_Custom',  
              data = focal_sp_data2
)



# model 1
# all predictors
# climate + additional


focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", all_predictors2)))


vs1 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'ranger', 
             metric = 'Kappa_Custom',  
             data = focal_sp_data2,
             replace = FALSE,
             sample.fraction = 1)


# model 3
# all predictors
# climate-only

focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", climate_predictors2)))


vs3 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'ranger', 
             metric = 'Kappa_Custom',  
             data = focal_sp_data2, 
             replace = FALSE,
             sample.fraction = 1)


# model 5
# conventional selection
# climate + additional


focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", selected_vars[[i]]$m5_vars)))


vs5 <- train(presence ~ .,
               trControl = trControlSp, 
               method = 'ranger', 
               metric = 'Kappa_Custom',
               replace = FALSE,
               sample.fraction = 1,  
               data = focal_sp_data2
)



# model 7
# conventional selection
# climate-only


focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", selected_vars[[i]]$m7_vars)))


vs7 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'ranger', 
             metric = 'Kappa_Custom',
             replace = FALSE,
             sample.fraction = 1,  
             data = focal_sp_data2
)



# model 9
# spatial selection
# climate + additional

focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", selected_vars[[i]]$m9_vars)))


vs9 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'ranger', 
             metric = 'Kappa_Custom',
             replace = FALSE,
             sample.fraction = 1,  
             data = focal_sp_data2
)


# model 11
# spatial selection
# climate-only

focal_sp_data2 <- focal_sp_data %>%
  select(any_of(c("presence", selected_vars[[i]]$m11_vars)))


vs11 <- train(presence ~ .,
             trControl = trControlSp, 
             method = 'ranger', 
             metric = 'Kappa_Custom',
             replace = FALSE,
             sample.fraction = 1,  
             data = focal_sp_data2
)




all_models <- list(
  vs2,
  vs4,
  vs6,
  vs8,
  vs10,
  vs12,
  vs1,
  vs3,
  vs5,
  vs7,
  vs9,
  vs11)

names(all_models) <- model_names



models[[i]] <- all_models




}

names(models) <- names(species_data)

toc()


saveRDS(models, "R_objects/models.RDS")



# calculate the thresholds for suitable/unsuitable, maximizing Kappa

threshold_function <- function(model){
  
  
  dt <- model$pred[,c("rowIndex", "obs", "yes")] %>%
    arrange(rowIndex) %>%
    mutate(obs = ifelse(obs == "yes", TRUE, FALSE),
           rowIndex = as.character(rowIndex))
  
  
  ths <-  optimal.thresholds(dt, opt.methods	= "MaxKappa")
  
  
  return(ths$yes)
  
  
  }


thresholds <- lapply(models, lapply, threshold_function)


saveRDS(thresholds, "R_objects/thresholds.RDS")



# create continuous projections


lapply_project_scenarios <- function(model){
  
  
  projections <- lapply(scenarios, predict, model, type = "prob", na.rm = TRUE)
  return(projections)

  
}


continuous_projections <- lapply(models, lapply, lapply_project_scenarios)


# convert to binary projections using thesholds

binary_projections <- map2(continuous_projections, thresholds, map2, function(proj, thres) lapply(proj, function(x) x$yes > thres))






#subsets of the full dataset



#only present projections
binary_projections_present <-lapply(binary_projections, lapply, function(x) x[length(x)])

#only future projections
binary_projections_future <-lapply(binary_projections, lapply, function(x) x[1:(length(x)-1)])


#save

binary_projections_present2 <- lapply(binary_projections_present, lapply, lapply, lapply, wrap)
saveRDS(binary_projections_present2, "R_objects/binary_projections_present.RDS")

binary_projections_future2 <- lapply(binary_projections_future, lapply, lapply, lapply, wrap)
saveRDS(binary_projections_future2, "R_objects/binary_projections_future.RDS")





##### PRESENT


# combine separate maps into species stacked rasters

present_stack <- lapply(binary_projections_present, lapply, lapply, rast)
present_stack <- lapply(present_stack, lapply, rast)
present_stack <- lapply(present_stack, rast)


# consensus maps

consensus_present <- lapply(present_stack, mean)

all_sb_consensus_present <- rast(consensus_present)

#sum of species where consensus > 50%

all_sb_consensus_present_50 <- all_sb_consensus_present > 0.5
all_sb_consensus_present_50 <- sum(all_sb_consensus_present_50)


consensus_present2 <- lapply(consensus_present, wrap)
saveRDS(consensus_present2, "R_objects/consensus_present.RDS")

all_sb_consensus_present_502 <- wrap(all_sb_consensus_present_50)
saveRDS(all_sb_consensus_present_502, "R_objects/all_sb_consensus_present_50.RDS")



##### FUTURE 


# combine separate maps into species stacked rasters


future_stack <- lapply(binary_projections_future, lapply, lapply, rast)
future_stack <- lapply(future_stack, lapply, rast)
future_stack <- lapply(future_stack, rast)



# consensus maps

consensus_future <- lapply(future_stack, mean)

all_sb_consensus_future <- rast(consensus_future)


#sum of species where consensus > 50%

all_sb_consensus_future_50 <- all_sb_consensus_future > 0.5
all_sb_consensus_future_50 <- sum(all_sb_consensus_future_50)


consensus_future2 <- lapply(consensus_future, wrap)
saveRDS(consensus_future2, "R_objects/consensus_future.RDS")

all_sb_consensus_future_502 <- wrap(all_sb_consensus_future_50)
saveRDS(all_sb_consensus_future_502, "R_objects/all_sb_consensus_future_50.RDS")





##### FIGURES



#add outline of study area to future area where treeline isn't included

consensus_present <- lapply(consensus_present, cover, study_area)
consensus_future <- lapply(consensus_future, cover, study_area)


# pair present and future maps for each species together

consensus_maps <- rep(list(rast()), length(consensus_future))

for(i in 1:length(consensus_maps)){
  
  consensus_maps[[i]] <- c(consensus_present[[i]],
                           consensus_future[[i]])
  
  names(consensus_maps[[i]]) <- c("2010", "2075")
  
}

names(consensus_maps) <- names(consensus_future)



#individual species


ggplot_consensus <- lapply(consensus_maps, function(x){
  
  ggplot() +
    geom_spatraster(data = x) +
    geom_spatvector(data = coastline, fill = NA) +
    facet_wrap(~lyr, ncol = 2) +
    scale_fill_viridis_c(na.value=NA, limits = c(0,1)) +
    theme_bw()
  
  
})


plot_titles <- c("American Golden-Plover", 
                 "Baird's Sandpiper", 
                 "Black-bellied Plover",
                 "Buff-breasted Sandpiper", 
                 "Dunlin",
                 "Pectoral Sandpiper",
                 "Red Phalarope", 
                 "Semipalmated Sandpiper", 
                 "Stilt Sandpiper", 
                 "White-rumped Sandpiper")


for(i in 1:length(ggplot_consensus)){
  
  
ggplot_consensus[[i]] <- ggplot_consensus[[i]] + 
  ggtitle(plot_titles[[i]])  
  
  
}




for(i in 1:length(ggplot_consensus)){
  
  ggsave(
    filename = paste(c("consensus_plots_species"),i, c(".tiff"), sep = ""),
    plot = marrangeGrob(ggplot_consensus[i], nrow=1, ncol=1, top=NULL), 
    width = 6, height = 3
  )
  
}






# all species, 50% consensus


consensus_all_50 <- c(all_sb_consensus_present_50, all_sb_consensus_future_50)
names(consensus_all_50) <- c("2010", "2075")


consensus_all_50 <- cover(consensus_all_50, study_area)


# convert to a categorical raster for plotting

m <- c(0, 0.9, 0,
       1, 1.9, 1,
       2, 2.9, 2,
       3, 3.9, 3,
       4, 4.9, 4,
       5, 9, 5)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
consensus_all_50 <- classify(consensus_all_50, rclmat, include.lowest=TRUE, brackets=TRUE)

consensus_all_50 <- as.factor(consensus_all_50)


all_sp_figure_50 <- ggplot() +
  geom_spatraster(data = consensus_all_50) +
  geom_spatvector(data = coastline, fill = NA) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_viridis_d (na.value=NA, na.translate = F,
                        breaks = seq(0, 5), labels = c(0, 1, 2, 3, 4, "5-8"))+
  theme_bw() +
  theme(legend.title = element_blank())



ggsave("all_sb_consensus_map.tiff",
       plot = all_sp_figure_50, 
       units = "in",
       width = 10, height = 5.5,
       bg = 'white')


