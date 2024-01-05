

area_function_rf <- function(splits, scenario, model_seed, predictors, threshold) {
  
  # prepare data
  z <- analysis(splits)
  z <- z %>%
    select(all_of(c("presence", "Region_Code", predictors)))
  
  
  #create spatial folds
  indices <- CreateSpacetimeFolds(z, spacevar = "Region_Code", k = 12)
  
  
  z <- z %>%
    select(-Region_Code)
  
  
  
  #create the model
  set.seed(model_seed)
  
  model <- train(presence ~ .,
                 trControl = trainControl(method = 'cv', number = 12, classProbs = TRUE,
                                          savePredictions = TRUE,  index = indices$index, indexOut = indices$indexOut), 
                method = 'ranger', 
                metric = 'Kappa',  
                data = z
  )
  
  
  
  
  #calculate threshold

  
  dt <- model$pred[,c("rowIndex", "obs", "yes")] %>%
    arrange(rowIndex) %>%
    mutate(obs = ifelse(obs == "yes", TRUE, FALSE),
           rowIndex = as.character(rowIndex))
  
  
  ths <-  optimal.thresholds(dt, opt.methods	= threshold)
  
  
  
  #create projection
  
  s <- scenarios[scenario]
  
  pc <- predict(s[[1]], model, type = "prob", na.rm = TRUE)  
  

  #apply threshold
  
  pt <- pc$yes > ths$yes
  


  
  
  #calculate suitable_area
  
  suitable_area <- round(calc_suitable(pt))
  
  return(suitable_area)
  
}





