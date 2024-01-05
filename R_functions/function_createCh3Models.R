



create_ch3_models <- function(x){
  
  
  #defining all possible combinations of model structures
  pool <- c("climate-only", "climate+additional")
  Var_selection <- c("none", "conventional", "spatial")
  model_type <- c("glm", "rf")
  
  
  combos_list <- list(pool, Var_selection, model_type)
  combos <- do.call(expand.grid, combos_list)
  
  output <- vector("list", nrow(combos))
  
  #create space time folds to use for all spatially selected models
  indices <- CreateSpacetimeFolds(x, spacevar = "Region_Code")
  
  
  #create a model for each combination
  
  for(i in 1:nrow(combos)){
    
    current_combo <- combos[i,]
    
    #define which predictiors the model will use
    if(current_combo$Var1 == "climate-only"){
      
      pred <- x %>%
        select(any_of(climate_predictors))
      
      pred2 <- pred %>%
        mutate(across(everything(), ~ .x^2))
      
      names(pred2) <- paste(names(pred2), "2", sep = "")
      
      pred <- cbind(pred, pred2)
      
      
    } else{
      
      pred <- x %>%
        select(any_of(all_predictors))
      
      
      pred2 <- pred %>%
        select(-lc_class, -substrate) %>%
        mutate(across(everything(), ~ .x^2))
      
      names(pred2) <- paste(names(pred2), "2", sep = "")
      
      pred <- cbind(pred, pred2)
      
    }
    
    
    #define which variable selection method the model will use
    
    if(current_combo$Var2 == "none"){
      
      mV = "minVar = ncol(pred), "
      ix = "), "
      
    } else if(current_combo$Var2 == "conventional"){
      
      mV = "minVar = 2, "
      ix = "), "
      
    } else {
      
      mV = "minVar = 2, "
      ix = ", index = indices$index), "
    }
    
    #define which model type is being created
    
    if(current_combo$Var3 == "glm"){
      
      me = "method = 'glm', "
      fa = "family = 'binomial', "
      tu = ""
      im = ""
      
    } else{
      
      me = "method = 'rf', "
      tu = "tuneGrid = data.frame('mtry' = 2), "
      im = "importance=TRUE, "
      fa = ""
    }
    
    #create the formula for the current model
    model_formula <- paste("ffs2(predictors = pred, 
                    response = x$presence, 
                    trControl = trainControl(method = 'cv', number = 12, summaryFunction = twoClassSummary, classProbs = TRUE", 
                           ix,
                           me,
                           fa,
                           tu,
                           im,
                           mV,
                           "metric = 'ROC',
                           verbose = FALSE)",
                           sep = "")
    
    #run the current model
    
    output[[i]] <- eval(parse(text = model_formula))
    
    
    
  }
  
  #output is a list of 6 models
  
  
  #add names to the models
  
  names_output <- combos %>%
    unite("names_output", Var1:Var3)
  
  names(output) <- names_output$names_output
  
  return(output)
  
  
}