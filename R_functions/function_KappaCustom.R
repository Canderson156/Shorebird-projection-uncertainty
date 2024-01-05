#allows kappa statistic to be calculated in functions from the caret and CAST packages using max_kappa instead of 0.5

kappa_custom <- function(data, lev = NULL, model = NULL) {
  
  dt <- data[,c("rowIndex", "obs", "yes")] %>%
    arrange(rowIndex) %>%
    mutate(obs = ifelse(obs == "yes", TRUE, FALSE),
           rowIndex = as.character(rowIndex))
  
  if(all(dt$obs == FALSE) == FALSE){
  
  ths <-  optimal.thresholds(dt, opt.methods	= "MaxKappa")
  
  
  cmx_test <- cmx(dt, ths$yes[1])
  
  
  
  k <- Kappa(cmx_test)
  
  c(Kappa_Custom = k[,1])
  
  }else{
    
    c(Kappa_Custom = NA)  
    
  }
  
}



