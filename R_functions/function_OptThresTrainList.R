
opt.thres.train.list <- function(x){
  
  output <- vector("list", length(x))
  
  
  for(i in 1:length(x)){
    
    
    dt <- x[[i]]$pred[,c(6,3,5)] %>%
      arrange(rowIndex) %>%
      mutate(obs = ifelse(obs == "yes", TRUE, FALSE),
             rowIndex = as.character(rowIndex))
    
    output[[i]] <- optimal.thresholds(dt)
    
    
    
  }
  
  names(output) <- names(x)
  return(output)
  
}  

