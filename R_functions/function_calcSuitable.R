

#calculate total suitable area (cell area * cell probability of occurrence)

calc_suitable <- function(x) {
  
  
  y <- x * cell_area
  
  area <- sum(values(y), na.rm = T)
  
  return(area)
  
}