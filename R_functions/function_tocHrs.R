toc_hrs <- function(){
  toc <- toc()
  x <- str_split(toc$callback_msg, " ")
  y <- as.numeric(x[[1]][[1]])/3600
  y <- round(y, digits = 2)
  output <- paste(y, "hrs elapsed")
  return(output)
}
