ggplotRegression_nolabel <- function(fit) {

    require(ggplot2)

    ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
      geom_point() + 
      stat_smooth(method = "lm", col = "red", se = FALSE)
}
