
# creates an summary output for glms and glmms

# model call coefficients AIC




glm.summary <- function(x) {

    output <- list(summary(x)$call, round(coef(summary(x)), digits = 2), AIC(x))

    names(output) <- c("Model", "Coefficients", "AIC")

    return(output)
}

