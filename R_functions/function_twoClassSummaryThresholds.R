




twoClassSummary_thresholds <- function (data, lev = NULL, model = NULL) {
  
  if (length(lev) > 2) {
    stop(paste("Your outcome has", length(lev), "levels. The twoClassSummary() function isn't appropriate."))
  }
  caret:::requireNamespaceQuietStop("pROC")
  if (!all(levels(data[, "pred"]) == lev)) {
    stop("levels of observed and predicted data do not match")
  }
  rocObject <- try(pROC::roc(data$obs, data[, lev[1]], direction = ">", 
                             quiet = TRUE), silent = TRUE)
  rocAUC <- if (inherits(rocObject, "try-error")) 
    NA
  else rocObject$auc
  out <- c(rocAUC, 
           caret::sensitivity(data[, "pred"], data[, "obs"]), 
           caret::specificity(data[, "pred"], data[, "obs"]),
           caret::sensitivity(data[, "pred"], data[, "obs"]) + caret::specificity(data[, "pred"], data[, "obs"]))
  names(out) <- c("ROC", "Sens", "Spec", "Sens+Spec")
  out
}