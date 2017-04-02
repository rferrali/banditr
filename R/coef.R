#' @include banditUcb.R
#' @include banditThompson.R


#' Export model coefficients.
#' @export
#'
#' @param object a bandit object
#' @param modelId (optional) a vector of ids of training jobs.
#' @param restricted a character in 'ridge', 'lasso' for the stage of the desired model.
#' @return Coefficients extracted from the model modelId. The default returns the ridge
#' coefficients of the last model.

coef.bandit <- function(object, what = "last") {
  what <- match.arg(what, c("last", "all"))
  if(what == "last") {
    return(coef(object$currentModel))
  }
  if (what == "all") {
    coef <- rCoef(object$banditData, what)
    j <- coef$jobTrain
    coef <- as.matrix(coef[,-1])
    rownames(coef) <- j
    return(coef)
  }
}

coef.merMod <- function(object, what = "last") {
  what <- match.arg(what, c("last", "all"))
  if(what == "last") {
    coef <- as.matrix(object$currentModel)
    v <- colnames(coef)
    select <- 1:(min(which(substr(v,1,2) == "b[" & substr(v, nchar(v), nchar(v)) == "]"))-1)
    coef <- coef[,select]
    coef <- apply(coef, 2, median)
    return(coef)
  } else {
    callNextMethod()
  }
}

setMethod("coef", signature(object = "bandit"), coef.bandit)
setMethod("coef", signature(object = "bandit_stan_glmer"), coef.merMod)

