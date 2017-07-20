

#' Extract coefficients from a bandit object
#' @param object an object inheriting from class \code{"bandit"}
#' @param what either \code{"last"} (the default) for the coefficients of the model estimated
#' in the last training job, or \code{"all"} for the coefficients of all
#' training jobs.
#' @return a numeric vector if \code{what = "last"}; a matrix if \code{what = "all"}.

#' @include banditUcb.R
#' @include banditThompson.R
#' @export

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

#' @export
setMethod("coef", signature(object = "bandit"), coef.bandit)
#' @export
setMethod("coef", signature(object = "bandit_stan_glmer"), coef.merMod)

