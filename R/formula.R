#' @include banditUcb.R
#' @include banditThompson.R



#' @export

formula.bandit <- function(x, ...) x$formula

formula.bandit_ucb <- function(x, what = "last", reduced = FALSE) {
  reduced <- as.logical(reduced)
  if(length(reduced) != 1) stop("reduced must be TRUE or FALSE")
  if(!reduced) {
    return(callNextMethod())
  }
  model <- rTrain(x$banditData, what)
  if(reduced & is.null(model$lasso)) {
    warning("The model was not fit with variable selection. Returning the full formula instead.")
    return(callNextMethod())
  } else {
    tt <- rownames(coef(model$glmnet))
    intercept <- coef(model$glmnet)[1,1] != 0
    tt <- tt[-1]
    reformulate(tt, response = all.vars(x$formula)[1], intercept = intercept)
  }
}

#' @export
setMethod("formula", signature(x = "bandit"), formula.bandit)
#' @export
setMethod("formula", signature(x = "bandit_ucb"), formula.bandit_ucb)
