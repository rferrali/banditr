#' @include banditUcb.R
#' @include banditThompson.R



#' Extracting the model formula used by some model in the bandit.
#'
#' @export
#' @param x a bandit object
#' @param modelId (optional) the id of a training job.
#' @param reduced (optional) logical; if TRUE and some variable selection was used, the formula
#' of the reduced model is returned.
#' @return by default, the bandit's formula.
#' variable selection occurs (lambda.lasso > 0), the formula for the ridge stage only
#' contains the variables that were selected.
#'

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

setMethod("formula", signature(x = "bandit"), formula.bandit)
setMethod("formula", signature(x = "bandit_ucb"), formula.bandit_ucb)
