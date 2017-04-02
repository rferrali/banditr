#' @include banditUcb.R
#' @include banditThompson.R


#' Extracting the model frame used by some model in the bandit.
#'
#' @export
#'
#' @param object a bandit object
#' @param modelId (optional) the id of a training job.
#' @param reduced (optional) logical; if TRUE and some variable selection was used, the model.frame
#' of the reduced model is returned.
#' @param  data  an optional data.frame, list or environment
#' (or object coercible by as.data.frame to a data.frame),
#' containing the variables in formula. Neither a matrix nor an array will be accepted.
#' @return by default, the model.frame used in the last model.
#'

model.frame.bandit <- function(formula, what = "last", ...) {
  f <- formula(formula, what = what, reduced = FALSE)
  mf <- rSamples(formula$banditData, what)
  mf$jobSamples <- mf$jobOutcome <- NULL
  xlev <- ifelse(formula$newLevels, NULL, formula$xlevels)
  model.frame(f, mf, xlev = xlev)
}

model.frame.bandit_ucb <- function(formula, what = "last", reduced = FALSE) {
  reduced <- as.logical(reduced)
  model <- rTrain(formula$banditData, what)
  if(reduced && is.null(model$lasso)) {
    reduced <- FALSE
    warning("The model was not fit with variable selection. Returning the full model.frame instead")
  }
  if(!reduced) {
    return(callNextMethod())
  } else {
    f <- formula(formula, what, reduced)
    mf <- rSamples(formula$banditData, what)
    xlev <- ifelse(formula$newLevels, NULL, formula$xlevels)
    model.frame(f, mf, xlev = xlev)
  }
}

model.frame.bandit_merMod <- function(formula, what = "last") {
  model <- rTrain(formula$banditData, what)
  rstanarm::model.frame.stanreg(model)
}

setMethod("model.frame", signature(formula = "bandit"), model.frame.bandit)
setMethod("model.frame", signature(formula = "bandit_ucb"), model.frame.bandit_ucb)
setMethod("model.frame", signature(formula = "bandit_stan_glmer"), model.frame.bandit_merMod)
