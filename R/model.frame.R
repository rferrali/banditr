#' @include banditUcb.R
#' @include banditThompson.R


#' @export

model.frame.bandit <- function(formula, what = "last", ...) {
  f <- formula(formula, what = what, reduced = FALSE)
  mf <- rSamples(formula$banditData, what)
  if(!is.null(formula$cap)) {
    if(nrow(mf) > formula$cap) {
      mf <- mf[order(-mf$jobOutcome),]
      mf <- mf[1:formula$cap,]
    }
  }
  mf$jobSamples <- mf$jobOutcome <- NULL
  if(formula$newLevels) {
    xlev <- NULL
  } else {
    xlev <- formula$xlevels
  }
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
    if(!is.null(formula$cap)) {
      if(nrow(mf) > formula$cap) {
        mf <- mf[order(-mf$jobOutcome),]
        mf <- mf[1:formula$cap,]
      }
    }
    if(formula$newLevels) {
      xlev <- NULL
    } else {
      xlev <- formula$xlevels
    }
    model.frame(f, mf, xlev = xlev)
  }
}

model.frame.bandit_merMod <- function(formula, what = "last") {
  model <- rTrain(formula$banditData, what)
  rstanarm::model.frame.stanreg(model)
}

setMethod("model.frame", signature(formula = "bandit"), model.frame.bandit)
#' @export
setMethod("model.frame", signature(formula = "bandit_ucb"), model.frame.bandit_ucb)
#' @export
setMethod("model.frame", signature(formula = "bandit_stan_glmer"), model.frame.bandit_merMod)
