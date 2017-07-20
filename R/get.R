
#'  Extract quantities from a bandit object
#'
#' @param object an object inheriting from class \code{"bandit"}
#' @param what the specific object to select. See details.
#'
#' @details
#' By default, \code{getSamples} extracts all samples
#' with a non-missing outcome. Alternatives are \code{"last"},
#' for the samples used in the last training job; \code{"remaining"} for all samples
#' with a missing outcome.
#'
#' By default, \code{getJobs} extracts a summary of all jobs done by the bandit. The
#' alternative \code{"last"} extracts a summary of the last job done by the bandit.
#'
#' By default, \code{getModel} extracts the model estimated at the last training job.
#' The id of a training job may be supplied.
#'
#' @return
#' For \code{getSamples} and \code{getJobs}, a \code{data.frame}. For \code{getModel},
#' a model object.


#' @include banditUcb.R
#' @include banditThompson.R

#' @export
#'
getSamples <- function(object, what = "current") {
  what <- match.arg(what, c("current", "last", "remaining"))
  rSamples(object$banditData, what)
}

#' @rdname getSamples
#' @export
getJobs <- function(object, what = "all") {
  what <- match.arg(what, c("last", "all"))
  rJobs(object$banditData, what)
}

#' @export
getModel.bandit <- function(object, what = "last") {
  if(length(what) > 1) stop("select only one model.")
  if(what == "last") {
    model <- object$currentModel
    data <- rSamples(object$banditData, what)
  } else {
    what <- as.numeric(what)
    if(is.na(what)) stop("use a correct job id.")
    jobs <- rJobs(object$banditData, "all")
    if(!what %in% jobs$job[jobs$type == "train"]) stop("the selected id does not match any training job.")
    model <- rTrain(object$banditData, what)
    data <- rSamples(object$banditData, paste0("job", what))
  }
  return(list(model = model,
              data = data))
}


getModel.bandit_ucb <- function(object, what = "last") {
  input <- callNextMethod()
  model <- input$model
  data <- input$data
  rm(input)
  xlev <- NULL
  mf <- model.frame(object$formula, data, xlev = xlev)
  model$data <- data
  model$model <- mf
  model$y <- model.response(mf)
  model$formula <- formula
  model$terms <- attr(x = mf, which = "terms")
  model$family <- object$family
  model$contrasts <- object$contrasts
  model$xlevels <- .getXlevels(model$terms, mf)
  model$weights <- NULL
  model$offset <- NULL
  model$method <- "glmnet"
  model$na.action <- attr(mf, "na.action")
  model$lambdaRidge <- model$parBandit$lambdaRidge
  model$lambdaLasso <- model$parBandit$lambdaLasso

  class(model) <- c("banditModel", "banditGlmnet")
  addCall(model, FUN = "banditGlmnet")
}

getModel.bandit_thompson <- function(object, what = "last") {
  input <- callNextMethod()
  model <- input$model
  data <- input$data
  rm(input)
  model$data <- data
  model$model <- model.frame(model)
  model$y <- model.response(model$model)
  return(model)
}

getModel.bandit_stan_lm <- function(object, what = "last") {
  m <- callNextMethod()
  addCall(m, FUN = "stan_lm")
}
getModel.bandit_stan_glm <- function(object, what = "last") {
  m <- callNextMethod()
  addCall(m, FUN = "stan_glm")
}
getModel.bandit_stan_glmer <- function(object, what = "last") {
  m <- callNextMethod()
  addCall(m, FUN = "stan_glmer")
}

#' @rdname getSamples
#' @export
setGeneric("getModel", function(object, what = "last") {})
#' @export
setMethod("getModel", signature(object = "bandit"), getModel.bandit)
#' @export
setMethod("getModel", signature(object = "bandit_ucb"), getModel.bandit_ucb)
#' @export
setMethod("getModel", signature(object = "bandit_thompson"), getModel.bandit_thompson)
#' @export
setMethod("getModel", signature(object = "bandit_stan_lm"), getModel.bandit_stan_lm)
#' @export
setMethod("getModel", signature(object = "bandit_stan_glm"), getModel.bandit_stan_glm)
#' @export
setMethod("getModel", signature(object = "bandit_stan_glmer"), getModel.bandit_stan_glmer)


