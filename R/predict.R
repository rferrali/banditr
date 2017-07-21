

#' Predict Method for bandit objects
#'
#' Obtains predictions from a bandit object.
#'
#' @usage
#' predict.bandit_ucb(object,
#'                    whatSamples = "remaining",
#'                    whatModel = "last",
#'                    type = c("response", "uncertainty", "score"),
#'                    robust = TRUE)
#' predict.bandit_thompson(object,
#'                         whatSamples = "remaining",
#'                         whatModel = "last",
#'                         type = c("response","weight"),
#'                         re.form = NULL)
#' @param object an object inheriting from class \code{"bandit"}
#' @param whatSamples the samples with which to predict. The default is all samples
#' with a missing outcome. .
#' Alternatives are \code{"current"}, for all samples with a non-missing outcome; \code{"last"},
#' for the samples used in the last training job; \code{"job\%i"}, for samples added
#' in job \code{"\%i"}; or a numeric vector of samples ids.
#' @param whatModel the fitted model with which to predict. The default uses the last
#' fitted model; alternatively, the job id of a training job can be supplied.
#' @param type the type of prediction required. See Details.
#' @param robust logical switch indicating whether generalized Moore-Penrose inverse
#' should be used if regular inversion fails when computing uncertainty.
#' @param re.form same as \code{re.form} in \link[rstanarm]{posterior_predict.stanreg}.
#'
#' @details
#' The \code{predict} method predictions using some model in
#' the bandit. It is used to select the next experimental arm.
#' For all methods, the \code{type} parameter may take values \code{"link"} and
#' \code{"response"}, for predictions on the scale of the linear predictors, and
#' on the scale of the response variable respectively. Thus, for a default binomial
#' model, \code{type = "response"} returns log-odds, while \code{type = "link"} returns
#' predicted probabilities.
#'
#' Objects of class \code{"bandit_ucb"} also support types
#' \code{"uncertainty"}, and \code{"score"}. Uncertainty is used to compute the score,
#' with score = response + alpha uncertainty.
#'
#' Objects of class \code{"bandit_thompson"} also support type \code{"weight"}, the
#' Thompson sampling weights.
#'
#' @return
#' If the \code{type} parameter has length 1, a vector of predictions.
#' Otherwise, a data frame of predictions.
#'
#' @include banditUcb.R
#' @include banditThompson.R
#' @import glmnet
#' @export
predict.bandit <- function(object,
                           whatSamples = "remaining",
                           whatModel = "last",
                           ...) {
  model <- rTrain(object$banditData, whatModel)
  samples <- rSamples(object$banditData, whatSamples)
  samples$jobSamples <- samples$jobOutcome <- NULL
  return(list(model = model,
              samples = samples))
}


predict.bandit_ucb <- function(object,
                               whatSamples = "remaining",
                               whatModel = "last",
                               type = c("response","uncertainty","score"),
                               robust = TRUE) {
  type <- match.arg(type,
                    c("link","response","class","uncertainty","score"),
                    several.ok = TRUE)
  data <- callNextMethod()
  # prediction
  reduced <- !is.null(data$model$lasso)
  tt <- terms(formula(object, whatModel, reduced = reduced))
  tt <- delete.response(tt)
  xlev <- ifelse(object$newLevels, NULL, object$xlevels)
  x <- model.frame.default(tt, data$samples, xlev = xlev)
  if(nrow(x) > 0) {
    x <- model.matrix.default(tt, x, object$contrasts)
    intercept <- as.logical(attr(x = tt, "intercept"))
    xGlmnet <- x
    if(intercept) xGlmnet <- dropIntercept(xGlmnet)
    if(any(c("response","score") %in% type)) {
      outResponse <- predict(data$model$glmnet, newx = xGlmnet, type = "response")[,1]
    }
    if(any(c("uncertainty","score") %in% type)) {
      xU <- model.frame(object, whatModel, reduced = reduced)
      xU <- model.matrix.default(tt, xU, object$contrasts)
      zU <- x
      tune <- rTune(object$banditData, whatModel, c("lambdaRidge", "lambdaLasso"))$lambdaRidge
      outUncertainty <- uncertainty(xU, zU, tune, robust)
      if("score" %in% type) {
        outScore <- outResponse + object$alpha * outUncertainty
      }
    }
  } else {
    outResponse <- outUncertainty <- outScore <- c()
  }

  # output
  out <- list()
  for (this in type) {
    if(this == "response") {
      out[[this]] <- outResponse
    } else if(this == "uncertainty") {
      out[[this]] <- outUncertainty
    } else if(this == "score") {
      out[[this]] <- outScore
    } else {
      if(nrow(x) > 0) {
        out[[this]] <- predict(model$glmnet, newx = xGlmnet, type = this)
      } else {
        out[[this]] <- c()
      }
    }
  }
  makePredict(out, names = data$samples$id)
}

#' @export

predict.bandit_thompson <- function(object,
                                    whatSamples = "remaining",
                                    whatModel = "last",
                                    type = c("response","weight"),
                                    re.form = NULL) {
  type <- match.arg(type,
                    c("link","response","weight"),
                    several.ok = TRUE)
  data <- callNextMethod()
  mfFit <- model.frame.bandit(object, whatModel)
  id <- data$samples$id
  data$samples <- get_all_vars(formula(data$model), data$samples)
  data$samples$y <- 0
  data$model$model <- mfFit
  out <- list()
  if("link" %in% type) {
    out$link <- rstanarm::posterior_linpred(data$model,
                                            transform = FALSE,
                                            newdata = data$samples,
                                            re.form = re.form)
    out$link <- colMeans(out$link)
  }
  if(any(c("response", "weight") %in% type)) {
    response <- rstanarm::posterior_linpred(data$model,
                                            transform = TRUE,
                                            newdata = data$samples,
                                            re.form = re.form)
    if("response" %in% type) {
      out$response <- colMeans(response)
    }
    if("weight" %in% type) {
      w <- apply(response, 1, which.max)
      w <- table(w)
      w <- w[match(1:ncol(response), names(w))]
      w[is.na(w)] <- 0
      w <- w/sum(w)
      w <- as.numeric(w)
      out$weight <- w
    }
  }
  makePredict(out, names = id)
}

setMethod("predict", signature(object = "bandit"), predict.bandit)
#' @export
setMethod("predict", signature(object = "bandit_ucb"), predict.bandit_ucb)
#' @export
setMethod("predict", signature(object = "bandit_thompson"), predict.bandit_thompson)
