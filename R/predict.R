#' @include banditUcb.R
#' @include banditThompson.R

#' Predicted values based on a model in the bandit.
#' @export
#' @import glmnet
#'
#' @param object a bandit object
#' @param samplesId (optional) the ids of some samples
#' @param modelId (optional) the id of a training job.
#' @param stage a character in 'ridge', 'lasso' for the stage of the desired model.
#' @param type a character containing one or more of the following:
#' "link","response","class","uncertainty","score"
#' @param robust for type = uncertainty, should robust estimation be used?
#'
#' @return a vector of predicted values, if only one type is selected. Otherwise, a
#' data.frame of predicted values. If a data.frame is returned, row.names refer to the
#' ids of samples. The default returns the predicted values for all the samples for
#' which the outcome is missing, using the last model.


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

predict.bandit_thompson <- function(object,
                                    whatSamples = "remaining",
                                    whatModel = "last",
                                    type = c("response","weight"),
                                    re.form = NULL) {
  type <- match.arg(type,
                    c("link","response","weight"),
                    several.ok = TRUE)
  data <- callNextMethod()
  id <- data$samples$id
  data$samples <- get_all_vars(formula(data$model), data$samples)
  data$samples$y <- 0
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
setMethod("predict", signature(object = "bandit_ucb"), predict.bandit_ucb)
setMethod("predict", signature(object = "bandit_thompson"), predict.bandit_thompson)
