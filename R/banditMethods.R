#### S3 methods ####


setGeneric("getJob", function(object, jobId = NULL) {})

setGeneric("getModel", function(object, modelId = NULL) {})

setGeneric("getParam", function(object, modelId = NULL, param = "lambdaRidge") {})

setGeneric("getSamples", function(object, samples = NULL) {
  if(!is.null(samples)) {
    object <- object[match(samples, object$id),]
    if(!identical(as.numeric(samples), as.numeric(object$id))) {
      stop("Some samples cannot be found in the bandit.")
    }
  }
  return(object)
})

setGeneric("getTuning", function(object, modelId = NULL, param = "lambdaRidge") {})

setGeneric("getPreviousTraining", function(object, jobId) {})

setGeneric("statistic", function(object, jobMin, jobMax, FUN, cumulative = TRUE) {})

setGeneric("statReward", function(object, jobs) {})

setGeneric("statMaxReward", function(object, jobs) {})

setGeneric("statExpectedReward", function(object, jobs) {})

setGeneric("statUncertainty", function(object, jobs) {})

setGeneric("statMSE", function(object, jobs) {})


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

model.frame.bandit <- function(formula, modelId = NULL, reduced = FALSE, data = NULL) {
  object <- formula
  rm(formula)
  model <- getModel(object, modelId)
  reduced <- as.logical(reduced)
  if(length(reduced) != 1) stop("reduced must be TRUE or FALSE")
  if(reduced & is.null(model$lasso)) {
    reduced <- FALSE
    warning("The model was not fit with variable selection. Returning the full model.frame instead")
  }
  formula <- formula(object, modelId, reduced = reduced)
  if (!is.null(data)) {
    data <- as.data.frame(data)
    stats::model.frame(formula, data)
  } else {
    mf <- getSamples(object, samples = model$data)
    model.frame(formula, mf, xlev = object$xlevels)
  }
}

setMethod("model.frame", signature(formula = "bandit"), model.frame.bandit)

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

formula.bandit <- function(x, modelId = NULL, reduced = FALSE) {
  model <- getModel(x, modelId)
  reduced <- as.logical(reduced)
  if(length(reduced) != 1) stop("reduced must be TRUE or FALSE")
  if(!reduced) {
    x$formula
  } else if(reduced & is.null(model$lasso)) {
    warning("The model was not fit with variable selection. Returning the full formula instead.")
    x$formula
  } else {
    tt <- rownames(coef(model$glmnet))
    intercept <- coef(model$glmnet)[1,1] != 0
    tt <- tt[-1]
    reformulate(tt, response = all.vars(object$formula)[1], intercept = intercept)
  }
}

setMethod("formula", signature(x = "bandit"), formula.bandit)

#' Extracts a design (or model) matrix, e.g., by expanding factors to a set
#' of dummary variables (depending on the contrasts) and expanding interactions
#' similarly.
#'
#' @export
#'
#' @param object a bandit object
#' @param modelId the id of a training job.
#' @param stage a character in 'ridge', 'lasso' for the stage of the desired model.
#' @param  data  an optional data.frame, list or environment
#' (or object coercible by as.data.frame to a data.frame),
#' containing the variables in formula. Neither a matrix nor an array will be accepted.
#' @return by default, the design matrix used in the last model, for the ridge stage.
#'

#' @export

model.matrix.bandit <- function(object, modelId = NULL, reduced = FALSE, data = NULL) {
  # computation
  mf <- model.frame(object, modelId, reduced, data = data)
  formula <- formula(object, modelId, reduced)
  x <- model.matrix.default(formula, data = mf, contrasts.arg = object$contrasts)
  return(x)
}

setMethod("model.matrix", signature(object = "bandit"), model.matrix.bandit)

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
                           samplesId = NULL,
                           modelId = NULL,
                           type = c("response","uncertainty","score"),
                           robust = TRUE) {
  model <- getModel(object, modelId)
  samples <- getSamples(object, samplesId)
  type <- match.arg(type,
                    c("link","response","class","uncertainty","score"),
                    several.ok = TRUE)
  # prediction
  reduced <- !is.null(model$lasso)
  tt <- terms(formula(object, modelId, reduced = reduced))
  tt <- delete.response(tt)
  x <- model.frame.default(tt, samples, xlev = object$xlevels)
  if(nrow(x) > 0) {
    x <- model.matrix.default(tt, x, object$contrasts)
    intercept <- as.logical(attr(x = tt, "intercept"))
    xGlmnet <- x
    if(intercept) xGlmnet <- dropIntercept(xGlmnet)
    if(any(c("response","score") %in% type)) {
      outResponse <- predict(model$glmnet, newx = xGlmnet, type = "response")[,1]
    }
    if(any(c("uncertainty","score") %in% type)) {
      xU <- model.matrix(object, model = modelId, reduced = reduced)
      zU <- x
      outUncertainty <- uncertainty(xU, zU, model$lambdaRidge, robust)
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
  if(length(out) == 1) {
    out <- unlist(out)
    names(out) <- samples$id
  } else {
    out <- as.data.frame(out)
    rownames(out) <- samples$id
  }
  return(out)
}


setMethod("predict", signature(object = "bandit"), predict.bandit)

#' Export model coefficients.
#' @export
#'
#' @param object a bandit object
#' @param modelId (optional) a vector of ids of training jobs.
#' @param restricted a character in 'ridge', 'lasso' for the stage of the desired model.
#' @return Coefficients extracted from the model modelId. The default returns the ridge
#' coefficients of the last model.

coef.bandit <- function(object, modelId = NULL, as.matrix = FALSE) {
  model <- getModel(object, modelId)
  if("protoBanditModel" %in% class(model)) model <- list(model)
  coefs <- lapply(model, function(m) {
    m <- glmnet::coef.glmnet(m$glmnet)
    v <- m[,1]
    names(v) <- rownames(m)

    sto <- v[match(object$variables, names(v))]
    sto[is.na(sto)] <- 0
    names(sto) <- object$variables
    return(sto)
  })
  if(length(coefs) == 1) {
    coefs <- coefs[[1]]
    if(as.matrix) {
      coefs <- matrix(coefs, ncol = 1)
      rownames(coefs) <- object$variables
      colnames(coefs) <- modelId
    }
    return(coefs)
  }
  coefs <- do.call(cbind, coefs)
  colnames(coefs) <- modelId
  return(coefs)
}

setMethod("coef", signature(object = "bandit"), coef.bandit)


#' @export

summary.bandit <- function(object) {}
setMethod("summary", signature(object = "bandit"), summary.bandit)


#' @export
print.summary.bandit <- function(x) {
  cat("Bandit started", as.character(x$start))
  cat("\nFormula: ")
  print(x$formula)
  cat("Lambda Ridge:", round(x$lambdaRidge, 4), ifelse(x$autoRidge, "(auto)", "(manual)"))
  cat("\nLambda Lasso:", round(x$lambdaLasso, 4), ifelse(x$autoLasso, "(auto)", "(manual)"))
  cat("\n\nCoefficients: \n")
  print(round(x$coef, 4))
  cat("\nSize of training set:", x$N, "samples")
  cat("\nNumber of training jobs:", x$nTraining)
  cat("\nNumber of tuning jobs:", x$nTuning)
  if(is.null(x$lastAddOutcomes)) {
    cat("\nReward: 0/0")
  } else {
    cat("\nReward as of ", as.character(x$lastAddOutcomes$date), " (job ", x$lastAddOutcomes$job, "): ",
        x$reward, sep="")
    if(!is.null(x$maxReward)) cat("/", x$maxReward, sep="")
  }
  cat("\n\nLast jobs:\n")
  print(as.data.frame(x$lastJobs), row.names = F)
}



#' @export

plot.bandit <- function(object, what = "reward", jobId = NULL, ...) {
  if(!is.null(jobId)) {
    jobId <- as.numeric(jobId)
    if(length(jobId) != 1) stop("Select only one job.")
    this <- getJob(object, jobId)
    if(this$type == "train") {
      model <- getModel(object, this$job)
      what <- match.arg(what, c("lambdaRidge", "lambdaLasso"))
      if(is.null(model$lasso) & what == "lambdaLasso") return(NULL)
      switch(what,
             lambdaRidge = glmnet::plot.glmnet(model$glmnet, ...),
             lambdaLasso = glmnet::plot.glmnet(model$lasso, ...))
    } else if(this$type == "tune") {
      tune <- getTuning(object, this$job)
      if(is.null(tune$model)) return(NULL)
      glmnet::plot.cv.glmnet(tune$model$cv.glmnet,
                             main = paste0("Tuning (", this$param, ") - ",
                                           tune$model$lambdaAuto, " = ", round(tune$value, 4)),
                             sub = paste0("Job #", this$job, ", ", this$date)
                             , ...)
    }
  } else {
    what <- match.arg(what, c("reward", "uncertainty", "MSE", "tuning", "coef"))
    switch(what,
           reward = plotReward(object, ...),
           uncertainty = plotUncertainty(object, ...),
           MSE = plotMSE(object, ...),
           tuning = plotTuning(object, ...),
           coef = plotCoef(object, ...))
  }
}

# setMethod("plot", signature(object = "bandit"), plot.bandit)

plotReward <- function(object, data = FALSE, cumulative = TRUE, expectedReward = TRUE, maxReward, ...) {
  pl <- getJob(object, 1:object$currentJob)
  pl <- pl[pl$type == "addOutcomes",]
  pl$param <- pl$value <- NULL
  if(nrow(pl) == 0) return(NULL)
  pl <- pl[order(pl$job),]
  pl$reward <- statistic(object, FUN = statReward, cumulative = cumulative)
  if(missing(maxReward) & object$family == "binomial") maxReward <- TRUE
  if(object$family != "binomial") maxReward <- FALSE
  maxReward <- as.logical(maxReward)
  if(length(maxReward) != 1) stop("maxReward must be TRUE or FALSE")
  if(maxReward) pl$maxReward <- statistic(object, FUN = statMaxReward, cumulative = cumulative)

  expectedReward <- as.logical(expectedReward)
  if(length(expectedReward) != 1) stop("expectedReward must be TRUE or FALSE")
  pl$expectedReward <- statistic(object, FUN = statExpectedReward, cumulative = cumulative)
  if(data) return(as.data.frame(pl))
  x <- 1:length(pl$job)
  names(x) <- pl$job
  ylab <- ifelse(cumulative, "Cumulative reward", "Reward")
  legendLab <- "reward"
  legendLty <- "solid"
  if(maxReward) {
    plot(x, pl$maxReward, xaxt = "n", xlab = "Job", ylab = ylab, type = "l", lty = "dashed", ...)
    lines(x, pl$reward)
    legendLab <- c(legendLab, "max. reward")
    legendLty <- c(legendLty, "dashed")
  } else {
    plot(x, pl$reward, xaxt = "n", xlab = "Job", ylab = ylab, ...)
  }
  if(expectedReward) {
    lines(x, pl$expectedReward, lty = "dotted")
    legendLab <- c(legendLab, "expected reward")
    legendLty <- c(legendLty, "dotted")
  }
  axis(1, at = x, labels = names(x))
  legend("topleft", legend = legendLab, lty = legendLty)
}

plotUncertainty <- function(object, data = FALSE, ...) {
  pl <- getJob(object, 1:object$currentJob)
  pl <- pl[pl$type == "addOutcomes",]
  pl$param <- pl$value <- NULL
  if(nrow(pl) == 0) return(NULL)
  pl <- pl[order(pl$job),]
  pl$uncertainty <- statistic(object, cumulative = FALSE, FUN = statUncertainty)
  if(data) return(as.data.frame(pl))
  else {
    x <- 1:length(pl$job)
    names(x) <- pl$job
    plot(x, pl$uncertainty, type = "l", xaxt = "n", xlab = "Job", ylab = "Mean uncertainty in selected arms", ...)
    axis(1, at = x, labels = names(x))
  }

}

plotMSE <- function(object, data = FALSE, ...) {
  pl <- getJob(object, 1:object$currentJob)
  pl <- pl[pl$type == "addOutcomes",]
  pl$param <- pl$value <- NULL
  if(nrow(pl) == 0) return(NULL)
  pl <- pl[order(pl$job),]
  pl$MSE <- statistic(object, cumulative = FALSE, FUN = statMSE)
  if(data) return(as.data.frame(pl))
  else {
    x <- 1:length(pl$job)
    names(x) <- pl$job
    plot(x, pl$uncertainty, type = "l", xaxt = "n", xlab = "Job", ylab = "MSE in selected arms", ...)
    axis(1, at = x, labels = names(x))
  }
}

plotTuning <- function(object, data = FALSE, param = "lambdaRidge", ...) {
  param <- match.arg(param, c("lambdaRidge", "lambdaLasso"))
  pl <- getJob(object, 1:object$currentJob)
  pl <- pl[which(pl$param == param),]
  if(nrow(pl) == 0) return(NULL)
  pl <- pl[order(pl$job),]
  models <- getTuning(object, pl$job, param)
  pl$lambdaAuto <- sapply(models, function(m) {
    if(is.null(m$model)) return("manual")
    return(m$model$lambdaAuto)
  })
  if(data) return(as.data.frame(pl))
  else {
    x <- 1:length(pl$job)
    names(x) <- pl$job
    plot(x, pl$value, type = "l", xaxt = "n", col = "grey75", xlab = "Job", ylab = paste("Value of lambda", stage), ...)
    points(x, pl$value, pch = 19, col = sapply(pl$lambdaAuto,
                                               function(i) {
                                                 switch(i,
                                                        manual = "black",
                                                        lambda.1se = "steelblue",
                                                        lambda.min = "tomato")
                                               }))
    axis(1, at = x, labels = names(x))
    legend("bottom", c("manual", "lambda.1se", "lambda.min"), col = c("black", "steelblue", "tomato"), horiz = T, pch = 19)
  }
}

plotCoef <- function(object, data = FALSE, ...) {
  m <- getJob(object, jobId = 1:object$currentJob)
  m <- coef.bandit(object, m$job[m$type=="train"], as.matrix = TRUE)
  if(data) return(m)

  ylim <- range(m) * 1.05
  m <- m[order(m[,ncol(m)]),]
  palette <- rainbow(nrow(m))
  plot(x = 1:ncol(m), y = m[1,], type = "l", col = palette[1],
       ylim = ylim, xaxt = "n", xlab = "Job", ylab = "Coefficient value")
  text(ncol(m), m[,ncol(m)], labels = rownames(m), offset = 1, pos = 2, col = palette, ...)
  if(nrow(m) > 1) {
    for(i in 2:nrow(m)) {
      lines(x = 1:ncol(m), y = m[i,], col = palette[i])
    }
  }
  axis(1, at = 1:ncol(m), labels = colnames(m))
}


