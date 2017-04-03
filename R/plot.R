#' @include banditUcb.R
#' @include banditThompson.R

#' @export

plot.bandit <- function(x, what = "reward", ...) {
    switch(what,
           reward = plotReward(x, ...),
           uncertainty = plotUncertainty(x, ...),
           MSE = plotMSE(x, ...),
           tuning = plotTuning(x, ...),
           coef = plotCoef(x, ...))
}

setMethod("plot", signature(x = "bandit"), plot.bandit)
#' @export
setMethod("plot", signature(x = "bandit_ucb"), function(x, what = "reward", ...) {
  what <- match.arg(what, c("reward", "uncertainty", "MSE", "tuning", "coef"))
  callNextMethod()
})
#' @export
setMethod("plot", signature(x = "bandit_thompson"), function(x, what = "reward", ...) {
  what <- match.arg(what, c("reward", "coef"))
  callNextMethod()
})



plotReward <- function(object, data = FALSE, cumulative = TRUE, expectedReward = TRUE, maxReward, ...) {

  if(missing(maxReward) & object$family == "binomial") maxReward <- TRUE
  if(object$family != "binomial") maxReward <- FALSE
  maxReward <- as.logical(maxReward)
  expectedReward <- as.logical(expectedReward)

  pl <- rStatistics(object$banditData)
  pl$max <- 1
  pl <- aggregate(cbind(y, response, max) ~ jobTrain, pl, sum)
  colnames(pl) <- c("job", "reward", "maxReward", "expectedReward")
  pl <- pl[order(pl$job),]
  if(!maxReward) pl$maxReward <- NULL
  if(!expectedReward) pl$expectedReward <- NULL
  if(cumulative) {
    for (i in 2:ncol(pl)) pl[,i] <- cumsum(pl[,i])
  }

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

plotCoef <- function(object, data = FALSE, ...) {
  m <- coef.bandit(object, what = "all")
  if(data) return(m)
  m <- t(m)
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

# plotUncertainty <- function(object, data = FALSE, ...) {
#   pl <- getJob(object, 1:object$currentJob)
#   pl <- pl[pl$type == "addOutcomes",]
#   pl$param <- pl$value <- NULL
#   if(nrow(pl) == 0) return(NULL)
#   pl <- pl[order(pl$job),]
#   pl$uncertainty <- statistic(object, cumulative = FALSE, FUN = statUncertainty)
#   if(data) return(as.data.frame(pl))
#   else {
#     x <- 1:length(pl$job)
#     names(x) <- pl$job
#     plot(x, pl$uncertainty, type = "l", xaxt = "n", xlab = "Job", ylab = "Mean uncertainty in selected arms", ...)
#     axis(1, at = x, labels = names(x))
#   }
# }


# plotMSE <- function(object, data = FALSE, ...) {
#   pl <- getJob(object, 1:object$currentJob)
#   pl <- pl[pl$type == "addOutcomes",]
#   pl$param <- pl$value <- NULL
#   if(nrow(pl) == 0) return(NULL)
#   pl <- pl[order(pl$job),]
#   pl$MSE <- statistic(object, cumulative = FALSE, FUN = statMSE)
#   if(data) return(as.data.frame(pl))
#   else {
#     x <- 1:length(pl$job)
#     names(x) <- pl$job
#     plot(x, pl$uncertainty, type = "l", xaxt = "n", xlab = "Job", ylab = "MSE in selected arms", ...)
#     axis(1, at = x, labels = names(x))
#   }
# }
#
# plotTuning <- function(object, data = FALSE, param = "lambdaRidge", ...) {
#   param <- match.arg(param, c("lambdaRidge", "lambdaLasso"))
#   pl <- getJob(object, 1:object$currentJob)
#   pl <- pl[which(pl$param == param),]
#   if(nrow(pl) == 0) return(NULL)
#   pl <- pl[order(pl$job),]
#   models <- getTuning(object, pl$job, param)
#   pl$lambdaAuto <- sapply(models, function(m) {
#     if(is.null(m$model)) return("manual")
#     return(m$model$lambdaAuto)
#   })
#   if(data) return(as.data.frame(pl))
#   else {
#     x <- 1:length(pl$job)
#     names(x) <- pl$job
#     plot(x, pl$value, type = "l", xaxt = "n", col = "grey75", xlab = "Job", ylab = paste("Value of lambda", stage), ...)
#     points(x, pl$value, pch = 19, col = sapply(pl$lambdaAuto,
#                                                function(i) {
#                                                  switch(i,
#                                                         manual = "black",
#                                                         lambda.1se = "steelblue",
#                                                         lambda.min = "tomato")
#                                                }))
#     axis(1, at = x, labels = names(x))
#     legend("bottom", c("manual", "lambda.1se", "lambda.min"), col = c("black", "steelblue", "tomato"), horiz = T, pch = 19)
#   }
# }
#
