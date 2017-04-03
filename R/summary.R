#' @include banditUcb.R
#' @include banditThompson.R

#' @export
summary.bandit <- function(object, ...) {
  out <- rSummary(object$banditData)
  stats <- rStatistics(object$banditData)
  out$coef <- coef(object)
  out$reward <- sum(stats$y)
  out$formula <- object$formula
  out$maxReward <- NULL
  out$family <- object$family
  if(object$family == "binomial") out$maxReward <- nrow(stats)
  class(out) <- "summary.bandit"
  return(out)
}

#' @export
setMethod("summary", signature(object = "bandit"), summary.bandit)

#' @export
print.summary.bandit <- function(x) {
  cat("Bandit started", as.character(x$start))
  cat("\nFormula: ")
  print(x$formula)
  cat("\nFamily: ")
  print(x$family)
  # cat("Lambda Ridge:", round(x$lambdaRidge, 4), ifelse(x$autoRidge, "(auto)", "(manual)"))
  # cat("\nLambda Lasso:", round(x$lambdaLasso, 4), ifelse(x$autoLasso, "(auto)", "(manual)"))
  cat("\n\nCoefficients: \n")
  print(round(x$coef, 4))
  cat("\nSize of training set:", x$N, "samples")
  cat("\nNumber of training jobs:", x$nTraining)
  cat("\nNumber of tuning jobs:", x$nTuning)
  if(is.null(x$lastAddOutcomes)) {
    cat("\nReward: 0")
    if(!is.null(x$maxReward)) cat("/0")
  } else {
    cat("\nReward as of ", as.character(x$lastAddOutcomes$date), " (job ", x$lastAddOutcomes$job, "): ",
        x$reward, sep="")
    if(!is.null(x$maxReward)) cat("/", x$maxReward, sep="")
  }
  cat("\n\nLast jobs:\n")
  print(as.data.frame(x$lastJobs), row.names = F)
}

