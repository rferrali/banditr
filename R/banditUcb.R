
#' An UCB bandit reference class (RC) object.

#' @usage
#' bandit_ucb(formula, data, family = c("gaussian", "binomial"),
#'            alpha = 1, contrasts = NULL, newLevels = FALSE,
#'            db = NULL, path = NULL)

#' @template argBandit
#' @param alpha the LinUCB tuning parameter. A positive scalar. Higher values of
#' \code{alpha} favor exploration over exploitation.
#' @template argBanditOpt

#' @details
#' The RC class \code{"bandit_ucb"} inherits from class \code{"\link{bandit}"}.
#'
#' The introductory vignette provides a detailed explanation of LinUCB algorithms, and
#' their implementation with \code{banditr}. See the Examples section.

#' @field alpha the linear UCB tuning parameter. See details.
#' @field family supported response type. See \code{\link{banditGlmnet}} for details.

#' @section Methods:

#' \code{train(parRidge = NULL, parLasso = NULL, seed = NULL)} trains the model using
#' \code{\link{banditGlmnet}} and all completed experiments.
#' \code{parRidge} and \code{parLasso} are optional lists of
#' parameters passed to \code{\link{banditGlmnet}} to control the fitting process.
#' \code{seed} is an optional seeding value for the random number generator.
#'
#' \code{tune(param = 'lambdaRidge', value = 'auto', lambdaAuto = 'lambda.1se',
#' parCvGlmnet = NULL, seed = NULL)} set the value of a tuning parameter of the bandit. \code{param} is either
#' \code{'lambdaRidge'} (the default) or \code{'lambdaLasso'}. Setting
#' \code{lambdaLasso} > 0 enables a first stage of variable selection.
#' \code{value} is either a scalar in [0,1] or 'auto'. If
#' set to \code{'auto'}, the parameter is picked using \code{\link[glmnet]{cv.glmnet}}.
#' \code{lambdaAuto} is either \code{'lambda.1se'} or \code{'lambda.min'}
#' depending on which outcome of \code{\link[glmnet]{cv.glmnet}} should be selected. It
#' is ignored if \code{value} is not \code{'auto'}. \code{parCvGlmnet} is a list of
#' parameters passed to \code{\link[glmnet]{cv.glmnet}}. This parameter is ignored if
#' \code{value} is not \code{'auto'}.
#'
#' \code{addSamples(df)} add samples to the bandit. \code{df} is coercible to
#' a data.frame, and can be appended to the \code{data.frame} used at creation.
#' In particular, it contains an \code{id} column that is a primary key.
#'
#' \code{addOutcomes(y)} add outcomes to the bandit. \code{y} is a named vector
#' whose names are samples ids.
#'
#' \code{undo()} cancel the last job.
#'
#' @examples vignette("introduction", "banditr")
#' @seealso \code{\link{bandit}}, \code{\link{bandit_thompson}}
#'
#' @export bandit_ucb
#' @exportClass bandit_ucb


bandit_ucb <- setRefClass("bandit_ucb",
                         contains = "bandit",
                         fields = list(alpha = "numeric",
                                       family = "character"))

bandit_ucb$methods(
  initialize = function(formula, data, family, alpha = 1, ...) {
    model.frame(formula, data)
    al <- as.numeric(alpha)
    if(length(al)>1) stop("alpha must be a scalar")
    initFields(alpha = al,
               family = family,
               statistics = c("response", "uncertainty", "score"),
               currentParams = list(lambdaRidge = 0,
                                    lambdaLasso = 0))
    callSuper(formula = formula, data = data, ...)
  },
  train = function(parRidge = NULL,
                   parLasso = NULL,
                   seed = NULL) {
    if(newLevels) {
      xlev <- NULL
    } else {
      xlev <- xlevels
    }
    args <- list(family = family,
                 lambdaRidge = currentParams$lambdaRidge,
                 lambdaLasso = currentParams$lambdaLasso,
                 parRidge = parRidge,
                 parLasso = parLasso,
                 xlevels = xlev)
    callSuper(FUN = protoBanditGlmnet, args = args, seed = seed)
  },
  tune = function(param = 'lambdaRidge',
                  value = 'auto',
                  lambdaAuto = 'lambda.1se',
                  parCvGlmnet = NULL,
                  seed = NULL) {
    if(newLevels) {
      xlev <- NULL
    } else {
      xlev <- xlevels
    }
    args <- list(param = param,
                 value = value,
                 lambdaAuto = lambdaAuto,
                 family = family,
                 currentLasso = currentParams$lambdaLasso,
                 parCvGlmnet = parCvGlmnet,
                 xlevels = xlev)
    callSuper(FUN = tuneBandit, args = args, seed = seed)
  },
  addSamples = function(df) {
    validateAddSamples(formula, df, newLevels, xlevels)
    callSuper(df)
  })
