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
    args <- list(family = family,
                 lambdaRidge = currentParams$lambdaRidge,
                 lambdaLasso = currentParams$lambdaLasso,
                 parRidge = parRidge,
                 parLasso = parLasso)
    callSuper(FUN = protoBanditGlmnet, args = args, seed = seed)
  },
  tune = function(param = 'lambdaRidge',
                  value = 'auto',
                  lambdaAuto = 'lambda.1se',
                  parCvGlmnet = NULL,
                  seed = NULL) {
    args <- list(param = param,
                 value = value,
                 lambdaAuto = lambdaAuto,
                 family = family,
                 currentLasso = currentParams$lambdaLasso,
                 parCvGlmnet = parCvGlmnet)
    callSuper(FUN = tuneBandit, args = args, seed = seed)
  },
  addSamples = function(df) {
    validateAddSamples(formula, df, newLevels, xlevels)
    callSuper(df)
  })
