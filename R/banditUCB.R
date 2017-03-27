banditUcb <- setRefClass("UCB",
                         contains = "bandit",
                         fields = list(alpha = "numeric"))

banditUcb$methods(
  train = function(data,
                   seed = NULL,
                   parRidge = NULL,
                   parLasso = NULL) {
    "train the bandit using available samples and latest values of the tuning
    parameters."

    protoBanditGlmnet(formula = formula,
                      family = family,
                      lambdaRidge = currentParams$lambdaRidge,
                      lambdaLasso = currentParams$lambdaLasso,
                      data = data,
                      seed = seed,
                      parRidge = parRidge,
                      parLasso = parLasso,
                      contrasts = contrasts)
  },
  tune = function(data,
                  param = 'lambdaRidge',
                  value = 'auto',
                  lambdaAuto = 'lambda.1se',
                  parCvGlmnet = NULL,
                  seed = NULL) {
    "set the value of a tuning parameter of the bandit. \\code{param} is either
    \\code{'lambdaRidge'} (the default) or \\code{'lambdaLasso'}. Setting
    \\code{lambdaLasso} > 0 enables a first stage of variable selection.
    \\code{value} is either a scalar in [0,1] or 'auto'. If
    set to \\code{'auto'}, the parameter is picked using \\code{\\link[glmnet]{cv.glmnet}}.
    \\code{lambdaAuto} is either \\code{'lambda.1se'} or \\code{'lambda.min'}
    depending on which outcome of \\code{\\link[glmnet]{cv.glmnet}} should be selected. It
    is ignored if \\code{value} is not \\code{'auto'}. \\code{parCvGlmnet} is a list of
    parameters passed to \\code{\\link[glmnet]{cv.glmnet}}. This parameter is ignored if
    \\code{value} is not \\code{'auto'}."

    tuneBandit(param = param,
               value = value,
               lambdaAuto = lambdaAuto,
               formula = formula,
               family = family,
               currentLasso = currentParams$lambdaLasso,
               data = data,
               seed = seed,
               parCvGlmnet = parCvGlmnet,
               contrasts = contrasts)
  })
