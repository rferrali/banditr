#' A bandit reference class (RC) object.
#'
#'
#' @field samples a tibble containing samples.
#' @field models a tibble containing trained models.
#' @field jobs a tibble containing completed jobs.
#' @field \code{alpha} the linear UCB tuning parameter.
#' @field \code{formula} an object of class "formula" (or one that can be coerced
#' to that class): a symbolic description of the model that is fitted.
#' @field \code{family} supported response type
#' @field \code{contrasts} an optional list. See the \code{contrasts.arg} of \code{\link{model.matrix.default}}.
#' @field \code{xlevels} (where relevant) a record of the levels of the factors used in fitting.
#' @field \code{currentJob} a numeric with the id of the current job.
#' @field \code{currentModel} the prototype of the latest fit.
#' @field \code{currentParams} a list of tuning parameters.
#' @field \code{variables} a character vectors of variables extracted from the bandit formula.




bandit <- setRefClass("bandit",
                      fields = list(alpha = "numeric",
                                    formula = "formula",
                                    family = "character",
                                    contrasts = "ANY",
                                    xlevels = "list",
                                    currentJob = "numeric",
                                    currentModel = "ANY",
                                    currentParams = "list",
                                    variables = "character"))

bandit$methods(
  initialize = function(formula, data, alpha = 1, contrasts = NULL, ...) {
    "initialization function"

    if(length(alpha)>1) stop("alpha must be a scalar")
    data <- as.data.frame(data)
    if(any(c("jobSamples","jobOutcome") %in% colnames(data))) stop("data uses some reserved column names (jobSamples, jobOutcome)")
    if(!"id" %in% colnames(data)) stop("data must have an id column")
    mf <- model.frame(formula, data, na.action = "na.fail")
    initFields(formula = formula,
               alpha = alpha,
               xlevels = .getXlevels(terms(formula), mf),
               contrasts = contrasts,
               currentJob = 1,
               currentModel = NULL,
               currentParams = list(lambdaRidge = 0,
                                    lambdaLasso = 0),
               variables = colnames(model.matrix.default(formula,
                                                         data = mf,
                                                         contrasts.arg = contrasts,
                                                         xlev = .getXlevels(terms(formula), mf))),
               ...)
    return(list(samples = data.frame(cbind(jobSamples = 1,
                                           jobOutcome = as.numeric(NA),
                                           data)),
                jobs = data.frame(job = 1,
                                  date = Sys.time(),
                                  type = "initialize",
                                  param = NA,
                                  value = NA)))
  },
  addSamples = function(df) {
    "add samples to the bandit. \\code{df} is coercible to a data.frame, and can be appended
    to the \\code{data.frame} used at creation. In particular, it contains an \\code{id} column
    that is a primary key."
    df <- as.data.frame(df)
    df$jobSamples <- currentJob+1
    df$jobOutcome <- NA

    tt <- terms(formula)
    tt <- delete.response(tt)
    model.frame(tt, df, na.action = "na.fail", xlev = xlevels)
    return(df)
  },
  addOutcomes = function(y) {
    if(is.null(names(y))) stop("y must be a named vector")
    if(any(is.na(y))) warning("some outcomes you are adding have NA's")
    ids <- as.numeric(names(y))
    if(any(duplicated(ids))) stop("some outcomes are duplicated.")
    na <- names(y)
    y <- as.numeric(y)
    names(y) <- na
    return(y)
  },
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
  }
)












# add <- df[21:23,]
# y <- add$y
# names(y) <- add$id
# add$y <- NA
#
#
# bdt <- banditDf(formula = f, family = "binomial", data = df[1:20,])
# bdt$addSamples(add)
# bdt$addOutcomes(y)
# bdt$train()
# bdt$tune()
# bdt$tune()
# bdt$train()
# bdt$undo()
# bdt$undo()
# bdt$undo()
# bdt$undo()
# bdt$undo()
# bdt$undo()
#
# bdt2 <- banditDb(db = list(connection = 'driver={SQL Server};server=DESKTOP-99SSI5O\\SQLEXPRESS;database=tafra'),
#                  formula = f, family = "binomial", data = df[1:20,], path = "../test/")
# bdt2$addSamples(add)
# bdt2$addOutcomes(y)
# bdt2$train()
# bdt2$tune()
# bdt2$tune()
# bdt2$train()
# bdt2$addSamples(df[201:1000,])
# bdt2$undo()
# bdt2$undo()
# bdt2$undo()
# bdt2$undo()
# bdt2$undo()
# bdt2$undo()

# bdt2$addOutcomes(y)
# conn <- odbcDriverConnect('driver={SQL Server};server=DESKTOP-99SSI5O\\SQLEXPRESS;database=tafra')
# sqlQuery(conn, "SELECT * FROM parties")



