#' @import methods

#' A bandit reference class (RC) object.

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
                      fields = list(formula = "formula",
                                    contrasts = "ANY",
                                    newLevels = "logical",
                                    xlevels = "list",
                                    currentJob = "numeric",
                                    currentModel = "ANY",
                                    currentParams = "list",
                                    banditData = "ANY",
                                    statistics = "character"))

bandit$methods(
  initialize = function(formula, data, family, contrasts = NULL, newLevels = FALSE,
                        db = NULL,
                        path = NULL,
                        ...) {
    "initialization function"
    isDf <- TRUE
    if(!is.null(db) & !is.null(path)) isDf <- FALSE
    data <- validateData(data)
    newLevels <<- newLevels
    if(!newLevels) {
      xlevels <<- .getXlevels(terms(formula), model.frame(formula, model.frame(data)))
    }
    samples <- data.frame(cbind(jobSamples = 1,
                               jobOutcome = as.numeric(NA),
                               data))
    jobs <- data.frame(job = 1,
                      date = Sys.time(),
                      type = "initialize",
                      param = NA,
                      value = NA)
    if(isDf) {
      data <- dfData(jobs = jobs, samples = samples, statistics = statistics)
    } else {
      data <- dbData(jobs = jobs, samples = samples, statistics = statistics, db = db, path = path)
    }
    initFields(formula = formula,
               banditData = data,
               contrasts = contrasts,
               currentJob = 1,
               currentModel = NULL)
  },
  addSamples = function(df) {
    "add samples to the bandit. \\code{df} is coercible to a data.frame, and can be appended
    to the \\code{data.frame} used at creation. In particular, it contains an \\code{id} column
    that is a primary key."

    df <- as.data.frame(df)
    df$jobSamples <- currentJob+1
    df$jobOutcome <- NA

    vViolatePrimaryKey(banditData, df)
    banditData <<- wSamples(banditData, df)
    banditData <<- wModel(banditData)
    banditData <<- wJob(banditData, currentJob, "addSamples")
    currentJob <<- currentJob + 1
  },
  addOutcomes = function(y, ...) {
    "add outcomes to the bandit. y is a named vector whose names are samples ids."

    if(is.null(names(y))) stop("y must be a named vector")
    if(any(is.na(y))) warning("some outcomes you are adding have NA's")
    ids <- as.numeric(names(y))
    if(any(duplicated(ids))) stop("some outcomes are duplicated.")
    y <- as.numeric(y)
    names(y) <- ids
    vOutcomeMismatch(banditData, y)
    pr <- predict(.self,
                  whatSamples = ids,
                  whatModel = "last",
                  type = statistics, ...)
    if(is.numeric(pr)) {
      pr <- data.frame(id = as.numeric(names(pr)),
                       s = pr)
      colnames(pr)[2] <- statistics
    } else {
      pr <- cbind(id = rownames(pr), pr)
    }
    rownames(pr) <- NULL
    banditData <<- wOutcome(banditData, y, currentJob)
    banditData <<- wModel(banditData)
    banditData <<- wStatistics(banditData, pr)
    banditData <<- wJob(banditData, currentJob, "addOutcomes")
    currentJob <<- currentJob + 1
  },
  train = function(FUN, args, seed) {
    "train the bandit using available samples and latest values of the tuning
    parameters."
    args$data <- args$formula <- args$contrats <- args$seed <- NULL
    data <- rSamples(banditData, what = "current")
    data$jobSamples <- data$jobOutcome <- NULL
    args <- c(args,
              list(data = data,
                   formula = formula,
                   contrasts = contrasts,
                   seed = seed))
    model <- do.call(FUN, args)
    model$x <- model$y <- model$data <- model$model <- NULL
    args$data <- NULL
    model$parBandit <- args
    banditData <<- wModel(banditData, model, currentJob)
    if(all(c("stanreg", "lmerMod") %in% class(model))) {
      coef <- as.matrix(model)
      v <- colnames(coef)
      select <- 1:(min(which(substr(v,1,2) == "b[" & substr(v, nchar(v), nchar(v)) == "]"))-1)
      coef <- coef[,select]
      coef <- apply(coef, 2, median)
    } else {
      coef <- coef(model)
    }
    banditData <<- wCoef(banditData, coef, currentJob)
    banditData <<- wJob(banditData, currentJob, "train")
    currentModel <<- model
    currentJob <<- currentJob + 1
  },
  tune = function(FUN, args, seed) {
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

    data <- rSamples(banditData, what = "current")
    args <- c(args,
              list(formula = formula,
                   data = data,
                   seed = seed,
                   contrasts = contrasts))
    tu <- do.call(FUN, args)
    banditData <<- wModel(banditData, tu$model, currentJob)
    banditData <<- wJob(banditData, currentJob, "tune", param = tu$param, value = tu$value)
    currentParams[[tu$param]] <<- tu$value
    currentJob <<- currentJob + 1
  },
  undo = function() {
    "cancels the last job."
    lastJob <- rJobs(banditData, "last")
    type <- lastJob$type
    banditData <<- dJobs(banditData)
    banditData <<- dModels(banditData)
    if (type == "addSamples") {
      banditData <<- dSamples(banditData)
    } else if (type == "addOutcomes") {
      banditData <<- dStatistics(banditData)
      banditData <<- dOutcomes(banditData)
    } else if (type == "train") {
      banditData <<- dCoef(banditData)
      currentModel <<- rTrain(banditData, "last")
    } else if (type == "tune") {
      currentParams <<- rTune(banditData, currentJob-1, c("lambdaRidge", "lambdaLasso"))
    }
    currentJob <<- currentJob - 1
  }
)





# x <- matrix(rnorm(10e3), 1e3, 10)
# beta <- -4:5
# y <- as.numeric(plogis(x %*% beta))
# y <- sapply(y, rbinom, n = 1, size = 1)
# colnames(x) <- paste0("v", 1:10)
#
# df <- as.data.frame(x)
# df <- cbind(id = 1:1000, y = y, df)
# rm(y, x, beta)
#
# f <- as.formula(y ~ -1 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10)
#
#
#
#
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



