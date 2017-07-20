
#' A bandit reference class (RC) object.


#' @template argBandit
#' @template argBanditOpt
#'
#' @details
#' The RC \code{"bandit"} class is the main object defined in
#'  \code{banditr}. It allows to manage bandit experiments easily. Two
#'  classes inherit from \code{"bandit"}: the \code{"\link{bandit_ucb}"}
#'  class for LinUCB algorithms, and the \code{"\link{bandit_thompson}"} for
#'  Thompson sampling algorithms.
#'
#'  The introductory vignette provides a detailed explanation of bandit algorithms, and
#' their implementation with \code{banditr}. See the Examples section.
#'
#' @field formula an object of class "formula" (or one that can be coerced
#' to that class): a symbolic description of the model that is fitted.
#' @field contrasts an optional list. See the \code{contrasts.arg} of \code{\link{model.matrix.default}}.
#' @field newLevels a logical value indicating whether to allow for new factor levels when adding samples.
#' @field xlevels if \code{newLevels} is false, a record of the levels of the factors used in fitting.
#' @field currentJob a numeric with the id of the current job.
#' @field currentModel the prototype of the latest fit.
#' @field currentParams a list of tuning parameters.
#' @field banditData the bandit's data.
#' @field statistics a character vector of statistics to be computed when adding outcomes
#'
#' @examples
#' vignette("introduction", "banditr")
#' @seealso \code{\link{bandit_ucb}}, \code{\link{bandit_thompson}}
#'
#' @import methods




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
  initialize = function(formula, data, family,
                        contrasts = NULL,
                        newLevels = FALSE,
                        db = NULL,
                        path = NULL,
                        ...) {
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



