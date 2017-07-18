#' A Thompson sampling bandit reference class (RC) object.

#' @field \code{gamma} the blabla tuning parameter. See details.

bandit_thompson <- setRefClass("bandit_thompson",
                         contains = "bandit",
                         fields = list(gamma = "numeric"))

bandit_thompson$methods(
  initialize = function(gamma = 1, ...) {
    ga <- as.numeric(gamma)
    if(length(ga)>1) stop("gamma must be a positive scalar")
    if(ga <= 0) stop("gamma must be a positive scalar")
    initFields(gamma = ga,
               statistics = "response",
               currentParams = list())
    callSuper(...)
  },
  train = function(FUN,
                   args,
                   seed = NULL) {
    args$algorithm <- NULL
    args <- c(args,
              list(algorithm = "sampling"))
    callSuper(FUN = FUN, args = args, seed = seed)
  },
  tune = function() {
    NULL
  })


#' @export bandit_stan_lm
#' @exportClass bandit_stan_lm


bandit_stan_lm <- setRefClass("bandit_stan_lm",
                              contains = "bandit_thompson")
bandit_stan_lm$methods(
  initialize = function(formula, data, ...) {
    mf <- model.frame(formula, data)
    callSuper(formula = formula, data = data, ...)
  },
  train = function(..., seed = NULL) {
    cl <- as.list(match.call(expand.dots = FALSE))
    args$x <- args$y <- args$model <- FALSE
    callSuper(FUN = rstanarm::stan_lm,
              args = cl$`...`,
              seed = cl$seed)
  },
  addSamples = function(df) {
    validateAddSamples(formula, df, newLevels, xlevels)
    callSuper(df)
  }
)

#' @export bandit_stan_glm
#' @exportClass bandit_stan_glm


bandit_stan_glm <- setRefClass("bandit_stan_glm",
                              contains = "bandit_thompson",
                              fields = list(family = "character"))
bandit_stan_glm$methods(
  initialize = function(formula, data, family, ...) {
    mf <- model.frame(formula, data)
    fam <- match.arg(family, c("gaussian", "binomial"))
    initFields(family = fam)
    callSuper(formula = formula, data = data, ...)
  },
  train = function(..., seed = NULL) {
    cl <- as.list(match.call(expand.dots = FALSE))
    args <- cl$`...`
    args$x <- args$y <- args$model <- FALSE
    args$family <- family
    callSuper(FUN = rstanarm::stan_glm,
              args = args,
              seed = cl$seed)
  },
  addSamples = function(df) {
    validateAddSamples(formula, df, newLevels, xlevels)
    callSuper(df)
  }
)

#' @export bandit_stan_glmer
#' @exportClass bandit_stan_glmer


bandit_stan_glmer <- setRefClass("bandit_stan_glmer",
                               contains = "bandit_thompson",
                               fields = list(family = "character"))
bandit_stan_glmer$methods(
  initialize = function(formula, data, family, newLevels = TRUE, contrasts = NULL, ...) {
    if(!newLevels) stop("newLevels must be TRUE for glmer bandits.")
    lme4::glFormula(formula = formula, data = data, family = family, contrasts = contrasts)
    fam <- match.arg(family, c("gaussian", "binomial"))
    initFields(family = fam)
    callSuper(formula = formula, data = data, newLevels = newLevels, contrasts = contrasts, ...)
  },
  train = function(..., seed = NULL) {
    cl <- as.list(match.call(expand.dots = FALSE))
    args <- cl$`...`
    args$family <- family
    callSuper(FUN = rstanarm::stan_glmer,
              args = args,
              seed = cl$seed)
  },
  addSamples = function(df) {
    validateAddSamples.bandit_merMod(formula, df, family, contrasts)
    callSuper(df)
  }
)

