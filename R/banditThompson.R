#' A Thompson sampling bandit reference class (RC) object.

#' @usage
#' bandit_stan_lm(formula, data, gamma = 1,
#'                contrasts = NULL, newLevels = FALSE,
#'                db = NULL, path = NULL)
#' bandit_stan_glm(formula, data, family = c("gaussian", "binomial"),
#'                 gamma = 1, contrasts = NULL, newLevels = FALSE,
#'                 db = NULL, path = NULL)
#' bandit_stan_glmer(formula, data, family = c("gaussian", "binomial"),
#'                   gamma = 1, contrasts = NULL, newLevels = FALSE,
#'                   db = NULL, path = NULL)
#' @template argBandit
#' @param gamma the Thompson sampling tuning parameter. A positive scalar. Higher
#' values of \code{gamma} favor exploitation over exploration.
#' @template argBanditOpt
#' @details
#' The RC class \code{"bandit_thompson"} inherits from class \code{"\link{bandit}"}.
#' Three classes inherit from \code{"bandit_thompson"}: \code{"bandit_stan_lm"},
#' \code{"bandit_stan_glm"}, and \code{"bandit_stan_glmer"}, for linear, generalized
#' linear, and mixed effect models respectively.
#'
#' The introductory vignette provides a detailed explanation of Thompson sampling
#'  algorithms, and
#' their implementation with \code{banditr}. See the Examples section.
#'
#' @field gamma the Thompson sampling tuning parameter.
#'
#' @section Methods:
#'
#' \code{train(..., seed = NULL)} train the model using the relevant function from
#' \code{\link{[rstanarm]rstanarm}} and all completed experiments. \code{...} are
#' additional parameters passed on to the relevant function in \code{rstanarm}.
#' \code{seed} is an optional seeding value for the random number generator.
#'
#' \code{tune()} currently not supported.
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
#' @seealso \code{\link{bandit}}, \code{\link{bandit_ucb}}
#'
#' @examples vignette("introduction", "banditr")

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


#' @rdname bandit_thompson-class
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
    args <- cl$`...`
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

#' @rdname bandit_thompson-class
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

#' @rdname bandit_thompson-class
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

