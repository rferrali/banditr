
#' Estimate a banditGlmnet model
#'
#' The syntax follows closely that of \code{\link[stats]{glm}}.
#' All parameters not defined on this page are defined as in
#' \code{\link[stats]{glm}}.
#'
#'
#' @export
#' @param family supported response type. Currently, either
#' \code{"gaussian"} (the default) or \code{"binomial"}.
#' @param lambdaRidge a scalar in [0,1]. Tuning parameter of the ridge regression.
#' @param lambdaLasso a scalar in [0,1]. Tuning parameter of the LASSO.
#' @param seed an (optional) seeding number.
#' @param parRidge an (optional) list of parameters passed on to glmnet for the ridge
#' stage. Format argument = value.
#' @param parLasso an (optional) list of parameters passed on to glmnet for the LASSO
#' stage. Format argument = value
#' @param method the method to be used in fitting the model.
#' The default \code{"glmnet"} uses \code{\link[glmnet]{glmnet}};
#' the alternative \code{"model.frame"} returns the model frame and does
#' no fitting.
#' @return a banditGlmnet object.




banditGlmnet <- function(formula,
                         family = c("gaussian", "binomial"),
                         lambdaRidge,
                         lambdaLasso,
                         data,
                         weights,
                         subset,
                         na.action,
                         offset,
                         seed = NULL,
                         parRidge = NULL,
                         parLasso = NULL,
                         model = TRUE,
                         method = "glmnet",
                         x = FALSE,
                         y = TRUE,
                         contrasts = NULL) UseMethod("banditGlmnet")

#' @export
banditGlmnet.default <- function(formula,
                                 family = c("gaussian", "binomial"),
                                 lambdaRidge,
                                 lambdaLasso,
                                 data,
                                 weights,
                                 subset,
                                 na.action,
                                 offset,
                                 seed = NULL,
                                 parRidge = NULL,
                                 parLasso = NULL,
                                 model = TRUE,
                                 method = "glmnet",
                                 x = FALSE,
                                 y = TRUE,
                                 contrasts = NULL) {
  call <- match.call()
  # validation
  formula <- as.formula(formula)
  method <- match.arg(method, c("glmnet", "model.frame"))
  if(missing(data)) data <- environment(formula)

    # model.frame
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action",
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  if(method == "model.frame") return(mf)
    # other components
  mt <- attr(x = mf, which = "terms")
  xarg <- x
  xfit <- x <- model.matrix(object = mt, data = mf, contrasts.arg = contrasts)
  intercept <- as.logical(attr(x = mt, "intercept"))
  if(intercept) xfit <- dropIntercept(xfit)
  yarg <- y
  y <- model.response(mf)
  weights <- model.weights(mf)
  offset <- model.offset(mf)

  # fitting
  fit <- banditGlmnet.fit(x = xfit,
                         y = y,
                         family = family,
                         lambdaRidge = lambdaRidge,
                         lambdaLasso = lambdaLasso,
                         intercept = intercept,
                         weights = weights,
                         offset = offset,
                         seed = seed,
                         parRidge = parRidge,
                         parLasso = parLasso)
  # output
  out <- c(fit,
           list(call = call,
                lambdaLasso = lambdaLasso,
                lambdaRidge = lambdaRidge,
                data = data,
                formula = formula,
                terms = mt,
                family = family,
                weights = weights,
                offset = offset,
                method = method,
                contrasts = attr(x, "contrasts"),
                xlevels = .getXlevels(mt, mf),
                na.action = attr(mf, "na.action"),
                parRidge = parRidge,
                parLasso = parLasso))
  if(xarg) out$x <- x
  if(yarg) out$y <- y
  if(model) out$model <- mf
  class(out) <- c("banditModel", "banditGlmnet")

  return(out)
}

#### S3 methods ####


#' @export
model.frame.banditModel <- function(formula, ...) {
  dots <- list(...)
  nargs <- dots[match(c("data", "na.action", "subset"), names(dots),
                      0L)]
  if (length(nargs) || is.null(formula$model)) {
    fcall <- formula$call
    fcall$method <- "model.frame"
    fcall[[1L]] <- quote(banditr::banditGlmnet)
    fcall[names(nargs)] <- nargs
    env <- environment(formula$terms)
    if (is.null(env))
      env <- parent.frame()
    eval(fcall, env)
  }
  else formula$model
}

#' @export
model.matrix.banditModel <- function(object, ...) {
  if (n_match <- match("x", names(object), 0L))
    x <- object[[n_match]]
  else {
    data <- model.frame(object, xlev = object$xlevels, ...)
    x <- NextMethod("model.matrix", data = data, contrasts.arg = object$contrasts)
  }
  return(x)
}

#' @export
#' @import glmnet
#'
predict.banditGlmnet <- function(object,
                                 newdata = NULL,
                                 type = c("link","response","uncertainty","score"),
                                 alpha = 1,
                                 robust = TRUE,
                                 na.action = na.pass, ...) {

  # validation
  type <- match.arg(type, c("link","response","nonzero","class","uncertainty","score"))
  alpha <- as.numeric(alpha)
  if(alpha<=0 | length(alpha)>1) stop("alpha must be a strictly positive scalar")
  # computation
  tt <- terms(object)
  if(is.null(newdata)) {
    x <- model.matrix(object)
    offset <- object$offset
  } else {
    tt <- delete.response(tt)
    mf <- model.frame(tt, newdata, na.action = na.action, xlev = object$xlevels, ...)
    x <- model.matrix(tt, mf, contrasts.arg = object$contrasts)
    offset <- model.offset(mf)
  }
  intercept <- as.logical(attr(x = tt, "intercept"))
  if(!type %in% c("uncertainty", "score")) {
    if(intercept) x <- dropIntercept(x)
    yhat <- predict(object$glmnet, newx = x, s = object$lambdaRidge, type = type, offset = offset)[,1]
    return(yhat)
  } else {
    z <- x
    x <- model.matrix.banditModel(object)
    u <- uncertainty(x, z, object$lambdaRidge, robust)
    if(type == "uncertainty") {
      return(u)
    } else {
      if(intercept) z <- dropIntercept(z)
      yhat <- glmnet::predict(object$glmnet, newx = z, s = object$lambdaRidge, type = "response", offset = offset)[,1]
      score <- yhat + alpha * u
      return(score)
    }
  }
}

#' @export
coef.banditGlmnet <- function(object) {
  m <- glmnet::coef.glmnet(object$glmnet)
  v <- m[,1]
  names(v) <- rownames(m)
  return(v)
}

uncertainty <- function(x, z, lambdaRidge, robust = TRUE) {
  xtxinv <- crossprod(x)
  xtxinv <- lambdaRidge * diag(nrow(xtxinv)) + xtxinv
  if(robust) {
    xtxinv <- tryCatch(solve(xtxinv),
                       error = function(e) {
                         warning("Error during inversion:\n",
                                 e, "\nSwitching to generalized inverse")
                         return(MASS::ginv(xtxinv))
                       })
  } else {
    xtxinv <- solve(xtxinv)
  }
  u <- sqrt(rowSums(z %*% xtxinv * z))
  return(u)
}

banditGlmnet.fit <- function(x,
                            y,
                            family,
                            lambdaRidge,
                            lambdaLasso,
                            intercept,
                            weights,
                            offset,
                            seed = NULL,
                            parRidge = NULL,
                            parLasso = NULL) {
  # validation
  family <- match.arg(family, c("gaussian", "binomial"))
  lambdaRidge <- as.numeric(lambdaRidge)
  if(length(lambdaRidge) > 1 | lambdaRidge < 0) stop("The lambdaRidge parameter must be a positive scalar")
  lambdaLasso <- as.numeric(lambdaLasso)
  if(length(lambdaLasso) > 1 | lambdaLasso < 0) stop("The lambdaLasso parameter must be a positive scalar")
  if(!is.null(seed)) {
    seed <- as.numeric(seed)
    if(length(seed) > 1) stop("seed must be a scalar.")
  } else {
    seed <- sample(1e5,1)
  }

  # preparation
  if(!is.null(parRidge)) parRidge <- as.list(parRidge)
  if(!is.null(parLasso)) parLasso <- as.list(parLasso)
  set.seed(seed)
  sparse <- F

  # Lasso stage
  if(lambdaLasso > 0) {
    if(length(intersect(names(parLasso), c("x","y","family","alpha","lambda","intercept","weights","offset"))) > 0) {
      stop("parLasso overrides some pre-set parameters.")
    }
    parLasso <- c(list(x = x,
                       y = y,
                       family = family,
                       alpha = 1,
                       lambda = lambdaLasso,
                       intercept = intercept),
                  parLasso)
    if(!is.null(weights)) parLasso$weights <- weights
    if(!is.null(offset)) parLasso$offset <- offset
    lasso <- do.call(glmnet::glmnet, parLasso)
    intercept <- TRUE
    keep <- coef(lasso)[,1]
    if (keep[1] == 0) intercept <- FALSE
    keep <- which(keep[-1] != 0)
    if (length(keep) < 2) {
      keep <- sample.int(ncol(x), size = 2)
      sparse <- T
      warning("The lasso is too sparse. Picking two covariates at random.")
    }
    x <- x[,keep]
  } else {
    lasso <- NULL
  }

  # Ridge stage
  if(length(intersect(names(parRidge), c("x","y","family","alpha","lambda","intercept","weights","offset"))) > 0) {
    stop("parRidge overrides some pre-set parameters.")
  }
  parRidge <- c(list(x = x,
                     y = y,
                     family = family,
                     alpha = 0,
                     lambda = lambdaRidge,
                     intercept = intercept),
                parRidge)
  if(!is.null(weights)) parRidge$weights <- weights
  if(!is.null(offset)) parRidge$offset <- offset
  glmnet <- do.call(glmnet::glmnet, parRidge)

  out <- list(glmnet = glmnet,
              lasso = lasso,
              seed = seed,
              sparse = sparse)

  return(out)
}
