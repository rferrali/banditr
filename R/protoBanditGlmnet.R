
protoBanditGlmnet <- function(formula,
                              family = c("gaussian", "binomial"),
                              lambdaRidge,
                              lambdaLasso,
                              data,
                              seed = NULL,
                              parRidge = NULL,
                              parLasso = NULL,
                              contrasts = NULL,
                              xlevels = NULL) UseMethod("protoBanditGlmnet")

protoBanditGlmnet.default <- function(formula,
                                 family = c("gaussian", "binomial"),
                                 lambdaRidge,
                                 lambdaLasso,
                                 data,
                                 seed = NULL,
                                 parRidge = NULL,
                                 parLasso = NULL,
                                 contrasts = NULL,
                                 xlevels = NULL) {
  call <- match.call()
  # model.frame
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- FALSE
  mf$xlev <- xlevels
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  # other components
  mt <- attr(x = mf, which = "terms")
  # mt <- terms(formula)
  x <- model.matrix(object = mt, data = mf, contrasts.arg = contrasts)
  intercept <- as.logical(attr(x = mt, "intercept"))
  if(intercept) x <- dropIntercept(x)
  y <- model.response(mf)

  # fitting
  fit <- banditGlmnet.fit(x = x,
                         y = y,
                         family = family,
                         lambdaRidge = lambdaRidge,
                         lambdaLasso = lambdaLasso,
                         intercept = intercept,
                         weights = NULL,
                         offset = NULL,
                         seed = seed,
                         parRidge = parRidge,
                         parLasso = parLasso)
  # output
  out <- c(fit,
           list(data = data$id, parRidge = parRidge, parLasso = parLasso))
  class(out) <- c("protoBanditGlmnet", "protoBanditModel")

  return(out)
}

coef.protoBanditGlmnet <- function(object) {
  coef <- glmnet::coef.glmnet(object$glmnet)[,1]
  if(is.null(object$lasso)) {
    return(coef)
  } else {
    c2 <- glmnet::coef.glmnet(object$lasso)[,1]
    coef <- coef[match(names(c2), names(coef))]
    names(coef) <- names(c2)
    coef[is.na(coef)] <- 0
    return(coef)
  }
}


tuneBandit <- function(param,
                 value,
                 lambdaAuto,
                 formula,
                 family = c("gaussian", "binomial"),
                 currentLasso,
                 data,
                 seed = NULL,
                 parCvGlmnet = NULL,
                 contrasts = NULL,
                 xlevels = NULL) {
  # validation
  if(is.null(seed)) seed <- sample(1e5, 1)
  param <- match.arg(param, c("lambdaLasso", "lambdaRidge"))
  lambdaAuto <- match.arg(lambdaAuto, c("lambda.1se", "lambda.min"))
  value <- as.vector(value)
  msg <- "value must be a positive scalar or 'auto'"
  if(length(value) != 1) stop(msg)
  if(is.character(value)) {
    if(value != "auto") stop(msg)
    if(currentLasso != 0 & param == "lambdaRidge") stop("Can't use automatic tuning with ridge when using lasso.")
  } else {
    value <- as.numeric(value)
    if(value < 0) stop(msg)
  }
  # estimation
  set.seed(seed)
  model <- NULL
  if(!is.numeric(value)) {
    data <- model.frame(formula, data, xlev = xlevels)
    x <- model.matrix(formula, data, contrasts.arg = contrasts)
    terms <- terms(formula)
    intercept <- as.logical(attr(terms, "intercept"))
    if(intercept) x <- dropIntercept(x)
    y <- model.response(data)
    alpha_par <- ifelse(param == "lambdaRidge", 0, 1)

    if(length(intersect(names(parCvGlmnet), c("x","y","family","alpha","lambda","intercept"))) > 0) {
      stop("parCvGlmnet overrides some pre-set parameters.")
    }
    pb <- parCvGlmnet
    parCvGlmnet <- c(list(x = x,
                          y = y,
                          family = family,
                          alpha = alpha_par,
                          intercept = intercept),
                     parCvGlmnet)
    model <- do.call(glmnet::cv.glmnet, parCvGlmnet)
    value <- model[[lambdaAuto]]
    model <- list(cv.glmnet = model,
                  seed = seed,
                  lambdaAuto = lambdaAuto,
                  value = value,
                  param = param,
                  parCvGlmnet = pb)
  }
  return(list(model = model,
              value = value,
              param = param))
}


