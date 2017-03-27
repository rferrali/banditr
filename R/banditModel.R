as.banditModel <- function(x) useMethod ("as.banditModel", x)

as.banditModel.protoBanditModel <- function(object,
                                            parent) {
  object$data <- getSamples(parent, object$data)
  object$formula <- parent$formula
  object$terms <- terms(object$formula)
  object$family <- parent$family
  object$xlevels <- parent$xlevels
  object$constrasts <- parent$contrasts

  fit
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
       na.action = attr(mf, "na.action"))
}


model.frame.protoBanditModel <- function(object, ...) {
  object <- as.banditModel(object)
  model.frame.banditModel(object, ...)
}

model.matrix.protoBanditModel <- function(object, ...) {
  object <- as.banditModel(object)
  model.matrix.banditModel(object, ...)
}
