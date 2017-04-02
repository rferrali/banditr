dropIntercept <- function(x) {
  dim <- dim(x)
  cn <- colnames(x)[-1]
  x <- x[,-1]
  x <- matrix(x, dim[1], dim[2]-1)
  colnames(x) <- cn
  x
}

validateData <- function(data) {
  data <- as.data.frame(data)
  if(any(c("jobSamples","jobOutcome") %in% colnames(data))) stop("data uses some reserved column names (jobSamples, jobOutcome)")
  if(!"id" %in% colnames(data)) stop("data must have an id column")
  return(data)
}

makePredict <- function(li, names) {
  if(length(li) == 1) {
    li <- unlist(li)
    names(li) <- names
  } else {
    li <- as.data.frame(li)
    rownames(li) <- names
  }
  return(li)
}

validateAddSamples <- function(formula, df, newLevels, xlevels) {
  tt <- terms(formula)
  tt <- delete.response(tt)
  if(newLevels) {
    model.frame(tt, df, na.action = "na.fail")
  } else {
    model.frame(tt, df, na.action = "na.fail", xlev = xlevels)
  }
}

validateAddSamples.bandit_merMod <- function(formula, df, family, contrasts) {
  df$y <- 0
  suppressMessages(lme4::glFormula(formula = formula, data = df, family = family, contrasts = contrasts,
                  na.action = "na.fail"))
}

addCall <- function(model, FUN, extra = list()) {
  cl <- c(model$parBandit, extra)
  cl <- cl[!sapply(cl, is.null)]
  cl$data <- quote(data)
  names <- names(cl)
  names <- names[!names %in% c("formula", "family", "data")]
  cl <- cl[c("formula", "family", "data", names)]
  cl <- c(as.symbol(FUN), cl)
  model$parBandit <- NULL
  model$call <- as.call(cl)
  model
}

