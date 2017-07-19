#' @param formula an object of class "formula" (or one that can be coerced
#' to that class): a symbolic description of the model that is fitted. The response
#' must be named \code{y}
#' @param data a data frame (or object coercible by \code{\link{as.data.frame}} to a data frame)
#' containing the variables in the model. \code{data} must contain a column named
#' \code{id} that uniquely identifies each observation, and a column named \code{y} that
#' contains the model response.
#' @param family a character string describing the error distribtion and link function
#' to be used in the model. Can be either \code{"binomial"} or \code{"gaussian"} (the default).

