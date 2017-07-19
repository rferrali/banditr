#'
#' \code{addSamples(df)} add samples to the bandit. \code{df} is coercible to
#' a data.frame, and can be appended to the \code{data.frame} used at creation.
#' In particular, it contains an \code{id} column that is a primary key.
#'
#' \code{addOutcomes(y)} add outcomes to the bandit. \code{y} is a named vector
#' whose names are samples ids.
#'
#' \code{undo()} cancels the last job.
