dropIntercept <- function(x) {
  dim <- dim(x)
  cn <- colnames(x)[-1]
  x <- x[,-1]
  x <- matrix(x, dim[1], dim[2]-1)
  colnames(x) <- cn
  x
}
