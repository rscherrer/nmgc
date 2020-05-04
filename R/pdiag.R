#' Proportion of a matrix that is on the diagonal
#'
#' @param x A matrix
#'
#' @export

pdiag <- function(x) sum(diag(x)) / sum(x)
