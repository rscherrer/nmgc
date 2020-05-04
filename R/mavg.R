#' Average over a list of matrices
#'
#' @param l A list of matrices
#'
#' @export

mavg <- function(l) Reduce('+', l) / length(l)
