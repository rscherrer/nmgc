#' Repeat multiple things multiple times
#'
#' @param x What to repeat
#' @param n How many times each?
#'
#' @export


mrep <- function(x, n) {
  do.call("c", mapply(function(x, n) rep(x, n), x, n, SIMPLIFY = FALSE))
}
