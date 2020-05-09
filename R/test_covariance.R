#' Test homogeneity of covariance matrices
#'
#' Uses Box's M-test to assess the homogeneity of multivariate covariance matrices between groups within each subset
#'
#' @param d A data frame
#' @param variables What variables to test?
#' @param grouping Grouping variable
#' @param nesting Nesting variable
#' @param add_signif Whether to add significance asterisk labels in an extra column
#'
#' @return A data frame with the chi-square statistics, degrees of freedom and P-value of Box's M-test performed between groups within each subset
#'
#' @export

test_covariance <- function(d, variables, grouping, nesting = NULL) {

  library(heplots)
  library(tidyverse)

  if (is.null(nesting)) {
    d$nesting <- factor(1)
    nesting <- "nesting"
  }

  d <- d[, c(nesting, grouping, variables)]
  d <- d %>% split(f = .[, nesting])

  res <- data.frame(t(sapply(d, function(d) {
    fit <- boxM(as.matrix(d[, variables]), d[[grouping]])
    return (c(chisq = fit$statistic, df = fit$parameter, pvalue = fit$p.value))
  })))

  res <- res %>% rownames_to_column(nesting)

  colnames(res) <- c(nesting, "chisq", "df", "pvalue")

  if (add_signif) res <- res %>% add_signif()

  return (res)

}
