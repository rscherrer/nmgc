#' Test homogeneity of covariance matrices
#'
#' Uses Box's M-test to assess the homogeneity of multivariate covariance matrices between groups within each subset
#'
#' @param d A data frame
#' @param variables What variables to test?
#' @param grouping Grouping variable
#' @param nesting Nesting variable
#' @param add_signif Whether to add significance asterisk labels in an extra column
#' @param univariate Whether to perform univariate Bartlett's tests instead
#'
#' @return A data frame with the chi-square statistics, degrees of freedom and P-value of Box's M-test performed between groups within each subset
#'
#' @export

test_covariance <- function(
  d, variables, grouping, nesting = NULL, add_signif = TRUE, univariate = FALSE
) {

  library(heplots)
  library(tidyverse)

  if (is.null(nesting)) {
    d$nesting <- factor(1)
    nesting <- "nesting"
  }

  d <- d[, c(nesting, grouping, variables)]

  d <- d %>% split(f = .[, nesting])

  # For each subset...
  res <- lapply(d, function(d) {

    # If univariate Bartlett's tests must be performed
    if (univariate) {

      # For each variable...
      out <- lapply(variables, function(variable) {
        fit <- bartlett.test(d[, variable], d[, grouping])
        return(c(
          K2 = unname(fit$statistic),
          df = unname(fit$parameter),
          pvalue = unname(fit$p.value)
        ))
      }) %>% do.call("rbind", .) %>% data.frame
      out <- cbind(variable = variables, out)
      return (out)

    }

    fit <- heplots::boxM(as.matrix(d[, variables]), d[[grouping]])
    return (c(chisq = fit$statistic, df = fit$parameter, pvalue = fit$p.value))

  }) %>% do.call("rbind", .) %>% data.frame

  res <- res %>% tibble::rownames_to_column(nesting)

  if (!univariate) colnames(res) <- c(nesting, "chisq", "df", "pvalue")

  if (add_signif) res <- res %>% add_signif()

  return (res)

}
