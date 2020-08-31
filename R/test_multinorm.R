#' Test multivariate normality
#'
#' Uses Henze-Zirkler's (or Shapiro's) test to assess the multivariate normality across groups and subsets of the data
#'
#' @param d A data frame
#' @param variables What variables to test?
#' @param grouping Grouping variable
#' @param nesting Nesting variable
#' @param univariate Special option to return the results of multiple univariate Sharpiro tests
#' @param add_signif Whether to add significance asterisk labels in an extra column
#'
#' @return A data frame with HZ statistics and P-values for each group in each subset, or a data frame with W statistics and P-values for each variable in each group and each subset, if `univariate` is TRUE.
#'
#' @export

test_multinorm <- function(d, variables, grouping, nesting = NULL, univariate = FALSE, add_signif = TRUE) {

  library(MVN)
  library(tidyverse)

  if (is.null(nesting)) {
    d$nesting <- factor(1)
    nesting <- "nesting"
  }

  d$grouping <- d[[grouping]]
  d$nesting <- d[[nesting]]

  if (univariate) {

    res <- d %>%
      gather_("variable", "score", variables) %>%
      group_by(nesting, grouping, variable) %>%
      nest() %>%
      mutate(test = map(data, function(data) {
        res <- shapiro.test(data$score)
        data.frame(W = res$statistic, pvalue = res$p.value)
      })) %>%
      select(-data) %>%
      unnest(cols = c(test))

  } else {

    res <- d %>%
      dplyr::group_by(nesting, grouping) %>%
      nest() %>%
      mutate(test = map(data, function(data) {
        res <- mvn(data[, variables], mvnTest = "hz")$multivariateNormality
        data.frame(HZ = res$HZ, pvalue = res$'p value')
      })) %>%
      dplyr::select(-data) %>%
      unnest(cols = c(test))

  }

  if (add_signif) res <- res %>% add_signif()

  return(res)

}
