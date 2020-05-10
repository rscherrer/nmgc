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

  if (univariate) {

    out <- d %>%
      split(f = .[c(nesting, grouping)]) %>%
      map_dfr(
        ~ .x %>% gather_("variable", "score", variables) %>%
          split(f = .[["variable"]]) %>%
          map_dfr(
            ~ shapiro.test(.x$score) %>%
              .[c("statistic", "p.value")] %>%
              data.frame,
            .id = "variable"
          ),
        .id = "nesting.grouping"
      ) %>%
      separate(nesting.grouping, into = c(nesting, grouping), sep = "\\.")

    out <- out %>% rename(W = "statistic", pvalue = "p.value")

    if (add_signif) out <- out %>% add_signif()

    return (out)

  }

  d <- d %>% split(f = .[, nesting])

  res <- do.call(rbind, lapply(d, function(d) {
    d <- d %>% split(f = .[, grouping])
    res <- data.frame(t(sapply(d, function(d) {
      res <- mvn(d[, variables], mvnTest = "hz")
      res <- res$multivariateNormality
      return (c(HZ = res$HZ, pvalue = res$'p value'))
    })))
    res$habitat <- rownames(res)
    rownames(res) <- NULL
    return (res)
  }))

  res[[nesting]] <- gsub("\\.[0-9]", "", rownames(res))
  rownames(res) <- NULL
  res <- res[, c(nesting, grouping, "HZ", "pvalue")]

  if (add_signif) res <- res %>% add_signif()

  return (res)

}
