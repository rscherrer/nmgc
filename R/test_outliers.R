#' Test multivariate outliers
#'
#' Finds multivariate outliers across groups and subsets using a quantile method based on Mahalanobis distance
#'
#' @param d A data frame
#' @param variables What variables to test?
#' @param grouping Grouping variable
#' @param nesting Nesting variable
#'
#' @return A nested list of outlier identities across groups and subsets
#'
#' @export

test_outliers <- function(d, variables, grouping, nesting = NULL) {

  if (is.null(nesting)) {
    d$nesting <- factor(1)
    nesting <- "nesting"
  }

  d <- d %>% split(f = .[, nesting])
  lapply(d, function(d) {
    d <- d %>% split(f = .[, grouping])

    res <- lapply(d, function(d) {
      res <- MVN::mvn(d[, variables], mvnTest = "hz", showOutliers = TRUE)
      res$multivariateOutlierss
    })

    return (res)
  })
}
