# Multiple Kruskal-Wallis tests

nanova_kruskal <- function(data, variables) {

  names(variables) <- variables

  res <- purrr::map_dfr(variables, function(variable, data) {
    res <- kruskal.test(data[[variable]], data[["group"]])
    res <- with(res, data.frame(statistic, parameter, p.value))
  }, data, .id = "variable")
  res <- res %>%
    dplyr::rename(chisq = "statistic", df = "parameter", pvalue = "p.value")

}
