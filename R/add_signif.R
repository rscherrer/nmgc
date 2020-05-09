#' Add significance column
#'
#' Adds a column with significance labels to a table of statistical results.
#'
#' @param df A data frame
#' @param pcol Name of the column containing P-values
#' @param thresholds A vector of P-value thresholds, in decreasing order
#' @param labels A vector of labels, one for each threshold
#'
#' @return A data frame
#'
#' @export

add_signif <- function(
  df, pcol = "pvalue", thresholds = c(0.05, 0.01, 0.001), labels = c("*", "**", "***")
) {

  df$signif <- rep("", nrow(df))
  for (i in seq_along(thresholds)) {
    df <- df %>% dplyr::mutate(signif = ifelse(get(pcol) < thresholds[i], labels[i], signif))
  }
  return (df)

}
