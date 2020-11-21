#' MANOVA

nanova_manova <- function(data, variables, test = "Pillai") {

  X <- data[, variables] %>% as.matrix

  res <- manova(X ~ group, data)
  res <- summary(res, test = test)
  res <- data.frame(res$stats)[1, ]
  rownames(res) <- NULL
  res <- res %>% dplyr::rename(
    df = "Df", pseudoF = "approx.F", num_df = "num.Df", den_df = "den.Df",
    pvalue = "Pr..F."
  )

}
