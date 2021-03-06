#' Semi-parametric MANOVA

nanova_smanova <- function(data, variables, seed = NULL, iter = 1000) {

  if (is.null(seed)) seed <- sample(1000, 1)

  # Semi-parametric MANOVA
  res <- MANOVA.RM::MANOVA.wide(
    do.call("cbind", data[variables]) ~ group,
    data = data, seed = seed, iter = iter
  )
  res <- data.frame(res$MATS, res$resampling[, 2])
  rownames(res) <- NULL
  res <- res %>%
    dplyr::rename(MATS = "Test.statistic", pvalue = "res.resampling...2.")

}
