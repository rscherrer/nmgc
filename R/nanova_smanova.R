#' Semi-parametric MANOVA

nanova_smanova <- function(data, variables) {

  library(MANOVA.RM)

  # Semi-parametric MANOVA
  res <- MANOVA.wide(
    do.call("cbind", data[variables]) ~ group,
    data = data, seed = seed, iter = iter
  )
  res <- data.frame(res$MATS, res$resampling[, 2])
  rownames(res) <- NULL
  res <- res %>% rename(MATS = "Test.statistic", pvalue = "res.resampling...2.")

}
