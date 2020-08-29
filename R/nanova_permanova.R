#' PERMANOVA

nanova_permanova <- function(data, variables, seed = NULL, iter = 1000) {

  library(vegan)

  if (is.null(seed)) seed <- sample(1000, 1)
  set.seed(seed)

  # PERMANOVA
  res <- adonis(
    dist(data[variables]) ~ group,
    data = data, permutations = iter
  )

  res <- data.frame(res$aov.tab)[1,]
  rownames(res) <- NULL
  res <- res %>% rename(
    df = "Df", pseudoF = "F.model", r2 = "R2", pvalue = "Pr..F."
  ) %>%
    select(df, pseudoF, r2, pvalue)

  return(res)

}
