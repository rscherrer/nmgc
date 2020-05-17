#' Posthoc tests

nanova_posthoc <- function(data, method = "tukey") {

  library(PMCMRplus)

  if (method == "tukey") this_posthoc <- tukeyTest
  if (method == "dunnett") this_posthoc <- dunnettT3Test
  if (method == "nemenyi") this_posthoc <- kwAllPairsNemenyiTest

  ph <- this_posthoc(data$score, data$group)

  levs <- levels(data$group)
  contrasts <- unique(expand.grid(levs, levs))
  contrasts <- contrasts %>% filter(as.character(Var1) < as.character(Var2))
  colnames(contrasts) <- c("contrast1", "contrast2")
  data.frame(
    contrasts,
    statistic = c(na.omit(c(ph$statistic))),
    pvalue = c(na.omit(c(ph$p.value)))
  )

}
