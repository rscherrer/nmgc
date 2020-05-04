
#' Plot classification results
#'
#' Plot the output of the `classify` function
#'
#' @param res The output of `classify`
#' @param facets Optional name of the facetting (nesting) variable
#' @param bins Number of bins for the histograms
#' @param fill Color of the histograms
#' @param alpha Transparency of the histograms
#' @param ylim Limits of the vertical axis
#' @param norm How to normalize the average confusion matrix: 0 for no normalization, 1 for by row, and 2 for by column
#' @param low,high,limits Parameters for the gradient fill scale in plotting the confusion matrices
#' @param xmin,xmax,ymin,ymax Coordinates of the confusion matrices as insets in the histogram plots
#' @param dfac Factor to scale the height of the null density line
#' @param rounding How to round the P-values
#' @param signif Significance threshold for P-values
#' @param px,py,phjust Parameters for `geom_text` when displaying P-values
#' @param type Type of plot to return: "histogram" (for histograms of classification success) or "confusion" (confusion matrix)
#' @param add_insets Whether to add confusion matrices as insets in the histogram plots
#' @param add_null Whether to add null density lines in the histogram plots
#' @param add_pvalues Whether to add P-values in the histogram plots
#'
#' @return A ggplot
#'
#' @return

plot_classif <- function(
  res, facets = NULL, bins = 15, fill = "seagreen", alpha = 0.5, ylim = c(0, 200),
  norm = 2, low = "white", high = "darkgreen", limits = c(0, 1), xmin = 0, xmax = 0.35,
  ymin = 120, ymax = 200, dfac = 500, rounding = 4, signif = 0.05, px = 1, py = 190,
  phjust = 1, type = "histogram", add_insets = TRUE, add_null = TRUE, add_pvalues = TRUE
) {

  library(tidyverse)

  p <- res$accu %>%
    ggplot(aes(x = accu)) +
    geom_histogram(bins = bins, fill = fill, alpha = alpha) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(x = "Generalization accuracy", y = "Count") +
    lims(x = c(0, 1), y = ylim)
  if (!is.null(facets)) p <- p + facet_wrap(. ~ get(facets))

  conf <- res$avg
  if (norm > 0) conf <- conf %>% map(~ .x %>% apply(., norm, function(x) x / sum(x)))
  conf <- conf %>% map_dfr(~ .x %>% data.frame %>% rownames_to_column("predicted") %>% gather_("true", "freq", colnames(.x)), .id = ".id")
  conf <- conf %>% mutate(accu = 1)
  if (!is.null(facets)) colnames(conf)[colnames(conf) == ".id"] <- facets

  if (type == "confusion") {

    p <- ggplot(conf, aes(x = true, y = predicted, fill = freq)) +
      geom_tile() +
      theme_bw() +
      scale_fill_gradient(low = low, high = high, limits = limits) +
      labs(x = "True", y = "Predicted", fill = "Frequency")
    if (!is.null(facets)) p <- p + facet_wrap(. ~ facets)
    return (p)

  }

  insets <- plot_insets(
    conf,
    plotfun = function(x) {
      p <- ggplot(x) +
        geom_tile(aes(x = true, y = predicted, fill = freq)) +
        theme_bw() +
        scale_fill_gradient(low = low, high = high, limits = limits) +
        scale_x_discrete(breaks = NULL) +
        scale_y_discrete(breaks = NULL) +
        labs(x = NULL, y = NULL) +
        theme(legend.position = "none")
    },
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    facets = facets
  )

  if (add_insets) p <- p + insets

  ngroups <- nlevels(factor(conf$true))
  null <- res$mean$ntest %>%
    map_dfr(~ data.frame(
      density = dbinom(seq(0, .x), size = .x, prob = 1 / ngroups),
      accu = seq(0, .x) / .x),
      .id = ".id")
  if (!is.null(facets)) colnames(null)[colnames(null) == ".id"] <- facets

  if (add_null) p <- p + geom_line(data = null, aes(x = accu, y = density * dfac), lty = 2)

  pround <- 1 / 10^rounding
  res$mean <- res$mean %>% mutate(
    plabel = round(pvalue, rounding) %>% paste("P =", .) %>%
      ifelse(pvalue < pround, paste("P <", format(pround, scientific = FALSE)), .) %>%
      ifelse(pvalue < signif, str_replace(., "$", "*"), .)
  )

  if (add_pvalues) p <- p + geom_text(data = res$mean, aes(label = plabel), x = px, y = py, hjust = phjust)

  return (p)
}
