#' Inset plots
#'
#' Make a list of inset plots suitable to add to some other (facetted) plot
#'
#' @param data A data frame
#' @param plotfun The function to be applied to (each facet of) the data frame to produce an inset plot. Make sure that you fix all arguments in the body of the function because this function only passed `data` to `plotfun`
#' @param xmin,xmax,ymin,ymax Coordinates of the inset plots on the main plots
#' @param facets The variable to facet by. Defaults to NULL for no facets.
#'
#' @return A list of plots
#'
#' @export


plot_insets <- function(data, plotfun, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, facets = NULL) {

  library(tidyverse)

  if (!is.null(facets)) data <- split(data, data[, facets]) else data <- list(data)

  data %>%
    purrr::map(~annotation_custom2(
      grob = ggplotGrob(plotfun(.)),
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax,
      data = .
    ))

}
