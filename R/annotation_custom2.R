#' Inset plots
#'
#' Plot inset figures on another grob
#'
#' @param grob The grob
#' @param xmin,xmax,ymin,ymax Inset plot coordinates
#' @param data The underlying data
#'
#' Code for inset plotting from Clare West at https://www.blopig.com/blog/2019/08/combining-inset-plots-with-facets-using-ggplot2/
#'
#' @export

# This function allows us to specify which facet to annotate
annotation_custom2 <- function (
  grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data
) {
  ggplot2::layer(
    data = data,
    stat = ggplot2::StatIdentity,
    position = ggplot2::PositionIdentity,
    geom = ggplot2:::GeomCustomAnn,
    inherit.aes = TRUE,
    params = list(
      grob = grob, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax
    )
  )
}
