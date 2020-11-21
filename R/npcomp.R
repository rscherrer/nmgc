#' Nested principal component analysis
#'
#' Perform principal component analyses across multiple subsets of the data
#'
#' @param data A data frame
#' @param variables The variables to analyze
#' @param center Whether to center the data
#' @param scale Whether to scale the data
#' @param nesting Optional nesting factor indicating the subsets. Defaults to NULL, where the output is that of a regular call to `prcomp`
#' @param combine If `nesting` is provided, whether to combine the results for the different subsets.
#' @param reduce If `combine` is TRUE, what principal components to retain from across the subsets.
#'
#' @return If `nesting` is NULL, the same as `prcomp`, otherwise, if `combine` is FALSE, a nested list of `prcomp` outputs, one for each subset. If `combine` is TRUE, a list of three data frames:
#' \itemize{
#' \item{`sdev`}{Contains the standard deviations along each principal component for each subset}
#' \item{`rotation`}{Contains the rotation matrices for each subset, bound by row}
#' \item{`x`}{The scores of each observation on each principal component}
#' }
#'
#' @export

npcomp <- function(
  data, variables, center = TRUE, scale = TRUE, nesting = NULL, combine = TRUE,
  reduce = NULL
) {

  library(tidyverse)

  if (is.null(nesting)) {
    data <- list(data) else data <- data %>% split(.[[nesting]])
  }
  data <- data %>%
    purrr::map(
      ~ .x %>%
        dplyr::select(dplyr::all_of(variables)) %>%
        prcomp(center = center, scale = scale)
    )

  if (is.null(nesting)) return(data[[1]])

  out <- data

  if (combine) {

    sdev <- data %>%
      purrr::map_dfr(
        ~ (.x$sdev / sum(.x$sdev))[reduce] %>% rbind %>% data.frame,
        .id = nesting
      )
    colnames(sdev) <- c(nesting, paste0("PC", reduce))
    rotation <- data %>%
      purrr::map_dfr(
        ~ .x$rotation[, reduce] %>%
          data.frame %>%
          tibble::rownames_to_column("variable"),
        .id = nesting
      )
    x <- data %>% purrr::map_dfr(~ .x$x[, reduce] %>% data.frame, .id = nesting)
    out <- list(sdev = sdev, rotation = rotation, x = x)

  }

  return(out)

}
