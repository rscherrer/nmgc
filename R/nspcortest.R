#' Nest spatial autocorrelation
#'
#' Perform multiple spatial autocorrelation tests within subsets of the data (e.g. islands) using permutations of individual observations. Particularly suited to designs with few sampling sites per subset.
#'
#' @param data A data frame
#' @param variables The variables to analyze
#' @param nesting Optional nesting factor
#' @param nperm Number of permutations
#' @param seed Optional random seed
#' @param lon,lat Longitude and latitude columns
#' @param verbose Whether to display messages
#' @param pb Whether to display progress bars
#' @param to_pcomp Variable to perform PCA on
#' @param center,scale Parameters for `npcomp`
#' @param keep Optional factor to keep in the `sites` data frame returned
#' @param add_signif Whether to add significance asterisk labels in an extra column
#'
#' @return A list with two data frames: one with the results of the correlation test on each subset, including observed Pearson's correlation, P-values and number of sites, and the second with the geographical coordinates and means of each trait for every site in the data.
#'
#' @export

nspcortest <- function(
  data, variables, nesting = NULL, nperm = 1000, seed = NULL, lon = "longitude",
  lat = "latitude", verbose = TRUE, pb = TRUE, to_pcomp = NULL, center = TRUE,
  scale = TRUE, keep = NULL, add_signif = TRUE
) {

  # Spatial autocorrelation by permutation test

  library(geosphere)
  library(tidyverse)
  library(pbapply)

  if (!is.null(to_pcomp)) data <- data %>%
      cbind(npcomp(
        data, to_pcomp, center, scale, nesting, combine = TRUE,
        reduce = variables
      )$x %>% data.frame %>% dplyr::select(-nesting))

  # Random seed
  if (!is.null(seed)) set.seed(seed)

  if (!verbose) pb <- FALSE
  if (pb) thissapply <- pbsapply else thissapply <- sapply

  if (is.null(nesting)) {
    data$nesting <- factor(1)
    nesting <- "nesting"
  }

  # Sampling site data (return this one)
  sites <- data %>%
    group_by_at(c(nesting, lon, lat, keep)) %>%
    dplyr::select(variables) %>%
    summarize_all(mean)

  out <- sites

  # Split the datasets by islands
  data <- data %>% split(f = .[, nesting])
  sites <- sites %>% split(f = .[, nesting])

  if (verbose) message("Testing spatial autocorrelation...")

  # For each island...
  res <- list(data, sites) %>%
    pmap_dfr(function(data, sites) {

      if (verbose) message(data[1, nesting])

      # Calculate geographic distances between sites
      geodist <- c(as.dist(distm(sites[, c(lon, lat)], fun = distGeo)))

      # Phenotypic distance between sites
      phedist <- c(as.dist(dist(sites[, variables])))

      # Pearson's correlation coefficient
      robs <- cor(geodist, phedist)

      # For each permutation...
      rperms <- pbsapply(seq_len(nperm), function(i) {

        # Reshuffle individuals across sites
        permuted <- sample(nrow(data), nrow(data), replace = FALSE)
        data$site <- data$site[permuted]

        # Recompute the average phenotype per site
        sites <- data %>%
          group_by(site) %>%
          summarize_at(variables, mean)

        # Recompute the phenotypic distance between sites
        phedist <- c(as.dist(dist(sites[, variables])))

        # Calculate the correlation between locations and randmized phenotypes
        cor(geodist, phedist)

      })

      # Compute the P-value
      pvalue <- length(which(rperms > robs)) / nperm

      data.frame(robs = robs, pvalue = pvalue)

    }, .id = nesting) %>%
    mutate(nsites = sapply(sites, nrow))

  if (add_signif) {
    res <- res %>% add_signif()
  }

  out <- list(res = res, sites = out)

}
