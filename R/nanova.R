#' Nested ANOVA
#'
#' Performs (M)ANOVAs for multiple variables across different subsets of the data. Models fitted with OLS and GLS (allowing one independent residual variance per group) are compared with AIC and the best-fitting model is retained. Multiple post-hoc comparisons can be performed too.
#'
#' @param data A data frame
#' @param variables The variables to analyze
#' @param nesting The name of the nesting factor
#' @param grouping The name of the grouping factor
#' @param posthoc Whether to perform post-hoc tests
#' @param to_pcomp Optional variables to perform PCA on
#' @param center,scale Parameters for `npcomp`
#' @param test Test to use for the MANOVA (defaults to Pillai's trace). See `?summary.manova`
#' @param univariate Whether to perform ANOVA (TRUE) or MANOVA (FALSE)
#' @param add_signif Whether to add significance asterisk labels in an extra column
#' @param parametric Whether to perform parametric tests (linear models instead of Kruskal-Wallis if `univariate` is TRUE, parametric instead of semi-parametric MANOVA if `univariate` is FALSE)
#' @param seed Optional seed for the semi-parametric MANOVA
#' @param iter Number of iterations for the parametric bootstrapping of the semi-parametric MANOVA (defaults to the recommended number 1,000)
#' @param random Optional random effect for fitting a mixed model in univariate ANOVA
#' @param pthreshold Threshold P-value to keep posthoc tests (set to 1 to keep all tests)
#'
#' @details The analysis of variance is performed using a likelihood ratio test between a model including the factor of interest and a null model with intercept only. The LRT is done with models fitted with maximum likelihood. Model comparison between OLS and GLS is done with models fitted with restricted maximum likelihood, that include the factor to be tested (as per Zuur et al. 2009).
#'
#' @note Note that `posthoc` will be set to FALSE automatically if `univariate` is FALSE. We do not do multivariate contrasts.
#'
#' @return A list with two objecst: `res`, containing the results of the variable-wise ANOVA tests, and `ph`, containing the results of the contrast-wise posthoc tests. `res` can be of multiple formats. If `manova` is FALSE, data frame containing the results of the ANOVAs for each variable and each subset of the data, including:
#'
#' \itemize{
##'  \item{best}{ The best-fitting model (OLS or GLS)}
##'  \item{df_model}{ Model degrees of freedom}
##'  \item{AICc}{ AICc of the model}
##'  \item{dAICc}{ Difference in AICc between the GLS-model and the OLS-model}
##'  \item{df_LRT}{ Degrees of freedom of the likelihood ratio test}
##'  \item{loglik}{ Log-likelihood of the full model in LRT}
##'  \item{lratio}{ Chi-square value i.e. likelihood ratio between the full and the null model in LRT}
##'  \item{pvalue}{ P-value of the LRT}
##'  \item{posthoc_test}{ Post-hoc test used: Wilcoxon or Tukey}
##'  \item{posthoc_p...}{ Extra columns with the P-values for each contrast in the multiple post-hoc comparisons}
##' }
#'
#' Otherwise, a data frame with MANOVA results for each subset, including, if `semiparametric` is FALSE,
#'
#' \itemize{
#' \item{df}{ The degrees of freedom of the MANOVA}
#' \item{"statistic's name"}{ One of the statistics defined in `summary.manova` (Pillai, Wilks, etc.)}
#' \item{num_df}{ Numerator degrees of freedom of the F-test}
#' \item{denom_df}{ Denominator degrees of freedom of the F-test}
#' \item{pseudoF}{ Approximate F-statistic}
#' \item{pvalue}{ P-value of the F-test}
#' }
#'
#' otherwise, the results of a semi-parametric MANOVA, with:
#'
#' \itemize{
#' \item{term}{ The term being tested, probably the grouping variable}
#' \item{MATS}{ The modified ANOVA-type statistic value}
#' \item{pvalue}{ P-value computed from the parametric bootstrap resampling}
#' }
#'
#' @export

nanova <- function(
  data,
  variables,
  grouping,
  nesting = NULL,
  univariate = TRUE,
  posthoc = TRUE,
  to_pcomp = NULL,
  center = TRUE,
  scale = TRUE,
  test = "Pillai",
  add_signif = TRUE,
  parametric = TRUE,
  seed = NULL,
  iter = 1000,
  random = NULL,
  pthreshold = 0.05
) {

  library(tidyverse)

  # Compute principal components if needed
  if (!is.null(to_pcomp)) data <- data %>%
      cbind(npcomp(
        data, to_pcomp, center, scale, nesting, combine = TRUE,
        reduce = variables
      )$x %>% data.frame %>% dplyr::select(-nesting))

  # Nested or unnested design
  if (is.null(nesting)) {
    data$nesting <- factor(1)
    nesting <- "nesting"
  } else data$nesting <- data[[nesting]]

  # Special column for grouping
  data$group <- data[, grouping]

  # Variables must be named
  names(variables) <- variables

  # Choose the function to run
  if (univariate) {
    if (parametric) {
      this_test <- function(x, y) nanova_anova(x, y, random = random)
    } else {
      this_test <- nanova_kruskal
    }
  } else {
    if (parametric) {
      this_test <- function(x, y) nanova_manova(x, y, test = test)
    } else {
      this_test <- function(x, y) nanova_smanova(x, y, seed = seed, iter = iter)
    }
  }

  # Perform analysis on each subset
  res <- data %>%
    group_by(nesting) %>%
    nest() %>%
    mutate(test = map(data, this_test, variables)) %>%
    dplyr::select(-data) %>%
    unnest(cols = c(test))

  # Add significance labels
  if (add_signif) res <- res %>% add_signif()

  if (!univariate) posthoc <- FALSE # we don't do multivariate posthoc tests around here
  ph <- NULL
  if (posthoc) {

    # Choose what post-hoc test to perform
    phtest <- res
    if (parametric) {
      phtest <- phtest %>% mutate(test = ifelse(best_fit %% 2 == 0, "dunnett", "tukey"))
    } else {
      phtest <- phtest %>% mutate(test = "nemenyi")
    }
    phtest <- phtest %>% select(nesting, variable, pvalue, test)

    # Perform posthoc tests
    ph <- data %>%
      gather_("variable", "score", variables) %>%
      group_by(nesting, variable) %>%
      nest() %>%
      right_join(phtest) %>%
      mutate(posthoc = map2(data, test, nanova_posthoc)) %>%
      filter(pvalue < pthreshold) %>%
      select(-data, -pvalue) %>%
      unnest(cols = c(posthoc))
    if (nrow(ph) > 0 & add_signif) ph <- ph %>% add_signif()

  }

  res <- list(res = res, ph = ph)

  return (res)

}
