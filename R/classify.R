#' Classification analysis
#'
#' Perform a replicated classification analysis of a multivariate dataset into categorical labels using machine learning tools and k-fold cross validation
#'
#' @param data A data frame
#' @param variables The variables used to classify
#' @param grouping Name of the grouping variable (the labels)
#' @param nesting Optional nesting variable, if the analysis must be conducted separately on different subsets of the data
#' @param method The data mining model used. Currently supports "SVM" and "LDA".
#' @param k Number of bins for the k-fold cross-validation procedure
#' @param nrep Number of replicate analyses (i.e. number of k-fold cross validations)
#' @param minsize Minimum size required per group for a training data set
#' @param seed Optional random seed to reset at the beginning
#' @param importance Whether to perform sensitivity analysis on the input (takes a while)
#' @param return_machine Whether to return the machines (takes space)
#' @param verbose Whether to display messages
#' @param pb Whether to display progress bars
#' @param digest Whether to return the results in a summarized format
#' @param test Whether to test the results against random guessing using a binomial distribution
#' @param to_pcomp Variable to perform PCA on
#' @param center Center the PCA
#' @param scale Scale the PCA
#' @param add_signif Whether to add significance asterisk labels in an extra column
#'
#' @return A list containing the results. If `digest` is `FALSE`, a nested list on two levels with, for each replicate (first level) cross-validation bin (second level), i.e. for each machine, the following fields:
#' \itemize{
#' \item{`conf`}{ The confusion matrix resulting from testing the machine}
#' \item{`imp`}{ A vector of relative importance of input variable}
#' \item{`machine`}{ The machine itself}
#' }
#' or a list of such nested lists if `nesting` is provided, with one nested list per subset of the data.
#' If `digest` is TRUE, returns a list with the following fields:
#' \itemize{
#' \item{`mean`}{ A data frame with a column "accu" containing the mean accuracy across all machines on a single row, or accuracies over subsets of the data on multiple rows if `nesting` is provided. If `test` is `TRUE`, extra columns are added, containing the number of observations, "n", the proportion of observations used in testing the machines, "ptest", the number of observations in a testing set, "ntest", the binomial P-value assessing the significance of the obtained result, "pvalue".}
#' \item{`avg`}{ The average over replicates of the average confusion matrices over cross-validation bins, or a list of those if `nesting` is provided}
#' \item{`accu`}{ A vector of classification accuracy across all machines, or a data frame with a column for the nesting variable if `nesting` is provided}
#' \item{`confs`}{ A nested list on two levels of confusion matrices, per replicate and cross-validation bin, or a list of such nested lists if `nesting` is provided}
#' \item{`imp`}{ A data frame with the relative importance of each input variable for each machine, with an extra column with a nesting variable if `nesting` is provided}
#' }
#'
#' @export

classify <- function(
  data,
  variables,
  grouping,
  nesting = NULL,
  method = "SVM",
  k = 5,
  nrep = 1,
  minsize = 5,
  seed = NULL,
  importance = FALSE,
  return_machine = FALSE,
  verbose = TRUE,
  pb = TRUE,
  digest = TRUE,
  test = TRUE,
  to_pcomp = NULL,
  center = TRUE,
  scale = TRUE,
  add_signif = TRUE
) {

  library(rminer)
  library(MASS)
  library(assertthat)
  library(tidyverse)
  library(pbapply)

  if (!is.null(to_pcomp)) data <- data %>%
      cbind(
        npcomp(
          data, to_pcomp, center, scale, nesting, combine = TRUE,
          reduce = variables
        )$x
      )

  # Random seed
  if (!is.null(seed)) set.seed(seed)

  ptesting <- 1 / k

  assertthat::assert_that(nrow(data) > k)
  assertthat::assert_that(floor(ptesting * nrow(data)) > 0)

  # Define the possible labels
  labels <- unique(data[, grouping])

  nested <- TRUE

  if (is.null(nesting)) {
    nested <- FALSE
    data$nesting <- factor(1)
    nesting <- "nesting"
  }

  data <- data %>% split(f = .[, nesting])

  if (!verbose) pb <- FALSE
  thislapply1 <- thislapply2 <- lapply
  if (pb) {
    if (nested) {
      thislapply1 <- pbapply::pblapply
    } else {
      thislapply2 <- pbapply::pblapply
    }
  }

  results <- thislapply1(data, function(data) {

    thislapply2(seq(nrep), function(i) {

      is_fine <- FALSE

      while (!is_fine) {

        # Randomly assign data to k testing groups
        groups <- rep(seq_len(k), each = floor(ptesting * nrow(data)))
        if (length(groups) < nrow(data)) {
          groups <- c(groups, seq_len(nrow(data) - length(groups)))
        }
        assertthat::assert_that(length(groups) == nrow(data))
        groups <- sample(groups, replace = FALSE)

        # Check that each label is sufficiently represented within each training
        # group
        is_fine <- all(sapply(seq_len(k), function(j) {

          represented <- table(data[which(groups != j), grouping])
          if (!all(labels %in% names(represented))) return (FALSE)
          return (all(represented > minsize))

        }))

      }

      # For each testing group...
      lapply(seq_len(k), function(j) {

        # Sample indices for a training dataset
        training <- which(groups != j)

        assertthat::assert_that(length(training) < nrow(data))

        # Downsample the training dataset to the size of the least represented
        # label
        targetsize <- min(table(data[training, grouping]))
        training <- do.call(
          "c",
          lapply(
            labels,
            function(lab) {
              sample(
                training[data[training, grouping] == lab], targetsize,
                replace = FALSE
              )
            }
          )
        )

        assertthat::assert_that(
          all(table(data[training, grouping]) == targetsize)
        )

        # Set up model formula
        model <- as.formula(
          paste(grouping, "~", paste(variables, collapse = " + "))
        )

        if (method == "SVM") {

          # Fit a support vector machine to the data
          machine <- rminer::fit(
            model, data = data[training, c(variables, grouping)], model = "svm",
            kernel = "rbfdot", task = "class"
          )

        } else if (method == "LDA") {

          # Or a linear discriminant analysis
          machine <- MASS::lda(
            formula = model, data = data[training, c(variables, grouping)]
          )

        } else stop("unknown method")

        # Sensitivity analysis of the fitted model (early exit)
        imp <- NULL
        if (importance) {
          if (method == "LDA") {
            imp <- rminer::Importance(
              machine, data = data[training, c(variables, grouping)],
              PRED = function(M, data) predict(M, data)$class
            )
          } else  if (method == "SVM") {
            imp <- rminer::Importance(
              machine, data = data[training, c(variables, grouping)]
            )
          }
          imp <- imp$imp[seq_along(variables)]
          names(imp) <- variables
        }

        # Predict the labels of the remaining data
        predictions <- predict(machine, newdata = data[groups == j, variables])

        if (method == "LDA") predictions <- predictions$class

        # Compare true and predicted labels
        conf <- table(predictions, data[groups == j, grouping])

        if (!return_machine) machine <- NULL

        return (list(conf = conf, imp = imp, machine = machine))

      })
    })
  })

  if (digest) {

    confs <- results %>% purrr::map(~ purrr::map(.x, ~ purrr::map(.x, "conf")))
    avg <- confs %>% purrr::map(~ mavg(.x %>% purrr::map(mavg)))
    accu <- confs %>%
      purrrr::map(~ do.call("c" , .x)) %>%
      purrr::map_dfr(~ purrr::map_dbl(.x, pdiag)) %>%
      tidyr::gather(key = "nesting", value = "accu")
    mean <- accu %>%
      dplyr::group_by(nesting) %>%
      dplyr::summarize(accu = mean(accu))

    colnames(accu)[colnames(accu) == "nesting"] <- nesting
    colnames(mean)[colnames(mean) == "nesting"] <- nesting

    if (importance) {
      imp <- results %>%
        purrr::map(
          ~ .x %>%
            purrr::map_dfr(
              ~ do.call(
                "rbind", .x %>% purrr::map(~ purrr::pluck(.x, "imp"))
              ) %>%
                data.frame()
            )
        ) %>%
        purrr::map2_dfr(names(.), ~ .x %>% dplyr::mutate(nesting = .y))
      colnames(imp)[colnames(imp) == "nesting"] <- nesting
    } else imp <- NULL

    if (test) {
      mean <- mean %>%
        dplyr::mutate(
          n = purrr::map_int(data, nrow),
          ptest = ptesting,
          ntest = floor(ptest * n),
          pvalue = 1 - pbinom(
            accu * ntest, size = ntest, prob = 1 / length(labels)
          )
        )
      if (add_signif) mean <- mean %>% add_signif()
    }

    return (list(mean = mean, avg = avg, accu = accu, confs = confs, imp = imp))

  }

  return (results)

}
