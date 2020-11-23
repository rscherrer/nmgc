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
  to_pcomp = NULL,
  center = TRUE,
  scale = TRUE,
  add_signif = TRUE
) {

  # Convert the dataset into a tibble if it is not already one
  if (!inherits(data, "tbl")) data <- tibble::as_tibble(data)

  # Compute principal components if needed
  if (!is.null(to_pcomp)) {
    data <- data %>%
      cbind(
        npcomp(
          data, to_pcomp, center, scale, nesting, combine = TRUE,
          reduce = variables
        )$x
      )
  }

  # Random seed
  if (!is.null(seed)) set.seed(seed)

  # Proportion of observations in the testing set
  ptesting <- 1 / k

  assertthat::assert_that(nrow(data) > k)
  assertthat::assert_that(floor(ptesting * nrow(data)) > 0)

  # Define the possible labels
  labels <- unique(data[[grouping]])

  nested <- TRUE

  # Artificial single nesting level if no nesting supplied
  if (is.null(nesting)) {
    nested <- FALSE
    data$nesting <- factor(1)
    nesting <- "nesting"
  }

  # Split the data into nesting levels
  data <- data %>% split(f = .[, nesting])

  # Decide on a looping function depending on whether we want a progress bar
  if (!verbose) pb <- FALSE
  thislapply1 <- thislapply2 <- lapply
  if (pb) {
    if (nested) {
      thislapply1 <- pbapply::pblapply
    } else {
      thislapply2 <- pbapply::pblapply
    }
  }

  # For each nesting level...
  machines <- thislapply1(data, function(data) {

    # For each replicate...
    thislapply2(seq(nrep), function(i) {

      is_fine <- FALSE

      # Sample the training and testing sets until valid
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

        # Sensitivity analysis of the fitted model
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
        predictions <- rminer::predict(
          machine, newdata = data[groups == j, variables]
        )

        if (method == "LDA") predictions <- predictions$class

        # Compare true and predicted labels
        conf <- table(predictions, data[[grouping]][groups == j])

        if (!return_machine) machine <- NULL

        # Return the confusion matrix, the results of the importance analysis
        # and the machine itself, if needed
        return (list(confmat = conf, importance = imp, machine = machine))

      }) # end of cross-validation bin
    }) # end of replicate
  }) # end of nesting level

  # Prepare a data frame with results for each machine
  res <- expand_grid(lvl = names(data), repl = seq(nrep), kbin = seq(k))

  # Fill in that data frame with the output of each machine
  res <- res %>%
    group_by(lvl, repl, kbin) %>%
    nest() %>%
    mutate(

      # Confusion matrix
      confmat = pmap(
        list(lvl, repl, kbin), ~ pluck(machines, ..1, ..2, ..3)$confmat
      ),

      # Vector of importance scores
      importance = pmap(
        list(lvl, repl, kbin), ~ pluck(machines, ..1, ..2, ..3)$importance
      ),

      # Fitted machine
      machine = pmap(
        list(lvl, repl, kbin), ~ pluck(machines, ..1, ..2, ..3)$machine
      )

    ) %>%
    select(-data) %>%
    ungroup() %>%
    mutate(
      accuracy = map_dbl(confmat, pdiag),
      ntested = map_int(confmat, sum)
    )

  # If the results of many machines must be summarized...
  if (digest) {

    # Summarize accuracy across machines for each replicate
    res <- res %>%
      group_by(lvl, repl) %>%
      nest() %>%
      mutate(

        # Summed confusion matrix
        confmat = map(data, ~ Reduce('+', .x$confmat)),

        # Total accuracy
        accuracy = map_dbl(confmat, pdiag),

        # Number of tested points (= sample size of each nesting level)
        ntested = map_int(confmat, sum),

        # Is accuracy greater than expected by chance? (one-tailed binomial)
        pvalue = map2_dbl(
          confmat, ntested,
          ~ binom.test(
            x = sum(diag(.x)),
            p = 1 / length(labels),
            n = .y,
            alternative = "greater"
          )$p.value
        )

      ) %>%
      ungroup() %>%
      select(-data)

    # Summarize the results over replicates for each nesting level
    # P-values are combined using the Z-transform test
    res <- res %>%
      mutate(

        # Convert one-tailed P-values into Z-scores
        zvalue = qnorm(p = pvalue, mean = 0, sd = 1)

      ) %>%
      group_by(lvl) %>%
      nest() %>%
      mutate(

        # Average confusion matrix
        confmat = map(data, ~ mavg(.x$confmat)),

        # Average accuracy
        mean = map_dbl(data, ~ mean(.x$accuracy)),

        # Standard error of accuracy
        stderr = map_dbl(data, ~ sqrt(var(.x$accuracy) / n())),

        # Stouffer's Z-statistic
        zs = map_dbl(data, ~ sum(.x$zvalue) / sqrt(n())),

        # Combined P-value over replicates
        pcombined = map_dbl(zs, pnorm, mean = 0, sd = 1)

      ) %>%
      select(-data) %>%
      ungroup()

  }

  return (res)

}
