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
#' @param nperm Number of permutations in the randomization test. Use 0 to not conduct a randomization test.
#' @param minsize Minimum size required per group for a training data set
#' @param seed Optional random seed to reset at the beginning
#' @param importance Whether to perform sensitivity analysis on the input (takes a while)
#' @param getmachine Whether to return the machines (takes space)
#' @param verbose Whether to display messages
#' @param pb Whether to display progress bars
#' @param digest Whether to return the results in a summarized format. If FALSE, returns the raw results for each machine.
#' @param topcomp Variable to perform PCA on
#' @param pccenter Center the PCA
#' @param pcscale Scale the PCA
#' @param showconf Whether to show confusion matrices as insets on accuracy histograms
#' @param showbinom Whether to show a binomial null distribution on accuracy histograms
#' @param showpval Whether to show P-values on accuracy histograms
#' @param cnorm Integer indicating whether to normalize the confusion matrices on display so as to make rows sum to one (1), or columns (2), or neither (0).
#' @param clims Limits of the range of frequencies displayed in the confusion matrices
#' @param clow Color associated with the lowest frequency in confusion matrix heatmaps
#' @param chigh Color associated with the highest frequency in confusion matrix heatmaps
#' @param hbins Number of bins in the histogram of accuracy scores
#' @param hfill Color of the histogram of accuracy scores
#' @param halpha Transparency of the histogram of accuracy scores
#' @param cxlim Vector of two values containing the bounds of the inset confusion matrices along the horizontal axis
#' @param cylim Vector of two values containing the bounds of the inset confusion matrices along the vertical axis (in proportion of the height of the plot)
#' @param blty Line type for displaying the null binomial distribution
#' @param ptoshow What P-value to show on the histogram plots (either of "pbinom" for the binomial test or "prandom" for the randomization test)
#' @param prounding Number of decimal places to round P-values on display
#' @param psignif Significance level for P-values on display. An asterisk will be added to each significant P-value. Use zero to avoid displaying any asterisk.
#' @param px Horizontal location of the P-values
#' @param py Vertical location of the P-values
#' @param phjust Horizontal justification of the P-values (e.g. 1 to align them to the right, 0 to the left and 0.5 to center them)
#'
#' @return If `digest` is FALSE, this function returns a nested list of raw classification results on three levels. The first level is for each separate plot, or nesting level, in the nested analysis. The second level is for each replicate analysis within each plot. The third level is for each machine, i.e. each cross-validation bin within each replicate. This third level is itself a list with for each machine, the confusion matrix from the classification (`conf`), a vector of importance scores for each variable from the sensitivity analysis (`imp`, only if `importance` is TRUE) and the trained machine itself (`machine`, only if `getmachine` is TRUE). These are the raw results for each machine. If `digest` is TRUE, however, the function returns a summarized version of the results. The output is then a list with three fields. The first field is a summary table (`summary`) of the results with, for each nesting level, the mean accuracy score (`accu`), the sample size (`n`, the total number of points tested within each replicate), the proportion of the data used for testing (`ptest`, which depends on `k`), the number of points tested by each machine (`ntest`), the P-value from a binomial test assessing the significance of the average accuracy score (`pbinom`) and the P-value from an equivalent randomization test (`prandom`), where the null distribution is computed by training `nperm` replicates on permuted data. There are three additional list-columns with, for each nesting level, the average confusion matrix over all replicates (`conf`), a data frame of importance scores (`imp`) for each variable (in columns) for each machine (in rows), and a vector of acccuracy scores (`accus`) where the `nrep` first values are for the replicates and the remaining `nperm` were measured on randomized data. Note that accuracy scores are measured by summing the confusion matrices of all cross-validation bins into one, yielding one score per replicate.
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
  nperm = 0,
  minsize = 5,
  seed = NULL,
  importance = FALSE,
  getmachine = FALSE,
  verbose = TRUE,
  pb = TRUE,
  digest = TRUE,
  topcomp = NULL,
  pccenter = TRUE,
  pcscale = TRUE,
  showconf = TRUE,
  showbinom = TRUE,
  showpval = TRUE,
  cnorm = 2,
  clims = c(0, 1),
  clow = "white",
  chigh = "darkgreen",
  hbins = 30,
  hfill = "seagreen",
  halpha = 0.5,
  cxlim = c(0, 0.35),
  cylim = c(0.5, 0.8),
  blty = 1,
  prounding = 4,
  ptoshow = "prandom",
  psignif = 0.05,
  px = 1,
  py = 3,
  phjust = 1

) {

  #### Set up ####

  # Compute principal components if needed
  if (!is.null(topcomp)) {
    data <- data %>%
      cbind(
        npcomp(
          data, topcomp, pccenter, pcscale, nesting, combine = TRUE,
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
  labels <- unique(data[, grouping])

  nested <- TRUE

  # Artificial single nesting level if no nesting supplied
  if (is.null(nesting)) {
    nested <- FALSE
    data$nesting <- factor(1)
    nesting <- "nesting"
  }

  # Split the data into each nesting level
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

  #### Train the machines ####

  # For each nesting level...
  results <- thislapply1(data, function(data) {

    # For each replicate (and randomized) analysis...
    thislapply2(seq(nrep + nperm), function(i) {

      # Randomize the data if needed
      if (i > nrep) data[[grouping]] <- sample(data[[grouping]])

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

        # Fit the classifier
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

        } else stop("unknown method") # could add other methods here

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
        conf <- table(predictions, data[groups == j, grouping])

        if (!getmachine) machine <- NULL

        return (list(conf = conf, imp = imp, machine = machine))

      }) # for each cross-validation bin
    }) # for each replicate
  }) # for each plot

  # Early exit if we want the raw results
  if (!digest) return(results)

  #### Summarize the results ####

  # For each plot...
  digested <- purrr::map_dfr(results, function(replicates) {

    # List of confusion matrices per replicate
    confs <- purrr::map(replicates, function(kbins) {

      # For each replicate sum the matrices of each testing set (i.e. k-bin)
      Reduce('+', purrr::map(kbins, ~ .x$conf))

    })

    # Separate the randomized and empirical confusion matrices
    if (nperm > 0) {

      assertthat::assert_that(length(confs) == nrep + nperm)
      randoms <- confs[seq(nrep + 1, length(confs))]
      confs <- confs[seq(nrep)]
      replicates <- replicates[seq(nrep)]

    }

    # Get the average confusion matrix for the plot
    avg <- mavg(confs)

    # Vector of replicate accuracies
    accus <- purrr::map_dbl(confs, pdiag)

    # Mean accuracy (just accuracy if only one replicate)
    mean <- mean(accus)

    prandom <- NULL

    # P-value from randomized distribution
    if (nperm > 0) {

      # Null distribution of accuracy scores
      accu0 <- purrr::map_dbl(randoms, pdiag)

      # Compute randomization P-value
      prandom <- 1 - length(which(mean > accu0)) / nperm

    } else accu0 <- NULL

    # Compute binomial P-value
    n <- sum(confs[[1]])
    ntest <- floor(ptesting * n)
    pbinom <- 1 - pbinom(mean * ntest, ntest, 1 / length(labels))

    # Optional data frame with importance scores
    if (importance) {
      imp <- purrr::map_dfr(replicates, function(kbins) {
        purrr::map_dfr(kbins, ~ .x$imp)
      })
    } else imp <- NULL

    # Return a row for the digested data frame over plots
    tibble::tibble(
      accu = mean,
      n = n,
      ptest = ptesting,
      ntest = ntest,
      pbinom = pbinom,
      prandom = prandom,
      conf = list(avg),
      imp = list(imp),
      accus = list(c(accus, accu0))
    )

  })

  # Reformat the summary table
  digested$nesting <- names(data)
  digested <- digested[, c(ncol(digested), seq(2, ncol(digested) - 1))]

  #### Confusion matrix plot ####

  # Extract average confusion matrices per plot
  confusions <- digested$conf
  names(confusions) <- digested$nesting

  # Reshape them into a data frame
  confusions <- purrr::map_dfr(confusions, function(conf) {

    # Normalize the matrix if needed (by row or column)
    if (cnorm > 0) conf <- apply(conf, cnorm, function(x) x / sum(x))

    conf <- as.data.frame(as.table(conf))
    colnames(conf) <- c("predicted", "true", "freq")
    return(conf)

  })

  # Customize
  confusions$nesting <- rep(unique(digested$nesting), each = length(labels)^2)
  confusions <- confusions[, c(ncol(confusions), seq(ncol(confusions) - 1))]
  confusions <- tibble::tibble(confusions)
  confusions$accu <- 1 # hack

  p1 <- ggplot2::ggplot(
    confusions,
    ggplot2::aes(x = true, y = predicted, fill = freq)
  ) +
    ggplot2::geom_tile() +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_gradient(low = clow, high = chigh, limits = clims) +
    ggplot2::labs(x = "True", y = "Predicted", fill = "Frequency")

  # Split into facets if needed
  if (length(unique(confusions$nesting)) > 1) {

    p1 <- p1 + ggplot2::facet_wrap(. ~ nesting)

  }

  #### Accuracy plot ####

  # Prepare an extra column to identify each dataset
  dataset <- c(rep("Empirical", nrep), rep("Randomized", nperm))

  # Assemble vectors of accuracy scores into a data frame
  accuracies <- digested %>%
    dplyr::mutate(
      accus = purrr::map(accus, ~ tibble::tibble(accu = .x, dataset = dataset))
    ) %>%
    dplyr::select(nesting, accus) %>%
    tidyr::unnest(cols = c(accus))

  # Initialize the plot
  p2 <- ggplot2::ggplot(accuracies, ggplot2::aes(x = accu))

  if (nperm > 0) {

    # Show the distribution of empirical versus randomized accuracies
    p2 <- p2 +
      ggplot2::geom_histogram(
        mapping = ggplot2::aes(fill = dataset), bins = hbins, alpha = halpha,
        position = "nudge"
      )

  } else {

    # Or the distribution of empirical accuracies only
    p2 <- p2 + ggplot2::geom_histogram(
      bins = hbins, fill = hfill, alpha = halpha, position = "nudge"
    )

  }

  # Fine tuning of the plot
  p2 <- p2 +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid = ggplot2::element_blank()) +
    ggplot2::labs(x = "Accuracy", y = "Count") +
    ggplot2::lims(x = c(0, 1)) +
    ggplot2::labs(fill = NULL) +
    ggplot2::scale_fill_manual(values = c(hfill, "grey"))

  # Split into facets if needed
  if (length(unique(accuracies$nesting)) > 1) {

    p2 <- p2 + ggplot2::facet_wrap(. ~ nesting)

  }

  # Measure the maximum bin height
  ymax <- max(ggplot2::ggplot_build(p2)[["data"]][[1]][["count"]])

  if (showconf) {

    assertthat::assert_that(cxlim[1] <= cxlim[2])
    assertthat::assert_that(cylim[1] <= cylim[2])

    # Create inset confusion matrix plots
    insets <- plot_insets(
      confusions,
      plotfun = function(x) {
        p <- ggplot2::ggplot(x) +
          ggplot2::geom_tile(ggplot2::aes(x = true, y = predicted, fill = freq)) +
          ggplot2::theme_bw() +
          ggplot2::scale_fill_gradient(
            low = clow, high = chigh, limits = clims
          ) +
          ggplot2::scale_x_discrete(breaks = NULL) +
          ggplot2::scale_y_discrete(breaks = NULL) +
          ggplot2::labs(x = NULL, y = NULL) +
          ggplot2::theme(legend.position = "none")
      },
      xmin = cxlim[1],
      xmax = cxlim[2],
      ymin = cylim[1] * ymax,
      ymax = cylim[2] * ymax,
      facets = "nesting"
    )

    # Add insets to histograms
    p2 <- p2 + insets

  }

  if (showbinom) {

    # Make a data frame to plot binomial distributions
    binomial <- digested %>%
      dplyr::mutate(binom = purrr::map(n, function(n) {

        tibble::tibble(
          x = seq(0, n) / n,
          y = dbinom(seq(0, n), size = n, prob = 1 / length(labels))
        )

      })) %>%
      dplyr::select(nesting, n, binom) %>%
      tidyr::unnest(cols = c(binom))

    # Add the null binomial distribution to the histograms
    p2 <- p2 +
      ggplot2::geom_line(
        data = binomial,
        ggplot2::aes(x = x, y = y * ymax / max(y)),
        lty = blty,
        color = "darkgrey"
      )

  }

  if (showpval) {

    pround <- 1 / 10^prounding

    # Add a column to the results to display P-values in a nice format
    digested <- digested %>%
      dplyr::mutate(
        plabel = round(get(ptoshow), prounding),
        plabel = paste("P =", plabel),
        plabel = ifelse(
          get(ptoshow) < pround,
          paste("P <", format(pround, scientific = FALSE)),
          plabel
        ),
        plabel = ifelse(
          get(ptoshow) < psignif,
          stringr::str_replace(plabel, "$", "*"),
          plabel
        )
      )

    # Show P-values
    p2 <- p2 +
      ggplot2::geom_text(
        data = digested,
        ggplot2::aes(label = plabel), x = px, y = py, hjust = phjust
      )

  }

  # Prepare the output
  output <- list(summary = digested, confplot = p1, accuplot = p2)

  return(output)

}
