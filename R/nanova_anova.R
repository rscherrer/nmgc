#' Multiple ANOVAs

nanova_anova <- function(data, variables, random = NULL) {

  library(nlme)
  library(MuMIn)

  map_dfr(variables, function(variable, data) {

    data$X <- data[[variable]]

    # Fit candidate models with different variance structures

    # Note: when using model fitting functions inside another function, avoid
    # passing them formulas as objects programatically, that will cause the anova.lme
    # call to crash. Instead create extra columns in the data frame with specific names
    # and explicitly pass those columns into the model fitting function

    # Regular ANOVA
    mod1 <- gls(X ~ group, data = data)

    # ANOVA with one variance per group
    mod2 <- gls(
      X ~ group, data = data,
      weights = varIdent(form = ~ 1 | group)
    )
    models <- list(mod1, mod2)

    # Optional equivalents with one added random effect
    if (!is.null(random)) {
      mod3 <- lme(
        X ~ group,
        data = data,
        random = formula(paste("~ 1 |", random))
      )
      mod4 <- lme(
        X ~ group,
        data = data,
        weights = varIdent(form = ~ 1 | group),
        random = formula(paste("~ 1 |", random))
      )
      models[[3]] <- mod3
      models[[4]] <- mod4
    }

    # Compare the AICc of the models
    aiccs <- unlist(do.call("AICc", models))
    dfs <- unname(aiccs[grep("df", names(aiccs))])
    aiccs <- unname(aiccs[grep("AICc", names(aiccs))])
    best <- which(aiccs == min(aiccs))
    best_mod <- models[[best]]
    best_df <- dfs[best]
    best_aicc <- aiccs[best]
    deltas <- aiccs - aiccs[1]
    best_delta <- deltas[best]
    weights <- exp(-0.5 * deltas)
    weights <- weights / sum(weights)
    best_weight <- weights[best]

    # AIC weights from Burnham, K. P., and D. R. Anderson. 2002. Model selection and multimodel inference : a practical information-theoretic approach. Springer, New York.

    # Refit the model with maximum likelihood (instead of REML)
    best_mod <- update(best_mod, method = "ML")

    # Perform the analysis of variance using likelihood ratio test
    best_mod0 <- update(best_mod, X ~ 1) # null model
    anova_table <- anova(best_mod, best_mod0) %>% unlist()

    # Assemble the output we want to display
    dflrt <- anova_table[["df1"]] - anova_table[["df2"]]
    loglik <- anova_table[["logLik1"]]
    lratio <- anova_table[["L.Ratio2"]]
    pvalue <- anova_table[["p-value2"]]

    # Prepare output
    data.frame(
      best_fit = best,
      df_model = best_df,
      AICc = best_aicc,
      dAICc = best_delta,
      AICcw = best_weight,
      df_LRT = dflrt,
      loglik = loglik,
      lratio = lratio,
      pvalue = pvalue
    )

  }, data, .id = "variable")

}
