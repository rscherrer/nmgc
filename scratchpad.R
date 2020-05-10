data <- read.csv("../DewlapColorAnolis/data/reflectance.csv")

library(nmgc)

res <- nanova(data, c("wl300", "wl700"), grouping = "habitat", nesting = "island",
              random = "site")
res$anova

# Test homogeneity of variances across islands

test_covariance(data, c("wl300", "wl700"), grouping = "habitat", univariate = TRUE)
