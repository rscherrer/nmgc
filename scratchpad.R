data <- read.csv("../DewlapColorAnolis/data/reflectance.csv")

library(nmgc)

res <- nanova(data, c("wl300", "wl700"), grouping = "habitat", nesting = "island",
              random = "site")
res$anova
