data <- read.csv("../DewlapColorAnolis/data/reflectance.csv")

library(nmgc)

variables <- paste0("wl", c(300, 400, 700))

nanova(data, variables, grouping = "habitat", nesting = "island",
       univariate = TRUE, parametric = TRUE, random = "site")
