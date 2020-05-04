rm(list = ls())

library(nmgc)

data <- read.csv("data/reflectance.csv", header = TRUE)

# Group-comparison functions
classify(data, c("PC1", "PC2"), grouping = "habitat", to_pcomp = c("wl300", "wl350"))
