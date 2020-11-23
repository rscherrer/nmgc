# Generate example data

rm(list = ls())

library(tidyverse)

set.seed(42)

d <- expand_grid(plot = c(1, 2), grp = c("A", "B", "C"))

d <- d %>%
  group_by(plot, grp) %>%
  nest() %>%
  mutate(values = map(
    data,
    ~ tibble(x = rnorm(100), y = rnorm(100), z = rnorm(100)))
  ) %>%
  select(-data) %>%
  unnest(cols = c(values)) %>%
  ungroup()

write_csv(d, file = "data/data.csv")
