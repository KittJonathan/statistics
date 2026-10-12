# Simplistics YouTube channel
# Using lme4 in R for Mixed Models
# https://www.youtube.com/watch?v=h5FmH08QIr4

# Packages ----

library(lme4)
library(tidyverse)

# Data ----

d_original <- read_csv("Simplistics/data/depression_wide.csv")

# Wide to long format
d <- d_original |> 
  pivot_longer(cols = depression_1:stress_4,
               names_to = c(".value", "Time"),
               names_pattern = "(.+)_(.+)")
head(d)

# Mixed Model Notation
# Four elements to identify
# 1) Outcome variable
# 2) Fixed predictors
# 3) Random predictors
# 4) Cluster variable