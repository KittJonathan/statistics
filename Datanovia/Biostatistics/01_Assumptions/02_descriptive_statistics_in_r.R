# Datanovia
# Biostatistics
# Assumptions
# Descriptive Statistics in R
# https://www.datanovia.com/fr/learn/biostatistics/assumptions/descriptive-statistics-in-r

# Packages ----

library(rstatix)

# Central tendency ----

x <- iris$Sepal.Length

mean(x)   # arithmetic mean

median(x) # middle value (robust to outliers)

freq <- table(x)
as.numeric(names(freq))[which.max(freq)]  # mode: the most frequent value

# Variability ----

range(x)  # min and max

IQR(x)    # interquartile range (Q3 - Q1), robust spread

var(x)    # variance

sd(x)     # standard deviation (sqrt of variance)

# All at once: get_summary_stats() ----

iris |> get_summary_stats(Sepal.Length, type = "common")
