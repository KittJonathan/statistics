# Datanovia
# Biostatistics
# Assumptions
# Normality Test in R: Shapiro-Wilk &  Q-Q plots
# https://www.datanovia.com/learn/biostatistics/assumptions/normality-test-in-r

# Packages ----

library(rstatix)
library(ggpubr)

# Visual methods ----

## Density plot ----

library(ggpubr)

ggdensity(ToothGrowth, x = "len", fill = "#3a86d4", alpha = 0.35,
          color = "#3a86d4", xlab = "Tooth length")

## Q-Q plot ----

ggqqplot(ToothGrowth, x = "len", color = "#3a86d4")

## Normal vs Skewed ----

set.seed(123)
normal.data <- rnorm(200)  # symmetric, normal
skewed.data <- rexp(200)   # right-skewed

ggarrange(
  ggdensity(normal.data, title = "Normal: symmetric bell",
            fill = "#3a86d4", alpha = 0.35, color = "#3a86d4"),
  ggdensity(skewed.data, title = "Skewed: long right tail",
            fill = "#3a86d4", alpha = 0.35, color = "#3a86d4"),
  ggqqplot(normal.data, title = "Normal: points on the line", color = "#3a86d4"),
  ggqqplot(skewed.data, title = "Skewed: points curve off", color = "#3a86d4"),
  ncol = 2, nrow = 2
)

# The Shapiro-Wilk test ----

ToothGrowth |> shapiro_test(len)

shapiro.test(ToothGrowth$len)

# Grouped and multiple variables

ToothGrowth |> 
  group_by(dose) |> 
  shapiro_test(len)

iris |> 
  shapiro_test(Sepal.Length, Petal.Width)
