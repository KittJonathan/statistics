# Datanovia
# Biostatistics
# Assumptions
# Transforming Data to Normality in R: Log, Square-Root & Inverse
# https://www.datanovia.com/learn/biostatistics/assumptions/data-transformation-in-r

# Packages ----

library(rstatix)
library(ggpubr)
library(moments)

# Measure skewness ----

skewness(iris$Sepal.Length, na.rm = TRUE)

# Transformation methods ----

# Severity    Positive skew (right)   Negative skew (left)
# Moderate    sqrt(x)                 sqrt(max(x + 1) - x)
# Greater     log10(x)                log10(max(x + 1) - x)
# Severe      1 / x                   1 / (max(x + 1) - x)

# Two skewed variables ----

df <- USJudgeRatings
c(CONT = skewness(df$CONT), PHYS = skewness(df$PHYS))

ggarrange(
  ggdensity(df, x = "CONT", fill = "#3a86d4", title = "CONT (right-skewed)") +
    stat_overlay_normal_density(color = "red", linetype = "dashed"),
  ggdensity(df, x = "PHYS", fill = "#3a86d4", title = "PHYS (left-skewed)") +
    stat_overlay_normal_density(color = "red", linetype = "dashed"),
  ncol = 2
)

# Apply a log10 transformation

df$CONT <- log10(df$CONT)                       # positive skew: plain log
df$PHYS <- log10(max(df$PHYS + 1) - df$PHYS)    # negative skew: mirrored log

ggarrange(
  ggdensity(df, x = "CONT", fill = "#3a86d4", title = "CONT (log10)") +
    stat_overlay_normal_density(color = "red", linetype = "dashed"),
  ggdensity(df, x = "PHYS", fill = "#3a86d4", title = "PHYS (mirrored log10)") +
    stat_overlay_normal_density(color = "red", linetype = "dashed"),
  ncol = 2
)

c(CONT = skewness(df$CONT), PHYS = skewness(df$PHYS))
