# Datanovia
# Biostatistics
# Assumptions
# Homogeneity of Variance Test in R: F-test, Bartlett, Levene, Fligner
# https://www.datanovia.com/learn/biostatistics/assumptions/homogeneity-of-variance-in-r

# Packages ----

library(rstatix)
library(ggpubr)

# F-test: compare two variances ----

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
var.test(len ~ supp, data = ToothGrowth)

# Compare several variances ----

## Bartlett's test ----

bartlett.test(weight ~ group, data = PlantGrowth)

# Collapse two grouping factors using interaction():
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
bartlett.test(len ~ interaction(supp, dose), data = ToothGrowth)

## Levene's test ----

# one grouping factor
PlantGrowth |> levene_test(weight ~ group)

# two grouping factors and their interaction
ToothGrowth |> levene_test(len ~ supp * dose)

## Fligner-Killeen’s test ----

fligner.test(weight ~ group, data = PlantGrowth)

PlantGrowth |> fligner_test(weight ~ group)
