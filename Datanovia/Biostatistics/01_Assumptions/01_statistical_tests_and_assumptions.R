# Datanovia
# Biostatistics
# Assumptions
# Statistical Tests and Assumptions in R: Which Test Should I Use?
# https://www.datanovia.com/learn/biostatistics/assumptions/statistical-tests-and-assumptions

# Packages ----

library(rstatix)

# Check before you compute ----

# Test for normality 

ToothGrowth |> 
  group_by(supp) |> 
  shapiro_test(len)

# Test for equal variances

ToothGrowth |> 
  levene_test(len ~ supp)

