# Datanovia
# Biostatistics
# Comparing Two Groups
# Cohen’s d Effect Size in R: Formula, Interpretation & Calculation
# https://www.datanovia.com/learn/biostatistics/two-groups/cohens-d-effect-size

# Packages ----

library(rstatix)
library(ggpubr)

# The data ----

data("ToothGrowth")
head(ToothGrowth, 3)

ggboxplot(
  ToothGrowth, x = "supp", y = "len",
  color = "supp", palette = "jco",
  add = c("jitter", "mean"), xlab = "Supplement", ylab = "Tooth length"
)

# Cohen's d for an independent t-test ----

sd_pooled <- ToothGrowth |> 
  dplyr::mutate(diff_sq = (len - mean(len))^2,
         .by = supp) |> 
  dplyr::mutate(sd_pooled = sqrt(sum(diff_sq / 58))) |> 
  dplyr::distinct(sd_pooled) |> 
  dplyr::pull()

mean_OJ <- ToothGrowth |> 
  dplyr::filter(supp == "OJ") |> 
  dplyr::summarise(mean_OJ = mean(len)) |> 
  dplyr::pull()

mean_VC <- ToothGrowth |> 
  dplyr::filter(supp == "VC") |> 
  dplyr::summarise(mean_VC = mean(len)) |> 
  dplyr::pull()
  
(mean_OJ - mean_VC) / sd_pooled

ToothGrowth |> 
  cohens_d(len ~ supp, var.equal = TRUE)

# Hedge's correction for small samples :
# Divide Cohen's d by (N - 3) / (N - 2.25) where N = n1 + n2
# Small samples: N < 50

ToothGrowth |> 
  cohens_d(len ~ supp, var.equal = TRUE,
           hedges.correction = TRUE)

# Welch version 
var_OJ <- ToothGrowth |> 
  dplyr::filter(supp == "OJ") |> 
  dplyr::summarise(var_OJ = var(len)) |> 
  dplyr::pull()

var_VC <- ToothGrowth |> 
  dplyr::filter(supp == "VC") |> 
  dplyr::summarise(var_VC = var(len)) |> 
  dplyr::pull()

(mean_OJ - mean_VC) / (sqrt((var_OJ + var_VC) / 2))

ToothGrowth |> 
  cohens_d(len ~ supp, var.equal = FALSE)

# Cohen's d for a one-sample t-test ----

# d = (m - mu) / s

(mean(ToothGrowth$len) - 0) / sd(ToothGrowth$len)

ToothGrowth |> 
  cohens_d(len ~ 1, mu = 0)

# Cohen's d for a paired test ----

# d = mean(diff) / sd(diff)

diffs <- ToothGrowth$len[1:30] - ToothGrowth$len[31:60]
mean(diffs) / sd(diffs)


ToothGrowth |> 
  cohens_d(len ~ supp, paired = TRUE)
