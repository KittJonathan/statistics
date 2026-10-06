# Datanovia
# Biostatistics
# Comparing Two Groups
# T-test in R: One-Sample, Independent (Student & Welch) & Paired
# https://www.datanovia.com/learn/biostatistics/two-groups/t-test-in-r

# Packages ----

library(rstatix)
library(ggpubr)

# Independent two-samples t-test ----

data("genderweight", package = "datarium")

# a couple of random rows per group
set.seed(123)
genderweight |> sample_n_by(group, size = 2)

genderweight |> 
  group_by(group) |> 
  get_summary_stats(weight, type = "mean_sd")

ggboxplot(
  genderweight, x = "group", y = "weight",
  color = "group", palette = c("#3a86d4", "#8338ec"),
  add = "jitter", xlab = "Group", ylab = "Weight"
)

# Welch t-test (the default — does NOT assume equal variances)
stat.test <- genderweight |> 
  t_test(weight ~ group) |> 
  add_significance()

stat.test

# add confidence intervals
genderweight |> 
  t_test(weight ~ group, detailed = TRUE)

# Run the classic Student test
# Student t-test (assumes equal variances)
genderweight |> t_test(weight ~ group, var.equal = TRUE)

# Base R Welch (default)
t.test(weight ~ group, data = genderweight)

# Run the test and plot the results

stat.test <- genderweight |> 
  t_test(weight ~ group) |> 
  add_significance() |> 
  add_xy_position(x = "group")

ggboxplot(
  genderweight, x = "group", y = "weight",
  color = "group", palette = c("#3a86d4", "#8338ec"),
  add = c("jitter", "mean"), xlab = "Group", ylab = "Weight"
) +
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

# One sample t-test ----

data("mice", package = "datarium")

mice |> get_summary_stats(weight, type = "mean_sd")

stat.test <- mice |> t_test(weight ~ 1, mu = 25)
stat.test

t.test(mice$weight, mu = 25)

ggboxplot(
  mice$weight, width = 0.5, add = c("mean", "jitter"),
  ylab = "Weight (g)", xlab = FALSE, color = "#3a86d4"
) +
  geom_hline(yintercept = 25, linetype = "dashed", color = "#8338ec") +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

# Paired t-test ----

data("mice2", package = "datarium")
head(mice2, 3)

# wide → long: gather the before and after values into one column
mice2.long <- data.frame(
  id     = factor(rep(mice2$id, times = 2)),
  group  = factor(rep(c("before", "after"), 
                      each = nrow(mice2)), 
                  levels = c("before", "after")),
  weight = c(mice2$before, mice2$after)
)
head(mice2.long, 3)

mice2.long |> 
  group_by(group) |> 
  get_summary_stats(weight, type = "mean_sd")

stat.test <- mice2.long |> 
  t_test(weight ~ group, paired = TRUE) |> 
  add_significance()
stat.test

# in base R
t.test(mice2$before, mice2$after, paired = TRUE)

# Test + plot
stat.test <- mice2.long |> 
  t_test(weight ~ group, paired = TRUE) |> 
  add_significance() %>%
  add_xy_position(x = "group")

ggpaired(
  mice2.long, x = "group", y = "weight",
  order = c("before", "after"),
  color = "group", palette = c("#3a86d4", "#8338ec"),
  line.color = "gray", line.size = 0.4,
  xlab = "Group", ylab = "Weight"
) +
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

# Check the assumptions ----

## Outliers ----

genderweight |> 
  group_by(group) |> 
  identify_outliers(weight)

## Normality - Shapiro + QQ ----

genderweight |> 
  group_by(group) |> 
  shapiro_test(weight)

ggqqplot(genderweight, x = "weight", facet.by = "group")

## Equal variances - for the Student test ----

genderweight |> 
  levene_test(weight ~ group)

# Report the effect size (Cohen's d) ----

# Effect size for the two-group comparison (Welch version)
genderweight |> 
  cohens_d(weight ~ group, var.equal = FALSE)
