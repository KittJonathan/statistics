# =============================================================================
# LOGISTIC REGRESSION EXPLAINED with the palmerpenguins dataset
# Goal: predict a penguin's sex (female / male)
# =============================================================================
#
# THE IDEA
# --------
# Linear regression predicts a number that can be anything (-Inf, +Inf).
# But for a binary outcome (female = 0, male = 1) we want a PROBABILITY,
# which must stay between 0 and 1.
#
# Logistic regression solves this in 3 steps:
#
#   1. Build a linear score (the "log-odds" or "logit"):
#         z = b0 + b1*x1 + b2*x2 + ...
#
#   2. Squash that score into (0, 1) with the logistic (sigmoid) function:
#         p = 1 / (1 + exp(-z))
#
#   3. Classify: predict "male" if p > 0.5 (i.e. if z > 0), otherwise "female".
#
# Equivalent view in terms of odds:
#         odds = p / (1 - p)           (e.g. p = 0.8  ->  odds = 4 to 1)
#         log(odds) = z                (the logit link)
#
# The coefficients b0, b1, ... are found by MAXIMUM LIKELIHOOD: we pick the
# values that make the observed data (who is actually male/female) most
# probable under the model.
# =============================================================================

# install.packages(c("palmerpenguins", "dplyr", "ggplot2"))  # if needed
library(palmerpenguins)
library(dplyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. DATA PREPARATION
# -----------------------------------------------------------------------------
# Keep rows with known sex and the variables we need.
# Body mass is converted to kg so coefficients are easier to read.
df <- penguins %>%
  filter(!is.na(sex), !is.na(body_mass_g), !is.na(bill_depth_mm),
         !is.na(flipper_length_mm), !is.na(bill_length_mm)) %>%
  mutate(body_mass_kg = body_mass_g / 1000)

# glm() treats the SECOND factor level as "success" (the event being modelled).
# Levels are alphabetical: female (reference, 0) then male (event, 1).
# So the model estimates P(sex = "male").
levels(df$sex)
table(df$sex)

# -----------------------------------------------------------------------------
# 2. A SIMPLE MODEL: sex ~ body mass
# -----------------------------------------------------------------------------
model1 <- glm(sex ~ body_mass_kg, data = df, family = binomial(link = "logit"))
summary(model1)

b0 <- coef(model1)["(Intercept)"]
b1 <- coef(model1)["body_mass_kg"]

cat("\nIntercept (b0):", round(b0, 3),
    "\nSlope     (b1):", round(b1, 3), "\n")

# -----------------------------------------------------------------------------
# 3. HOW A PROBABILITY IS CALCULATED - STEP BY STEP
# -----------------------------------------------------------------------------
# Take a penguin weighing 4.2 kg.

mass <- 4.2

# Step 1: linear score (log-odds)
z <- b0 + b1 * mass

# Step 2: convert log-odds -> odds -> probability
odds <- exp(z)
p    <- odds / (1 + odds)           # identical to 1 / (1 + exp(-z))

cat("\n--- Manual calculation for a", mass, "kg penguin ---\n")
cat("1) log-odds  z = b0 + b1*x      =", round(z, 4), "\n")
cat("2) odds      = exp(z)           =", round(odds, 4), "\n")
cat("3) prob      = odds / (1 + odds) =", round(p, 4), "\n")

# Check against R's built-in prediction
p_r <- predict(model1, newdata = data.frame(body_mass_kg = mass), type = "response")
cat("   predict() gives             =", round(p_r, 4), "\n")
stopifnot(isTRUE(all.equal(unname(p), unname(p_r))))

# -----------------------------------------------------------------------------
# 4. INTERPRETING THE COEFFICIENTS
# -----------------------------------------------------------------------------
# b1 is the change in LOG-ODDS of being male for each extra kg.
# exp(b1) is the ODDS RATIO: how much the odds MULTIPLY per extra kg.
cat("\n--- Interpretation ---\n")
cat("Odds ratio per +1 kg   :", round(exp(b1), 2), "\n")
cat("Odds ratio per +100 g  :", round(exp(b1 * 0.1), 3), "\n")

# The probability does NOT change by a constant amount - it follows an S-curve.
# The decision boundary (p = 0.5  <=>  z = 0) is where b0 + b1*x = 0:
boundary <- -b0 / b1
cat("Decision boundary (p = 0.5) at body mass =", round(boundary, 3), "kg\n")

# -----------------------------------------------------------------------------
# 5. HOW THE COEFFICIENTS ARE FOUND: MAXIMUM LIKELIHOOD
# -----------------------------------------------------------------------------
# For each penguin, the model gives p_i = P(male).
#   - if the penguin IS male,   its likelihood contribution is p_i
#   - if the penguin IS female, its likelihood contribution is 1 - p_i
# The log-likelihood is the sum of the logs of those contributions.
# glm() finds b0, b1 that maximise it (via iteratively reweighted least squares).

y     <- as.numeric(df$sex == "male")
p_hat <- predict(model1, type = "response")

loglik_manual <- sum(y * log(p_hat) + (1 - y) * log(1 - p_hat))
cat("\n--- Log-likelihood ---\n")
cat("Manual :", round(loglik_manual, 4), "\n")
cat("logLik():", round(as.numeric(logLik(model1)), 4), "\n")

# A model with random coefficients fits worse (lower log-likelihood):
loglik_for <- function(b0, b1) {
  p <- 1 / (1 + exp(-(b0 + b1 * df$body_mass_kg)))
  sum(y * log(p) + (1 - y) * log(1 - p))
}
cat("Fitted coefficients      :", round(loglik_for(b0, b1), 2), "\n")
cat("Arbitrary (b0=-5, b1=1)  :", round(loglik_for(-5, 1), 2), "\n")

# -----------------------------------------------------------------------------
# 6. PLOT: THE LOGISTIC CURVE
# -----------------------------------------------------------------------------
grid <- data.frame(body_mass_kg = seq(min(df$body_mass_kg) - 0.3,
                                      max(df$body_mass_kg) + 0.3,
                                      length.out = 300))
grid$prob <- predict(model1, newdata = grid, type = "response")

# Mark the example penguin from step 3
example <- data.frame(body_mass_kg = mass, prob = as.numeric(p))

ggplot() +
  # Observed data: 0 = female, 1 = male (jittered vertically to avoid overlap)
  geom_jitter(data = df,
              aes(x = body_mass_kg, y = as.numeric(sex == "male"), colour = sex),
              height = 0.03, alpha = 0.5, size = 1.8) +
  # Fitted logistic curve
  geom_line(data = grid, aes(x = body_mass_kg, y = prob),
            linewidth = 1.3, colour = "black") +
  # 50% threshold and decision boundary
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = boundary, linetype = "dotted", colour = "grey40") +
  # Worked example from step 3
  geom_point(data = example, aes(x = body_mass_kg, y = prob),
             size = 4, shape = 21, fill = "gold", colour = "black", stroke = 1.2) +
  annotate("text", x = mass + 0.1, y = p - 0.07, hjust = 0,
           label = paste0("4.2 kg -> P(male) = ", round(p, 2))) +
  annotate("text", x = boundary + 0.05, y = 0.12, hjust = 0, size = 3.5,
           label = paste0("decision boundary\n(", round(boundary, 2), " kg)")) +
  scale_colour_manual(values = c(female = "#E07B39", male = "#3B7DD8")) +
  scale_y_continuous(breaks = seq(0, 1, 0.25)) +
  labs(title = "Logistic regression: probability of being male vs. body mass",
       subtitle = "Black curve = p = 1 / (1 + exp(-(b0 + b1 * mass)));  points = observed penguins",
       x = "Body mass (kg)", y = "P(sex = male)", colour = "Actual sex") +
  theme_minimal(base_size = 13)

# -----------------------------------------------------------------------------
# 7. PLOT: THE SAME CURVE ON THE LOG-ODDS SCALE
# -----------------------------------------------------------------------------
# On the logit scale the model is just a straight line - that is the whole
# point of the logit link: linear in the predictors, S-shaped in probability.
grid$logodds <- predict(model1, newdata = grid, type = "link")

ggplot(grid, aes(body_mass_kg, logodds)) +
  geom_line(linewidth = 1.3, colour = "firebrick") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  labs(title = "On the log-odds scale the model is a straight line",
       subtitle = "log-odds = 0 corresponds to p = 0.5",
       x = "Body mass (kg)", y = "log-odds of being male") +
  theme_minimal(base_size = 13)

# -----------------------------------------------------------------------------
# 8. A BETTER MODEL: several predictors
# -----------------------------------------------------------------------------
# Body mass differs a lot between species, which blurs the sex signal.
# Adding species + bill measurements helps a lot.
model2 <- glm(sex ~ body_mass_kg + bill_depth_mm + bill_length_mm + species,
              data = df, family = binomial)
summary(model2)

# Compare models: lower AIC = better trade-off between fit and complexity
cat("\nAIC model 1:", round(AIC(model1), 1),
    "| AIC model 2:", round(AIC(model2), 1), "\n")

# Probabilities work exactly the same way - z is just a longer sum:
#   z = b0 + b1*mass + b2*bill_depth + b3*bill_length + b4*speciesChinstrap + ...
new_penguin <- data.frame(body_mass_kg = 4.0, bill_depth_mm = 17,
                          bill_length_mm = 45, species = "Adelie")
cat("P(male) for the new penguin:",
    round(predict(model2, newdata = new_penguin, type = "response"), 3), "\n")

# -----------------------------------------------------------------------------
# 9. EVALUATING PREDICTIONS (train / test split)
# -----------------------------------------------------------------------------
set.seed(42)
train_idx <- sample(nrow(df), size = 0.75 * nrow(df))
train <- df[train_idx, ]
test  <- df[-train_idx, ]

fit_m1 <- glm(sex ~ body_mass_kg, data = train, family = binomial)
fit_m2 <- glm(sex ~ body_mass_kg + bill_depth_mm + bill_length_mm + species,
              data = train, family = binomial)

evaluate <- function(fit, data, label) {
  prob <- predict(fit, newdata = data, type = "response")
  pred <- factor(ifelse(prob > 0.5, "male", "female"), levels = levels(data$sex))
  cm   <- table(Predicted = pred, Actual = data$sex)
  cat("\n---", label, "---\n")
  print(cm)
  cat("Accuracy:", round(mean(pred == data$sex), 3), "\n")
}

evaluate(fit_m1, test, "Model 1 (body mass only)")
evaluate(fit_m2, test, "Model 2 (multiple predictors)")

# -----------------------------------------------------------------------------
# SUMMARY
# -----------------------------------------------------------------------------
# * Logistic regression models log-odds as a linear function of predictors.
# * The sigmoid turns log-odds into a probability between 0 and 1.
# * exp(coefficient) = odds ratio per one-unit increase of the predictor.
# * Coefficients are estimated by maximum likelihood.
# * The decision boundary (p = 0.5) is where the linear score equals zero.
# =============================================================================
