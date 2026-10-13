# Our Coding Club
# Introduction to Linear Mixed Models 
# https://ourcodingclub.github.io/tutorials/mixed-models/

# Packages ----

library(tidyverse)
library(lme4)
library(ggeffects)
library(sjPlot)
library(stargazer)

theme_set(theme_bw())

# Data ----

load("OurCodingClub/data/dragons.RData")
head(dragons)

hist(dragons$testScore)

# Standardise the explanatory variables
dragons$bodyLength2 <- scale(dragons$bodyLength, center = TRUE, scale = TRUE)

# Fit all data in one analysis ----

basic.lm <- lm(testScore ~ bodyLength2, data = dragons)
summary(basic.lm)

(prelim_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore)) +
    geom_point() +
    geom_smooth(method = "lm"))

# Plot the residuals
plot(basic.lm, which = 1)

# qq plot
plot(basic.lm, which = 2)

# Are our data independent?
boxplot(testScore ~ mountainRange, data = dragons)

(colour_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = mountainRange)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

# Run multiple analyses ----

(split_plot <- ggplot(aes(bodyLength, testScore), data = dragons) + 
   geom_point() + 
   facet_wrap(~ mountainRange) + # create a facet for each mountain range
   xlab("length") + 
   ylab("test score"))

# Modify the current model ----

# Add mountain range as a fixed effect 
mountain.lm <- lm(testScore ~ bodyLength2 + mountainRange, data = dragons)
summary(mountain.lm)

# Mixed effects models ----

# Is there an association between dragon's body length and the test score,
# after controlling for the variation in mountain ranges?

mixed.lmer <- lmer(testScore ~ bodyLength2 + (1|mountainRange), data = dragons)
summary(mixed.lmer)

# Variance for mountainRange ~339.7
# Variance for residuals     ~223.8

339.7 / (339.7 + 223.8)
# The difference between mountain ranges explains ~60% of the variance that's "left over"
# after the variance explained by our fixed effects.

# Check our assumptions

plot(mixed.lmer)

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))

# Types of random effects ----

## Crossed random effects ----

# An effect is fully crossed when all the subjects have experienced all the 
# levels of that effect.

## Nested random effects ----

str(dragons)
# We took samples from 3 sites per mountain range and 8 mountain ranges in total

dragons <- within(dragons, sample <- factor(mountainRange:site))
length(unique(dragons$sample))

# Our second mixed model ----

mixed.WRONG <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|site), data = dragons)
# treats the 2 random effects as if they are crossed
summary(mixed.WRONG)

mixed.lmer2 <- lmer(testScore ~ bodyLength2 + (1|mountainRange) + (1|sample), data = dragons)
# the nesting is taken into account
summary(mixed.lmer2)

(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
        facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
        geom_point(alpha = 0.5) +
        theme_classic() +
        geom_line(data = cbind(dragons, pred = predict(mixed.lmer2)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
        theme(legend.position = "none",
              panel.spacing = unit(2, "lines"))  # adding space between panels
)

# Introducing random slopes ----

mixed.ranslope <- lmer(testScore ~ bodyLength2 + (1 + bodyLength2|mountainRange/site), data = dragons)
summary(mixed.ranslope)

### plot
(mm_plot <- ggplot(dragons, aes(x = bodyLength, y = testScore, colour = site)) +
        facet_wrap(~mountainRange, nrow=2) +   # a panel for each mountain range
        geom_point(alpha = 0.5) +
        theme_classic() +
        geom_line(data = cbind(dragons, pred = predict(mixed.ranslope)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
        theme(legend.position = "none",
              panel.spacing = unit(2, "lines"))  # adding space between panels
)

# Presenting your model results ----

## Plotting model predictions ----

# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer2, terms = c("bodyLength2"))  # this gives overall predictions for the model

# Plot the predictions 

(ggplot(pred.mm) + 
        geom_line(aes(x = x, y = predicted)) +          # slope
        geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                    fill = "lightgrey", alpha = 0.5) +  # error band
        geom_point(data = dragons,                      # adding the raw data (scaled values)
                   aes(x = bodyLength2, y = testScore, colour = mountainRange)) + 
        labs(x = "Body Length (indexed)", y = "Test Score", 
             title = "Body length does not affect intelligence in dragons") + 
        theme_minimal()
)

ggpredict(mixed.lmer2, terms = c("bodyLength2", "mountainRange"), type = "random") %>% 
    plot() +
    labs(x = "Body Length", y = "Test Score", title = "Effect of body size on intelligence in dragons") + 
    theme_minimal()

# Visualise random effects 
(re.effects <- plot_model(mixed.ranslope, type = "re", show.values = TRUE))

# show summary
summary(mixed.ranslope)

stargazer(mixed.lmer2, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")
