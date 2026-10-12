# Our Coding Club
# Introduction to Linear Mixed Models 
# https://ourcodingclub.github.io/tutorials/mixed-models/

# Packages ----

library(tidyverse)

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
