# Digital Soil Mapping with R
# https://zia207.quarto.pub/
# Generalized Linear Models 
# https://zia207.quarto.pub/generalized-linear-models.html
# 2026-10-12

# Packages ----

library(tidyverse)
library(caret)
library(Metrics)
library(tidymodels)

theme_set(theme_bw())

# Data ----

df <- read_csv("GLM/data/gp_soil_data.csv") |> 
  select(SOC, DEM, Slope, TPI, MAT, MAP, NDVI, NLCD, FRG)

# Caret package ----

## Create dummy variables ----

dummies <- dummyVars(SOC ~ ., data = df)
dummies.df <- as.data.frame(predict(dummies, newdata = df))
dummies.df$SOC <- df$SOC

## Data splitting ----

set.seed(3456)
trainIndex <- createDataPartition(dummies.df$SOC, p = .80, list = FALSE, times = 1)

df_train <- dummies.df[trainIndex, ]
df_test <- dummies.df[-trainIndex, ]

# Density plot all, train and test data
ggplot() +
  geom_density(data = dummies.df, aes(SOC)) +
  geom_density(data = df_train,  aes(SOC), color = "green") +
  geom_density(data = df_test,  aes(SOC), color = "red") +
  labs(x = "Soil Organic Carbon (kg/g)",
       y = "Density")

## Set control parameters ----

set.seed(123)
train.control <- trainControl(method = "repeatedcv",
                              number = 10, repeats = 5,
                              preProc = c("center", "scale", "nzv"))


## Train the model ----

model.glm <- train(SOC ~ ., data = df_train,
                   method = "glm",
                   trControl = train.control)

## Summary GLM model ----

print(model.glm)

## Training performance ----

getTrainPerf(model.glm)

## Prediction ----

df_test$Pred.SOC <- predict(model.glm, df_test)

## Prediction performance ----

RMSE <- Metrics::rmse(actual = df_test$SOC, predicted = df_test$Pred.SOC)
MAE <- Metrics::mae(actual = df_test$SOC, predicted = df_test$Pred.SOC)
MSE <- Metrics::mse(actual = df_test$SOC, predicted = df_test$Pred.SOC)
MDAE <- Metrics::mdae(actual = df_test$SOC, predicted = df_test$Pred.SOC)

# Tidymodels ----

## Split data ----

set.seed(1245)
split <- initial_split(df, prop = 0.8, strata = SOC)
train <- training(split)
test <- testing(split)

# Density plot
ggplot() +
  geom_density(data = df, aes(SOC)) +
  geom_density(data = train,  aes(SOC), color = "green") +
  geom_density(data = test,  aes(SOC), color = "red") +
  labs(x = "Soil Organic Carbon (kg/g)",
       y = "Density")

## Create recipe ----

glm_recipe <- 
  recipe(SOC ~ ., data = train) |> 
  step_zv(all_predictors()) |> 
  step_dummy(all_nominal()) |> 
  step_normalize(all_numeric_predictors())

## Build a model ----

glm_mod <- 
  linear_reg() |> 
  set_engine("glm")

## Create a workflow ----

glm_wflow <- 
  workflow() |> 
  add_model(glm_mod) |> 
  add_recipe(glm_recipe)

## Fit GLM model ----

glm_fit <- 
  glm_wflow |> 
  fit(data = train)

glm_fit |> 
  extract_fit_parsnip() |> 
  tidy()

## Prediction ----

test_df <- as.data.frame(test)
test_df$GLM.SOC <- predict(glm_fit, new_data = test)
head(test_df)
