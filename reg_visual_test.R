# R program to illustrate
# Graph plotting in
# Polynomial regression

# Importing required library
library(tidyverse)
library(caret)
theme_set(theme_classic())

# Load the data
data("Boston", package = "MASS")
# Split the data into training and test set
set.seed(123)
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]

# Build the model
model <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(RMSE = RMSE(predictions, test.data$medv),
           R2 = R2(predictions, test.data$medv))

ggplot(train.data, aes(lstat, medv) ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))
