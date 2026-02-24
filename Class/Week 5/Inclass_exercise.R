#clearing the workspace
rm (list = ls())

#Calling for libraries
library(tidyverse)
library(tidycensus)
library(sf)
library (ggplot2)
library(broom) #Cleans a regression model output into a tibble
library(scales) 
library(caret)

#calling for data
challenge_data <- get_acs(
  geography = "county",
  state = "PA",
  variables = c(
    home_value = "B25077_001",      # YOUR TARGET
    total_pop = "B01003_001",       # Total population
    median_income = "B19013_001",   # Median household income
    median_age = "B01002_001",      # Median age
    percent_college = "B15003_022", # Bachelor's degree or higher
    median_rent = "B25058_001",     # Median rent
    poverty_rate = "B17001_002"     # Population in poverty
  ),
  year = 2022,
  output = "wide"
)

#Sampling a random county
sample_county <- sample(unique(challenge_data$NAME), 1)

#Retrieving data for the sample county
final_data <- challenge_data %>%
  filter(challenge_data$NAME == sample_county)

#Creating a model for median home value with poverty rate as the predictor
model_1 <- lm (home_valueE ~ poverty_rateE, data = challenge_data)
summary (model_1)

#Creating a model for median home value with median income as the predictor
model_2 <- lm (home_valueE ~ median_incomeE, data = challenge_data)
summary (model_2)

#Creating a model for median home value with total population as the predictor
model_3 <- lm (home_valueE ~ total_popE, data = challenge_data)
summary (model_3)

#Creating a combined model
final_model <- lm (home_valueE ~ median_incomeE + total_popE, data = challenge_data)
summary(final_model)
plot(final_model)

#Train/Test Split ####
set.seed(123)
n <- nrow(challenge_data)

# 70% training, 30% testing
train_indices <- sample(1:n, size = 0.7 * n)
train_data <- challenge_data[train_indices, ]
test_data <- challenge_data[-train_indices, ]

# Fit on training data only
model_train <- lm(median_incomeE ~ total_popE, data = train_data)

# Predict on test data
test_predictions <- predict(model_train, newdata = test_data)

#Evalutate the predictions ####
# Calculate prediction error (RMSE)
rmse_test <- sqrt(mean((test_data$home_valueE - test_predictions)^2))
rmse_train <- summary(model_train)$sigma

cat("Training RMSE:", round(rmse_train, 0), "\n")

#Cross Validation in Action ####

# 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

cv_model <- train(home_valueE ~ median_incomeE + total_popE,
                  data = challenge_data,
                  method = "lm",
                  trControl = train_control)

cv_model$results

#Checking Assumptions ####
#Assumption 1: Linearity#####
challenge_data$residuals <- residuals(final_model)
challenge_data$fitted <- fitted(final_model)

ggplot(challenge_data, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

#Assumption 2: Constant Variance#####
library(lmtest)
bptest(final_model)

#Assumption: Normality of Residuals
plot (final_model)

#Assumption 3: Multicolinearity#####
library(car)
vif (final_model) # Variance Inflation Factor

#Assumption 4: No influential Outlier#####
# Rule of thumb: Cook's D > 4/n
# Add diagnostic measures
challenge_data <- challenge_data %>%
  mutate(
    cooks_d = cooks.distance(final_model),
    leverage = hatvalues(final_model),
    is_influential = cooks_d > 4/nrow(challenge_data)
  )

# Plot Cook's distance
ggplot(challenge_data, aes(x = 1:nrow(challenge_data), y = cooks_d)) +
  geom_point(aes(color = is_influential), size = 2) +
  geom_hline(yintercept = 4/nrow(challenge_data), 
             linetype = "dashed", color = "red") +
  scale_color_manual(values = c("grey60", "red")) +
  labs(title = "Cook's Distance",
       x = "Observation", y = "Cook's D") +
  theme_minimal() +
  theme(legend.position = "none")

threshold <- 4/nrow(challenge_data)

influential <- challenge_data %>%
  filter(cooks_d > threshold) %>%
  select(NAME, median_incomeE, total_popE, home_valueE, cooks_d) %>%
  arrange(desc(cooks_d))

head(influential, 2)

#Improving the Model ####
final_model_1 <- lm (home_valueE ~ median_incomeE + total_popE + median_rentE, data = challenge_data)
summary(final_model_1)

#Checking Residual Plot
#Assumption 1: Linearity#####
challenge_data$residuals1 <- residuals(final_model_1)
challenge_data$fitted1 <- fitted(final_model_1)

ggplot(challenge_data, aes(x = fitted1, y = residuals1)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Residual Plot", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

#Cross Validation in Action for new model ####

# 10-fold cross-validation
train_control <- trainControl(method = "cv", number = 10)

cv_model_1 <- train(home_valueE ~ median_incomeE + total_popE + median_rentE,
                  data = challenge_data,
                  method = "lm",
                  trControl = train_control)

cv_model_1$results
