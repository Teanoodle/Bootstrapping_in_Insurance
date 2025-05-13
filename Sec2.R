# Load required libraries
library(dplyr)
library(tidyverse)
library(boot)
# Load the dataset
data <- read.csv("/Users/Jun/Desktop/3years_Claims.csv")

# Explore the structure of the data
str(data)

# Clean and preprocess the data (convert categorical variables to factors)
data <- data %>%
  mutate(
    pol_payd = as.factor(pol_payd),
    pol_usage = as.factor(pol_usage),
    drv_sex1 = as.factor(drv_sex1),
    vh_fuel = as.factor(vh_fuel),
    vh_type = as.factor(vh_type)
  )




######################################
# Create age groups for easier comparison
data$drv_age_group <- cut(data$drv_age1, 
                          breaks = c(0, 25, 40, 60, 80, Inf), 
                          labels = c("0-25", "26-40", "41-60", "61-80", "81+"), 
                          right = FALSE)

# Fit a linear model to test the significance of driver's age group
model <- lm(claim_amount ~ drv_age_group, data = data)

# Print the model summary to see coefficients and p-values
summary(model)

# Boxplot to compare claim amounts by driver's age group
boxplot(claim_amount ~ drv_age_group, 
        data = data, 
        main = "Claim Amount by Driver's Age Group", 
        xlab = "Driver's Age Group", 
        ylab = "Claim Amount", 
        col = "lightblue")

# Add jittered points to show the distribution
points(jitter(as.numeric(data$drv_age_group)), data$claim_amount, pch = 16, col = rgb(0, 0, 0, 0.2))



###############################################################
#########################################
#########################################
# Bootstrapping for Linear Model with Age Groups
lm_statistic <- function(data, indices) {
  sample_data <- data[indices, ]  # Resample data with replacement
  model <- lm(claim_amount ~ drv_age_group, data = sample_data)
  return(coef(model))  # Extract coefficients
}

# Perform bootstrapping for Linear Model
set.seed(123)  # For reproducibility
lm_bootstrap <- boot(data = data, statistic = lm_statistic, R = 1000)

# Print bootstrap results for Linear Model
print(lm_bootstrap)

###################################################################################
# Parametric estimates for Linear Model
parametric_lm <- coef(model)
cat("\nParametric Coefficients for Linear Model:\n")
print(parametric_lm)

#########################################
# Visualize Bootstrapped Distributions

# Plot bootstrapped coefficient distributions for Linear Model
plot(lm_bootstrap, index = 2, main = "Bootstrapped Coefficients for Linear Model")




############################################################################
############################################################################
############################################################################
# Parametric estimates for Linear Model
parametric_lm <- coef(model)  # Extract parametric coefficients
parametric_ci <- confint(model)  # Parametric confidence intervals

cat("\n=== Parametric Linear Model Results ===\n")
cat("Coefficients:\n")
print(parametric_lm)
cat("Confidence Intervals:\n")
print(parametric_ci)

# Bootstrapped results for Linear Model
cat("\n=== Bootstrapped Linear Model Results ===\n")
for (i in 1:length(parametric_lm)) {
  # Extract bootstrapped confidence intervals for each coefficient
  ci <- boot.ci(lm_bootstrap, index = i, type = c("basic", "perc"))
  cat("Coefficient for", names(parametric_lm)[i], ":\n")
  cat("Bootstrapped Confidence Intervals:\n")
  print(ci)
}

# Combine results into a data frame for easier comparison
comparison <- data.frame(
  Coefficient = names(parametric_lm),
  Parametric_Estimate = parametric_lm,
  Parametric_CI_Lower = parametric_ci[, 1],
  Parametric_CI_Upper = parametric_ci[, 2],
  Bootstrapped_CI_Basic_Lower = sapply(1:length(parametric_lm), function(i) boot.ci(lm_bootstrap, index = i, type = "basic")$basic[4]),
  Bootstrapped_CI_Basic_Upper = sapply(1:length(parametric_lm), function(i) boot.ci(lm_bootstrap, index = i, type = "basic")$basic[5]),
  Bootstrapped_CI_Percentile_Lower = sapply(1:length(parametric_lm), function(i) boot.ci(lm_bootstrap, index = i, type = "perc")$perc[4]),
  Bootstrapped_CI_Percentile_Upper = sapply(1:length(parametric_lm), function(i) boot.ci(lm_bootstrap, index = i, type = "perc")$perc[5])
)

cat("\n=== Comparison Table ===\n")
print(comparison)

# Visualization of Bootstrapped vs Parametric Confidence Intervals
library(ggplot2)

comparison_long <- comparison %>%
  pivot_longer(
    cols = starts_with("Parametric") | starts_with("Bootstrapped"),
    names_to = "Method",
    values_to = "Value"
  )

ggplot(comparison_long, aes(x = Coefficient, y = Value, color = Method)) +
  geom_point(position = position_dodge(width = 0.5)) +
  theme_minimal() +
  labs(
    title = "Parametric vs Bootstrapped Confidence Intervals",
    x = "Coefficient",
    y = "Value"
  )





########################################################
###########################################################
######################################################
######################################################
###########################################################
# ==========================================================
# 1. Variability of Coefficients: Parametric vs Bootstrapped
# ==========================================================

# A. Extract Parametric Coefficients
parametric_coefficients <- coef(model)

# B. Extract Bootstrapped Coefficients
bootstrapped_coefficients <- lm_bootstrap$t  # Coefficients from each bootstrap iteration

# C. Plot the Distribution of a Specific Coefficient
# Example: Coefficient for "drv_age_group26-40"
index <- 2  # Index for "drv_age_group26-40"
hist(bootstrapped_coefficients[, index], breaks = 30, 
     main = "Bootstrapped Coefficient Distribution (drv_age_group26-40)",
     xlab = "Coefficient Value", col = "lightgreen", border = "white")
abline(v = parametric_coefficients[index], col = "red", lwd = 2, lty = 2)

# ==========================================================
# 2. Confidence Intervals: Parametric vs Bootstrapped
# ==========================================================

# A. Parametric Confidence Intervals
parametric_ci <- confint(model)

# B. Bootstrapped Confidence Intervals
bootstrapped_ci <- data.frame(
  Coefficient = names(parametric_coefficients),
  Bootstrapped_CI_Lower = sapply(1:length(parametric_coefficients), 
                                 function(i) boot.ci(lm_bootstrap, index = i, type = "basic")$basic[4]),
  Bootstrapped_CI_Upper = sapply(1:length(parametric_coefficients), 
                                 function(i) boot.ci(lm_bootstrap, index = i, type = "basic")$basic[5])
)

# Combine and Compare
cat("\nParametric Confidence Intervals:\n")
print(parametric_ci)
cat("\nBootstrapped Confidence Intervals:\n")
print(bootstrapped_ci)

# ==========================================================
# 3. Residual Diagnostics: Parametric vs Bootstrapped
# ==========================================================

# A. Parametric Residuals
residuals_parametric <- residuals(model)

# Plot Residuals for Parametric Model
hist(residuals_parametric, breaks = 30, 
     main = "Residuals Histogram (Parametric Model)",
     xlab = "Residuals", col = "lightblue", border = "white")

# B. Bootstrapped Residuals
bootstrapped_predictions <- apply(lm_bootstrap$t, 1, function(coef) {
  predict(lm(claim_amount ~ drv_age_group, data = data), newdata = data, se.fit = FALSE)
})
bootstrapped_mean_predictions <- rowMeans(bootstrapped_predictions)
bootstrapped_residuals <- data$claim_amount - bootstrapped_mean_predictions

# Plot Residuals for Bootstrapped Model
hist(bootstrapped_residuals, breaks = 30, 
     main = "Residuals Histogram (Bootstrapped Model)",
     xlab = "Residuals", col = "lightgreen", border = "white")

# ==========================================================
# 4. Stability of Predictions
# ==========================================================

# A. Plot Actual vs Fitted for Parametric Model
plot(data$claim_amount, fitted(model),
     main = "Fitted vs Actual Values (Parametric Model)",
     xlab = "Actual Claim Amount", ylab = "Fitted Claim Amount",
     pch = 16, col = "blue")
abline(0, 1, col = "red", lwd = 2)

# B. Plot Actual vs Fitted for Bootstrapped Model
plot(data$claim_amount, bootstrapped_mean_predictions,
     main = "Fitted vs Actual Values (Bootstrapped Model)",
     xlab = "Actual Claim Amount", ylab = "Fitted Claim Amount",
     pch = 16, col = "green")
abline(0, 1, col = "red", lwd = 2)

# ==========================================================
# 5. Cross-Validation (Optional Stability Test)
# ==========================================================
library(caret)
set.seed(123)
cv_parametric <- train(
  claim_amount ~ drv_age_group,
  data = data,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)
cat("\nCross-Validation Results (Parametric Model):\n")
print(cv_parametric)

# Bootstrapped Cross-Validation (Optional)
# Cross-validation doesn't directly apply to bootstrapped coefficients
# Bootstrapping itself serves as a form of robustness validation







# ==========================================================
# 6. Random Forest for Classification and Regression
# ==========================================================
library(randomForest)
library(e1071)  # For tune.randomForest

# Ensure necessary variables are factors or numeric
data <- data %>%
  mutate(
    drv_age_group = as.factor(drv_age_group)
  )

# Split data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(data), size = 0.7 * nrow(data))  # 70% Training data
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Random Forest for Regression (Predicting Claim Amount)
set.seed(123)
rf_reg_model <- randomForest(
  claim_amount ~ drv_age_group + pol_payd + pol_usage + drv_sex1 + vh_fuel + vh_type,
  data = train_data,
  ntree = 500,       # Number of decision trees
  mtry = 3,          # Number of variables tried at each split
  importance = TRUE  # Enable variable importance calculation
)

# Output the Random Forest model summary
cat("\n=== Random Forest Regression Model ===\n")
print(rf_reg_model)

# Variable Importance Plot
cat("\n=== Variable Importance (Regression) ===\n")
varImpPlot(rf_reg_model, main = "Variable Importance (Regression)")

# Make predictions on the test set
rf_reg_predictions <- predict(rf_reg_model, newdata = test_data)

# Evaluate Model Performance (Mean Squared Error)
mse_rf <- mean((test_data$claim_amount - rf_reg_predictions)^2)
cat("Random Forest Regression - Mean Squared Error (MSE):", mse_rf, "\n")

# Random Forest for Classification (Predicting Claim Flag)
if ("claim_flag" %in% colnames(data)) {
  set.seed(123)
  rf_class_model <- randomForest(
    claim_flag ~ drv_age_group + pol_payd + pol_usage + drv_sex1 + vh_fuel + vh_type,
    data = train_data,
    ntree = 500,
    mtry = 3,
    importance = TRUE,
    type = "classification"
  )
  
  # Output the Random Forest model summary
  cat("\n=== Random Forest Classification Model ===\n")
  print(rf_class_model)
  
  # Variable Importance Plot for Classification
  cat("\n=== Variable Importance (Classification) ===\n")
  varImpPlot(rf_class_model, main = "Variable Importance (Classification)")
  
  # Make predictions on the test set
  rf_class_predictions <- predict(rf_class_model, newdata = test_data, type = "class")
  
  # Confusion Matrix
  if ("claim_flag" %in% colnames(test_data)) {
    confusion_matrix <- table(test_data$claim_flag, rf_class_predictions)
    cat("\n=== Confusion Matrix ===\n")
    print(confusion_matrix)
    
    # Classification Accuracy
    accuracy_rf <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    cat("Random Forest Classification - Accuracy:", accuracy_rf, "\n")
  }
}

# ==========================================================
# 6.1 Tuning Random Forest using tune.randomForest
# ==========================================================
cat("\n=== Tuning Random Forest ===\n")
set.seed(123)
rf_tune <- tune.randomForest(
  claim_amount ~ drv_age_group + pol_payd + pol_usage + drv_sex1 + vh_fuel + vh_type,
  data = train_data,
  mtry = 2:4,
  ntree = c(100, 300, 500),
  tunecontrol = tune.control(sampling = "cross", cross = 5)  # 5-fold cross-validation
)

# Print tuning results
print(rf_tune)

# Plot tuning results
plot(rf_tune, main = "Tuning Results for Random Forest")

# Extract the best parameters
best_mtry <- rf_tune$best.parameters$mtry
best_ntree <- rf_tune$best.parameters$ntree
cat("Best mtry:", best_mtry, "Best ntree:", best_ntree, "\n")

# Train final Random Forest with best parameters
set.seed(123)
rf_optimized <- randomForest(
  claim_amount ~ drv_age_group + pol_payd + pol_usage + drv_sex1 + vh_fuel + vh_type,
  data = train_data,
  mtry = best_mtry,
  ntree = best_ntree,
  importance = TRUE
)

# Output optimized model summary
cat("\n=== Optimized Random Forest Regression Model ===\n")
print(rf_optimized)

# Variable Importance Plot
varImpPlot(rf_optimized, main = "Variable Importance (Optimized Random Forest)")

