# Main Script---------------------------
# Course: Statistical Methods for High Dimensional Data
# Final Project

# Load Required Libraries --------------------------------
rm(list=ls())
library(readxl)
library(ggplot2)
library(caret)
library(glmnet)
library(skimr)
library(reshape2)
library(caTools)
library(ResourceSelection)
library(pROC) # For AUC Curve
library(PRROC)    # For Precision-Recall Curve
library(MASS)  # For stepwise regression functions
library(car)
library(gglasso)
library(doParallel)
library(mgcv) # for GAM
library(sparsepca) # for sparse pca
library(e1071) # for SVM
library(dplyr)
library(randomForest)
library(xgboost)
library(Matrix)


# Set Working Directory ---------------------------------
data_path <- "../data/data.xlsx"
#data_path <- "../data/data_og_syntetic_age.xlsx" # delete this, just for testing

# Import Data -------------------------------------------
data <- read_excel(data_path)

# Data Preprocessing ------------------------------------


## Co-debtor-----------
# before removing ids, use them to check if client has co-debtor
colnames(data)

# Aggregate to count distinct 'ID Cliente' by 'No Pagaré Rotativo'
result <- aggregate(`ID Cliente` ~ `No Pagaré Rotativo`, data = data, FUN = function(x) length(unique(x)))

# Rename columns of result
colnames(result) <- c("No Pagaré Rotativo", "Distinct ID Cliente Count")

# View result max count by id
max(result$`Distinct ID Cliente Count`)
# so there is some credits with more than one client associated

# mark if the credit has more than one ID.
# Compute the distinct ID Cliente count per No Pagaré Rotativo
distinct_counts <- aggregate(`ID Cliente` ~ `No Pagaré Rotativo`, data = data, FUN = function(x) length(unique(x)))

# Add a column indicating whether the count is greater than 1
distinct_counts$MoreThanOne <- as.numeric(distinct_counts$`ID Cliente` > 1)


# Merge this information back into the original dataframe
data <- merge(data, distinct_counts[, c("No Pagaré Rotativo", "MoreThanOne")], by = "No Pagaré Rotativo", all.x = TRUE)
colnames(data)

# View the updated dataframe
head(data)


## Remove ID Variables
id_vars <- c("Código de Crédito", "ID Cliente", "No Pagaré Rotativo")
data <- data[, !names(data) %in% id_vars]

## Remove Single-Value Columns
single_value_vars <- c("Clasificación Tipo Crédito")
data <- data[, !names(data) %in% single_value_vars]

## Rename Columns for Clarity
friendly_names <- c("agency", "status", "rating", "work", "age", "civil_status",
                    "income_group", "city_born", "max_education", "gender", 
                    "contributions_balance", "credit_limit", "capital_balance",
                    "capital_due30", "days_due", "date_approval",
                    "installment", "periodicity", "credit_duration", "date_limit",
                    "dtf_approval_date", "fx_approval_date", "city_pop_2018","datacredito","default_90", "has_codebtor")

if (length(friendly_names) == ncol(data)) {
  colnames(data) <- friendly_names
} else {
  stop("Column name mismatch.")
}


## Handle Missing Values
na_counts <- colSums(is.na(data))
print(na_counts)

# Transformations ---------------------------------------

## Filter Credit Limit > 50,000
data <- data[data$credit_limit > 50000, ]

## Map Periodicity to Numeric
# data <- data %>%
#   mutate(periodicity_num = case_when(
#     periodicity == "Mensual" ~ 30,
#     periodicity == "Bimensual" ~ 60,
#     periodicity == "Quincenal" ~ 15,
#     TRUE ~ NA_real_
#   )) %>%
#   select(-periodicity)




## Create Derived Variables
data <- data %>%
  mutate(
    #installment_periodic = installment / periodicity_num,
    time_difference_days = as.numeric(difftime(as.Date(date_limit), as.Date(date_approval), units = "days"))
  )

## Date furhter information
# First create year, month, day, weekday

# Extract features from the date-time variables
# date approval
data$m_date_approval <- format(data$date_approval, "%m")
data$wd_date_approval <- weekdays(data$date_approval)

# date limit
data$m_date_limit <- format(data$date_limit, "%m")
data$wd_date_limit <- weekdays(data$date_limit)


## Convert POSIXct to numeric
## Identify columns of type POSIXct
posix_columns <- sapply(data, function(col) inherits(col, "POSIXct"))

## Convert POSIXct columns to numeric
data[posix_columns] <- lapply(data[posix_columns], as.numeric)



## Convert Characters to Factors
data[] <- lapply(data, function(x) if (is.character(x)) as.factor(x) else x)

str(data)

## Convert all numeric to log
# Identify numeric columns in the dataframe
numeric_columns <- sapply(data, is.numeric)

# Exclude the specified columns
exclude_columns <- c("default_90", "dtf_apporval_date", "has_codebtor")
columns_to_transform <- setdiff(names(data)[numeric_columns], exclude_columns)

## Log transform------------
# Apply the natural logarithm to the selected columns, adding 1 to handle zeros
#data[columns_to_transform] <- lapply(data[columns_to_transform], function(col) log(col + 1))

# Move "default_90" to the last column
data <- data[, c(setdiff(names(data), "default_90"), "default_90")]

#REMOVE AGE because of Noise--------------
data <- data[, !names(data) %in% c("age")]






# Train-Test Split --------------------------------------
set.seed(1)
split <- sample.split(data$default_90, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)


# Exploratory Data Analysis -----------------------------

## Numeric Variables------------
numeric_data <- train_data[sapply(train_data, is.numeric)]
skim(numeric_data)
par(mfrow=c(1,1))
### Histograms
for (col_name in colnames(numeric_data)) {
  hist(numeric_data[[col_name]], main = paste("Histogram of", col_name),
       xlab = col_name, col = "lightblue", border = "black")
}

### Box Plots---------------

for (col_name in colnames(numeric_data)) {
  print(ggplot(train_data, aes(x = factor(default_90), y = .data[[col_name]], fill = factor(default_90))) +
          geom_boxplot(alpha = 0.7) +
          labs(title = paste("Boxplot of", col_name, "by Target"),
               x = "Target",
               y = col_name,
               fill = "Target") +
          theme_minimal())
}

### Density Plots--------------
for (col_name in colnames(numeric_data)) {
  print(ggplot(train_data, aes(x = .data[[col_name]], fill = factor(default_90))) +
          geom_density(alpha = 0.5) +
          labs(title = paste("Density Plot of", col_name), x = col_name, fill = "Target") +
          theme_minimal())
}


### Non-Numeric Variables------------
non_numeric_data <- train_data[!sapply(train_data, is.numeric)]
unique_counts <- sapply(non_numeric_data, function(x) length(unique(x)))
mode_values <- sapply(non_numeric_data, function(x) names(which.max(table(x))))
print(mode_values)

### Correlation Analysis ----------------------------------

cor_matrix <- cor(numeric_data, use = "complete.obs")
cor_matrix
melted_cor_matrix <- melt(cor_matrix)
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  coord_fixed() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Barplots by Target-------
for (col_name in colnames(non_numeric_data)) {
  print(
    ggplot(train_data, aes_string(x = col_name, fill = "factor(default_90)")) +
      geom_bar(position = "fill") +  # Use position = "dodge" for side-by-side bars
      labs(title = paste("Bar Plot of", col_name, "by Target"),
           x = col_name, y = "Proportion", fill = "Target") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

# Create a vector to store p-values
p_values <- c()

# Perform Chi-square tests and collect p-values
for (col_name in colnames(non_numeric_data)) {
  contingency_table <- table(non_numeric_data[[col_name]], train_data$default_90)
  chi2_result <- chisq.test(contingency_table)
  
  # Store the p-value
  p_values <- c(p_values, chi2_result$p.value)
}

# Create a data frame for plotting
chi2_results_df <- data.frame(
  Variable = colnames(non_numeric_data),
  P_value = p_values
)

# Plot the p-values
ggplot(chi2_results_df, aes(x = reorder(Variable, -P_value), y = P_value)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +  # Significance threshold
  labs(title = "P-values from Chi-square Tests for Non-numeric Variables vs default_90",
       x = "Variable",
       y = "P-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# MODELS------------
## Helper functions---------------

# Calculate accuracy, precission, recall, f1 from probs and actual
calculate_metrics <- function(predicted_probs, actual_labels, threshold = 0.5) {
  # Convert probabilities to binary predictions based on the threshold
  predicted_labels <- ifelse(predicted_probs > threshold, 1, 0)
  
  # Create confusion matrix
  conf_matrix <- confusionMatrix(factor(predicted_labels), factor(actual_labels))
  
  # Extract components of the confusion matrix
  cm <- conf_matrix$table
  TP <- cm[2, 2]  # True Positives
  FP <- cm[1, 2]  # False Positives
  TN <- cm[1, 1]  # True Negatives
  FN <- cm[2, 1]  # False Negatives
  
  # Manually calculate metrics
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  accuracy <- (TP + TN) / sum(cm)
  
  # Return results as a list
  return(list(
    Precision = precision,
    Recall = recall,
    F1 = f1_score,
    Accuracy = accuracy
  ))
}

# Function to find the optimal threshold based on probs and actual
find_optimal_threshold <- function(predicted_probs, actual_labels, thresholds) {
  results <- data.frame(Threshold = numeric(), Precision = numeric(),
                        Recall = numeric(), F1 = numeric(), Accuracy = numeric())
  
  for (threshold in thresholds) {
    # Safeguard against confusion matrices that don't return 4 cells
    tryCatch({
      metrics <- calculate_metrics(predicted_probs, actual_labels, threshold)
      results <- rbind(results, c(Threshold = threshold, metrics))
    }, error = function(e) {})
  }
  
  # Convert results to data frame
  results <- as.data.frame(results)
  
  # Filter for valid rows (non-NA F1 scores)
  results <- results[!is.na(results$F1), ]
  
  # Find the threshold that maximizes F1 score
  optimal_threshold <- results$Threshold[which.max(results$F1)]
  
  return(list(OptimalThreshold = optimal_threshold, Metrics = results))
}
# function to plot curves on varios levels of threshold
plot_metrics <- function(metrics) {
  # Reshape data for ggplot2
  metrics_long <- reshape2::melt(metrics, id.vars = "Threshold",
                                 variable.name = "Metric", value.name = "Value")
  
  # Plot metrics
  ggplot(metrics_long, aes(x = Threshold, y = Value, color = Metric)) +
    geom_line() +
    labs(title = "Metrics Across Thresholds", x = "Threshold", y = "Value") +
    theme_minimal()
}


## 1. Logistic Regression----------------
logistic_model <- glm(default_90 ~ ., data = train_data, family = binomial)


# Make predictions on the test data
predicted_probs <- predict(logistic_model, newdata = test_data, type = "response")


### A) Model Evaluation---------------
logit_metrics <- calculate_metrics(predicted_probs, test_data$default_90)
logit_metrics

# accuracy is slightly better than naive model
# f1 score bad

### B) Post-Estimation Plots---------------
par(mfrow = c(2,2))
plot(logistic_model)


# reset grid
par(mfrow = c(1,1))
#### B.1) ROC Curve and AUC ------------------------------------
# Predicted probabilities for the test data
predicted_prob <- predict(logistic_model, newdata = test_data, type = "response")
# True labels
true_labels <- test_data$default_90

roc_curve <- roc(true_labels, predicted_prob)
auc_value <- auc(roc_curve)

# Plot ROC Curve
par(mfrow = c(1, 2))  # Set plot layout for ROC and PR curves side by side
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### B.2) Precision-Recall (PR) Curve --------------------------
# Generate PR Curve
pr_curve <- pr.curve(scores.class0 = predicted_prob[true_labels == 1],
                     scores.class1 = predicted_prob[true_labels == 0],
                     curve = TRUE)

# Plot PR Curve
plot(pr_curve, main = paste("Precision-Recall Curve (AUC =", round(pr_curve$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")

# auc > 0.8 is reasonable model, naive model gets 0.8
# The PR AUC of 0.64 is moderate

# precision drops significantly, indicating that the model- 
#- misclassifies many observations as positive when predicting defaults.
# Define a range of thresholds to evaluate
thresholds <- seq(0, 1, by = 0.05)  # Example grid of thresholds


thresholds_logit<- find_optimal_threshold(predicted_prob, true_labels, thresholds)
thresholds_logit
plot_metrics(thresholds_logit)

# Results on optimal threshold
logit_metrics <- calculate_metrics(predicted_probs, test_data$default_90,0.3)
logit_metrics

# Are better, similar accuracy and more F1, likely more False positive classified

#### B.3) Confusion Matrix Heatmap -----------------------------

# Predicted classes
predicted_class <- ifelse(predicted_prob > 0.3, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$default_90)

# Heatmap
ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()

# Model predicts very good 0 class. But for 1 class struggles a bit

#### B.4) Coefficient Plot -------------------------------------
# Extract coefficients
coefficients <- summary(logistic_model)$coefficients
coef_data <- as.data.frame(coefficients)
coef_data$Variable <- rownames(coefficients)
rownames(coef_data) <- NULL

# Plot coefficients
ggplot(coef_data, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Coefficient Plot", x = "Variable", y = "Estimate") +
  theme_minimal()


## 2.Stepwise------------------------
### 2.1 Forward----------------------
### 2.2 Backward---------------------
### 2.3 All Stepwise-----------------
## 3. Lasso--------------------------
## 4. Ridge--------------------------
## 5. Elastic Net--------------------
## 6. Group Lasso--------------------
## 7. GAM----------------------------
## 8. SPCA---------------------------
## 9. SVM----------------------------
### 9.1) Feature Scaling ------------------
train_data <- train_data %>%
  mutate(across(c(default_90, has_codebtor), as.factor)) # Convert binary variables to factors

test_data <- test_data %>%
  mutate(across(c(default_90, has_codebtor), as.factor)) # Ensure consistency in test data

# Step 1: Identify variable types
binary_vars <- c("default_90", "has_codebtor")
factor_vars <- names(train_data)[sapply(train_data, is.factor) & !names(train_data) %in% binary_vars]
numeric_vars <- names(train_data)[sapply(train_data, is.numeric)]

# Step 2: Scale numeric variables only
scaler <- preProcess(train_data[, numeric_vars], method = c("center", "scale"))
train_data_scaled <- train_data
test_data_scaled <- test_data

# Apply scaling only to numeric variables
train_data_scaled[, numeric_vars] <- predict(scaler, train_data[, numeric_vars])
test_data_scaled[, numeric_vars] <- predict(scaler, test_data[, numeric_vars])

# Step 3: Check the final datasets
str(train_data_scaled)
str(test_data_scaled)

# get only features for prediction step
test_data_features <- test_data_scaled %>% select(-default_90)


### 9.2) Train Model--------------------
svm_model <- svm(
  default_90 ~ ., 
  data = train_data_scaled, 
  kernel = "radial", 
  cost = 1, 
  gamma = 0.1,
  probability = TRUE # Train the model with probability enabled
)

### 9.3) Fine Tuning----------------------

# Step 1: Adjust target variable levels to valid R names
train_data_scaled$default_90 <- factor(train_data_scaled$default_90, levels = c(0, 1), labels = c("No", "Yes"))
test_data_scaled$default_90 <- factor(test_data_scaled$default_90, levels = c(0, 1), labels = c("No", "Yes"))

# Step 2: Define trainControl for cross-validation
train_control <- trainControl(
  method = "cv",          # Cross-validation
  number = 5,             # Number of folds
  classProbs = TRUE,      # Compute class probabilities
  verboseIter = TRUE      # Show progress during training
)

# Step 3: Define parameter grid for cost and gamma
tune_grid <- expand.grid(
  C = c(0.1, 1, 10, 100),  # Values for cost
  sigma = c(0.01, 0.1, 1)  # Values for gamma
)

# Step 4: Train the SVM model with grid search
svm_model <- train(
  default_90 ~ .,           # Formula
  data = train_data_scaled, # Training data
  method = "svmRadial",     # Radial kernel SVM
  tuneGrid = tune_grid,     # Hyperparameter grid
  trControl = train_control, # Cross-validation settings
  probability = TRUE # Train the model with probability enabled
)

# Step 5: View the best parameters and results
print(svm_model$bestTune)  # Best combination of cost and gamma
#sigma C
#4  0.01 1
print(svm_model)  # Full results

# Step 6: Make predictions on the test set
test_data_features <- test_data_scaled %>% select(-default_90)  # Exclude the target variable
svm_preds <- predict(svm_model, newdata = test_data_features)


# Step 7: Evaluate the model's performance
confusionMatrix(svm_preds, test_data_scaled$default_90)

## 10. Random Forest------------------
## 11. XGBoost------------------------




