# Load Required Libraries --------------------------------
rm(list=ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
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


# Set Working Directory ---------------------------------
data_path <- "../data/data.xlsx"
plots_dir = "../plots/rafa/"

# Import Data -------------------------------------------
data <- read_excel(data_path)

# Data Preprocessing ------------------------------------

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
                    "dtf_approval_date", "fx_approval_date", "default_90")
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

## Map Education Levels to Numeric
# If we remove this then the model does not find min lambda
# data <- data %>%
#   mutate(max_education = case_when(
#     max_education == "primaria" ~ 1,
#     max_education == "secundaria" ~ 2,
#     max_education == "técnico" ~ 3,
#     max_education == "tecnólogo" ~ 4,
#     max_education == "Universitario" ~ 5,
#     max_education == "Posgrado" ~ 6,
#     TRUE ~ NA_real_
#   ))



## Create Derived Variables
data <- data %>%
  mutate(
    #installment_periodic = installment / periodicity_num,
    time_difference_days = as.numeric(difftime(as.Date(date_limit), as.Date(date_approval), units = "days"))
  )

## Date furhter information
# First create year, month, day, weekday

# Extract features from the date-time variables
Sys.setlocale("LC_TIME", "en_US")
weekday_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

# date approval
data$m_date_approval <- format(data$date_approval, "%m")
data$m_date_approval_sin <- sin(2 * pi * as.numeric(data$m_date_approval) / 12)
data$m_date_approval_cos <- cos(2 * pi * as.numeric(data$m_date_approval) / 12)

data$wd_date_approval <- weekdays(data$date_approval)
data$wd_date_approval <- factor(data$wd_date_approval, levels = weekday_levels)
data$wd_date_approval_sin <- sin(2 * pi * as.numeric(data$wd_date_approval) / 7)
data$wd_date_approval_cos <- cos(2 * pi * as.numeric(data$wd_date_approval) / 7)

# date limit
data$m_date_limit <- format(data$date_limit, "%m")
data$m_date_limit_sin <- sin(2 * pi * as.numeric(data$m_date_limit) / 12)
data$m_date_limit_cos <- cos(2 * pi * as.numeric(data$m_date_limit) / 12)

data$wd_date_limit <- weekdays(data$date_limit)
data$wd_date_limit <- factor(data$wd_date_limit, levels = weekday_levels)
data$wd_date_limit_sin <- sin(2 * pi * as.numeric(data$wd_date_limit) / 7)
data$wd_date_limit_cos <- cos(2 * pi * as.numeric(data$wd_date_limit) / 7)

## Convert POSIXct to numeric
## Identify columns of type POSIXct
posix_columns <- sapply(data, function(col) inherits(col, "POSIXct"))

## Convert POSIXct columns to numeric
data[posix_columns] <- lapply(data[posix_columns], as.numeric)



## Convert Characters to Factors
data[] <- lapply(data, function(x) if (is.character(x)) as.factor(x) else x)



## Convert all numeric to log
# Identify numeric columns in the dataframe
numeric_columns <- sapply(data, is.numeric)

# Exclude the specified columns
exclude_columns <- c("default_90", "dtf_apporval_date")
columns_to_transform <- setdiff(names(data)[numeric_columns], exclude_columns)

## Log transform------------
# Apply the natural logarithm to the selected columns, adding 1 to handle zeros
# ----------------- ESTO QUIZÁ CAMBIE -----------------
data[columns_to_transform] <- lapply(data[columns_to_transform], function(col) log(col + 1))

# Move "default_90" to the last column
data <- data[, c(setdiff(names(data), "default_90"), "default_90")]

#REMOVE AGE because of Noise--------------
data <- data[, !names(data) %in% c("age")]

# Train-Test Split --------------------------------------
set.seed(123)
split <- sample.split(data$default_90, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)



# Exploratory Data Analysis -----------------------------

## Numeric Variables------------
# numeric_data <- train_data %>% select(where(is.numeric)) DEPRECATED
numeric_data <- train_data[sapply(train_data, is.numeric)]
skim(numeric_data)

### Histograms

n_cols <- 5
n_rows <- ceiling(length(colnames(numeric_data)) / n_cols)


pdf(paste(plots_dir,"00_EDA__numeric_vars_histograms.pdf", sep=""), width = n_cols * 3, height = n_rows * 3)  # Adjust dimensions
par(mfrow = c(n_rows, n_cols))

for (col_name in colnames(numeric_data)) {
  hist(numeric_data[[col_name]], main = paste("Histogram of", col_name),
       xlab = col_name, col = "lightblue", border = "black")
}
dev.off()  # Close the PDF device
par(mfrow = c(1, 1))


### Box Plots---------------

n_cols <- 5
n_rows <- ceiling(length(colnames(numeric_data)) / n_cols)

plots <- list()
for (col_name in colnames(numeric_data)) {
  plots[[col_name]] <- ggplot(train_data, aes(x = factor(default_90), y = .data[[col_name]], fill = factor(default_90))) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplot of", col_name, "by Target"),
         x = "Target",
         y = col_name,
         fill = "Target") +
    theme_minimal()
}

# Arrange plots in a grid and save to PDF
pdf(file = paste(plots_dir, "01_EDA__numeric_vars_boxplots.pdf", sep=""), width = n_cols * 3, height = n_rows * 3)  # Adjust dimensions
grid.arrange(grobs = plots, ncol = n_cols)
dev.off()



### Density Plots--------------
n_cols <- 5
n_rows <- ceiling(length(colnames(numeric_data)) / n_cols)

plots <- list()
for (col_name in colnames(numeric_data)) {
  plots[[col_name]] <- ggplot(train_data, aes(x = .data[[col_name]], fill = factor(default_90))) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Density Plot of", col_name), x = col_name, fill = "Target") +
    theme_minimal()
}

# Arrange plots in a grid and save to PDF
pdf(file = paste(plots_dir, "02_EDA__numeric_vars_densities.pdf", sep=""), width = n_cols * 3, height = n_rows * 3)  # Adjust dimensions
grid.arrange(grobs = plots, ncol = n_cols)
dev.off()




### Non-Numeric Variables------------
non_numeric_data <- train_data %>% select(where(~ !is.numeric(.)))
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
# All bimensual are defaulty (is just 1)

### Chi Squared------------

# The output will show the Chi-square statistic, degrees of freedom, and the p-value.
# A p-value less than 0.05 indicates that there is a significant association 
# between the non-numeric variable and the target variable default_90.

# Create a vector to store p-values
p_values <- c()

# Perform Chi-square tests and collect p-values
for (col_name in colnames(non_numeric_data)) {
  contingency_table <- table(non_numeric_data[[col_name]], train_data$default_90)
  chi2_result <- chisq.test(contingency_table)
  
  # Store the p-value
  p_values <- c(p_values, chi2_result$p.value)
}

contingency_table
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
# periodicity, wd_date_limit, civil_status, agency, city_born

# Model Training and Evaluation ------------------------

## Logistic Regression----------------
logistic_model <- glm(default_90 ~ ., data = train_data, family = binomial)
summary(logistic_model)

### Evaluate Logistic Regression------------
evaluate_model <- function(model, test_data, target_col) {
  predicted_prob <- predict(model, newdata = test_data, type = "response")
  predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
  accuracy <- mean(predicted_class == test_data[[target_col]])
  confusion <- confusionMatrix(factor(predicted_class), factor(test_data[[target_col]]))
  f1_score <- 2 * (confusion$byClass["Pos Pred Value"] * confusion$byClass["Sensitivity"]) /
    (confusion$byClass["Pos Pred Value"] + confusion$byClass["Sensitivity"])
  return(list(accuracy = accuracy, f1_score = f1_score))
}

logistic_results <- evaluate_model(logistic_model, test_data, "default_90")
print(logistic_results)

# accuracy is worse than naive model
# f1 score > 0.7 is good ?
### Post-Estimation Plots---------------
par(mfrow = c(2,2))
plot(logistic_model)


par(mfrow = c(1,1))
#### 1. ROC Curve and AUC ------------------------------------
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

#### 2. Precision-Recall (PR) Curve --------------------------
# Generate PR Curve
pr_curve <- pr.curve(scores.class0 = predicted_prob[true_labels == 1],
                     scores.class1 = predicted_prob[true_labels == 0],
                     curve = TRUE)

# Plot PR Curve
plot(pr_curve, main = paste("Precision-Recall Curve (AUC =", round(pr_curve$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")

# auc < 0.8 is not reasonable model, naive model gets 0.8
# The PR AUC of 0.42 is relatively low,  the model struggles with  the minority class (default)

# precision drops significantly, indicating that the model- 
#- misclassifies many observations as positive when predicting defaults.

#### 3. Confusion Matrix Heatmap -----------------------------

# Predicted classes
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$default_90)

# Heatmap
ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()
# model classifies too many false negative (439 predicted as good but actual default)
# model struggles also to predict actual defaulting clients

#### 4. Coefficient Plot -------------------------------------
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


## Stepwise Logistic Regression----------------

### 1. Forward Selection ------------------------
# Start with an empty model
start_model <- glm(default_90 ~ 1, data = train_data, family = binomial)

# Define the full model with all predictors
full_model <- glm(default_90 ~ ., data = train_data, family = binomial)

# Perform forward selection
forward_model <- step(
  start_model, 
  scope = list(lower = start_model, upper = full_model), 
  direction = "forward", 
  trace = 0
)

# Evaluate Forward Selection Model
forward_results <- evaluate_model(forward_model, test_data, "default_90")
print(forward_results)

### 2. Backward Elimination ---------------------
# Start with the full model
backward_model <- step(
  full_model, 
  direction = "backward", 
  trace = 0
)

# Evaluate Backward Elimination Model
backward_results <- evaluate_model(backward_model, test_data, "default_90")
print(backward_results)

### Post-Estimation Plots for Forward Model ----------------


#### 1. ROC PR Curve and AUC for Forward Model----------
# Predicted probabilities for the forward model
predicted_prob <- predict(forward_model, newdata = test_data, type = "response")

# True labels
true_labels <- test_data$default_90
roc_curve <- roc(true_labels, predicted_prob)
auc_value <- auc(roc_curve)

# Plot ROC Curve
par(mfrow = c(1, 2))  # Set layout to show ROC and PR side by side
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### 2. Precision-Recall (PR) Curve for Forward Model ------
# Generate PR curve
pr_curve <- pr.curve(scores.class0 = predicted_prob[true_labels == 1],
                     scores.class1 = predicted_prob[true_labels == 0],
                     curve = TRUE)

# Plot PR Curve
plot(pr_curve, main = paste("Precision-Recall Curve (AUC =", round(pr_curve$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")


#### 3. Confusion Matrix Heatmap for Forward Model---------
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$default_90)

ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap (Forward Model)", x = "Actual", y = "Predicted") +
  theme_minimal()

#### 4. Coefficient Plot for Forward Model--------------
coef_data <- as.data.frame(summary(forward_model)$coefficients)
coef_data$Variable <- rownames(coef_data)
rownames(coef_data) <- NULL

ggplot(coef_data, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Coefficient Plot (Forward Model)", x = "Variable", y = "Estimate") +
  theme_minimal()



### Post-Estimation Plots for Backward Model ----------------

#### 1. ROC Curve and AUC for Backward Model ----------
# Predicted probabilities for the backward model
predicted_prob <- predict(backward_model, newdata = test_data, type = "response")

# True labels
true_labels <- test_data$default_90


roc_curve <- roc(true_labels, predicted_prob)
auc_value <- auc(roc_curve)

# Plot ROC Curve
par(mfrow = c(1, 2))  # Set layout to show ROC and PR side by side
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### 2. Precision-Recall (PR) Curve for Backward Model ------
# Generate PR curve
pr_curve <- pr.curve(scores.class0 = predicted_prob[true_labels == 1],
                     scores.class1 = predicted_prob[true_labels == 0],
                     curve = TRUE)

# Plot PR Curve
plot(pr_curve, main = paste("Precision-Recall Curve (AUC =", round(pr_curve$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")


#### 3. Confusion Matrix Heatmap for Backward Model ---------
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$default_90)

ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap (Backward Model)", x = "Actual", y = "Predicted") +
  theme_minimal()

#### 4. Coefficient Plot for Backward Model --------------
coef_data <- as.data.frame(summary(backward_model)$coefficients)
coef_data$Variable <- rownames(coef_data)
rownames(coef_data) <- NULL

ggplot(coef_data, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Coefficient Plot (Backward Model)", x = "Variable", y = "Estimate") +
  theme_minimal()


## Lasso Logistic Regression--------------

### 1. Prepare Data for glmnet --------------------------------
# Convert data to matrix form (required by glmnet)

x_train <- as.matrix(train_data[, colnames(train_data) != "default_90"])
y_train <- train_data$default_90

x_test <- as.matrix(test_data[, colnames(test_data) != "default_90"])
y_test <- test_data$default_90

# scale predictors
#x_train <- scale(x_train)
#x_test <- scale(x_test)


### 2. Perform Cross-Validation for Lasso Logistic Regression --
set.seed(123)
cv_lasso <- cv.glmnet(x_train, y_train, family = "binomial",
                      standarize = T,alpha = 1, nfolds = 10)

# Plot of Cross-Validation Results 
# Optimal lambda
lambda_min <- cv_lasso$lambda.min
lambda_1se <- cv_lasso$lambda.1se

# Plot deviance against log(lambda)
par(mfrow= c(1,1))
plot(cv_lasso, main = "Cross-Validation for Lasso Logistic Regression")
abline(v = log(lambda_min), col = "blue", lty = 2, lwd = 2)  # Vertical line for lambda.min
abline(v = log(lambda_1se), col = "red", lty = 2, lwd = 2)   # Vertical line for lambda.1se
legend("topright", legend = c("lambda.min", "lambda.1se"), col = c("blue", "red"), lty = 2, lwd = 2)

# IF We find the lambda to the top left, meaning it could be further left:
# Define a custom lambda sequence (smaller values)
# lambda_grid <- 10^seq(-10, 2, length = 100)  # Adjust range to include smaller lambda values
# 
# # Perform Cross-Validation with custom lambda grid
# set.seed(123)
# cv_lasso <- cv.glmnet(
#   x_train, y_train, 
#   family = "binomial", 
#   alpha = 1, 
#   lambda = lambda_grid, 
#   nfolds = 10
# )
# 
# # Optimal lambdas
# lambda_min <- cv_lasso$lambda.min
# lambda_1se <- cv_lasso$lambda.1se
# 
# # Plot the updated deviance vs log(lambda) with the extended search range
# plot(cv_lasso, main = "Extended Cross-Validation for Lasso Logistic Regression")
# abline(v = log(lambda_min), col = "blue", lty = 2, lwd = 2)  # Vertical line for lambda.min
# abline(v = log(lambda_1se), col = "red", lty = 2, lwd = 2)   # Vertical line for lambda.1se
# legend("topright", legend = c("lambda.min", "lambda.1se"), col = c("blue", "red"), lty = 2, lwd = 2)
# 
# 
# # Lambda is still max left, there is something off with the model

# Optimal lambda
lambda_optimal <- cv_lasso$lambda.min
cat("Optimal lambda:", lambda_optimal, "\n")


### 3. Fit the Final Model with Optimal Lambda -----------------
lasso_model <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = lambda_optimal)

###  4. Evaluate the Model with Adjusted Threshold ------------
evaluate_lasso <- function(predicted_prob, y_test, threshold = 0.3) {
  # Predicted classes with custom threshold
  predicted_class <- ifelse(predicted_prob > threshold, 1, 0)
  
  # Calculate accuracy
  accuracy <- mean(predicted_class == y_test)
  
  # Create confusion matrix
  confusion <- confusionMatrix(factor(predicted_class), factor(y_test))
  
  # Calculate AUC
  auc_value <- auc(roc(y_test, predicted_prob))
  
  # Extract precision and recall, handling NA values
  precision <- confusion$byClass["Pos Pred Value"]
  recall <- confusion$byClass["Sensitivity"]
  
  if (is.na(precision) || is.na(recall) || (precision + recall) == 0) {
    f1_score <- NA
  } else {
    f1_score <- 2 * (precision * recall) / (precision + recall)
  }
  
  return(list(accuracy = accuracy, f1_score = f1_score, auc = auc_value, confusion = confusion))
}
# This model is awful, maybe adjusting threshold can improve

### 5. Predicted Probabilities and Evaluation -----------------
predicted_prob <- predict(lasso_model, s = lambda_optimal, newx = x_test, type = "response")
lasso_results <- evaluate_lasso(predicted_prob, y_test, threshold = 0.5)
print(lasso_results)

# True labels
true_labels <- y_test

#### 1. ROC Curve and AUC for Lasso Model----------
roc_curve <- roc(true_labels, predicted_prob)
auc_value <- auc(roc_curve)

# Plot ROC Curve
par(mfrow = c(1, 2))  # Set layout to show ROC and PR side by side
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### 2. Precision-Recall (PR) Curve for Lasso Model ------
# Generate PR curve
pr_curve <- pr.curve(scores.class0 = predicted_prob[true_labels == 1],
                     scores.class1 = predicted_prob[true_labels == 0],
                     curve = TRUE)

# Plot PR Curve
plot(pr_curve, main = paste("Precision-Recall Curve (AUC =", round(pr_curve$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")


### Fine Tune Threshold--------
# Define a function to evaluate metrics at different thresholds
optimize_threshold <- function(predicted_prob, true_labels, thresholds) {
  results <- data.frame(Threshold = numeric(), Precision = numeric(), Recall = numeric(), F1_Score = numeric())
  
  for (threshold in thresholds) {
    # Convert probabilities to predicted classes
    predicted_class <- ifelse(predicted_prob > threshold, 1, 0)
    
    # Calculate confusion matrix
    confusion <- confusionMatrix(factor(predicted_class), factor(true_labels))
    
    # Extract precision and recall
    precision <- confusion$byClass["Pos Pred Value"]
    recall <- confusion$byClass["Sensitivity"]
    
    # Handle NA values
    if (is.na(precision) || is.na(recall) || (precision + recall) == 0) {
      f1_score <- NA
    } else {
      # Calculate F1 score
      f1_score <- 2 * (precision * recall) / (precision + recall)
    }
    
    # Append results
    results <- rbind(results, data.frame(Threshold = threshold, Precision = precision, Recall = recall, F1_Score = f1_score))
  }
  
  return(results)
}

# Define a range of thresholds to evaluate
thresholds <- seq(0.1, 0.9, by = 0.05)

# Optimize threshold
threshold_results <- optimize_threshold(predicted_prob, y_test, thresholds)

# Find the threshold that maximizes F1 score
optimal_threshold <- threshold_results$Threshold[which.max(threshold_results$F1_Score)]
cat("Optimal Threshold:", optimal_threshold, "\n")

# Plot Precision, Recall, and F1 Score vs Threshold
threshold_results_melted <- reshape2::melt(threshold_results, id.vars = "Threshold", variable.name = "Metric", value.name = "Value")
ggplot(threshold_results_melted, aes(x = Threshold, y = Value, color = Metric)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Precision, Recall, and F1 Score vs Threshold", x = "Threshold", y = "Value") +
  scale_color_manual(values = c("blue", "red", "green"))

# the results suggest that the optimal threshold is 0.3


#### (2) Confusion Matrix Heatmap------------
predicted_class <- ifelse(predicted_prob > 0.3, 1, 0) # using optimal threshold
conf_matrix <- table(Predicted = predicted_class, Actual = y_test)
ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()

#### (3) Coefficient Plot ----------------------------------------
coef_data <- as.data.frame(as.matrix(coef(lasso_model, s = lambda_optimal)))
coef_data$Variable <- rownames(coef_data)
colnames(coef_data) <- c("Coefficient", "Variable")
coef_data <- coef_data %>% filter(Coefficient != 0 & Variable != "(Intercept)")

ggplot(coef_data, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Lasso Coefficient Plot", x = "Variable", y = "Coefficient") +
  theme_minimal()


## Elastic Net Logistic Regression--------------

### 1. Prepare Data for glmnet --------------------------------

### 2. Perform Cross-Validation for Elastic Net Logistic Regression --
# Elastic net uses `alpha` to mix lasso (alpha = 1) and ridge (alpha = 0)
set.seed(123)
cv_elastic_net <- cv.glmnet(
  x_train, y_train, 
  standarize = T,
  family = "binomial", 
  alpha = 0.5,        # Elastic net (mix of lasso and ridge)
  nfolds = 10         # 10-fold cross-validation
)

# Plot of Cross-Validation Results
# Optimal lambdas
lambda_min <- cv_elastic_net$lambda.min
lambda_1se <- cv_elastic_net$lambda.1se

# Plot deviance against log(lambda)
par(mfrow = c(1, 1))
plot(cv_elastic_net, main = "Cross-Validation for Elastic Net Logistic Regression")
abline(v = log(lambda_min), col = "blue", lty = 2, lwd = 2)  # Vertical line for lambda.min
abline(v = log(lambda_1se), col = "red", lty = 2, lwd = 2)   # Vertical line for lambda.1se
legend("topright", legend = c("lambda.min", "lambda.1se"), col = c("blue", "red"), lty = 2, lwd = 2)

cat("Optimal lambda (min):", lambda_min, "\n")
cat("Optimal lambda (1se):", lambda_1se, "\n")

### 3. Fit the Final Model with Optimal Lambda -----------------
elastic_net_model <- glmnet(
  x_train, y_train, 
  family = "binomial", 
  standarize = T,
  alpha = 0.5,        # Elastic net
  lambda = lambda_min # Use lambda.min for final model
)

### 4. Evaluate Model ------------------------------------------
# Predicted probabilities
predicted_prob <- predict(elastic_net_model, s = lambda_min, newx = x_test, type = "response")

# Predicted classes
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)

# Model evaluation metrics
evaluate_elastic_net <- function(predicted_class, predicted_prob, y_test) {
  accuracy <- mean(predicted_class == y_test)
  confusion <- confusionMatrix(factor(predicted_class), factor(y_test))
  auc_value <- auc(roc(y_test, predicted_prob))
  f1_score <- 2 * (confusion$byClass["Pos Pred Value"] * confusion$byClass["Sensitivity"]) /
    (confusion$byClass["Pos Pred Value"] + confusion$byClass["Sensitivity"])
  return(list(accuracy = accuracy, f1_score = f1_score, auc = auc_value))
}

elastic_net_results <- evaluate_elastic_net(predicted_class, predicted_prob, y_test)
print(elastic_net_results)

true_labels <- y_test

### 5. Post-Estimation Plots -----------------------------------
#### (1) ROC Curve and AUC-------------
roc_curve <- roc(true_labels, predicted_prob)
auc_value <- auc(roc_curve)

# Plot ROC Curve
par(mfrow = c(1, 2))  # Set layout to show ROC and PR side by side
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### (2) Precision-Recall (PR) Curve -------------
# Generate PR curve
pr_curve <- pr.curve(scores.class0 = predicted_prob[true_labels == 1],
                     scores.class1 = predicted_prob[true_labels == 0],
                     curve = TRUE)

# Plot PR Curve
plot(pr_curve, main = paste("Precision-Recall Curve (AUC =", round(pr_curve$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")

# Happens like the prior model, so adjust the threshold

### Threshold fine tunning--------------
# Define thresholds range
thresholds <- seq(0.1, 0.9, by = 0.05)

# Optimize threshold for elastic net model
threshold_results <- optimize_threshold(predicted_prob, y_test, thresholds)

# Find the threshold that maximizes F1 score
optimal_threshold <- threshold_results$Threshold[which.max(threshold_results$F1_Score)]
cat("Optimal Threshold:", optimal_threshold, "\n")

# Plot Precision, Recall, and F1 Score vs Threshold
threshold_results_melted <- reshape2::melt(threshold_results, id.vars = "Threshold", variable.name = "Metric", value.name = "Value")
ggplot(threshold_results_melted, aes(x = Threshold, y = Value, color = Metric)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Precision, Recall, and F1 Score vs Threshold", x = "Threshold", y = "Value") +
  scale_color_manual(values = c("blue", "red", "green"))
# again 0.3 as threshold optimal

#### (2) Confusion Matrix Heatmap-------------
predicted_class <- ifelse(predicted_prob > 0.3, 1, 0) # using optimal threshold
conf_matrix <- table(Predicted = predicted_class, Actual = y_test)
ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()
# marginally better, with the adjusted threshold

#### (3) Coefficient Plot------------
coef_data <- as.data.frame(as.matrix(coef(elastic_net_model, s = lambda_min)))
coef_data$Variable <- rownames(coef_data)
colnames(coef_data) <- c("Coefficient", "Variable")
coef_data <- coef_data %>% filter(Coefficient != 0 & Variable != "(Intercept)")


ggplot(coef_data, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Elastic Net Coefficient Plot", x = "Variable", y = "Coefficient") +
  theme_minimal()


## Group Lasso Logistic Regression--------------

### 1. Prepare Data for Group Lasso -------------------------
# Define groups for numeric and factor variables
# Train Set
train_data_no_target <- train_data[, !colnames(train_data) %in% "default_90"]
numeric_data_train <- train_data_no_target[sapply(train_data_no_target, is.numeric)]
non_numeric_data_train <- train_data_no_target[, !sapply(train_data_no_target, is.numeric)]

group_vector_train <- c()  # Stores group IDs
dummy_list_train <- list()  # Stores dummy variables
group_id <- 1

# Numeric variables (each variable is its own group)
group_vector_train <- c(group_vector_train, rep(group_id, ncol(numeric_data_train)))
group_id <- group_id + 1

# Factor variables (each factor's dummies are a group)
for (col_name in colnames(non_numeric_data_train)) {
  # Create dummy variables, dropping the first level
  dummies <- model.matrix(as.formula(paste("~", col_name)), data = train_data_no_target)[, -1]
  
  # Ensure dummies is treated as a matrix
  dummies <- as.matrix(dummies)
  
  # Check if the resulting dummies matrix has at least one column
  if (!is.null(ncol(dummies)) && ncol(dummies) > 0) {
    # Store the dummies in the list
    dummy_list_train[[col_name]] <- dummies
    
    # Add group markers for the current set of dummy variables
    group_vector_train <- c(group_vector_train, rep(group_id, ncol(dummies)))
    
    # Increment the group ID for the next factor
    group_id <- group_id + 1
  } else {
    # Handle cases where no dummy variables are created (e.g., single-level factors)
    warning(paste("No dummy variables created for", col_name, "as it has only one level."))
  }
}


# Combine numeric and dummy data for the train set
dummy_data_train <- do.call(cbind, dummy_list_train)
combined_data_train <- cbind(numeric_data_train, dummy_data_train)
X_train <- as.matrix(combined_data_train)
y_train <- ifelse(train_data$default_90 == 1, 1, -1)  # Convert target to {-1, 1}

# Test Set
test_data_no_target <- test_data[, !colnames(test_data) %in% "default_90"]
numeric_data_test <- test_data_no_target[sapply(test_data_no_target, is.numeric)]
non_numeric_data_test <- test_data_no_target[, !sapply(test_data_no_target, is.numeric)]

group_vector_test <- c()
dummy_list_test <- list()
group_id <- 1

# Numeric variables
group_vector_test <- c(group_vector_test, rep(group_id, ncol(numeric_data_test)))
group_id <- group_id + 1

# Factor variables
for (col_name in colnames(non_numeric_data_test)) {
  dummies <- model.matrix(as.formula(paste("~", col_name)), data = test_data_no_target)[, -1]
  dummies <- as.matrix(dummies)
  
  if (!is.null(ncol(dummies)) && ncol(dummies) > 0) {
    dummy_list_test[[col_name]] <- dummies
    group_vector_test <- c(group_vector_test, rep(group_id, ncol(dummies)))
    group_id <- group_id + 1
  } else {
    warning(paste("No dummy variables created for", col_name, "as it has only one level."))
  }
}


# Combine numeric and dummy data for the test set
dummy_data_test <- do.call(cbind, dummy_list_test)
combined_data_test <- cbind(numeric_data_test, dummy_data_test)
X_test <- as.matrix(combined_data_test)
y_test <- ifelse(test_data$default_90 == 1, 1, -1)  # Convert target to {-1, 1}

### 2. Standardize Predictors ---------------------------------
X_train_scaled <- scale(X_train)
X_test_scaled <- scale(X_test)

### 3. Fit the Group Lasso Model ------------------------------

# Set up parallel processing
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Fit group lasso model
fit_gglasso <- gglasso(
  x = X_train_scaled, 
  y = y_train, 
  group = group_vector_train, 
  loss = "logit", 
  nlambda = 50
)

# Stop parallel processing
stopCluster(cl)

# Plot Group Lasso Fit
plot(fit_gglasso, main = "Group Lasso Coefficients Across Lambda")

### 4. Perform Cross-Validation for Group Lasso -----------------
set.seed(123)
cv_fit_gglasso <- cv.gglasso(
  x = X_train_scaled, 
  y = y_train, 
  group = group_vector_train, 
  loss = "logit", 
  nfolds = 10
)

# Optimal lambda
lambda_min_gl <- cv_fit_gglasso$lambda.min
cat("Optimal lambda (min):", lambda_min_gl, "\n")



# Optimal lambda
lambda_min_gl <- cv_fit_gglasso$lambda.min
lambda_1se_gl <- cv_fit_gglasso$lambda.1se
cat("Optimal lambda (min):", lambda_min_gl, "\n")
cat("Optimal lambda (1se):", lambda_1se_gl, "\n")

# Plot Cross-Validation Results for Group Lasso
plot(cv_fit_gglasso, main = "Cross-Validation for Group Lasso")
abline(v = log(lambda_min_gl), col = "blue", lty = 2, lwd = 2)  # Vertical line for lambda.min
abline(v = log(lambda_1se_gl), col = "red", lty = 2, lwd = 2)   # Vertical line for lambda.1se
legend("topright", legend = c("lambda.min", "lambda.1se"), col = c("blue", "red"), lty = 2, lwd = 2)

### 5. Fit the Final Group Lasso Model -------------------------
group_lasso_model <- gglasso(
  x = X_train_scaled, 
  y = y_train, 
  group = group_vector_train, 
  loss = "logit", 
  lambda = lambda_min_gl
)

### 6. Evaluate Group Lasso Model ------------------------------
# Predicted probabilities
log_odds_gl <- predict(group_lasso_model, newx = X_test_scaled, type = "link")
predicted_prob_gl <- 1 / (1 + exp(-log_odds_gl))

# Predicted classes
predicted_class_gl <- ifelse(predicted_prob_gl > 0.5, 1, -1)

# Model evaluation metrics
evaluate_group_lasso <- function(predicted_class, predicted_prob, y_test) {
  accuracy <- mean(predicted_class == y_test)
  confusion <- confusionMatrix(factor(predicted_class), factor(y_test))
  auc_value <- auc(roc(y_test, predicted_prob))
  f1_score <- 2 * (confusion$byClass["Pos Pred Value"] * confusion$byClass["Sensitivity"]) /
    (confusion$byClass["Pos Pred Value"] + confusion$byClass["Sensitivity"])
  return(list(accuracy = accuracy, f1_score = f1_score, auc = auc_value))
}


group_lasso_results <- evaluate_group_lasso(predicted_class_gl, predicted_prob_gl, y_test)
print(group_lasso_results)

### 7. Post-Estimation Plots for Group Lasso -------------------

y_test_binary <- ifelse(y_test == 1, 1, 0)

#### (1) ROC Curve and AUC-------------
roc_curve_gl <- roc(y_test_binary, predicted_prob_gl)
auc_value_gl <- auc(roc_curve_gl)

# Plot ROC Curve
par(mfrow = c(1, 2))  # Set layout to show ROC and PR side by side
plot(roc_curve_gl, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value_gl, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### (2) Precision-Recall (PR) Curve -------------
# Generate PR curve
pr_curve_gl <- pr.curve(scores.class0 = predicted_prob_gl[y_test_binary == 1],
                        scores.class1 = predicted_prob_gl[y_test_binary == 0],
                        curve = TRUE)

# Plot PR Curve
plot(pr_curve_gl, main = paste("Precision-Recall Curve (AUC =", round(pr_curve_gl$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")


# This one is also shitty, try to fix via threshold

#### (2) Confusion Matrix Heatmap-----------
predicted_class_gl <- ifelse(predicted_prob_gl > 0.3, 1, -1)
conf_matrix_gl <- table(Predicted = predicted_class_gl, Actual = y_test)
ggplot(as.data.frame(conf_matrix_gl), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap (Group Lasso)", x = "Actual", y = "Predicted") +
  theme_minimal()

#### (3) Coefficient Plot---------------
coef_data_gl <- as.data.frame(as.matrix(coef(group_lasso_model)))
coef_data_gl$Variable <- rownames(coef_data_gl)
colnames(coef_data_gl) <- c("Coefficient", "Variable")
coef_data_gl <- coef_data_gl %>% filter(Coefficient != 0 & Variable != "(Intercept)")

ggplot(coef_data_gl, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Group Lasso Coefficient Plot", x = "Variable", y = "Coefficient") +
  theme_minimal()


## GAM Model----------------
# Using most relevant poly order (`s()` indicates the poly order for each predictor)
gam_model <- gam(default_90 ~ s(contributions_balance) + s(installment) + s(capital_balance) + s(credit_limit) + gender + income_group  , 
                 data = train_data, 
                 family = binomial)

# Summary of GAM Model
summary(gam_model)

# Evaluate GAM Model 
evaluate_gam_model <- function(model, test_data, target_col) {
  predicted_prob <- predict(model, newdata = test_data, type = "response")
  predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
  accuracy <- mean(predicted_class == test_data[[target_col]])
  confusion <- confusionMatrix(factor(predicted_class), factor(test_data[[target_col]]))
  f1_score <- 2 * (confusion$byClass["Pos Pred Value"] * confusion$byClass["Sensitivity"]) /
    (confusion$byClass["Pos Pred Value"] + confusion$byClass["Sensitivity"])
  return(list(accuracy = accuracy, f1_score = f1_score))
}

gam_results <- evaluate_gam_model(gam_model, test_data, "default_90")
print(gam_results)

### Post-Estimation Plots --------------------------------------

# Predicted Probabilities
predicted_prob_gam <- predict(gam_model, newdata = test_data, type = "response")


# True labels
y_test <- test_data$default_90



#### (1) ROC Curve and AUC-------------
roc_curve_gam <- roc(y_test, predicted_prob_gam)
auc_value_gam <- auc(roc_curve_gam)

# Plot ROC Curve
par(mfrow = c(1, 2))  # Set layout to show ROC and PR curves side by side
plot(roc_curve_gam, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value_gam, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### (2) Precision-Recall (PR) Curve -------------
# Generate PR curve

# Filter predicted probabilities based on the true labels
scores_class0 <- predicted_prob_gam[test_data$default_90 == 1]
scores_class1 <- predicted_prob_gam[test_data$default_90 == 0]

# Make sure the filtered vectors are numeric
scores_class0 <- as.numeric(scores_class0)
scores_class1 <- as.numeric(scores_class1)
pr_curve <- pr.curve(scores.class0 = scores_class0,
                     scores.class1 = scores_class1,
                     curve = TRUE)
plot(pr_curve, main = paste("Precision-Recall Curve (AUC =", round(pr_curve$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")

# this model sucks, lets try adjusting threshold
### Fine Tunning Threshold------------
# Predicted Probabilities from GAM Model
predicted_prob_gam <- predict(gam_model, newdata = test_data, type = "response")

# True labels from test set
y_test <- test_data$default_90

# Threshold Optimization Function
optimize_threshold <- function(predicted_prob, true_labels, thresholds) {
  results <- data.frame(Threshold = numeric(), Precision = numeric(), Recall = numeric(), F1_Score = numeric())
  
  for (threshold in thresholds) {
    # Convert probabilities to predicted classes
    predicted_class <- ifelse(predicted_prob > threshold, 1, 0)
    
    # Calculate confusion matrix
    confusion <- confusionMatrix(factor(predicted_class), factor(true_labels), positive = "1")
    
    # Extract precision and recall
    precision <- confusion$byClass["Pos Pred Value"]
    recall <- confusion$byClass["Sensitivity"]
    
    # Handle NA values
    if (is.na(precision) || is.na(recall) || (precision + recall) == 0) {
      f1_score <- NA
    } else {
      # Calculate F1 score
      f1_score <- 2 * (precision * recall) / (precision + recall)
    }
    
    # Append results
    results <- rbind(results, data.frame(Threshold = threshold, Precision = precision, Recall = recall, F1_Score = f1_score))
  }
  
  return(results)
}

# Define a range of thresholds to evaluate
thresholds <- seq(0.1, 0.9, by = 0.05)

# Optimize threshold
threshold_results <- optimize_threshold(predicted_prob_gam, y_test, thresholds)

# Find the threshold that maximizes F1 score
optimal_threshold <- threshold_results$Threshold[which.max(threshold_results$F1_Score)]
cat("Optimal Threshold:", optimal_threshold, "\n")

# Plot Precision, Recall, and F1 Score vs Threshold

threshold_results_melted <- reshape2::melt(threshold_results, id.vars = "Threshold", variable.name = "Metric", value.name = "Value")
ggplot(threshold_results_melted, aes(x = Threshold, y = Value, color = Metric)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Precision, Recall, and F1 Score vs Threshold", x = "Threshold", y = "Value") +
  scale_color_manual(values = c("blue", "red", "green"))



#### 2. Confusion Matrix Heatmap -----------------------------
# Predicted classes
predicted_class <- ifelse(predicted_prob > 0.25, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$default_90)

# Heatmap
ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()


## PCA reg--------------------

### Prepare Data for Sparse PCA ---------------------------------
# Exclude target variable for PCA

# Create x_train_spca as matrix without column default 90 as in group lasso
x_train_spca <- X_train

### Apply Sparse PCA to Train Data ------------------------------
set.seed(123)

# Center the data and check the scale parametrization-
k <- 10  # Number of components to consider initially

fit <- spca(x_train_spca, k = k, scale = TRUE, verbose = FALSE)
### Calculate Cumulative Variance Explained --------------------
explained_variance <- cumsum(fit$sdev^2) / sum(fit$sdev^2)

components <- 1:length(explained_variance)
explained_variance_df <- data.frame(Components = components, CumulativeVariance = explained_variance)
ggplot(explained_variance_df, aes(x = Components, y = CumulativeVariance)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.90, linetype = "dashed", color = "red") +
  labs(title = "Cumulative Variance Explained by Components", x = "Number of Components", y = "Cumulative Variance")


# LAB CODE adaptation
### Step 1: Initial Sparse PCA Fit --------------------------------
k <- 10  # Number of components to consider
set.seed(123)
fit <- spca(x_train_pca, k = k, scale = TRUE, verbose = FALSE)
summary(fit)

### Step 2: Choose Optimal Alpha -----------------------------------
var.sp <- NULL
reg.par <- seq(0, 0.25, length = 20)  # Regularization parameters for alpha tuning

for (j in 1:20) {
  fit <- spca(X, k = 5, alpha = reg.par[j], scale = TRUE, verbose = FALSE)
  var.sp[j] <- sum(fit$sdev^2)  # Store explained variance
  print(c(reg.par[j], sum(fit$sdev^2)))  # Print current alpha and explained variance
}

# Plot Explained Variance vs Regularization Parameter (Alpha)
par(mfrow = c(1, 1))
plot(reg.par, var.sp, type = "l", xlab = expression(lambda[1]), ylab = "Explained Variance", lwd = 2)

# Calculate variance reduction
explained_variance_drop <- 1 - var.sp / var.sp[1]
plot(diff(explained_variance_drop), type = "b", main = "Change in Explained Variance", ylab = "Difference", xlab = "Index of Regularization Parameter")

# Select alpha value based on plot (e.g., 9th parameter just before the elbow)
optimal_alpha_index <- 3
optimal_alpha <- reg.par[optimal_alpha_index]
cat("Optimal Alpha:", optimal_alpha, "\n")

### Step 3: Fit Sparse PCA with Optimal Alpha -----------------------
fit <- spca(x_train_spca, k = 5, alpha = optimal_alpha, scale = TRUE, verbose = FALSE)

### Step 4: Print Non-Zero Loadings ---------------------------------
V <- fit$loadings
for (j in 1:ncol(V)) {
  ind <- which(V[, j] != 0)  # Get indices of non-zero loadings
  v <- V[ind, j]  # Extract non-zero loadings
  names(v) <- colnames(X)[ind]  # Name the loadings by the original feature names
  cat("Component", j, ":\n")
  print(v)
}

### Step 5: Regression on Principal Components ----------------------
# Standardize the input data and calculate principal component scores
pcData <- as.data.frame(scale(X) %*% V)

# Assuming you have a response variable `y_train`
# Fit a linear model using the principal components as predictors
y_train_pca <- train_data$default_90  # Replace `default_90` with your target variable


logistic_model_pca <- glm(y_train_pca ~ ., data = pcData, family = binomial)
summary(logistic_model_pca)


### Post Estimation---------------
# Predicted Probabilities for Test Data
predicted_prob_pca <- predict(logistic_model_pca, newdata = pcData, type = "response")

# True labels from train data (y_train_pca)
y_train_pca <- train_data$default_90  # Replace with the correct target column

# Evaluate Logistic Model Function
evaluate_logistic_model <- function(predicted_prob, true_labels, threshold = 0.5) {
  predicted_class <- ifelse(predicted_prob > threshold, 1, 0)
  
  # Calculate accuracy
  accuracy <- mean(predicted_class == true_labels)
  
  # Calculate confusion matrix
  confusion <- confusionMatrix(factor(predicted_class), factor(true_labels), positive = "1")
  
  # Calculate F1 score
  precision <- confusion$byClass["Pos Pred Value"]
  recall <- confusion$byClass["Sensitivity"]
  
  if (!is.na(precision) && !is.na(recall) && (precision + recall) != 0) {
    f1_score <- 2 * (precision * recall) / (precision + recall)
  } else {
    f1_score <- NA
  }
  
  return(list(accuracy = accuracy, f1_score = f1_score, confusion = confusion))
}

# Evaluate Logistic Model
logistic_results <- evaluate_logistic_model(predicted_prob_pca, y_train_pca)
print(logistic_results$accuracy)
print(logistic_results$f1_score)
print(logistic_results$confusion)

#### 1. ROC and AUC--------------
# ROC Curve and AUC for Logistic Model
roc_curve_pca <- roc(y_train_pca, predicted_prob_pca)
auc_value_pca <- auc(roc_curve_pca)

# Plot ROC Curve
plot(roc_curve_pca, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value_pca, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#PR Curve

# Filter predicted probabilities based on the true labels
scores_class0_pca <- predicted_prob_pca[y_train_pca == 1]
scores_class1_pca <- predicted_prob_pca[y_train_pca == 0]

# Generate PR Curve
pr_curve_pca <- pr.curve(scores.class0 = scores_class0_pca, scores.class1 = scores_class1_pca, curve = TRUE)
plot(pr_curve_pca, main = paste("Precision-Recall Curve (AUC =", round(pr_curve_pca$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")


### Fine tunning threshold
# Threshold Optimization Function
optimize_threshold <- function(predicted_prob, true_labels, thresholds) {
  results <- data.frame(Threshold = numeric(), Precision = numeric(), Recall = numeric(), F1_Score = numeric())
  
  for (threshold in thresholds) {
    # Convert probabilities to predicted classes
    predicted_class <- ifelse(predicted_prob > threshold, 1, 0)
    
    # Calculate confusion matrix
    confusion <- confusionMatrix(factor(predicted_class), factor(true_labels), positive = "1")
    
    # Extract precision and recall
    precision <- confusion$byClass["Pos Pred Value"]
    recall <- confusion$byClass["Sensitivity"]
    
    # Handle NA values
    if (is.na(precision) || is.na(recall) || (precision + recall) == 0) {
      f1_score <- NA
    } else {
      # Calculate F1 score
      f1_score <- 2 * (precision * recall) / (precision + recall)
    }
    
    # Append results
    results <- rbind(results, data.frame(Threshold = threshold, Precision = precision, Recall = recall, F1_Score = f1_score))
  }
  
  return(results)
}

# Define a range of thresholds to evaluate
thresholds <- seq(0.1, 0.9, by = 0.05)

# Optimize threshold
threshold_results <- optimize_threshold(predicted_prob_pca, y_train_pca, thresholds)

# Find the threshold that maximizes F1 score
optimal_threshold <- threshold_results$Threshold[which.max(threshold_results$F1_Score)]
cat("Optimal Threshold:", optimal_threshold, "\n")

# Plot Precision, Recall, and F1 Score vs Threshold
library(reshape2)

threshold_results_melted <- melt(threshold_results, id.vars = "Threshold", variable.name = "Metric", value.name = "Value")
ggplot(threshold_results_melted, aes(x = Threshold, y = Value, color = Metric)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Precision, Recall, and F1 Score vs Threshold", x = "Threshold", y = "Value") +
  scale_color_manual(values = c("blue", "red", "green"))

#### 3. Confussion Matrix-------------

# Predicted classes using the optimal threshold
predicted_class_optimal <- ifelse(predicted_prob_pca > optimal_threshold, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = predicted_class_optimal, Actual = y_train_pca)

# Convert to a data frame for plotting
conf_matrix_df <- as.data.frame(conf_matrix)

# Plot Confusion Matrix Heatmap
ggplot(conf_matrix_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()



## SVM---------------
# needs scaling (done for group lasso)
# Combine X_train_scaled and y_train to create the training dataset in data frame format
train_data_svm <- as.data.frame(X_train_scaled)
train_data_svm$default_90 <- factor(y_train)

# Train SVM Model with Linear Kernel using Scaled Data
svm_model <- svm(default_90 ~ ., data = train_data_svm, kernel = "linear", probability = TRUE)

# Evaluate the SVM model on scaled test set
# Combine X_test_scaled and y_test to create the test dataset in data frame format
test_data_svm <- as.data.frame(X_test_scaled)
test_data_svm$default_90 <- factor(y_test)

# Predict class probabilities for the test set
predicted_prob_svm <- attr(predict(svm_model, newdata = test_data_svm, probability = TRUE), "probabilities")[, 2]

# Predicted class labels using threshold of 0.5
predicted_class_svm <- ifelse(predicted_prob_svm > 0.5, 1, -1)

# Convert predicted classes to a factor (for compatibility with confusionMatrix)
predicted_class_svm <- factor(predicted_class_svm, levels = c(-1, 1))
y_test_factor <- factor(y_test, levels = c(-1, 1))
# Continue from here-------------------------------------------------------------------

# Notes:
# All models should have coeficient paths