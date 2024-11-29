# Load Required Libraries --------------------------------
rm(list=ls())
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(glmnet)
library(skimr)
library(reshape2)
library(caTools)
library(ResourceSelection)
library(pROC)
library(car)
library(gglasso)
library(doParallel)
library(mgcv) # for GAM
library(sparsepca) # for sparse pca

# Set Working Directory ---------------------------------
data_path <- "../data/data.xlsx"

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



## Convert all numeric to log
# Identify numeric columns in the dataframe
numeric_columns <- sapply(data, is.numeric)

# Exclude the specified columns
exclude_columns <- c("default_90", "dtf_apporval_date")
columns_to_transform <- setdiff(names(data)[numeric_columns], exclude_columns)

## Log transform------------
# Apply the natural logarithm to the selected columns, adding 1 to handle zeros
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
numeric_data <- train_data %>% select(where(is.numeric))
skim(numeric_data)
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

# f1 score > 0.7 is good
### Post-Estimation Plots---------------
par(mfrow = c(2,2))
plot(logistic_model)

#### 1. ROC Curve and AUC ------------------------------------
par(mfrow = c(1,1))

# Predicted probabilities
predicted_prob <- predict(logistic_model, newdata = test_data, type = "response")

# ROC and AUC
roc_curve <- roc(test_data$default_90, predicted_prob)
auc_value <- auc(roc_curve)

# Plot ROC Curve
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")
# auc > 0.7 is reasonable model
#### 2. Confusion Matrix Heatmap -----------------------------

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

#### 3. Coefficient Plot -------------------------------------
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

library(MASS)  # For stepwise regression functions

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
# Predicted probabilities
predicted_prob <- predict(forward_model, newdata = test_data, type = "response")

#### 1. ROC Curve and AUC for Forward Model----------
roc_curve <- roc(test_data$default_90, predicted_prob)
auc_value <- auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### 2. Confusion Matrix Heatmap for Forward Model---------
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$default_90)

ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap (Forward Model)", x = "Actual", y = "Predicted") +
  theme_minimal()

#### 3. Coefficient Plot for Forward Model--------------
coef_data <- as.data.frame(summary(forward_model)$coefficients)
coef_data$Variable <- rownames(coef_data)
rownames(coef_data) <- NULL

ggplot(coef_data, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Coefficient Plot (Forward Model)", x = "Variable", y = "Estimate") +
  theme_minimal()



### Post-Estimation Plots for Backward Model ----------------

# Predicted probabilities
predicted_prob <- predict(backward_model, newdata = test_data, type = "response")

#### 1. ROC Curve and AUC for Backward Model ----------
roc_curve <- roc(test_data$default_90, predicted_prob)
auc_value <- auc(roc_curve)
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### 2. Confusion Matrix Heatmap for Backward Model ---------
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$default_90)

ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap (Backward Model)", x = "Actual", y = "Predicted") +
  theme_minimal()

#### 3. Coefficient Plot for Backward Model --------------
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

# We find the lambda to the top left, meaning it could be further left:
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

### 5. Predicted Probabilities and Evaluation -----------------
predicted_prob <- predict(lasso_model, s = lambda_optimal, newx = x_test, type = "response")
lasso_results <- evaluate_lasso(predicted_prob, y_test, threshold = 0.5)
print(lasso_results)

### 6. Post-Estimation Plots ----------------------------------

#### (1) ROC Curve and AUC---------
roc_curve <- roc(y_test, predicted_prob)
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(lasso_results$auc, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### (2) Confusion Matrix Heatmap------------
predicted_class <- ifelse(predicted_prob > 0.3, 1, 0)
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


# We find the lambda to the top left, meaning it could be further left:
# Define a custom lambda sequence (smaller values)
lambda_grid <- 10^seq(-10, 2, length = 100)  # Adjust range to include smaller lambda values

# Perform Cross-Validation with custom lambda grid
set.seed(123)
cv_elastic_net <- cv.glmnet(
  x_train, y_train,
  family = "binomial",
  standarize = T,
  alpha = 0.5,
  lambda = lambda_grid,
  nfolds = 10
)


# Optimal lambdas
lambda_min <- cv_elastic_net$lambda.min
lambda_1se <- cv_elastic_net$lambda.1se

# Plot the updated deviance vs log(lambda) with the extended search range
plot(cv_elastic_net, main = "Extended Cross-Validation for Elastic Net Regression")
abline(v = log(lambda_min), col = "blue", lty = 2, lwd = 2)  # Vertical line for lambda.min
abline(v = log(lambda_1se), col = "red", lty = 2, lwd = 2)   # Vertical line for lambda.1se
legend("topright", legend = c("lambda.min", "lambda.1se"), col = c("blue", "red"), lty = 2, lwd = 2)




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

### 5. Post-Estimation Plots -----------------------------------

#### (1) ROC Curve and AUC-------------
roc_curve <- roc(y_test, predicted_prob)
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(elastic_net_results$auc, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### (2) Confusion Matrix Heatmap-------------
conf_matrix <- table(Predicted = predicted_class, Actual = y_test)
ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()

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

#### (1) ROC Curve and AUC---------
roc_curve_gl <- roc(y_test, predicted_prob_gl)
plot(roc_curve_gl, col = "blue", main = paste("ROC Curve (AUC =", round(group_lasso_results$auc, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### (2) Confusion Matrix Heatmap-----------
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

#### 1. ROC Curve and AUC ------------------------------------
# Predicted probabilities
predicted_prob <- predict(gam_model, newdata = test_data, type = "response")

# ROC and AUC
roc_curve <- roc(test_data$default_90, predicted_prob)
auc_value <- auc(roc_curve)

# Plot ROC Curve
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### 2. Confusion Matrix Heatmap -----------------------------
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


## PCA reg--------------------

### Prepare Data for Sparse PCA ---------------------------------
# Exclude target variable for PCA

x_train <- as.matrix(train_data[, colnames(train_data) != "default_90"])
### Apply Sparse PCA to Train Data ------------------------------
set.seed(123)

# Center the data and check the scale parametrization-
k <- 10  # Number of components to consider initially

fit <- spca(X_train, k = k, scale = TRUE, verbose = FALSE)

### Calculate Cumulative Variance Explained --------------------
explained_variance <- cumsum(fit$sdev^2) / sum(fit$sdev^2)
# plot and choose graphically
components_70 <- which(explained_variance >= 0.70)[1]  # Select components that explain at least 70% of variance

cat("Number of components explaining at least 70% of variance:", components_70, "\n")

### Use Only Top Components -------------------------------------
# Fit sparse PCA again with the optimal number of components
fit <- spca(X_train, k = components_70, alpha = 0.1, scale = TRUE, verbose = FALSE)

# Extract Principal Components
V <- fit$loadings
pc_train <- as.data.frame(x_train %*% V)

### Prepare Test Data for Sparse PCA ---------------------------
x_test <- as.matrix(test_data[, colnames(test_data) != "default_90"])
pc_test <- as.data.frame(x_test %*% V)

### Add Target Variable to Principal Components ----------------
pc_train$default_90 <- train_data$default_90
pc_test$default_90 <- test_data$default_90

### Logistic Regression with Principal Components --------------
logistic_model_pca <- glm(default_90 ~ ., data = pc_train, family = binomial)

### Summary of Logistic Regression Model -----------------------
summary(logistic_model_pca)

### Evaluate Logistic Regression on Test Data ------------------
evaluate_model_pca <- function(model, test_data, target_col) {
  predicted_prob <- predict(model, newdata = test_data, type = "response")
  predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
  accuracy <- mean(predicted_class == test_data[[target_col]])
  confusion <- confusionMatrix(factor(predicted_class), factor(test_data[[target_col]]))
  f1_score <- 2 * (confusion$byClass["Pos Pred Value"] * confusion$byClass["Sensitivity"]) /
    (confusion$byClass["Pos Pred Value"] + confusion$byClass["Sensitivity"])
  return(list(accuracy = accuracy, f1_score = f1_score))
}

pca_results <- evaluate_model_pca(logistic_model_pca, pc_test, "default_90")
print(pca_results)

### Post-Estimation Plots --------------------------------------

#### 1. ROC Curve and AUC ------------------------------------
# Predicted probabilities
predicted_prob <- predict(logistic_model_pca, newdata = pc_test, type = "response")

# ROC and AUC
roc_curve <- roc(pc_test$default_90, predicted_prob)
auc_value <- auc(roc_curve)

# Plot ROC Curve
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### 2. Confusion Matrix Heatmap -----------------------------
# Predicted classes
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = predicted_class, Actual = pc_test$default_90)

# Heatmap
ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()



## SVM---------------

