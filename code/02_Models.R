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
                    "dtf_approval_date", "fx_approval_date", "city_pop_2018","default_90", "has_codebtor")

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
numeric_data <- train_data[sapply(train_data, is.numeric)]
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


# Opt binning-------------

# This might help the model find better relationships
# with categorical variables

#install.packages("woeBinning")
library(woeBinning)
# binning <- woeBinning::woe.binning(
#   df = train_data,
#   target.var = "default_90", # Target variable
#   pred.var = "agency", # Categorical feature to bin
#   min.perc.total = 0.05, # Minimum percentage per bin
#   min.perc.class = 0.01  # Minimum percentage of target class per bin
# )
# 
# woeBinning::woe.binning.table(binning)
# 
# data_binned <- woeBinning::woe.binning.deploy(train_data, binning)
# 
# View(data_binned)
# 
# ggplot(data_binned, aes(x = agency.binned, fill = factor(default_90))) +
#   geom_bar(position = "fill") +  # Stacked proportions, use "dodge" for side-by-side
#   labs(
#     title = paste("Bar Plot of", "agency.binned", "by Target"),
#     x = "Agency (Binned)", 
#     y = "Proportion", 
#     fill = "Target"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Binning for some vars
vars_to_binn <- c('rating', 'status', 'wd_date_approval', 'm_date_approval', 'work',
                  'max_education', 'city_born', 'agency')

# Initialize a list to store binning objects for each variable
binning_list <- list()

# Iterate over the variables to create binning for each
for (var in vars_to_binn) {
  print(var)
  binning_list[[var]] <- woeBinning::woe.binning(
    df = train_data,
    target.var = "default_90", # Target variable
    pred.var = var,           # Current variable to bin
    min.perc.total = 0.05,    # Minimum percentage per bin
    min.perc.class = 0.01     # Minimum percentage of target class per bin
  )
}

# Display binning summary for each variable
for (var in vars_to_binn) {
  cat("\nBinning summary for", var, ":\n")
  print(woeBinning::woe.binning.table(binning_list[[var]]))
}

# Apply binning transformations to the dataset
data_binned <- train_data
for (var in vars_to_binn) {
  data_binned <- woeBinning::woe.binning.deploy(data_binned, binning_list[[var]])
}

#View(data_binned)

# See results in plots

# Iterate over the binned variables and generate plots
for (var in vars_to_binn) {
  # Construct the binned variable name
  binned_var <- paste0(var, ".binned")
  
  # Check if the binned variable exists in the data
  if (binned_var %in% names(data_binned)) {
    # Generate and print the plot
    plot <- ggplot(data_binned, aes_string(x = binned_var, fill = "factor(default_90)")) +
      geom_bar(position = "fill") +  # Stacked proportions
      labs(
        title = paste("Bar Plot of", binned_var, "by Target"),
        x = var, 
        y = "Proportion", 
        fill = "Target"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Print the plot
    print(plot)
  } else {
    cat("Binned variable", binned_var, "does not exist in the data.\n")
  }
}

# misc. level are vars that statistically insignificant so they are
# grouped together 


# Apply binning to test data (under training rules)
test_data_binned <- test_data
for (var in vars_to_binn) {
  # Check if the binning object exists and the variable is in test_data
  if (!is.null(binning_list[[var]]) && var %in% names(test_data)) {
    # Apply the binning to the test data
    test_data_binned <- woeBinning::woe.binning.deploy(test_data_binned, binning_list[[var]])
  } else {
    cat("Skipping variable:", var, "as it is not found in test_data or binning_list.\n")
  }
}

# Check the binned test data
head(test_data_binned)


# Drop original columns from train_data and test_data
train_data_binned <- data_binned[, !names(data_binned) %in% vars_to_binn]
test_data_binned <- test_data_binned[, !names(test_data_binned) %in% vars_to_binn]

# End Opt binning-------------------------------


# Model Training and Evaluation ------------------------
## Helper Functions------------------
# Define the function
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

# Function to find the optimal threshold
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


## Logistic Regression----------------
logistic_model <- glm(default_90 ~ ., data = train_data, family = binomial)
logistic_model_b <- glm(default_90 ~ ., data = train_data_binned, family = binomial)

# Make predictions on the test data
predicted_probs <- predict(logistic_model, newdata = test_data, type = "response")
# now check on binned data
predicted_probs_b <- predict(logistic_model_b, newdata = test_data_binned, type = "response")

### Model Evaluation---------------
logit_metrics <- calculate_metrics(predicted_probs, test_data$default_90)
logit_metrics
# metrics on binned data
logit_metrics_b <- calculate_metrics(predicted_probs_b, test_data_binned$default_90)
logit_metrics_b

# accuracy is worse than naive model
# f1 score bad
# Binning does not help in this case

### Post-Estimation Plots---------------
par(mfrow = c(2,2))
plot(logistic_model)
plot(logistic_model_b)
# binning helps a little with res vs leverage

# reset grid
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
# Define a range of thresholds to evaluate
thresholds <- seq(0, 1, by = 0.05)  # Example grid of thresholds


thresholds_logit<- find_optimal_threshold(predicted_prob, true_labels, thresholds)
thresholds_logit
plot_metrics(thresholds_logit)


# for binned data fine tune threshold
thresholds_logit_b <- find_optimal_threshold(predicted_probs_b, true_labels, thresholds)
thresholds_logit_b
plot_metrics(thresholds_logit_b)


logit_metrics <- calculate_metrics(predicted_probs, test_data$default_90,0.25)
logit_metrics

#### 3. Confusion Matrix Heatmap -----------------------------

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

# Make predictions on the test data
predicted_probs_fw <- predict(forward_model, newdata = test_data, type = "response")

### Model Evaluation---------------
fw_metrics <- calculate_metrics(predicted_probs_fw, test_data$default_90)
fw_metrics



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

### 2. Backward Elimination ---------------------
# Start with the full model
backward_model <- step(
  full_model, 
  direction = "backward", 
  trace = 0
)

# Make predictions on the test data
predicted_probs_bw <- predict(backward_model, newdata = test_data, type = "response")

### Model Evaluation---------------
bw_metrics <- calculate_metrics(predicted_probs_bw, test_data$default_90)
bw_metrics


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

### Model Evaluation---------------
predicted_probs_l <- predict(lasso_model, s = lambda_optimal, newx = x_test, type = "response")
lasso_metrics <- calculate_metrics(predicted_probs_l, test_data$default_90, 0.15)
lasso_metrics

# True labels
true_labels <- y_test

#### 1. ROC Curve and AUC for Lasso Model----------
roc_curve <- roc(true_labels, predicted_probs_l)
auc_value <- auc(roc_curve)

# Plot ROC Curve
par(mfrow = c(1, 2))  # Set layout to show ROC and PR side by side
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### 2. Precision-Recall (PR) Curve for Lasso Model ------
# Generate PR curve
pr_curve <- pr.curve(scores.class0 = predicted_probs_l[true_labels == 1],
                     scores.class1 = predicted_probs_l[true_labels == 0],
                     curve = TRUE)

# Plot PR Curve
plot(pr_curve, main = paste("Precision-Recall Curve (AUC =", round(pr_curve$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")


### Fine Tune Threshold--------
# Define a function to evaluate metrics at different thresholds




# Define a range of thresholds to evaluate
thresholds <- seq(0, 1, by = 0.05)  # Example grid of thresholds

# Optimize threshold
threshold_results <- find_optimal_threshold(predicted_probs_l, true_labels, thresholds)
threshold_results
plot_metrics(threshold_results)


#### (2) Confusion Matrix Heatmap------------
predicted_class <- ifelse(predicted_prob > 0.15, 1, 0) # using optimal threshold

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

# get lasso vars to use in GAM
vars_lasso <- coef_data$Variable
print(vars_lasso)

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
predicted_probs_en <- predict(elastic_net_model, s = lambda_min, newx = x_test, type = "response")

### Model Evaluation---------------
en_metrics <- calculate_metrics(predicted_probs_en, test_data$default_90, 0.3)
en_metrics

# Optimize threshold
# Optimize threshold
threshold_results_en <- find_optimal_threshold(predicted_probs_en, true_labels, thresholds)
threshold_results_en
plot_metrics(threshold_results_en)
# use 0.15 aswell


### 5. Post-Estimation Plots -----------------------------------
#### (1) ROC Curve and AUC-------------
roc_curve <- roc(true_labels, predicted_probs_en)
auc_value <- auc(roc_curve)

# Plot ROC Curve
par(mfrow = c(1, 2))  # Set layout to show ROC and PR side by side
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### (2) Precision-Recall (PR) Curve -------------
# Generate PR curve
pr_curve <- pr.curve(scores.class0 = predicted_probs_en[true_labels == 1],
                     scores.class1 = predicted_probs_en[true_labels == 0],
                     curve = TRUE)

# Plot PR Curve
plot(pr_curve, main = paste("Precision-Recall Curve (AUC =", round(pr_curve$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")

# Happens like the prior model, so adjust the threshold

#### (2) Confusion Matrix Heatmap-------------
predicted_class <- ifelse(predicted_probs_en > 0.25, 1, 0) # using optimal threshold
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
group_vector_train <- c(group_vector_train, seq(group_id, length.out = ncol(numeric_data_train)))
group_id <- max(group_vector_train) + 1

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
group_vector_test <- c(group_vector_test, seq(group_id, length.out = ncol(numeric_data_test)))
group_id <- max(group_vector_test) + 1


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
par(mfrow=c(1,1))
plot(fit_gglasso, main = "Group Lasso Coefficients Across Lambda")

### 4. Perform Cross-Validation for Group Lasso -----------------
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

set.seed(123)
cv_fit_gglasso <- cv.gglasso(
  x = X_train_scaled, 
  y = y_train, 
  group = group_vector_train, 
  loss = "logit", 
  nfolds = 10
)

stopCluster(cl)
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
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

group_lasso_model <- gglasso(
  x = X_train_scaled, 
  y = y_train, 
  group = group_vector_train, 
  loss = "logit", 
  lambda = lambda_min_gl
)
stopCluster(cl)

### 6. Evaluate Group Lasso Model ------------------------------
# Predicted probabilities
log_odds_gl <- predict(group_lasso_model, newx = X_test_scaled, type = "link")
log_odds_gl
predicted_probs_gl <- 1 / (1 + exp(-log_odds_gl))

gl_metrics <- calculate_metrics(predicted_probs_gl, test_data$default_90)
gl_metrics


### 7. Post-Estimation Plots for Group Lasso -------------------
y_test_binary <- test_data$default_90


#### (1) ROC Curve and AUC-------------
roc_curve_gl <- roc(y_test_binary, predicted_probs_gl)
auc_value_gl <- auc(roc_curve_gl)

# Plot ROC Curve
par(mfrow = c(1, 2))  # Set layout to show ROC and PR side by side
plot(roc_curve_gl, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value_gl, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### (2) Precision-Recall (PR) Curve -------------
# Generate PR curve
pr_curve_gl <- pr.curve(scores.class0 = predicted_probs_gl[y_test_binary == 1],
                        scores.class1 = predicted_probs_gl[y_test_binary == 0],
                        curve = TRUE)

# Plot PR Curve
plot(pr_curve_gl, main = paste("Precision-Recall Curve (AUC =", round(pr_curve_gl$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")


# This one is also shitty, try to fix via threshold

# Optimize threshold
threshold_results_gl <- find_optimal_threshold(predicted_probs_gl, true_labels, thresholds)
threshold_results_gl
plot_metrics(threshold_results_gl)

#### (2) Confusion Matrix Heatmap-----------
predicted_class_gl <- ifelse(predicted_probs_gl > 0.25, 1, -1)
conf_matrix_gl <- table(Predicted = predicted_class_gl, Actual = y_test)
ggplot(as.data.frame(conf_matrix_gl), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap (Group Lasso)", x = "Actual", y = "Predicted") +
  theme_minimal()

#### (3) Coefficient Plot---------------

# Top N coefficients Group Lasso
# Convert coefficients to data frame and filter
coef_data_gl <- as.data.frame(as.matrix(coef(group_lasso_model)))
coef_data_gl$Variable <- rownames(coef_data_gl)
colnames(coef_data_gl) <- c("Coefficient", "Variable")
coef_data_gl <- coef_data_gl %>%
  filter(Coefficient != 0 & Variable != "(Intercept)")

# Select top N coefficients by absolute value
top_n <- 25  # Adjust this number to show more/less coefficients
coef_data_gl <- coef_data_gl %>%
  mutate(AbsCoefficient = abs(Coefficient)) %>%
  arrange(desc(AbsCoefficient)) %>%
  filter(row_number() <= top_n)

# Plot the top N coefficients
ggplot(coef_data_gl, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Group Lasso Top Coefficients Plot",
       x = "Variable", y = "Coefficient") +
  theme_minimal()

## GAM Model----------------
# we can use the natural variable selection that lasso provides:
print(vars_lasso)

# Using most relevant poly order (`s()` indicates the poly order for each predictor)
# GAM using Lasso selected vars
gam_model <- gam(default_90 ~ agency + income_group + s(contributions_balance) +
                   s(credit_limit)+
                    s(date_approval) + s(credit_duration) + s(dtf_approval_date)+ 
                    has_codebtor + m_date_limit, 
                 data = train_data, 
                 family = binomial)
summary(gam_model)

# re run model with no splines for pvalue <0.05
gam_model <- gam(default_90 ~ agency + income_group + s(contributions_balance) +
                   s(credit_limit)+
                   s(date_approval) + credit_duration + dtf_approval_date+ 
                   has_codebtor + m_date_limit, 
                 data = train_data, 
                 family = binomial)
summary(gam_model)


plot(gam_model, pages = 1, rug = TRUE)

# Summary of GAM Model
summary(gam_model)
### Model Evaluation-------------------
predicted_probs_gam <- predict(gam_model, newdata = test_data, type = "response")
gam_metrics <- calculate_metrics(predicted_probs_gam, test_data$default_90, 0.25)
gam_metrics


### Post-Estimation Plots --------------------------------------

# True labels
y_test <- test_data$default_90



#### (1) ROC Curve and AUC-------------
roc_curve_gam <- roc(y_test, predicted_probs_gam)
auc_value_gam <- auc(roc_curve_gam)

# Plot ROC Curve
par(mfrow = c(1, 2))  # Set layout to show ROC and PR curves side by side
plot(roc_curve_gam, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value_gam, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### (2) Precision-Recall (PR) Curve -------------
# Generate PR curve

# Filter predicted probabilities based on the true labels
scores_class0 <- predicted_probs_gam[test_data$default_90 == 1]
scores_class1 <- predicted_probs_gam[test_data$default_90 == 0]

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
threshold_results_gam <- find_optimal_threshold(predicted_probs_gam, true_labels, thresholds)
threshold_results_gam
plot_metrics(threshold_results_gam)


#### 2. Confusion Matrix Heatmap -----------------------------
# Predicted classes
predicted_class <- ifelse(predicted_probs_gam > 0.25, 1, 0)

# Confusion matrix
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$default_90)

# Heatmap
ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()

## GAM Iterative----------------


# Ensure the target variable is binary
train_data$default_90 <- as.factor(train_data$default_90) # Convert target to factor if needed


# List to store the best functions for each variable
best_functions <- list()

for (var in vars_lasso) {
  # Get the unique value count for the variable
  unique_values <- length(unique(train_data[[var]]))
  
  # Set up GAM models
  gam_models <- list()
  
  if (is.numeric(train_data[[var]])) {
    # For numeric variables, include smooth terms only if enough unique values exist
    gam_models$linear <- gam(as.formula(paste("default_90 ~", var)), 
                             family = binomial(link = "logit"), data = train_data)
    
    if (unique_values > 3) { # Minimum for a smooth term to work well
      gam_models$smooth <- gam(as.formula(paste("default_90 ~ s(", var, ", k=", min(10, unique_values - 1), ")")), 
                               family = binomial(link = "logit"), data = train_data)
    }
    if (unique_values > 3) {
      gam_models$cubic_spline <- gam(as.formula(paste("default_90 ~ s(", var, ", bs='cs', k=", min(10, unique_values - 1), ")")), 
                                     family = binomial(link = "logit"), data = train_data)
    }
  } else if (is.factor(train_data[[var]])) {
    # For factor variables, only test a linear term
    gam_models$linear <- gam(as.formula(paste("default_90 ~", var)), 
                             family = binomial(link = "logit"), data = train_data)
  } else {
    # Skip variables that are neither numeric nor factor
    next
  }
  
  # Ensure at least one model exists to compare
  if (length(gam_models) == 0) next
  
  # Compare models using AIC
  model_aic <- sapply(gam_models, AIC)
  best_model <- names(which.min(model_aic))
  
  # Store the best function and model for the variable
  best_functions[[var]] <- list(
    best_function = best_model,
    model = gam_models[[best_model]]
  )
}

# Review the best functions for each variable
print(best_functions)



# STOP HERE-------------------------------------------
# from this point onwards is pending adjustments
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

# Load required libraries
library(e1071)
library(caret)
library(dplyr)

# Step 1: Scale the data
# Assuming train_data and test_data are data frames with predictors and the target column is `default_90`

# Separate predictors and target variable
X_train <- train_data %>% select(-default_90)
Y_train <- train_data$default_90
X_test <- test_data %>% select(-default_90)
Y_test <- test_data$default_90

# Standardize the predictors
scaler <- preProcess(X_train, method = c("center", "scale"))
X_train_scaled <- predict(scaler, X_train)
X_test_scaled <- predict(scaler, X_test)

# Step 2: Define the parameter grid
tune_grid <- expand.grid(
  C = c(0.1, 1, 10, 100),
  gamma = c(0.1, 1, 10),  # Note: 'scale' and 'auto' are not directly available in e1071
  kernel = "radial" # Corresponds to 'rbf' in Python
)

# Step 3: Set up cross-validation and train the SVM model
train_control <- trainControl(method = "cv", number = 5, summaryFunction = f1Score, verboseIter = TRUE)

svm_model <- train(
  x = X_train_scaled,
  y = as.factor(Y_train),
  method = "svmRadial",
  tuneGrid = tune_grid,
  trControl = train_control,
  metric = "F1"
)

# Step 4: Retrieve the best parameters and model
best_params <- svm_model$bestTune
best_model <- svm_model$finalModel

# Step 5: Predict on the test set
Y_pred <- predict(best_model, X_test_scaled)

# Evaluate the model's performance
confusionMatrix(Y_pred, as.factor(Y_test))


## Random Forest-------------------



# Convert the target variable to factor if not already
train_data$default_90 <- as.factor(train_data$default_90)
test_data$default_90 <- as.factor(test_data$default_90)


# Fit the Random Forest model
rf_model <- randomForest(
  default_90 ~ .,       # Formula: target ~ predictors (all other columns)
  data = train_data,    # Training dataset
  ntree = 500,          # Number of trees (default is 500)
  mtry = sqrt(ncol(train_data) - 1),  # Number of variables tried at each split
  importance = TRUE,    # Measure variable importance
  na.action = na.omit   # Handle missing values by omitting them
)

# View model summary
print(rf_model)

# Predict on test data
predictions <- predict(rf_model, newdata = test_data)

# Confusion Matrix
confusion_matrix <- table(Predicted = predictions, Actual = test_data$default_90)
print(confusion_matrix)

# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Plot variable importance
importance(rf_model)       # Numerical importance
varImpPlot(rf_model)       # Plot importance

# Tune mtry
tune_rf <- tuneRF(
  train_data[, -which(names(train_data) == "default_90")], # Exclude target variable
  train_data$default_90,
  stepFactor = 1.5,   # Increment of mtry
  ntreeTry = 500,     # Number of trees to try
  improve = 0.01,     # Minimum improvement in OOB error
  trace = TRUE
)

# tunned results
tuned_mtry <- tune_rf[which.min(tune_rf[, 2]), 1]  # Extract mtry with minimum OOB error
print(paste("Optimal mtry:", tuned_mtry))

# tunned model
# Re-run the Random Forest model
rf_model_tuned <- randomForest(
  default_90 ~ ., 
  data = train_data, 
  ntree = 500,        # Number of trees (can increase if needed)
  mtry = tuned_mtry,  # Optimal mtry from tuning
  importance = TRUE,
  na.action = na.omit
)

# Print the tuned model summary
print(rf_model_tuned)

# Predict on test data
predictions_tuned <- predict(rf_model_tuned, newdata = test_data)

# Confusion Matrix
confusion_matrix_tuned <- table(Predicted = predictions_tuned, Actual = test_data$default_90)
print(confusion_matrix_tuned)

# Calculate accuracy
accuracy_tuned <- sum(diag(confusion_matrix_tuned)) / sum(confusion_matrix_tuned)
print(paste("Tuned Model Accuracy:", accuracy_tuned))

# metrics of tunned model
# Extract elements from the confusion matrix
TP <- confusion_matrix_tuned[2, 2]  # True Positives (default_90 = 1, Predicted = 1)
TN <- confusion_matrix_tuned[1, 1]  # True Negatives (default_90 = 0, Predicted = 0)
FP <- confusion_matrix_tuned[1, 2]  # False Positives (default_90 = 0, Predicted = 1)
FN <- confusion_matrix_tuned[2, 1]  # False Negatives (default_90 = 1, Predicted = 0)

# Calculate Recall
recall <- TP / (TP + FN)
print(paste("Recall:", recall))

# Calculate Precision
precision <- TP / (TP + FP)
print(paste("Precision:", precision))

# Calculate F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("F1 Score:", f1_score))


## XGBoost-----------------

### 1. Prepare data-------------
# Ensure the target variable is a factor
train_data$default_90 <- as.factor(train_data$default_90)
test_data$default_90 <- as.factor(test_data$default_90)

# Convert train and test datasets to numeric matrices
train_matrix <- model.matrix(default_90 ~ . - 1, data = train_data)
test_matrix <- model.matrix(default_90 ~ . - 1, data = test_data)

# Extract the labels
train_labels <- as.numeric(train_data$default_90) - 1  # Convert to 0/1
test_labels <- as.numeric(test_data$default_90) - 1    # Convert to 0/1

### 2. Train Model----------------
# Define XGBoost parameters
params <- list(
  objective = "binary:logistic",  # Binary classification
  eval_metric = "logloss",        # Evaluation metric
  eta = 0.1,                      # Learning rate
  max_depth = 6,                  # Maximum depth of trees
  gamma = 0,                      # Minimum loss reduction
  colsample_bytree = 0.8,         # Column subsample ratio
  subsample = 0.8                 # Row subsample ratio
)

# Convert data to DMatrix format
dtrain <- xgb.DMatrix(data = train_matrix, label = train_labels)
dtest <- xgb.DMatrix(data = test_matrix, label = test_labels)

# Train the XGBoost model
xgb_model <- xgb.train(
  params = params, 
  data = dtrain, 
  nrounds = 100,                  # Number of boosting rounds
  watchlist = list(train = dtrain, test = dtest), 
  early_stopping_rounds = 10,    # Early stopping
  print_every_n = 10             # Print progress every 10 rounds
)


### 3. Evaluate Model-------------
# Predict probabilities on test data
pred_probs_xgb <- predict(xgb_model, newdata = dtest)
xgb_metrics <- calculate_metrics(pred_probs_xgb, test_data$default_90, 0.20)
xgb_metrics



### 4. Plots---------------

#### Confussion Matrix

threshold <- 0.20
predicted_class <- ifelse(pred_probs_xgb > threshold, 1, 0)

# Create confusion matrix
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$default_90)



conf_matrix_df <- as.data.frame(as.table(conf_matrix))

# Plot heatmap
ggplot(conf_matrix_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(
    title = "Confusion Matrix Heatmap",
    x = "Actual",
    y = "Predicted"
  ) +
  theme_minimal()

#### Coefficients-------------------

##### Shap---------------
library(SHAPforxgboost)
library(data.table)

# Convert your test dataset into a DMatrix
dtest <- xgb.DMatrix(data = test_matrix)

# Generate SHAP values for the test data
shap_values <- shap.values(xgb_model = xgb_model, X_train = train_matrix)

# Extract SHAP scores
shap_score <- shap_values$shap_score


# Plot a summary of SHAP values
shap_long <- shap.prep(xgb_model = xgb_model, X_train = train_matrix)
shap.plot.summary(shap_long) # this takes too long to render

# we can save it to save time
png("shap_summary_plot.png", width = 800, height = 600)
shap.plot.summary(shap_long)
dev.off()

# ligther plots
# Sample a subset of shap_long (e.g., 1000 rows)
shap_long_sample <- shap_long[1:min(2000, nrow(shap_long)), ]
# Plot the summary for the sampled data
shap.plot.summary(shap_long_sample)

##### Importance ---------------
xgb.importance(feature_names = colnames(train_matrix), model = xgb_model) %>%
  xgb.plot.importance()

# sample
xgb.importance(colnames(train_matrix), model = xgb_model)
# Continue from here-------------------------------------------------------------------

# Notes:
# All models should have coeficient paths