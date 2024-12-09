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
library(doParallel)
library(gridExtra)
library(ggthemes)
library(ROSE)
library(reshape2)
library(MLmetrics)

Sys.setlocale("LC_TIME", "en_US")

## Helper functions---------------

#### Evaluate Logistic Regression ------------
evaluate_model <- function(model, test_data, target_col) {
  predicted_prob <- predict(model, newdata = test_data, type = "response")
  predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
  accuracy <- mean(predicted_class == test_data[[target_col]])
  confusion <- confusionMatrix(factor(predicted_class), factor(test_data[[target_col]]))
  
  # Evita errori per F1 score se ci sono zero TP o FN
  pos_pred_value <- confusion$byClass["Pos Pred Value"]
  sensitivity <- confusion$byClass["Sensitivity"]
  f1_score <- ifelse(is.na(pos_pred_value) | is.na(sensitivity), 0,
                     2 * (pos_pred_value * sensitivity) / (pos_pred_value + sensitivity))
  
  return(list(accuracy = accuracy, f1_score = f1_score))
}

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


# Set Working Directory ---------------------------------
data_path <- "../data/data.xlsx"
plots_dir = "../plots/grid_plots/"

# Import Data -------------------------------------------
data <- read_excel(data_path)





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

# Data Preprocessing ------------------------------------

#Initial check of the data
summary(data)
anyNA(data)


## Single-Value columns

# check if there are sing-val col
constant_cols <- sapply(data, function(col) length(unique(col)) == 1)
names(data)[constant_cols]

# remove them
data <- data[, !names(data) %in% "Clasificación Tipo Crédito"]

## Remove ID Variables
id_vars <- c("Código de Crédito", "ID Cliente", "No Pagaré Rotativo")
data <- data[, !names(data) %in% id_vars]


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


# Removing AGE because it is a leaker --------------
data <- data[, !names(data) %in% c("age")]

## DateTime variables-----------
# Create Derived Variables
data <- data %>%
  mutate(
    time_difference_days = as.numeric(difftime(as.Date(date_limit), as.Date(date_approval), units = "days"))
  )

# First create year, month, day, weekday
# Extract features from the date-time variables

# date approval
data$m_date_approval <- format(data$date_approval, "%m")
data$wd_date_approval <- weekdays(data$date_approval)

# date limit
data$m_date_limit <- format(data$date_limit, "%m")
data$wd_date_limit <- weekdays(data$date_limit)

# Remove date_approval and date_limit:
data <- data[, !(names(data) %in% c("date_approval", "date_limit"))]


# capital_due30: -------------

# Represents the amount by which customers have exceeded their credit limit,
### specifically for those who have been overdue for more than 30 days.
### But most of clients have not overdue the credit, so the most of values are 0
# - does not add information to our analysis

# -> It will get transformed to a binary variable 0/1 indicating if a client has overdue the credit in 30+ days
# --> assuming that 30+ days overdue is more likely to progress to 90+ days overdue rather than who's currently at 0 days
data <- data %>%
  mutate(
    capital_due30_binary = ifelse(capital_due30 > 0, 1, 0)
  )

# Remove capital_due30 in order to keep only capital_due30_binary
data <- data[, !names(data) %in% "capital_due30"]

# Move "default_90" to the last column
data <- data[, c(setdiff(names(data), "default_90"), "default_90")]

# income_group has numbers indicating ascending groups, so it's more appropriate
# to consider it as an ordinal categorical variable instead of a numerical variable
data$income_group <- factor(data$income_group, ordered = TRUE)


# Checking distributions on the entire dataset as an initial exploration
## Numeric Variables------------

numeric_data <- data %>% select(where(is.numeric))
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
  plots[[col_name]] <- ggplot(data, aes(x = factor(default_90), y = .data[[col_name]], fill = factor(default_90))) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplot of", col_name, "by Target"),
         x = "Target",
         y = col_name,
         fill = "Target") +
    theme_minimal()
}

pdf(file = paste(plots_dir, "01_EDA__numeric_vars_boxplots.pdf", sep=""), width = n_cols * 3, height = n_rows * 3)  # Adjust dimensions
grid.arrange(grobs = plots, ncol = n_cols)
dev.off()


### Density Plots--------------
n_cols <- 5
n_rows <- ceiling(length(colnames(numeric_data)) / n_cols)

plots <- list()
for (col_name in colnames(numeric_data)) {
  plots[[col_name]] <- ggplot(data, aes(x = .data[[col_name]], fill = factor(default_90))) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Density Plot of", col_name), x = col_name, fill = "Target") +
    theme_minimal()
}

pdf(file = paste(plots_dir, "02_EDA__numeric_vars_densities.pdf", sep=""), width = n_cols * 3, height = n_rows * 3)  # Adjust dimensions
grid.arrange(grobs = plots, ncol = n_cols)
dev.off()


### Correlation Matrix -------------------
cor_matrix <- cor(numeric_data, use = "complete.obs")
melted_cor_matrix <- melt(cor_matrix)

melted_cor_matrix <- melted_cor_matrix[as.numeric(melted_cor_matrix$Var1) > as.numeric(melted_cor_matrix$Var2), ]

ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  coord_fixed() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()  # in order to remove the grid
  ) +
  scale_x_discrete(limits = unique(melted_cor_matrix$Var1)) + 
  scale_y_discrete(limits = unique(melted_cor_matrix$Var2))



## Non-Numeric Variables------------

non_numeric_data <- data %>% select(where(~ !is.numeric(.)))
unique_counts <- sapply(non_numeric_data, function(x) length(unique(x)))
mode_values <- sapply(non_numeric_data, function(x) names(which.max(table(x))))
print(mode_values)

### Chi Squared------------

# The output will show the Chi-square statistic, degrees of freedom, and the p-value.
# A p-value less than 0.05 indicates that there is a significant association 
# between the non-numeric variable and the target variable default_90.

# Create a vector to store p-values
p_values <- c()

# Perform Chi-square tests and collect p-values
for (col_name in colnames(non_numeric_data)) {
  contingency_table <- table(non_numeric_data[[col_name]], data$default_90)
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


# Transformations ---------------------------------------
# Applying log transformation only to too much skewed features bc for the others could be sufficient the scaling
data$contributions_balance <- log1p(data$contributions_balance)
data$credit_limit <- log1p(data$credit_limit)
data$capital_balance <- log1p(data$capital_balance)
data$installment <- log1p(data$installment)
data$time_difference_days <- log1p(data$time_difference_days)


# Distributions after the log-transformations (again on the entire dataset)


### Histograms

n_cols <- 5
n_rows <- ceiling(length(colnames(numeric_data)) / n_cols)


pdf(paste(plots_dir,"03_EDA__numeric_vars_after_log_histograms.pdf", sep=""), width = n_cols * 3, height = n_rows * 3)  # Adjust dimensions
par(mfrow = c(n_rows, n_cols))

for (col_name in colnames(numeric_data)) {
  hist(data[[col_name]], main = paste("Histogram of", col_name),
       xlab = col_name, col = "lightblue", border = "black")
}
dev.off()  # Close the PDF device
par(mfrow = c(1, 1))


### Box Plots---------------

n_cols <- 5
n_rows <- ceiling(length(colnames(numeric_data)) / n_cols)

plots <- list()
for (col_name in colnames(numeric_data)) {
  plots[[col_name]] <- ggplot(data, aes(x = factor(default_90), y = .data[[col_name]], fill = factor(default_90))) +
    geom_boxplot(alpha = 0.7) +
    labs(title = paste("Boxplot of", col_name, "by Target"),
         x = "Target",
         y = col_name,
         fill = "Target") +
    theme_minimal()
}

pdf(file = paste(plots_dir, "04_EDA__numeric_vars_after_log_boxplots.pdf", sep=""), width = n_cols * 3, height = n_rows * 3)  # Adjust dimensions
grid.arrange(grobs = plots, ncol = n_cols)
dev.off()


### Density Plots--------------
n_cols <- 5
n_rows <- ceiling(length(colnames(numeric_data)) / n_cols)

plots <- list()
for (col_name in colnames(numeric_data)) {
  plots[[col_name]] <- ggplot(data, aes(x = .data[[col_name]], fill = factor(default_90))) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Density Plot of", col_name), x = col_name, fill = "Target") +
    theme_minimal()
}

pdf(file = paste(plots_dir, "05_EDA__numeric_vars_after_log_densities.pdf", sep=""), width = n_cols * 3, height = n_rows * 3)  # Adjust dimensions
grid.arrange(grobs = plots, ncol = n_cols)
dev.off()



# Train-Test Split --------------------------------------
set.seed(123)
split <- sample.split(data$default_90, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)


## Scaling -----------------

# do not consider binary variables for scaling:
binary_vars <- names(data)[sapply(data, function(x) all(x %in% c(0, 1)))]

# Exclude binary variables and the target from the numeric_data dataframe
vars_to_scale <- numeric_data[, !(names(numeric_data) %in% binary_vars)]

# Calculate mean and standard deviation from the training set for the variables to scale:

## Extract column names from vars_to_scale
vars_to_scale_names <- colnames(vars_to_scale)

## Mean and SD in train_data scaling vars_to_scale
mean_train <- apply(train_data[vars_to_scale_names], 2, mean)
sd_train <- apply(train_data[vars_to_scale_names], 2, sd)

# Scale the training set
train_data_scaled <- train_data
train_data_scaled[vars_to_scale_names] <- sweep(train_data[vars_to_scale_names], 2, mean_train, "-")
train_data_scaled[vars_to_scale_names] <- sweep(train_data_scaled[vars_to_scale_names], 2, sd_train, "/")

# Scale the test set
test_data_scaled <- test_data
test_data_scaled[vars_to_scale_names] <- sweep(test_data[vars_to_scale_names], 2, mean_train, "-")
test_data_scaled[vars_to_scale_names] <- sweep(test_data_scaled[vars_to_scale_names], 2, sd_train, "/")

# Check scaling Z-score -> mean = 0; sd = 1
summary(train_data_scaled[vars_to_scale_names])
apply(train_data_scaled[vars_to_scale_names], 2, sd)


# Model Training and Evaluation (only on BALANCED data) ------------------------

## SVM ---------------
### 9.1) Feature adjusting ------------------
train_data <- train_data %>%
  mutate(across(c(default_90, has_codebtor), as.factor)) # Convert binary variables to factors

test_data <- test_data %>%
  mutate(across(c(default_90, has_codebtor), as.factor)) # Ensure consistency in test data


# Step 2: Scale numeric variables only / is already scaled
#scaler <- preProcess(train_data[, numeric_vars], method = c("center", "scale"))
train_data_scaled <- train_data
test_data_scaled <- test_data

# get only features for prediction step
test_data_features <- test_data_scaled %>% select(-default_90)


### 9.2) Train Model--------------------
### 9.2.1) Standard--------------------
# svm_model <- svm(
#   default_90 ~ ., 
#   data = train_data_scaled, 
#   kernel = "radial", 
#   cost = 1, 
#   gamma = 0.1,
#   probability = TRUE # Train the model with probability enabled
# )
# 
# # Predict probabilities
# predicted_probs <- predict(svm_model, newdata = test_data_features, probability = TRUE)
# 
# # Extract the probabilities
# probabilities <- attr(predicted_probs, "probabilities")
# 
# prob_class_1 <- probabilities[, "1"]  # Replace "1" with the exact name of your class
# head(prob_class_1)

### 9.3) Fine Tuning----------------------

# Step 1: Adjust target variable levels to valid R names
train_data_scaled$default_90 <- factor(train_data_scaled$default_90, levels = c(0, 1), labels = c("No", "Yes"))
test_data_scaled$default_90 <- factor(test_data_scaled$default_90, levels = c(0, 1), labels = c("No", "Yes"))

head(train_data_scaled$default_90)



# Step 2: Define trainControl for cross-validation
train_control <- trainControl(
  method = "cv",          # Cross-validation
  number = 3,             # Number of folds
  classProbs = TRUE,      # Compute class probabilities
  verboseIter = TRUE      # Show progress during training
)

# Step 3: Define parameter grid for cost and gamma
# tune_grid <- expand.grid(
#   C = c(0.1, 1, 10, 100),  # Values for cost
#   sigma = c(0.01, 0.1, 1)  # Values for gamma
# )

tune_grid <- expand.grid(
  C = c(1, 10),  # Values for cost
  sigma = c(0.01, 0.1)  # Values for gamma
)

# Register parallel backend
cl <- makeCluster(detectCores() - 1)  # Use all but one core
registerDoParallel(cl)

# Step 4: Train the SVM model with grid search
svm_model <- train(
  default_90 ~ .,           # Formula
  data = train_data_scaled, # Training data
  method = "svmRadial",     # Radial kernel SVM
  tuneGrid = tune_grid,     # Hyperparameter grid
  trControl = train_control # Cross-validation settings
)
stopCluster(cl)

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

### 9.2.2) SVM on f1 score------------

# hyperparameters grid 
# tune_grid <- expand.grid(
#   C = c(0.1, 1, 10, 100),  # Values for cost
#   sigma = c(0.01, 0.1, 1)  # Values for gamma
# )

# smaller grid for testing
tune_grid <- expand.grid(
  C = c(1, 10),  # Values for cost
  sigma = c(0.01, 0.1)  # Values for gamma
)

# Register parallel backend
cl <- makeCluster(detectCores() - 1)  # Use all but one core
registerDoParallel(cl)

ctrl <- trainControl(method = "cv", # choose your CV method 
                     number = 3, # choose a bigger number once it runs / maybe 5
                     summaryFunction = prSummary, # TO TUNE ON F1 SCORE
                     classProbs = T,
                     verboseIter = T,
                     sampling = "smote" # is good for imbalanced dataset
)

svm_model <- train(default_90 ~., data = train_data_scaled,
                   method = "svmRadial",
                   #preProcess = c("center", "scale"),
                   #tuneLength = 10,
                   metric = "F", # The metric used for tuning is the F1 SCORE
                   trControl = ctrl,
                   tuneGrid = tune_grid,     # Hyperparameter grid
                   #probability = TRUE # Train the model with probability enabled
)
stopCluster(cl)

svm_model


svm_preds <- predict(svm_model, newdata = test_data_features)


# Evaluate Model Performance
confusionMatrix(svm_preds, test_data_scaled$default_90)

# probabilities:

svm_probs <- predict(svm_model, newdata = test_data_features, type = "prob")

head(svm_probs)

prob_class_yes <- svm_probs$Yes
head(prob_class_yes)
svm_metrics <- calculate_metrics(prob_class_yes, test_data$default_90, threshold = 0.5)

svm_metrics


thresholds <- seq(0, 1, by = 0.05)  # Example grid of threshold
opt_metrics_svm <- find_optimal_threshold(prob_class_yes, test_data$default_90, thresholds)
opt_metrics_svm
plot_metrics(opt_metrics_svm)

