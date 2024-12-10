# Main Script---------------------------
# Course: Statistical Methods for High Dimensional Data
# Final Project

# Initialize program -------------------------
## Load Required Libraries --------------------------------
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
library(fastDummies)

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


## Data and plots paths ---------------------------------
data_path <- "../data/data.xlsx"
plots_dir = "../plots/grid_plots/"

data <- read_excel(data_path)

# Data Preprocessing ------------------------------------


## Additional column: Co-debtor -----------
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

# binary variables should not be considered as numeric variables, so we can detect them and change their class
binary_vars <- names(data)[sapply(data, function(x) all(x %in% c(0, 1)))]
data[binary_vars] <- lapply(data[binary_vars], factor, levels = c(0, 1))


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


### Bar Plots for non numeric variables------------
n_cols <- 5
n_rows <- ceiling(length(names(non_numeric_data)) / n_cols)

plots <- list()
for (col_name in names(non_numeric_data)) {
  plots[[col_name]] <- ggplot(non_numeric_data, aes_string(x = col_name)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = paste("Bar Plot of", col_name),
         x = col_name,
         y = "Frequency") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

pdf(file = paste(plots_dir, "Barplots_non_numeric_data.pdf", sep = ""), 
    width = n_cols * 3, height = n_rows * 3)
grid.arrange(grobs = plots, ncol = n_cols)
dev.off()

#check for labels with too low values (I've seen it from barplots)
table(non_numeric_data$periodicity)

#bimensual has only 1 observation -> let's remove it from the dataset (just an idea)
data <- data[data$periodicity != "bimensual", ]

#updating also the non_numeric_data
non_numeric_data <- non_numeric_data[rownames(non_numeric_data) %in% rownames(data), ]

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
data$days_due <- log1p(data$days_due)
data$city_pop_2018 <- log1p(data$city_pop_2018)


# Distributions after the log-transformations (again on the entire dataset)
# update numeric_data that is the data we use for histograms
numeric_data <- data[sapply(data, is.numeric)]


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

# Exclude binary variables and the target from the numeric_data dataframe -> already done previously

## Extract column names from vars_to_scale
vars_to_scale_names <- colnames(numeric_data)

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

## Oversampling of minority class --------------

# Check how much unbalanced the data are

table(train_data_scaled$default_90)

train_data_balanced <- ovun.sample(default_90 ~ ., data = train_data_scaled, method = "over")$data
table(train_data_balanced$default_90)

## Logistic Regression (Balanced Training Data) ----------------
logistic_model_balanced <- glm(default_90 ~ ., data = train_data_balanced, family = binomial)
summary(logistic_model_balanced)


logistic_results_balanced <- evaluate_model(logistic_model_balanced, test_data_scaled, "default_90")
print(logistic_results_balanced)

# f1 score > 0.7 is good
### Post-Estimation Plots---------------
par(mfrow = c(2,2))
plot(logistic_model_balanced)

#### 1. ROC Curve and AUC ------------------------------------
par(mfrow = c(1,1))

# Predicted probabilities
predicted_prob_balanced <- predict(logistic_model_balanced, newdata = test_data_scaled, type = "response")

# ROC and AUC
roc_curve_balanced <- roc(test_data_scaled$default_90, predicted_prob_balanced)
auc_value_balanced <- auc(roc_curve_balanced)

# Plot ROC Curve
plot(roc_curve_balanced, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value_balanced, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")
# auc > 0.7 is reasonable model
#### 2. Confusion Matrix Heatmap -----------------------------

# Predicted classes
predicted_class_balanced <- ifelse(predicted_prob_balanced > 0.5, 1, 0)

# Confusion matrix
conf_matrix_balanced <- table(Predicted = predicted_class_balanced, Actual = test_data_scaled$default_90)

# Heatmap
ggplot(as.data.frame(conf_matrix_balanced), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap (Balanced Data)", x = "Actual", y = "Predicted") +
  theme_minimal()

#### 3. Coefficient Plot -------------------------------------
# Extract coefficients
coefficients_balanced <- summary(logistic_model_balanced)$coefficients
coef_data_balanced <- as.data.frame(coefficients_balanced)
coef_data_balanced$Variable <- rownames(coefficients_balanced)
rownames(coef_data_balanced) <- NULL

# Plot coefficients
ggplot(coef_data_balanced, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Coefficient Plot (Balanced Data)", x = "Variable", y = "Estimate") +
  theme_minimal()


## Stepwise Logistic Regression (Balanced Training Data) ----------------

### 1. Forward Selection ------------------------

start_model_balanced <- glm(default_90 ~ 1, data = train_data_balanced, family = binomial)

# Define the full model with all predictors
full_model_balanced <- glm(default_90 ~ ., data = train_data_balanced, family = binomial)

# Perform forward selection
forward_model_balanced <- step(
  start_model_balanced, 
  scope = list(lower = start_model_balanced, upper = full_model_balanced), 
  direction = "forward", 
  trace = 0
)

summary(forward_model_balanced)

# Evaluate Forward Selection Model
forward_results_balanced <- evaluate_model(forward_model_balanced, test_data_scaled, "default_90")
print(forward_results_balanced)



### 2. Backward Elimination ---------------------
# Start with the full model
backward_model_balanced <- step(
  full_model_balanced, 
  direction = "backward", 
  trace = 0
)

summary(backward_model_balanced)

# Evaluate Backward Elimination Model
backward_results_balanced <- evaluate_model(backward_model_balanced, test_data_scaled, "default_90")
print(backward_results_balanced)

### Post-Estimation Plots for Forward Model ----------------
# Predicted probabilities
predicted_prob_forward_balanced <- predict(forward_model_balanced, newdata = test_data_scaled, type = "response")

#### 1. ROC Curve and AUC for Forward Model ----------
roc_curve_forward_balanced <- roc(test_data_scaled$default_90, predicted_prob_forward_balanced)
auc_value_forward_balanced <- auc(roc_curve_forward_balanced)
plot(roc_curve_forward_balanced, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value_forward_balanced, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### 2. Confusion Matrix Heatmap for Forward Model ---------
predicted_class_forward_balanced <- ifelse(predicted_prob_forward_balanced > 0.5, 1, 0)
conf_matrix_forward_balanced <- table(Predicted = predicted_class_forward_balanced, Actual = test_data_scaled$default_90)

ggplot(as.data.frame(conf_matrix_forward_balanced), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap (Forward Model - Balanced)", x = "Actual", y = "Predicted") +
  theme_minimal()

#### 3. Coefficient Plot for Forward Model --------------
coef_data_forward_balanced <- as.data.frame(summary(forward_model_balanced)$coefficients)
coef_data_forward_balanced$Variable <- rownames(coef_data_forward_balanced)
rownames(coef_data_forward_balanced) <- NULL

ggplot(coef_data_forward_balanced, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Coefficient Plot (Forward Model - Balanced)", x = "Variable", y = "Estimate") +
  theme_minimal()




## Lasso Logistic Regression (balanced training set) --------------

### 1. Prepare Data for glmnet --------------------------------

x_train_balanced <- as.matrix(train_data_balanced[, colnames(train_data_balanced) != "default_90"])
y_train_balanced <- train_data_balanced$default_90

x_test <- as.matrix(test_data_scaled[, colnames(test_data_scaled) != "default_90"])
y_test <- test_data_scaled$default_90

### 2. Perform Cross-Validation for Lasso Logistic Regression --
set.seed(123)
cv_lasso_balanced <- cv.glmnet(
  x_train_balanced, y_train_balanced, 
  family = "binomial", 
  alpha = 1, 
  standardize = FALSE,  # Dati già scalati
  nfolds = 10
)

# Plot dei risultati di Cross-Validation
lambda_min_balanced <- cv_lasso_balanced$lambda.min
lambda_1se_balanced <- cv_lasso_balanced$lambda.1se

par(mfrow = c(1, 1))
plot(cv_lasso_balanced, main = "Cross-Validation for Lasso Logistic Regression (Balanced Dataset)")
abline(v = log(lambda_min_balanced), col = "blue", lty = 2, lwd = 2)
abline(v = log(lambda_1se_balanced), col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("lambda.min", "lambda.1se"), col = c("blue", "red"), lty = 2, lwd = 2)


cat("Optimal lambda (balanced dataset):", lambda_min_balanced, "\n")

### 3. Fit the Final Model with Optimal Lambda -----------------
lasso_model_balanced <- glmnet(
  x_train_balanced, y_train_balanced, 
  family = "binomial", 
  alpha = 1, 
  lambda = lambda_min_balanced,
  standardize = FALSE  # data already scaled
)

###  4. Evaluate the Model with Adjusted Threshold ------------
evaluate_lasso <- function(predicted_prob, y_test, threshold = 0.3) {
  predicted_class <- ifelse(predicted_prob > threshold, 1, 0)
  accuracy <- mean(predicted_class == y_test)
  confusion <- confusionMatrix(factor(predicted_class), factor(y_test))
  auc_value <- auc(roc(y_test, predicted_prob))
  
  precision <- confusion$byClass["Pos Pred Value"]
  recall <- confusion$byClass["Sensitivity"]
  
  f1_score <- ifelse(is.na(precision) || is.na(recall), 0, 
                     2 * (precision * recall) / (precision + recall))
  
  return(list(accuracy = accuracy, f1_score = f1_score, auc = auc_value, confusion = confusion))
}

### 5. Predicted Probabilities and Evaluation -----------------
predicted_prob_balanced <- predict(lasso_model_balanced, s = lambda_min_balanced, newx = x_test, type = "response")
lasso_results_balanced <- evaluate_lasso(predicted_prob_balanced, y_test, threshold = 0.5)
print(lasso_results_balanced)



### 6. Post-Estimation Plots ----------------------------------

#### (1) ROC Curve and AUC---------
roc_curve_balanced <- roc(y_test, predicted_prob_balanced)
plot(roc_curve_balanced, col = "blue", main = paste("ROC Curve (AUC =", round(lasso_results_balanced$auc, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")



#### (2) Confusion Matrix Heatmap------------
predicted_class_balanced <- ifelse(predicted_prob_balanced > 0.3, 1, 0)
conf_matrix_balanced <- table(Predicted = predicted_class_balanced, Actual = y_test)
ggplot(as.data.frame(conf_matrix_balanced), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap (Balanced Dataset)", x = "Actual", y = "Predicted") +
  theme_minimal()


#### (3) Coefficient Plot ----------------------------------------
coef_data_balanced <- as.data.frame(as.matrix(coef(lasso_model_balanced, s = lambda_min_balanced)))
coef_data_balanced$Variable <- rownames(coef_data_balanced)
colnames(coef_data_balanced) <- c("Coefficient", "Variable")
coef_data_balanced <- coef_data_balanced %>% filter(Coefficient != 0 & Variable != "(Intercept)")

ggplot(coef_data_balanced, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Lasso Coefficient Plot (Balanced Dataset)", x = "Variable", y = "Coefficient") +
  theme_minimal()


# Model Training and Evaluation (origial unbalanced data) ----


## 1.- Logistic Regression----------------
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





## Stepwise Logistic Regression (original data) ----------------

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


## 2.- Backward Elimination ---------------------
# Start with the full model
backward_model <- step(
  full_model, 
  direction = "backward", 
  trace = 0
)

# Make predictions on the test data
predicted_probs_bw <- predict(backward_model, newdata = test_data, type = "response")

#### Model Evaluation: Post-Estimation Plots for Backward Model ----
bw_metrics <- calculate_metrics(predicted_probs_bw, test_data$default_90)
bw_metrics

##### 1. ROC Curve and AUC for Backward Model ----------
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

##### 2. Precision-Recall (PR) Curve for Backward Model ------
# Generate PR curve
pr_curve <- pr.curve(scores.class0 = predicted_prob[true_labels == 1],
                     scores.class1 = predicted_prob[true_labels == 0],
                     curve = TRUE)

# Plot PR Curve
plot(pr_curve, main = paste("Precision-Recall Curve (AUC =", round(pr_curve$auc.integral, 2), ")"),
     col = "red", xlab = "Recall", ylab = "Precision")


##### 3. Confusion Matrix Heatmap for Backward Model ---------
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
conf_matrix <- table(Predicted = predicted_class, Actual = test_data$default_90)

ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap (Backward Model)", x = "Actual", y = "Predicted") +
  theme_minimal()


##### 4. Coefficient Plot for Backward Model --------------
coef_data <- as.data.frame(summary(backward_model)$coefficients)
coef_data$Variable <- rownames(coef_data)
rownames(coef_data) <- NULL

ggplot(coef_data, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Coefficient Plot (Backward Model)", x = "Variable", y = "Estimate") +
  theme_minimal()


## 3.- Lasso Logistic Regression--------------

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


## 4.- Elastic Net Logistic Regression--------------

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

threshold_results_en <- find_optimal_threshold(predicted_probs_en, true_labels, thresholds)
plot_metrics(threshold_results_en)


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


#### (2) Confusion Matrix Heatmap-------------
predicted_class <- ifelse(predicted_probs_en > 0.25, 1, 0) # using optimal threshold
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



## 5.- Group Lasso Logistic Regression--------------

### 1. Prepare Data for Group Lasso -------------------------

# Define groups for numeric and factor variables
data_no_target <- data[, !colnames(data) %in% "default_90"]
numeric_data <- data_no_target[sapply(data_no_target, is.numeric)]
non_numeric_data <- data_no_target[, !sapply(data_no_target, is.numeric)]

group_vector <- c()  # Stores group IDs
dummy_list <- list()  # Stores dummy variables
group_id <- 1

# Numeric variables (each variable is its own group)
group_vector <- c(group_vector, seq(group_id, length.out = ncol(numeric_data)))
group_id <- max(group_vector) + 1

# Factor variables (each factor's dummies are a group)
for (col_name in colnames(non_numeric_data)) {
  # Create dummy variables, dropping the first level
  dummies <- as.matrix(
    model.matrix(as.formula(paste("~", col_name)),
                           data = data_no_target)[, -1]
    )
  
  # Check if the resulting dummies matrix has at least one column
  if (!is.null(ncol(dummies)) && ncol(dummies) > 0) {
    # Store the dummies in the list
    dummy_list[[col_name]] <- dummies
    
    # Add group markers for the current set of dummy variables
    group_vector <- c(group_vector, rep(group_id, ncol(dummies)))
    
    # Increment the group ID for the next factor
    group_id <- group_id + 1
  } else {
    # Handle cases where no dummy variables are created (e.g., single-level factors)
    warning(paste("No dummy variables created for", col_name, "as it has only one level."))
  }
}

# Split dummy data
dummy_data <- do.call(cbind, dummy_list)
dummy_data_train = subset(dummy_data, split)
dummy_data_test = subset(dummy_data, !split)

# Split and scale numeric data
numeric_data_train = subset(numeric_data, split)
numeric_data_test = subset(numeric_data, !split)

preproc <- preProcess(numeric_data_train, method = c("center", "scale"))
numeric_data_train_scaled <- predict(preproc, numeric_data_train)
numeric_data_test_scaled <- predict(preproc, numeric_data_test)

# Merge data and define target variable
X_train <- as.matrix(cbind(numeric_data_train_scaled, dummy_data_train))
X_test <- as.matrix(cbind(numeric_data_test_scaled, dummy_data_test))

y <- 2*data$default_90 - 1  # Convert target {0,1} to {-1, 1}
y_train = subset(y, split)
y_test = subset(y, !split)


### 2. Perform Cross-Validation for Group Lasso -----------------
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

set.seed(123)
cv_fit_gglasso <- cv.gglasso(
  x = X_train, 
  y = y_train, 
  group = group_vector, 
  loss = "logit", 
  nfolds = 10
)

stopCluster(cl)

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


### 3. Fit the Final Group Lasso Model -------------------------
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

group_lasso_model <- gglasso(
  x = X_train, 
  y = y_train, 
  group = group_vector, 
  loss = "logit", 
  lambda = lambda_min_gl
)
stopCluster(cl)

### 4. Evaluate Group Lasso Model ------------------------------
# Predicted probabilities
log_odds_gl <- predict(group_lasso_model, newx = X_test, type = "link")
log_odds_gl
predicted_probs_gl <- 1 / (1 + exp(-log_odds_gl))

gl_metrics <- calculate_metrics(predicted_probs_gl, test_data$default_90)
gl_metrics

### 5. Post-Estimation Plots for Group Lasso -------------------
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


# Optimize threshold
thresholds <- seq(0, 1, by = 0.05)  # Example grid of thresholds
threshold_results_gl <- find_optimal_threshold(predicted_probs_gl, y_test_binary, thresholds)
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



## 6.- GAM Model----------------
# we can use the natural variable selection that lasso provides:
print(vars_lasso)

# Using most relevant poly order (`s()` indicates the poly order for each predictor)
# GAM using Lasso selected vars
gam_model <- gam(default_90 ~ agency + income_group + s(contributions_balance) +
                   s(credit_limit)+
                   s(credit_duration) + s(dtf_approval_date)+ 
                   has_codebtor + m_date_limit, 
                 data = train_data, 
                 family = binomial)

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


## 7.- GAM Iterative [TO DO if there's time] ----------------


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



## 8.- Sparse PCA regression--------------------

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
fit <- spca(x_train_spca, k = k, scale = TRUE, verbose = FALSE)
summary(fit)

### Step 2: Choose Optimal Alpha -----------------------------------
var.sp <- NULL
reg.par <- seq(0, 0.25, length = 20)  # Regularization parameters for alpha tuning

for (j in 1:20) {
  fit <- spca(x_train_spca, k = 5, alpha = reg.par[j], scale = TRUE, verbose = FALSE)
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
  names(v) <- colnames(x_train_spca)[ind]  # Name the loadings by the original feature names
  cat("Component", j, ":\n")
  print(v)
}

### Step 5: Regression on Principal Components ----------------------
# Standardize the input data and calculate principal component scores
pcData <- as.data.frame(scale(x_train_spca) %*% V)

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
par(mfrow = c(1,2))
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

par(mfrow = c(1,1))



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

threshold_results_melted <- melt(threshold_results, id.vars = "Threshold", variable.name = "Metric", value.name = "Value")
ggplot(threshold_results_melted, aes(x = Threshold, y = Value, color = Metric)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "Precision, Recall, and F1 Score vs Threshold", x = "Threshold", y = "Value") +
  scale_color_manual(values = c("blue", "red", "green"))




#### 3. Confussion Matrix-------------

predicted_class_optimal <- ifelse(predicted_prob_pca > optimal_threshold, 1, 0)
conf_matrix <- table(Predicted = predicted_class_optimal, Actual = y_train_pca)
conf_matrix_df <- as.data.frame(conf_matrix)

# Plot Confusion Matrix Heatmap
ggplot(conf_matrix_df, aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()



## 9.- SVM ---------------

# Step 1: Scale the data

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



## 10.- Random Forest-------------------

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




## 11.- XGBoost-----------------

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


### 4. Plots ----

#### Confussion Matrix ----

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


### SHAP [TO DO if there's time...] -----------------