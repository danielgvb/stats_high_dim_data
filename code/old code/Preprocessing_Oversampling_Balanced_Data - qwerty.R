# Load Required Libraries --------------------------------
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
library(gridExtra)
library(ggthemes)

# Set Working Directory ---------------------------------
data_path <- "../data/data.xlsx"

# Import Data -------------------------------------------
data <- read_excel(data_path)

# Data Preprocessing ------------------------------------

#Initial check of the data
summary(data)

## Handle Missing Values
#na_counts <- colSums(is.na(data))
#print(na_counts)

# Ideally we should not check each column
# because in HD dataset we could have a very huge number of features,
# so maybe anyNA() could be more appropriate and faster
anyNA(data)

## Single-Value columns

## Remove Single-Value Columns
#single_value_vars <- c("Clasificación Tipo Crédito")
#data <- data[, !names(data) %in% single_value_vars]

# check if there are sing-val col
constant_cols <- sapply(data, function(col) length(unique(col)) == 1)
names(data)[constant_cols]

# eventually remove them
data <- data[, !names(data) %in% "Clasificación Tipo Crédito"]


## Remove ID Variables
id_vars <- c("Código de Crédito", "ID Cliente", "No Pagaré Rotativo")
data <- data[, !names(data) %in% id_vars]


## Rename Columns for Clarity
# friendly_names <- c("agency", "status", "rating", "work", "age", "civil_status",
#                     "income_group", "city_born", "max_education", "gender", 
#                     "contributions_balance", "credit_limit", "capital_balance",
#                     "capital_due30", "days_due", "date_approval",
#                     "installment", "periodicity", "credit_duration", "date_limit",
#                     "dtf_approval_date", "fx_approval_date", "city_pop_2018", "default_90")


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


#REMOVE AGE because of Noise--------------
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
# I think they're not useful now that they have been used to create a new variable
data <- data[, !(names(data) %in% c("date_approval", "date_limit"))]

# capital_due30: -------------

# - represents the amount by which customers have exceeded their credit limit,
### specifically for those who have been overdue for more than 30 days.
### But most of clients have not overdue the credit, so the most of values are 0
# - does not add information to our analysis

# -> I suggest to transform it in binary variable 0/1 indicating if a client has overdue the credit in 30+ days
# --> assuming that 30+ days overdue is more likely to progress to 90+ days overdue rather than who's currently at 0 days
data <- data %>%
  mutate(
    capital_due30_binary = ifelse(capital_due30 > 0, 1, 0)
  )

# Remove capital_due30 in order to keep only capital_due30_binary
data <- data[, !names(data) %in% "capital_due30"]

# Move "default_90" to the last column
data <- data[, c(setdiff(names(data), "default_90"), "default_90")]


# Check distributions (on the entire dataset, in order to understand what to do) -----------------------------------

## Numeric Variables------------

# income_group has numbers indicating groups, so maybe it's more appropriate
# considering it as an ordinal categorical variable instead of a numerical variable
data$income_group <- factor(data$income_group, ordered = TRUE)

numeric_data <- data %>% select(where(is.numeric))
skim(numeric_data)

### Histograms
for (col_name in colnames(numeric_data)) {
  hist(numeric_data[[col_name]], main = paste("Histogram of", col_name),
       xlab = col_name, col = "lightblue", border = "black")
}

### Box Plots---------------
for (col_name in colnames(numeric_data)) {
  print(
    ggplot(data, aes(x = factor(default_90), y = .data[[col_name]], fill = factor(default_90))) +
      geom_boxplot(alpha = 0.7) +  # Add transparency to boxplots
      labs(
        title = paste("Boxplot of", col_name, "by Target"),
        x = "Target", 
        y = col_name, 
        fill = "Target"
      ) +
      theme_economist() +  # Apply The Economist theme
      scale_fill_economist() +  # Use The Economist fill palette
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels if needed
        axis.title.x = element_text(margin = margin(t = 10))  # Add margin to x-axis title
      )
  )
}

### Density Plots--------------
for (col_name in colnames(numeric_data)) {
  print(
    ggplot(data, aes(x = .data[[col_name]], fill = factor(default_90))) +
      geom_density(alpha = 0.5) +  # Set transparency for overlapping densities
      labs(
        title = paste("Density Plot of", col_name),
        x = col_name, 
        fill = "Target"
      ) +
      theme_economist() +  # Apply The Economist theme
      scale_fill_economist() +  # Use Economist palette for the fill aesthetic
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        axis.title.x = element_text(margin = margin(t = 10))  # Add margin to x-axis title
      )
  )
}

### Correlation Matrix -------------------
cor_matrix <- cor(numeric_data, use = "complete.obs")
melted_cor_matrix <- melt(cor_matrix)

# We could filter the upper or lower half of the corr matrix in order to get a easier visualization of correlations

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
# if it's just for EDA, we could check non-num var on the entire dataset
# if its results influence the analysis, we should do it only on the training set (bc of data leakage risk)

# I don't think the results influence the analysis so I'm doing it on the entire dataset, but we should talk about it

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


# Transformations ---------------------------------------
# Applying log transformation only to too much skewed features bc for the others could be sufficient the scaling
data$contributions_balance <- log1p(data$contributions_balance)
data$credit_limit <- log1p(data$credit_limit)
data$capital_balance <- log1p(data$capital_balance)
data$installment <- log1p(data$installment)
data$time_difference_days <- log1p(data$time_difference_days)


# Distributions after the log-transformations (again on the entire dataset)

### Histograms ----------------
for (col_name in colnames(numeric_data)) {
  hist(numeric_data[[col_name]], main = paste("Histogram of", col_name),
       xlab = col_name, col = "lightblue", border = "black")
}

### Box Plots---------------
for (col_name in colnames(numeric_data)) {
  print(
    ggplot(data, aes(x = factor(default_90), y = .data[[col_name]], fill = factor(default_90))) +
      geom_boxplot(alpha = 0.7) +  # Add transparency to boxplots
      labs(
        title = paste("Boxplot of", col_name, "by Target"),
        x = "Target", 
        y = col_name, 
        fill = "Target"
      ) +
      theme_economist() +  # Apply The Economist theme
      scale_fill_economist() +  # Use The Economist fill palette
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels if needed
        axis.title.x = element_text(margin = margin(t = 10))  # Add margin to x-axis title
      )
  )
}

### Density Plots--------------
for (col_name in colnames(numeric_data)) {
  print(
    ggplot(data, aes(x = .data[[col_name]], fill = factor(default_90))) +
      geom_density(alpha = 0.5) +  # Set transparency for overlapping densities
      labs(
        title = paste("Density Plot of", col_name),
        x = col_name, 
        fill = "Target"
      ) +
      theme_economist() +  # Apply The Economist theme
      scale_fill_economist() +  # Use Economist palette for the fill aesthetic
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        axis.title.x = element_text(margin = margin(t = 10))  # Add margin to x-axis title
      )
  )
}


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

### Oversampling of minority class --------------

# Check how much unbalanced the data are

table(train_data_scaled$default_90)

# oversampling of minority class:
# install.packages("ROSE")
library(ROSE)

train_data_balanced <- ovun.sample(default_90 ~ ., data = train_data_scaled, method = "over")$data
table(train_data_balanced$default_90)


## Logistic Regression (Balanced Training Data) ----------------
logistic_model_balanced <- glm(default_90 ~ ., data = train_data_balanced, family = binomial)
summary(logistic_model_balanced)

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

logistic_results_balanced <- evaluate_model(logistic_model_balanced, test_data_scaled, "default_90")
print(logistic_results_balanced)

# f1 score > 0.7 is good
#### Post-Estimation Plots---------------
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

library(MASS)  # For stepwise regression functions

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

### Post-Estimation Plots for Backward Model ----------------

# Predicted probabilities
predicted_prob_backward_balanced <- predict(backward_model_balanced, newdata = test_data_scaled, type = "response")

#### 1. ROC Curve and AUC for Backward Model ----------
roc_curve_backward_balanced <- roc(test_data_scaled$default_90, predicted_prob_backward_balanced)
auc_value_backward_balanced <- auc(roc_curve_backward_balanced)
plot(roc_curve_backward_balanced, col = "blue", main = paste("ROC Curve (AUC =", round(auc_value_backward_balanced, 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

#### 2. Confusion Matrix Heatmap for Backward Model ---------
predicted_class_backward_balanced <- ifelse(predicted_prob_backward_balanced > 0.5, 1, 0)
conf_matrix_backward_balanced <- table(Predicted = predicted_class_backward_balanced, Actual = test_data_scaled$default_90)

ggplot(as.data.frame(conf_matrix_backward_balanced), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap (Backward Model - Balanced)", x = "Actual", y = "Predicted") +
  theme_minimal()

#### 3. Coefficient Plot for Backward Model --------------
coef_data_backward_balanced <- as.data.frame(summary(backward_model_balanced)$coefficients)
coef_data_backward_balanced$Variable <- rownames(coef_data_backward_balanced)
rownames(coef_data_backward_balanced) <- NULL

ggplot(coef_data_backward_balanced, aes(x = reorder(Variable, Estimate), y = Estimate)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Coefficient Plot (Backward Model - Balanced)", x = "Variable", y = "Estimate") +
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