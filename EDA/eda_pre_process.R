# EDA Statistical Methods High Dim Data--------------

# Import Data-------------
library(readxl)
data <- read_excel("C:/Users/danie/Documents/GitHub/stats_high_dim_data/data/data.xlsx")

# 16 December
# 20 min presentation and 10 of discussion

# Remove ids-------
colnames(data)
# Some vars are ids, lets remove them to avoid perfect classification
data <- data[, !names(data) %in% c("Código de Crédito", "ID Cliente",
                                   "No Pagaré Rotativo")]
str(data)

# Distinct values-------------
# Distinct values to see balance
# Count the number of distinct values for each column
num_distinct_values <- sapply(data, function(col) length(unique(col)))

# Print the number of distinct values for each column
num_distinct_values

#  "Clasificación Tipo Crédito" is single value, remove
data <- data[, !names(data) %in% c("Clasificación Tipo Crédito")]


# New user friendly names
colnames(data)

friendly_names <- c("agency", "status", "rating", "work", "age", "civil_status",
                    "income_group", "city_born", "max_education", "gender", 
                    "contributions_balance", "credit_limit", "capital_balance",
                    "capital_due30", "days_due", "date_approval",
                    "installment", "periodicity","credit_duration", "date_limit",
                    "dtf_approval_date", "fx_approval_date",
                    "default_90")

# Check that the number of new names matches the number of columns
if (length(friendly_names) == ncol(data)) {
  # Assign the new names to the data frame
  colnames(data) <- friendly_names
} else {
  stop("The number of column names provided does not match the number of columns in the data frame.")
}

# Null Values--------
# Count of NA values in each column
na_counts <- colSums(is.na(data))

# Display columns with their respective NA counts
print(na_counts)
# No null values

# Variables Transformation-------------

# Create new variables

# % of credit already payed
#data$contributions_limit <- data$contributions_balance / data$credit_limit
#data$capital_limit <- data$capital_balance / data$credit_limit
#data$installment_limit <- data$installment / data$credit_limit
#data$installment_contributions <- data$installment / data$contributions_balance
#data$installment_capital <- data$installment / data$capital_balance



# Subset the data frame for credit limit above 50.000
# credits below 50.000 seem odd (is like 12 EUR)

subset_data <- data[data$credit_limit > 50000, ]

# see dimension of subset data
nrow(subset_data)
mean(subset_data$default_90)

# Use subset data because is more sense:
data <- subset_data

# Some plots of the created variables to see if they are useful
# library(ggplot2)
# ggplot(subset_data, aes(x = factor(default_90), y = contributions_limit)) +
#   geom_boxplot(fill = "lightblue") +
#   labs(title = "Box Plot of Contributions / credit Limit by Target",
#        x = "Target",
#        y = "Contributions Limit") +
#   theme_minimal()
# 
# ggplot(subset_data, aes(x = factor(default_90), y = capital_limit)) +
#   geom_boxplot(fill = "lightblue") +
#   labs(title = "Box Plot of Capital / credit Limit by Target",
#        x = "Target",
#        y = "Capital balance / Credit Limit") +
#   theme_minimal()
# 
# ggplot(subset_data, aes(x = factor(default_90), y = installment_limit)) +
#   geom_boxplot(fill = "lightblue") +
#   labs(title = "Box Plot of Intallment / credit Limit by Target",
#        x = "Target",
#        y = "Installment / credit Limit") +
#   theme_minimal()





# Assuming `data` is your data frame and `var` is the variable name as a string
# library(ggplot2)
# var <- "pct_payed"
# 
# # Create a CDF plot using ggplot2
# ggplot(subset_data, aes_string(x = var)) +
#   stat_ecdf(geom = "step") +
#   labs(title = paste("CDF Plot of", var),
#        x = var,
#        y = "Cumulative Probability") +
#   theme_minimal()


# Transform periodicity into numeric
library(dplyr)


# Mapping the values
data <- data %>%
  mutate(periodicity_num = case_when(
    periodicity == "Mensual" ~ 30,
    periodicity == "Bimensual" ~ 60,
    periodicity == "Quincenal" ~ 15,
    TRUE ~ NA_real_  # Fallback for any unexpected values
  ))




# Mapp educ level
data <- data %>%
  mutate(max_education = case_when(
    max_education == "secundaria" ~ 2,
    max_education == "técnico" ~ 3,
    max_education == "tecnólogo" ~ 4,
    max_education == "Universitario" ~ 5,
    max_education == "Posgrado" ~ 6,
    max_education == "primaria" ~ 1,
    TRUE ~ NA_real_  # Fallback for any unexpected values
  ))

# drop periodicity in chr

data <- data %>%
  select(-periodicity)



# Create new var
# cuota / periodicidad

data$installment_periodic <- data$installment / data$periodicity_num

hist(data$installment_periodic)
## Transform dates----------

# First create year, month, day, weekday

# Extract features from the date-time variables
# date approval
#data$y_date_approval <- format(data$date_approval, "%Y")
#data$m_date_approval <- format(data$date_approval, "%m")
#data$d_date_approval <- format(data$date_approval, "%d")
#data$wd_date_approval <- weekdays(data$date_approval)

# date limit
#data$y_date_limit <- format(data$date_limit, "%Y")
#data$m_date_limit <- format(data$date_limit, "%m")
#data$d_date_limit <- format(data$date_limit, "%d")
#data$wd_date_limit <- weekdays(data$date_limit)


# Convert POSIXct to numeric (number of seconds since 1970-01-01)
# as numeric
data$date_approval <- as.numeric(data$date_approval)
data$date_limit <- as.numeric(data$date_limit)



# Calculate the time difference between the two POSIXct variables (in days, hours, etc.)
data$time_difference_days <- as.numeric(difftime(data$date_limit, data$date_approval, units = "days"))

hist(data$time_difference_days)
mean(data$time_difference_days)




## Character to factor-------------
# Convert all character columns to factors

data[] <- lapply(data, function(x) if (is.character(x)) as.factor(x) else x)


data <- data[, c(setdiff(names(data), "default_90"), "default_90")]

# Train test split---------------------
#install.packages("caTools")
library(caTools)

# Set a seed for reproducibility
set.seed(123)

# Create a train-test split (70% train, 30% test)
split <- sample.split(data$default_90, SplitRatio = 0.7)


# Split the data into train and test sets
train_data <- subset(data, split == TRUE)      # Training set (70%)
test_data <- subset(data, split == FALSE)  # Test set (30%)


# Print the number of rows in each set to verify
cat("Number of rows in training set:", nrow(train_data), "\n")
cat("Number of rows in test set:", nrow(test_data), "\n")



# EDA------------
library(skimr)
library(ggplot2)
library(reshape2)
## Numeric---------
numeric_data <- train_data[sapply(train_data, is.numeric)]
# Display the list of numeric column names
colnames(numeric_data)
# Describe basic statistics
skim(numeric_data)
### Histograms-------------
# Basic histogram
# Loop through each numeric column and create a histogram
for (col_name in colnames(numeric_data)) {
  hist(numeric_data[[col_name]], main = paste("Histogram of", col_name),
       xlab = col_name, col = "lightblue", border = "black")
}

### Transform numeric to logarithm---------------

# Exclude the specified columns
exclude_columns <- c("default_90", "dtf_approval_date")

# Apply log transformation to numeric variables, excluding the specified columns
train_data <- train_data %>%
  mutate(across(
    where(is.numeric) & !all_of(exclude_columns), 
    ~ log(. + 1)  # Add 1 to avoid issues with log(0)
  ))

test_data <- test_data %>%
  mutate(across(
    where(is.numeric) & !all_of(exclude_columns), 
    ~ log(. + 1)
  ))


### Correlation----------
# Correlation of Numeric Values:
# Calculate the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Ugly heatmap
# Heatmap 
heatmap(cor_matrix, Rowv = NA, Colv = NA)

# Melt the correlation matrix into a long format
melted_cor_matrix <- melt(cor_matrix)

# Set the factor levels for axes to maintain the order
melted_cor_matrix$Var1 <- factor(melted_cor_matrix$Var1, levels = colnames(cor_matrix))
melted_cor_matrix$Var2 <- factor(melted_cor_matrix$Var2, levels = colnames(cor_matrix))

# Create the heatmap
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed() +
  labs(title = "Heatmap of Correlation Matrix",
       x = "Variables",
       y = "Variables")
# Looks like Age is "negatively correlated"  with default values
# Capital balance & credit limit are highly correlated
# Installment is highly correlated with the credit limit and capital balance
# Installment is slighty negatively correlated with default_90


### Box Plots----------

for (col_name in colnames(numeric_data)) {
  print(
    ggplot(train_data, aes(x = factor(default_90), y = .data[[col_name]])) +
      geom_boxplot(fill = "lightblue") +
      labs(title = paste("Box Plot of", col_name, "by Target"),
           x = "Target",
           y = col_name) +
      theme_minimal()
  )
}

# Findings:
# For most variables non default seem to have more "outliers" while the mean of
# all vars is slightly different
# But for age there is a huge difference: defaulting clients are limited to an age
# band of 18-25 while non defaulting are from 25 onwards


### Density plots---------
for (col_name in colnames(numeric_data)) {
  print(
    ggplot(train_data, aes(x = .data[[col_name]], fill = factor(default_90))) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Densiy Plot of", col_name, "by Target"), x = col_name, fill = "Target") +
      theme_minimal()
  )
}



## Non-Numeric------------

# Subset non-numeric columns
non_numeric_data <- train_data[sapply(train_data, Negate(is.numeric))]

# unique values
unique_counts <- sapply(non_numeric_data, function(x) length(unique(x)))
print(unique_counts)

# Mode analysis
# Function to find the mode
find_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Apply the function to each non-numeric column
mode_values <- sapply(non_numeric_data, find_mode)
print(mode_values)

# The representative cliente is in agency 2, active, credit rating A, independent
# single, born in Bogota, studied up to tecnico, male, pays monthly


### Barplots----

# Loop through each non-numeric column and create a bar plot
for (col_name in colnames(non_numeric_data)) {
  print(
    ggplot(train_data, aes_string(x = col_name)) +
      geom_bar(fill = "lightblue", color = "black") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = paste("Bar Plot of", col_name),
           x = col_name,
           y = "Count") +
      scale_y_continuous(expand = c(0, 0))
  )
}

# Findings:
# date limit is focused on 2015, almost no one pays bimontly, most credits
# were approved on 2014, but come from 2006, gender balanced, educ balanced
# except for "tecnico", city mostly bogota followed by barrancabermeja,
# civil status balanced, work balanced, rating mostly A, mostly active clients
# most are from agency 2, and looks like lower agencies are "larger"
# 

#### Barplots by target----------

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

# findings:
# credits limit at 2009,2011,2020 are bad credits
# bimonthly are bad clients
# older approvals are defalut, specially mostly 2005-2007
# gender is balanced in defaults
# educ is balanced in defaults
# defulty cities barranquilla, cartagena, la dorada, valledupar
# singles are more likely to default
# work status balanced
# B rating is the best
# status Asociado inactivo is the worst
# agencies: 4, 9, 12, 16, 17 are the worst
# 
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
# Findings:
# city_born, agency,civil_status, show evidence of impacting 
# the default_90 target variable

# Groups for group lasso

head(train_data)

# Model----------------
## Logistic regression-------------------
# Basic logistic regression
model_base <- glm(default_90 ~ ., data = train_data, family = binomial)
par(mfrow = c(2, 2))
plot(model_base)

#Observations:


# Improvement: Consider adding interaction terms or non-linear terms
# (e.g., quadratic or polynomial terms) to better model the relationship.

# While logistic regression does not assume normality of residuals, 
# extreme deviations may indicate influential observations or outliers 
# that could affect the model fit. Investigate these points (e.g., rows 3183, 2030, and 3087).
summary(model_base)

### Metrics-----------
# prediction

# Load required library
library(caret)

# Define the function
evaluate_model <- function(model, test_data, target_col, threshold = 0.5) {
  # Predict probabilities on the test_data
  predicted_prob <- predict(model, newdata = test_data, type = "response")
  
  # Convert probabilities to binary predictions using the threshold
  predicted_class <- ifelse(predicted_prob > threshold, 1, 0)
  
  # Calculate accuracy
  accuracy <- mean(predicted_class == test_data[[target_col]])
  
  # Create confusion matrix
  conf_matrix <- confusionMatrix(
    factor(predicted_class), 
    factor(test_data[[target_col]])
  )
  
  # Extract Precision, Recall, and F1 Score
  precision <- conf_matrix$byClass["Pos Pred Value"]
  recall <- conf_matrix$byClass["Sensitivity"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Return a list with accuracy and F1 score
  return(list(
    accuracy = round(accuracy, 4),
    f1_score = round(f1_score, 4)
  ))
}

# Run function to evaluate results
results_base <- evaluate_model(model_base, test_data, target_col = "default_90")

# Print the results
print(paste("Accuracy:", results_base$accuracy))
print(paste("F1 Score:", results_base$f1_score))

# Stepwise regression------------
## Backward model--------------
# Fit the full model (all predictors)
full_model <- glm(default_90 ~ ., data = train_data, family = binomial)

# Perform backward stepwise selection
b_stepwise_model <- step(full_model, direction = "backward")

# View the summary of the selected model
summary(b_stepwise_model)

plot(b_stepwise_model)


results_backward <- evaluate_model(b_stepwise_model, test_data, target_col = "default_90")

# Print the results
print(paste("Accuracy:", results_backward$accuracy))
print(paste("F1 Score:", results_backward$f1_score))


## Forward Model----------
# Fit the null model (no predictors)
null_model <- glm(default_90 ~ 1, data = train_data, family = binomial)

# Fit the full model for scope (all predictors)
full_model <- glm(default_90 ~ ., data = train_data, family = binomial)

# Perform forward stepwise selection
f_stepwise_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# View the summary of the selected model
summary(f_stepwise_model)

# the model is clearly misspecified, residuals are not normal and 

# metrics
results_forward <- evaluate_model(f_stepwise_model, test_data, target_col = "default_90")

# Print the results
print(paste("Accuracy:", results_forward$accuracy))
print(paste("F1 Score:", results_forward$f1_score))

# 

# Glmnet-----------------------
# remove age to experiment
#train_data <- train_data[, !names(train_data) %in% c("age")]
#test_data <- test_data[, !names(test_data) %in% c("age")]

# Load necessary package
library(glmnet)

# Prepare training data matrix (assuming data is the training set)
train.x <- model.matrix(default_90 ~ . - 1, data = train_data)  # -1 removes the intercept column to avoid duplication


# Prepare target variable
train.y <- as.numeric(train_data$default_90)  # Ensure it's in numeric format (0 and 1) for glmnet



# Prepare training data matrix (assuming data is the training set)
test.x <- model.matrix(default_90 ~ . - 1, data = test_data)  # -1 removes the intercept column to avoid duplication

# Prepare target variable
test.y <- as.numeric(test_data$default_90)  # Ensure it's in numeric format (0 and 1) for glmnet



# Run the glmnet model
# Lasso bc alpha = 1 by default, If we want we can do elastic net by setting a = 0.5
model_lasso <- glmnet(train.x, train.y, family = "binomial", standardize = T) # Data has different scales, so stand = True
par(mfrow=c(1,1))
plot(model_lasso)


### Function for model evaluation

evaluate_glmnet_model <- function(model, test_x, test_y, threshold = 0.5) {
  # Predict probabilities using the glmnet model
  predicted_prob <- predict(model, newx = test_x, type = "response")[, 1]
  
  # Convert probabilities to binary predictions using the threshold
  predicted_class <- ifelse(predicted_prob > threshold, 1, 0)
  
  # Calculate accuracy
  accuracy <- mean(predicted_class == test_y)
  
  # Create confusion matrix
  conf_matrix <- confusionMatrix(
    factor(predicted_class), 
    factor(test_y)
  )
  
  # Extract Precision, Recall, and F1 Score
  precision <- conf_matrix$byClass["Pos Pred Value"]
  recall <- conf_matrix$byClass["Sensitivity"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Return a list with accuracy and F1 score
  return(list(
    accuracy = round(accuracy, 4),
    f1_score = round(f1_score, 4)
  ))
}


# Evaluate the Lasso model on the test set
results_lasso <- evaluate_glmnet_model(model_lasso, test.x, test.y)

# Print the results
print(paste("Accuracy:", results_lasso$accuracy))
print(paste("F1 Score:", results_lasso$f1_score))


# CV
# Perform cross-validation
set.seed(123)  # For reproducibility
cv_model1 <- cv.glmnet(train.x, train.y, family = "binomial", standardize = TRUE)

# Optimal Lambda
# Extract optimal lambda values
optimal_lambda1 <- cv_model1$lambda.min
lambda_1se1 <- cv_model1$lambda.1se

# Print the optimal lambda
print(paste("Optimal lambda (lambda.min):", optimal_lambda1))
print(paste("Optimal lambda (lambda.1se):", lambda_1se1))

# Plot the cross-validation curve
plot(cv_model1)

# Add labels for lambda.min and lambda.1se
abline(v = log(c(optimal_lambda1, lambda_1se1)), col = c("blue", "red"), lty = 2)
legend("topright", legend = c("lambda.min", "lambda.1se"), col = c("blue", "red"), lty = 2)


# Plot coefficient paths
#Put names of variables instead of number!!!!!!!!!!!!!--------------
plot(cv_model1$glmnet.fit, xvar = "lambda", label = TRUE)



# Extract coefficients at lambda.min
optimal_coefficients1 <- coef(cv_model1, s = "lambda.min")

# Convert to a readable format
coef_df1 <- as.data.frame(as.matrix(optimal_coefficients1))
coef_df1$Variable <- rownames(coef_df1)
rownames(coef_df1) <- NULL
colnames(coef_df1)[1] <- "Coefficient"

# Display non-zero coefficients
non_zero_coefs1 <- coef_df1[coef_df1$Coefficient != 0, ]
print(non_zero_coefs1)


## Metrics

# Evaluate the model at lambda.min
results_lambda_min <- evaluate_glmnet_model(
  model = cv_model1$glmnet.fit,  # The actual model inside cv.glmnet
  test_x = test.x, 
  test_y = test.y, 
  threshold = 0.5
)

# Evaluate the model at lambda.1se
results_lambda_1se <- evaluate_glmnet_model(
  model = cv_model1$glmnet.fit,
  test_x = test.x, 
  test_y = test.y, 
  threshold = 0.5
)

# Print the results for lambda.min
print(paste("Accuracy (lambda.min):", results_lambda_min$accuracy))
print(paste("F1 Score (lambda.min):", results_lambda_min$f1_score))

# Print the results for lambda.1se
print(paste("Accuracy (lambda.1se):", results_lambda_1se$accuracy))
print(paste("F1 Score (lambda.1se):", results_lambda_1se$f1_score))


# ROC Curve

# Load pROC package for ROC analysis
#install.packages("pROC")
# library(pROC)
# 
# # Compute ROC curve
# roc_obj1 <- roc(test.y, as.numeric(predicted_probabilities1))
# 
# # Plot ROC curve
# plot(roc_obj1, col = "blue", main = "ROC Curve")
# abline(a = 0, b = 1, lty = 2, col = "gray")
# 
# # Calculate AUC
# auc_value1 <- auc(roc_obj1)
# print(paste("AUC:", auc_value1))
# 
# 
# 
# 
# # measure = class 
# cv.model_c <- cv.glmnet(train.x, train.y, family = "binomial", type.measure = "class")
# plot(cv.model_c)
# 
# # measure = AUC
# cv.model_auc <- cv.glmnet(train.x, train.y, family = "binomial", type.measure = "auc")
# plot(cv.model_auc)

## Second order lasso----------------
## Elastic net-----------------------


# Group Lasso--------------
# acommodate the data for group lasso

# Create an empty list to store the dummy variables and initialize the group vector
# Initialize an empty list to store the dummy variables and the group vector
##### Train-------------
# Remove the target variable `default_90` from train_data for group creation
train_data_no_target <- train_data[, !colnames(train_data) %in% "default_90"]

# Create an empty list to store the dummy variables and initialize the group vector
dummy_list_train <- list()
group_vector_train <- c()  # This will store group identifiers for each column

# Initialize a group ID counter
group_id <- 1

# Select numeric columns from train_data without the target variable
numeric_data_train <- train_data_no_target[sapply(train_data_no_target, is.numeric)]

# Assign the first group ID to all numeric columns
group_vector_train <- c(group_vector_train, rep(group_id, ncol(numeric_data_train)))

# Increment the group ID for the next set of variables
group_id <- group_id + 1

# Select only non-numeric (categorical) columns, excluding the target variable
non_numeric_data <- train_data_no_target[, !sapply(train_data_no_target, is.numeric)]

# Loop over each column name in non_numeric_data
for (col_name in colnames(non_numeric_data)) {
  # Dynamically create dummy variables, dropping the first level
  print(col_name)
  dummies <- model.matrix(as.formula(paste("~", col_name)), data = train_data_no_target)[, -1]
  
  # Check if dummies has at least one column
  if (!is.null(ncol(dummies)) && ncol(dummies) > 0) {
    # Store the dummies in the list with the column name as the key
    dummy_list_train[[col_name]] <- dummies
    
    # Add group markers for the current set of dummy variables
    group_vector_train <- c(group_vector_train, rep(group_id, ncol(dummies)))
    
    # Increment the group ID for the next factor
    group_id <- group_id + 1
  } else {
    # If dummies has zero columns, skip this factor variable or handle it as needed
    warning(paste("No dummy variables created for", col_name, "as it has only one level in all rows."))
  }
}

# Combine all dummy matrices into one data frame or matrix
dummy_data_train <- do.call(cbind, dummy_list_train)

# Combine numeric data and dummy data into a single data frame
combined_data_train <- cbind(numeric_data_train, dummy_data_train)


##### Test---------

# Remove the target variable `default_90` from test_data for group creation
test_data_no_target <- test_data[, !colnames(test_data) %in% "default_90"]

# Create an empty list to store the dummy variables and initialize the group vector
dummy_list_test <- list()
group_vector_test <- c()  # This will store group identifiers for each column

# Initialize a group ID counter
group_id <- 1

# Select numeric columns from test_data without the target variable
numeric_data_test <- test_data_no_target[sapply(test_data_no_target, is.numeric)]

# Assign the first group ID to all numeric columns
group_vector_test <- c(group_vector_test, rep(group_id, ncol(numeric_data_test)))

# Increment the group ID for the next set of variables
group_id <- group_id + 1

# Select only non-numeric (categorical) columns, excluding the target variable
non_numeric_data <- test_data_no_target[, !sapply(test_data_no_target, is.numeric)]

# Loop over each column name in non_numeric_data
for (col_name in colnames(non_numeric_data)) {
  # Dynamically create dummy variables, dropping the first level
  print(col_name)
  dummies <- model.matrix(as.formula(paste("~", col_name)), data = test_data_no_target)[, -1]
  
  # Check if dummies has at least one column
  if (!is.null(ncol(dummies)) && ncol(dummies) > 0) {
    # Store the dummies in the list with the column name as the key
    dummy_list_test[[col_name]] <- dummies
    
    # Add group markers for the current set of dummy variables
    group_vector_test <- c(group_vector_test, rep(group_id, ncol(dummies)))
    
    # Increment the group ID for the next factor
    group_id <- group_id + 1
  } else {
    # If dummies has zero columns, skip this factor variable or handle it as needed
    warning(paste("No dummy variables created for", col_name, "as it has only one level in all rows."))
  }
}

# Combine all dummy matrices into one data frame or matrix
dummy_data_test <- do.call(cbind, dummy_list_test)

# Combine numeric data and dummy data into a single data frame
combined_data_test <- cbind(numeric_data_test, dummy_data_test)


# Group Lasso model

library(gglasso)

# Separate the target variable `default_90` from `combined_data_train`
# Separate the target variable `default_90` from combined_data_train
y_train <- as.numeric(train_data$default_90)  # Extract target variable as numeric vector

# Adjust y_train to be in {-1, 1} format for binary classification
y_train <- ifelse(y_train == 1, 1, -1)  # Assuming `1` means the positive class and `0` the negative

# Remove `default_90` from combined_data_train to get only predictors
X_train <- as.matrix(combined_data_train)  # Convert predictors to a matrix

# Ensure grp matches the predictor matrix X_train
grp_train <- group_vector_train  # This assumes group_vector_train is aligned with the predictor columns


###### GLasso----------
# Ensure the required library is loaded
library(gglasso)
library(Matrix)

# Convert to sparse matrix
X_train_sparse <- as(X_train, "sparseMatrix")

# Convert back to dense matrix (required by gglasso)
X_train_dense <- as.matrix(X_train_sparse)

# Standardize predictors
X_train_scaled <- scale(X_train_dense)

install.packages("doParallel")
library(doParallel)

# Set up parallel backend
cl <- makeCluster(detectCores() - 1)  # Use all but one core
registerDoParallel(cl)


# See this link for sparse group_lasso
# https://cran.r-project.org/web/packages/sparsegl/index.html

# Fit the group lasso model
fit_gglasso_train <- gglasso(
  x = X_train_scaled, 
  y = y_train, 
  group = grp_train, 
  loss = "logit", 
  nlambda = 50
)

# Stop the parallel backend
stopCluster(cl)

plot(fit_gglasso_train)


evaluate_gglasso_model <- function(model, test_x, test_y, lambda, threshold = 0.5) {
  # Predict probabilities using the gglasso model at a specific lambda
  predicted_prob <- predict(model, newx = test_x, s = lambda, type = "response")
  
  # Convert probabilities to binary predictions using the threshold
  predicted_class <- ifelse(predicted_prob > threshold, 1, -1)  # Adjust to match {-1, 1} format of `y_train`
  
  # Convert test_y to {1, -1} format if needed
  test_y <- ifelse(test_y == 1, 1, -1)
  
  # Calculate accuracy
  accuracy <- mean(predicted_class == test_y)
  
  # Create confusion matrix
  conf_matrix <- confusionMatrix(
    factor(predicted_class), 
    factor(test_y)
  )
  
  # Extract Precision, Recall, and F1 Score
  precision <- conf_matrix$byClass["Pos Pred Value"]
  recall <- conf_matrix$byClass["Sensitivity"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Return a list with accuracy and F1 score
  return(list(
    accuracy = round(accuracy, 4),
    f1_score = round(f1_score, 4)
  ))
}


# Convert test set to a matrix
test_x_dense <- as.matrix(test.x)  # Ensure test.x is in dense format

# Define the lambda to evaluate
lambda_to_evaluate <- fit_gglasso_train$lambda[10]  # Example: Use the 10th lambda value

# Evaluate the model
results_gglasso <- evaluate_gglasso_model(
  model = fit_gglasso_train, 
  test_x = test_x_dense, 
  test_y = test.y, 
  lambda = lambda_to_evaluate
)

# Print results
print(paste("Accuracy (lambda):", results_gglasso$accuracy))
print(paste("F1 Score (lambda):", results_gglasso$f1_score))



######GLasso----------


# tips for speed

# paralel computing
# library(doParallel)
# 
# # Set up parallel backend
# cl <- makeCluster(detectCores() - 1)  # Use all but one core
# registerDoParallel(cl)
# 
# # Run gglasso
# fit_gglasso_train <- gglasso(x = X_train, y = y_train, group = grp_train, loss = "logit")
# 
# # Stop the parallel backend
# stopCluster(cl)

# standard data
# Standardize predictors
X_train_scaled <- scale(X_train)

# Fit model with standardized data
fit_gglasso_train <- gglasso(x = X_train_scaled, y = y_train, group = grp_train,
                             loss = "logit", standardize = FALSE,
                             nlambda = 50, lambda.min.ratio = 0.1,thresh = 1e-3)






# sparse matrices
library(Matrix)

# Convert to sparse matrix
X_train_sparse <- as(X_train, "sparseMatrix")

# Fit gglasso with sparse matrix
fit_gglasso_train <- gglasso(x = X_train_sparse, y = y_train, group = grp_train, loss = "logit")



# Fit the gglasso model
fit_gglasso_train <- gglasso(x = X_train, y = y_train, group = grp_train, loss = "logit",
                             nlambda = 50, lambda.min.ratio = 0.1,thresh = 1e-3)
# standard nlambda = 100, sun with 25 to take less time

# plot results
plot(fit_gglasso_train)

# Inspect the model
print(fit_gglasso_train)




##Cross Validation
#
#enlarge on the left to get the minimum
set.seed(333)
fit.cv_gglasso =cv.gglasso(x=X,y=Y,group=grp, loss = 'logit',nfolds=10, lambda.factor=0.0001)
plot(fit.cv)
#
#Pick the best Lambda
#enlarge fitting on the left to get minimum
fit_gglasso <- gglasso(x=X, y=Y, group=grp, loss='logit',lambda.factor=0.0001)
lmbda <- fit.cv$lambda.1se
lmbda1 <- fit.cv$lambda.min
#
coefs=coef(object=fit,s=lmbda)
coefs1=coef(object=fit,s=lmbda1)
#