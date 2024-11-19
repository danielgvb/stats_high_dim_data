# Load Required Libraries --------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)
library(glmnet)
library(gglasso)
library(skimr)
library(reshape2)
library(caTools)

# Set Working Directory ---------------------------------
data_path <- "C:/Users/danie/Documents/GitHub/stats_high_dim_data/data/data.xlsx"

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
data <- data %>%
  mutate(periodicity_num = case_when(
    periodicity == "Mensual" ~ 30,
    periodicity == "Bimensual" ~ 60,
    periodicity == "Quincenal" ~ 15,
    TRUE ~ NA_real_
  )) %>%
  select(-periodicity)

## Map Education Levels to Numeric
data <- data %>%
  mutate(max_education = case_when(
    max_education == "primaria" ~ 1,
    max_education == "secundaria" ~ 2,
    max_education == "técnico" ~ 3,
    max_education == "tecnólogo" ~ 4,
    max_education == "Universitario" ~ 5,
    max_education == "Posgrado" ~ 6,
    TRUE ~ NA_real_
  ))



## Create Derived Variables
data <- data %>%
  mutate(
    installment_periodic = installment / periodicity_num,
    time_difference_days = as.numeric(difftime(as.Date(date_limit), as.Date(date_approval), units = "days"))
  )


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

# Apply the natural logarithm to the selected columns, adding 1 to handle zeros
data[columns_to_transform] <- lapply(data[columns_to_transform], function(col) log(col + 1))

# Move "default_90" to the last column
data <- data[, c(setdiff(names(data), "default_90"), "default_90")]



# Train-Test Split --------------------------------------
set.seed(123)
split <- sample.split(data$default_90, SplitRatio = 0.7)
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)




# Exploratory Data Analysis -----------------------------

## Numeric Variables
numeric_data <- train_data %>% select(where(is.numeric))
skim(numeric_data)
### Histograms
for (col_name in colnames(numeric_data)) {
  hist(numeric_data[[col_name]], main = paste("Histogram of", col_name),
       xlab = col_name, col = "lightblue", border = "black")
}

### Density Plots
for (col_name in colnames(numeric_data)) {
  print(ggplot(train_data, aes(x = .data[[col_name]], fill = factor(default_90))) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Density Plot of", col_name), x = col_name, fill = "Target") +
    theme_minimal())
}


## Non-Numeric Variables
non_numeric_data <- train_data %>% select(where(~ !is.numeric(.)))
unique_counts <- sapply(non_numeric_data, function(x) length(unique(x)))
mode_values <- sapply(non_numeric_data, function(x) names(which.max(table(x))))
print(mode_values)
# Correlation Analysis ----------------------------------

cor_matrix <- cor(numeric_data, use = "complete.obs")
melted_cor_matrix <- melt(cor_matrix)
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() +
  coord_fixed() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Model Training and Evaluation ------------------------

## Logistic Regression
logistic_model <- glm(default_90 ~ ., data = train_data, family = binomial)

## Evaluate Logistic Regression
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



# Add further models (Lasso, Elastic Net, Group Lasso) similarly in modular functions
