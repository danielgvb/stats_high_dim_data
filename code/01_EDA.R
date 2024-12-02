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
## Co-debtor-----------
# before removing ids, use them to check if client has co-debtor
colnames(data)

# Aggregate to count distinct 'ID Cliente' by 'No Pagaré Rotativo'
result <- aggregate(`ID Cliente` ~ `No Pagaré Rotativo`, data = data, FUN = function(x) length(unique(x)))

# Rename columns for clarity (optional)
colnames(result) <- c("No Pagaré Rotativo", "Distinct ID Cliente Count")

# View result
max(result$`Distinct ID Cliente Count`)

# mark if the pagare has more than one ID.
# Compute the distinct ID Cliente count per No Pagaré Rotativo
distinct_counts <- aggregate(`ID Cliente` ~ `No Pagaré Rotativo`, data = data, FUN = function(x) length(unique(x)))

# Step 2: Add a column indicating whether the count is greater than 1
distinct_counts$MoreThanOne <- as.numeric(distinct_counts$`ID Cliente` > 1)


# Step 3: Merge this information back into the original dataframe
data <- merge(data, distinct_counts[, c("No Pagaré Rotativo", "MoreThanOne")], by = "No Pagaré Rotativo", all.x = TRUE)
colnames(data)

# View the updated dataframe
print(data)



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
                    "dtf_approval_date", "fx_approval_date","city_pop_2018", "default_90", "has_codebtor")
if (length(friendly_names) == ncol(data)) {
  colnames(data) <- friendly_names
} else {
  stop("Column name mismatch.")
}


## Handle Missing Values
anyNA(data)


# Transformations ---------------------------------------

## Filter Credit Limit > 50,000
data <- data[data$credit_limit > 50000, ]

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
#data[columns_to_transform] <- lapply(data[columns_to_transform], function(col) log(col + 1))

# Move "default_90" to the last column
data <- data[, c(setdiff(names(data), "default_90"), "default_90")]

#REMOVE AGE because of Noise--------------
#data <- data[, !names(data) %in% c("age")]

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
  print(
    ggplot(train_data, aes(x = factor(default_90), y = .data[[col_name]], fill = factor(default_90))) +
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
    ggplot(train_data, aes(x = .data[[col_name]], fill = factor(default_90))) +
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

# Higher correlation are: 
# Extract the last column of the correlation matrix
last_col <- cor_matrix[, ncol(cor_matrix)]
last_col

# Get the absolute values of correlations (excluding the last column itself)
abs_cor <- abs(last_col[-ncol(cor_matrix)])

# Top corr
N <- 5 

# Sort absolute correlations in descending order and extract the top N
top_vars <- names(sort(abs_cor, decreasing = TRUE)[1:N])

# Display the top N variables excluding age
top_vars[2:5]

#### Plots for top correlated variables----------
##### Histograms--------
# Select the top variables for plotting
selected_vars <- top_vars[2:5]

# Extract Economist color palette (limited to 4 colors for the selected variables)
custom_colors <- economist_pal()(4)

# Create a list to store the plots
plot_list <- lapply(seq_along(selected_vars), function(i) {
  ggplot(train_data, aes_string(x = selected_vars[i])) +
    geom_histogram(bins = 30, fill = custom_colors[i], color = "black", alpha = 0.8) +
    labs(title = paste("Histogram of", selected_vars[i]), x = selected_vars[i], y = "Count") +
    theme_economist()  # Apply The Economist theme
})

# Arrange the plots in a 2x2 grid
grid.arrange(grobs = plot_list, ncol = 2, nrow = 2)

##### Density---------
# Create a list to store the density plots
plot_list <- lapply(selected_vars, function(var_name) {
  ggplot(train_data, aes_string(x = var_name, fill = "factor(default_90)")) +
    geom_density(alpha = 0.5) +  # Add density plot with transparency
    labs(
      title = paste("Density Plot of", var_name),
      x = var_name,
      fill = "Target"
    ) +
    theme_economist() +  # Apply The Economist theme
    scale_fill_economist()  # Use The Economist palette for the fill aesthetic
})

# Arrange the plots in a 2x2 grid
grid.arrange(grobs = plot_list, ncol = 2, nrow = 2)


### Barplots by Target-------
for (col_name in colnames(non_numeric_data)) {
  print(
    ggplot(train_data, aes_string(x = col_name, fill = "factor(default_90)")) +
      geom_bar(position = "fill") +  # Use position = "fill" for proportions
      labs(
        title = paste("Bar Plot of", col_name, "by Target"),
        x = col_name, 
        y = "Proportion", 
        fill = "Target"
      ) +
      theme_economist() +  # Apply The Economist theme
      scale_fill_economist() +  # Use Economist color palette
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # Adjust angle, horizontal and vertical alignment
        axis.title.x = element_text(margin = margin(t = 20)),  # Increase top margin of x-axis title
        plot.margin = margin(t = 10, r = 10, b = 30, l = 10)  # Add extra margin to the entire plot
      )
  )
}
# take city, agency, civil status, wd_data_limit, periodicity


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
# different style
# ggplot(chi2_results_df, aes(x = reorder(Variable, -P_value), y = P_value)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +  # Significance threshold
#   labs(title = "P-values from Chi-square Tests for Non-numeric Variables vs default_90",
#        x = "Variable",
#        y = "P-value") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(chi2_results_df, aes(x = reorder(Variable, -P_value), y = P_value)) +
  geom_bar(stat = "identity", fill = adjustcolor(economist_pal()(1), alpha.f = 0.8)) +  # Lighter, semi-transparent bars
  geom_hline(yintercept = 0.05, color = "#E3120B", linetype = "dashed", size = 1.3) +  # Fixed threshold line
  labs(
    title = "P-values from Chi-square Tests for Non-numeric Variables vs default_90",
    x = "Variable",
    y = "P-value"
  ) +
  theme_economist() +  # Apply The Economist theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.title.x = element_text(margin = margin(t = 40)),  # Move the x-axis title higher (t = 40)
    plot.title = element_text(hjust = 0.5),  # Center the title
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10)  # Adjust plot margins
  )
