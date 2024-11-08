# EDA Statistical Methods High Dim Data--------------

# Import Data-------------
library(readxl)
data <- read_excel("GitHub/stats_high_dim_data/data/data.xlsx")
View(data)

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


# EDA------------
library(skimr)
library(ggplot2)
library(reshape2)
## Numeric---------
numeric_data <- data[sapply(data, is.numeric)]
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

# Replace 'target' with the name of your binary target variable
for (col_name in colnames(numeric_data)) {
  print(
    ggplot(data, aes(x = factor(default_90), y = .data[[col_name]])) +
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
    ggplot(data, aes(x = .data[[col_name]], fill = factor(default_90))) +
      geom_density(alpha = 0.5) +
      labs(title = paste("Densiy Plot of", col_name, "by Target"), x = col_name, fill = "Target") +
      theme_minimal()
  )
}



## Non-Numeric------------

# Subset non-numeric columns
non_numeric_data <- data[sapply(data, Negate(is.numeric))]

View(non_numeric_data)

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
    ggplot(data, aes_string(x = col_name)) +
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
    ggplot(data, aes_string(x = col_name, fill = "factor(default_90)")) +
      geom_bar(position = "fill") +  # Use position = "dodge" for side-by-side bars
      labs(title = paste("Bar Plot of", col_name, "by Target"),
           x = col_name, y = "Proportion", fill = "Target") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  )
}

# findings:
# credits limit at some point in 2011-ish are bad
# bimonthly are bad clients
# older approvals are defalut, specially 2006-2008
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
# Findings:
# city_born, civil_status, date_limit, status show evidence of impacting 
# the default_90 target variable

# Variables Transformation-------------

