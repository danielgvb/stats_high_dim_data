### POST ESTIMATION PLOTS FOR GLASSO

### 5. Post-Estimation Plots for Group Lasso -------------------

# (1) Cross-validation and lambda optimization
# Since gglasso doesn't have built-in cross-validation, simulate one if needed.

# Plot the Group Lasso Fit
plot(fit_gglasso, main = "Group Lasso Coefficients Across Lambda")

# Highlight optimal lambda (minimum)
lambda_min <- min(fit_gglasso$lambda)
abline(v = log(lambda_min), col = "blue", lty = 2, lwd = 2)
legend("topright", legend = c("lambda.min"), col = "blue", lty = 2, lwd = 2)

# (2) ROC Curve and AUC
# Predict probabilities on the test set
log_odds <- predict(fit_gglasso, newx = X_test_scaled, type = "link")[, which.min(fit_gglasso$lambda)]
predicted_prob <- 1 / (1 + exp(-log_odds))

roc_curve <- roc(y_test, predicted_prob)
plot(roc_curve, col = "blue", main = paste("ROC Curve (AUC =", round(auc(roc_curve), 2), ")"))
abline(a = 0, b = 1, lty = 2, col = "gray")

# (3) Confusion Matrix Heatmap
predicted_class <- ifelse(predicted_prob > 0.5, 1, -1)
conf_matrix <- table(Predicted = predicted_class, Actual = y_test)

ggplot(as.data.frame(conf_matrix), aes(x = Actual, y = Predicted, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = Freq), color = "black") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()

# (4) Coefficient Plot
coef_matrix <- coef(fit_gglasso, s = lambda_min)
coef_data <- data.frame(
  Variable = rownames(coef_matrix),
  Coefficient = as.vector(coef_matrix)
)
coef_data <- coef_data %>% filter(Coefficient != 0 & Variable != "(Intercept)")

ggplot(coef_data, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  coord_flip() +
  labs(title = "Group Lasso Coefficient Plot", x = "Variable", y = "Coefficient") +
  theme_minimal()

# (5) Calibration Curve
calibration_data <- data.frame(
  Predicted = predicted_prob,
  Observed = ifelse(y_test == 1, 1, 0) # Convert {-1, 1} to {0, 1} for visualization
)
calibration_data$Bin <- cut(calibration_data$Predicted, breaks = 10, include.lowest = TRUE)

calibration_curve <- calibration_data %>%
  group_by(Bin) %>%
  summarize(
    Mean_Predicted = mean(Predicted),
    Mean_Observed = mean(Observed)
  )

ggplot(calibration_curve, aes(x = Mean_Predicted, y = Mean_Observed)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Calibration Curve", x = "Mean Predicted Probability", y = "Mean Observed Probability") +
  theme_minimal()