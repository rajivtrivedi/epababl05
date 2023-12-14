# library loads -----------------------------------------------------------


setwd("c:/users/rajiv.trivedi/downloads")
library("readxl")


# Read Data ---------------------------------------------------------------

data <- read_excel("Cyber Security Perception Responses.xlsx", sheet = "for_analysis")
data$concern_binary <- as.factor(data$concern_binary)
levels(data$concern_binary) <- c(0, 1)

library(caret)

data_up  = upSample(data,data$concern_binary)

# Run the Initial Model ---------------------------------------------------

# Fit a logistic regression model
logistic_model <- glm(concern_binary ~ is_male + is_female + age_group + is_graduate + is_postgraduate + 
                        is_schooling + is_other_education + is_business + is_service + is_student + 
                        is_homemaker + income_group, 
                      family = binomial, data = data_up)
summary(logistic_model)

# Display the summary of the logistic regression model
# summary(logistic_model)
# 
# 
# # first output ------------------------------------------------------------
# Call:
#   glm(formula = concern_binary ~ is_male + is_female + age_group + 
#         is_graduate + is_postgraduate + is_schooling + is_other_education + 
#         is_business + is_service + is_student + is_homemaker + income_group, 
#       family = binomial, data = data)
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)  
# (Intercept)           3.04411    2.47700   1.229   0.2191  
# is_male               0.31590    0.53517   0.590   0.5550  
# is_female                  NA         NA      NA       NA  
# age_group            -1.41187    0.66132  -2.135   0.0328 *
#   is_graduate           0.17080    0.75573   0.226   0.8212  
# is_postgraduate      -0.32645    0.81499  -0.401   0.6887  
# is_schooling         16.20203 1028.88699   0.016   0.9874  
# is_other_education         NA         NA      NA       NA  
# is_business           0.06435    1.76436   0.036   0.9709  
# is_service            0.82151    1.34747   0.610   0.5421  
# is_student           -2.56162    1.86721  -1.372   0.1701  
# is_homemaker               NA         NA      NA       NA  
# income_group          0.06522    0.17495   0.373   0.7093  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 142.79  on 103  degrees of freedom
# Residual deviance: 122.69  on  94  degrees of freedom
# AIC: 142.69
# 
# Number of Fisher Scoring iterations: 14

# Remove singularities ----------------------------------------------------

# Fit a logistic regression model
logistic_model <- glm(concern_binary ~ is_male  + age_group + is_graduate + is_postgraduate + 
                        is_schooling +  is_business + is_service + is_student + 
                         income_group, 
                      family = binomial, data = data_up)

# Display the summary of the logistic regression model
summary(logistic_model)
library(car)


vif_values <- vif(logistic_model)
vif_values

summary(logistic_model)



# Final Model after removing less significant and multicolinearity --------


# Fit the revised logistic regression model
final_model <- glm(concern_binary ~ age_group + is_student,
                   family = binomial, data = data_up)

# Display the summary of the final model
summary(final_model)
# Call:
#   glm(formula = concern_binary ~ age_group + is_student, family = binomial, 
#       data = data)
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   5.0151     1.6287   3.079 0.002076 ** 
#   age_group    -1.7268     0.6137  -2.814 0.004897 ** 
#   is_student   -3.5396     1.0728  -3.299 0.000969 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 142.79  on 103  degrees of freedom
# Residual deviance: 128.11  on 101  degrees of freedom
# AIC: 134.11
# 
# Number of Fisher Scoring iterations: 4
# Conclusion --------------------------------------------------------------

# Overall, this model suggests that both 'age_group' and 'is_student'
# are important predictors of the binary response variable 'concern_binary',
# and the model provides a good fit to the data



# Prediction --------------------------------------------------------------

# Apply predictions and name the output as 'predicted_concern'
data_up$predicted_concern <- predict(final_model, type = "response")

# Convert predicted probabilities to binary predictions (0 or 1)
data_up$predicted_binary <- ifelse(data_up$predicted_concern >= 0.5, 1, 0)

# Confusion Matrix
conf_matrix <- table(data_up$predicted_binary, data_up$concern_binary)
#            Actual 0 Actual 1
# Predicted 0      52       33
# Predicted 1       6       25
# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# Sensitivity (True Positive Rate)
sensitivity <- conf_matrix[2, 2] / sum(conf_matrix[2, ])

# Specificity (True Negative Rate)
specificity <- conf_matrix[1, 1] / sum(conf_matrix[1, ])

# F1 Score
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- sensitivity
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("F1 Score:", f1_score, "\n")
# Accuracy: 0.6637931 
# Sensitivity: 0.8064516 
# Specificity: 0.6117647 
# F1 Score: 0.5617978 
# Interpretation of the Accuracy measures ---------------------------------

# Confusion Matrix:
#            Actual 0 Actual 1
# Predicted 0      52       33
# Predicted 1       6       25
# 
# Confusion Matrix
TP <- 25
TN <- 52
FP <- 33
FN <- 6

# Formulas
accuracy <- (TP + TN) / (TP + TN + FP + FN)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
precision <- TP / (TP + FP)
f1_score <- 2 * (precision * sensitivity) / (precision + sensitivity)

# Display Results
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity (Recall):", sensitivity, "\n")
cat("Specificity:", specificity, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1_score, "\n")

# Accuracy: 0.6637931 
# Sensitivity (Recall): 0.8064516 
# Specificity: 0.6117647 
# Precision: 0.4310345 
# F1 Score: 0.5617978 

# ROC Curve and find optimum threshold and optimum prediction---------------------------------------------------------------
library(pROC)

# Create ROC curve
roc_curve <- roc(data_up$concern_binary, data_up$predicted_concern)

# Find the optimal threshold
optimal_threshold <- coords(roc_curve, "best", ret = c("threshold"))

# Print the optimal threshold
optimal_threshold
# 0.5971897

data$optimum_prediction <- ifelse(data$predicted_concern >= optimal_threshold, 1, 0)
# Confusion Matrix
conf_matrix <- table(data$optimum_prediction, data$concern_binary)
conf_matrix
conf_matrix

# 0  1
# 1 46 58
# The optimum threshold returned by R functoin coords does not classify any value as 0
# hence not using that threshold





# Choose threshold with max sensitivity code  -------------------------------------------------------------------


threshold_values <- seq(0, 1, by = 0.01)

# Initialize vectors to store metrics
sensitivity_values <- numeric(length(threshold_values))
specificity_values <- numeric(length(threshold_values))
accuracy_values <- numeric(length(threshold_values))

# Calculate metrics for each threshold
for (i in seq_along(threshold_values)) {
  threshold <- threshold_values[i]
  confusion_matrix <- table(data$concern_binary, ifelse(data$predicted_concern >= threshold, 1, 0))
  
  # Check if the confusion matrix has the required dimensions
  if (nrow(confusion_matrix) == 2 && ncol(confusion_matrix) == 2) {
    sensitivity_values[i] <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
    specificity_values[i] <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
    accuracy_values[i] <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  } else {
    sensitivity_values[i] <- NA
    specificity_values[i] <- NA
    accuracy_values[i] <- NA
  }
}

# Find the threshold that maximizes sensitivity
optimal_threshold <- threshold_values[which.max(sensitivity_values)]

# Display the optimal threshold
cat("Optimal Threshold:", optimal_threshold, "\n")

# Optimal Threshold: 0.44 


# Accuracy measures with best sensitivity values --------------------------
data$optimum_prediction <- ifelse(data$predicted_concern >= optimal_threshold, 1, 0)
# Confusion Matrix
conf_matrix <- table(data$optimum_prediction, data$concern_binary)
conf_matrix

# Definitions
TruePositives <- 37
TrueNegatives <- 27
FalsePositives <- 21
FalseNegatives <- 19

# Accuracy measures
Accuracy <- (TruePositives + TrueNegatives) / sum(c(TruePositives, TrueNegatives, FalsePositives, FalseNegatives))
Sensitivity <- TruePositives / sum(c(TruePositives, FalseNegatives))
Specificity <- TrueNegatives / sum(c(TrueNegatives, FalsePositives))
Precision <- TruePositives / sum(c(TruePositives, FalsePositives))
F1Score <- 2 * (Precision * Sensitivity) / (Precision + Sensitivity)

cat("Accuracy measures:\n")
cat("Accuracy:", round(Accuracy, 4), "\n")
cat("Sensitivity (Recall):", round(Sensitivity, 4), "\n")
cat("Specificity:", round(Specificity, 4), "\n")
cat("Precision:", round(Precision, 4), "\n")
cat("F1 Score:", round(F1Score, 4), "\n")


# Definitions:
#   
# - True Positives (TP): Instances correctly predicted as positive (Predicted 1, Actual 1).
# - True Negatives (TN): Instances correctly predicted as negative (Predicted 0, Actual 0).
# - False Positives (FP): Instances incorrectly predicted as positive (Predicted 1, Actual 0).
# - False Negatives (FN): Instances incorrectly predicted as negative (Predicted 0, Actual 1).
# 
# Accuracy measures:
#   
#   - Accuracy: Proportion of correct predictions out of the total instances.
# - Accuracy = (TP + TN) / (TP + TN + FP + FN)
# = (37 + 27) / (37 + 27 + 21 + 19)
# = 64 / 104
# ≈ 0.6154
# 
# - Sensitivity (Recall): Proportion of actual positive instances correctly predicted.
# - Sensitivity = TP / (TP + FN)
# = 37 / (37 + 19)
# ≈ 0.6607
# 
# - Specificity: Proportion of actual negative instances correctly predicted.
# - Specificity = TN / (TN + FP)
# = 27 / (27 + 21)
# ≈ 0.5625
# 
# - Precision: Proportion of predicted positive instances that are actually positive.
# - Precision = TP / (TP + FP)
# = 37 / (37 + 21)
# ≈ 0.6379
# 
# - F1 Score: Harmonic mean of precision and recall (sensitivity).
# - F1 Score = 2 * (Precision * Sensitivity) / (Precision + Sensitivity)
# = 2 * (0.6379 * 0.6607) / (0.6379 + 0.6607)
# ≈ 0.6481




# Formula for the Predicting new Observation ------------------------------


#=1/(1+EXP(-(5.0151+(-1.7268*age_group)+(-3.5396*is_student))))

