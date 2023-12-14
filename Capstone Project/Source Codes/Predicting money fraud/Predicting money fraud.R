library(readxl)
library(car)
library(caTools)  # For sample.split function

# Read the data
selected_features_rf <- read_excel("C:/Users/Jay Patel/Desktop/IIMA/Capstone Project/Capstone Project/MNY_LOST AS RESPONSE/SMOTE DATA/selected_features_rf.xlsx")

# View column names
colnames(selected_features_rf)

# Set seed for reproducibility
set.seed(123)

# Split the data into training (70%), validation (15%), and test (15%) sets
split_index_train <- sample.split(selected_features_rf$mny_lost_epayment, SplitRatio = 0.7)
train_data <- subset(selected_features_rf, split_index_train == TRUE)
temp_data <- subset(selected_features_rf, split_index_train == FALSE)

split_index_valid_test <- sample.split(temp_data$mny_lost_epayment, SplitRatio = 0.5)
valid_data <- subset(temp_data, split_index_valid_test == TRUE)
test_data <- subset(temp_data, split_index_valid_test == FALSE)

# Fit the logistic regression model on training data
Model_lgR <- glm(mny_lost_epayment ~ https_checking + bup_freq_epayment + bup_data_epayment + 
                   postgraduation + soft_keyboard + auth_method_securitypin + lessthan_two_lac +
                   os_update + age_41to60 + professional + knowledge_epayment + cyber_fraud_epayment +
                   age_below_25 + morethan_twenty_lac + male + privacy_percp + policy_reading +
                   auth_method_password + service + cookies + female + auth_method_faceid,
                 data = train_data, family = binomial)

# Print the summary of the model
summary(Model_lgR)
# Calculate VIF
vif_values <- car::vif(Model_lgR)
# Find the variable with the maximum VIF
max_vif_variable <- names(vif_values)[which.max(vif_values)]
max_vif_value <- max(vif_values)
# Print the results
cat("Variable with Maximum VIF:", max_vif_variable, "\n")
cat("Maximum VIF Value:", max_vif_value, "\n")



# Removing Male
# Fit the logistic regression model on training data
Model_lgR1 <- glm(mny_lost_epayment ~ https_checking + bup_freq_epayment + bup_data_epayment + 
                    postgraduation + soft_keyboard + auth_method_securitypin + lessthan_two_lac +
                    os_update + age_41to60 + professional + knowledge_epayment + cyber_fraud_epayment +
                    age_below_25 + morethan_twenty_lac + privacy_percp + policy_reading +
                    auth_method_password + service + cookies + female + auth_method_faceid,
                  data = train_data, family = binomial)

# Print the summary of the model
summary(Model_lgR1)
# Calculate VIF
vif_values <- car::vif(Model_lgR1)
# Find the variable with the maximum VIF
max_vif_variable <- names(vif_values)[which.max(vif_values)]
max_vif_value <- max(vif_values)
# Print the results
cat("Variable with Maximum VIF:", max_vif_variable, "\n")
cat("Maximum VIF Value:", max_vif_value, "\n")

# Removing Age below 25
# Removing Male
# Fit the logistic regression model on training data
Model_lgR2 <- glm(mny_lost_epayment ~ https_checking + bup_freq_epayment + bup_data_epayment + 
                    postgraduation + soft_keyboard + auth_method_securitypin + lessthan_two_lac +
                    os_update + age_41to60 + professional + knowledge_epayment + cyber_fraud_epayment
                  + morethan_twenty_lac + privacy_percp + policy_reading +
                    auth_method_password + service + cookies + female + auth_method_faceid,
                  data = train_data, family = binomial)

# Print the summary of the model
summary(Model_lgR2)
# Calculate VIF
vif_values <- car::vif(Model_lgR2)
# Find the variable with the maximum VIF
max_vif_variable <- names(vif_values)[which.max(vif_values)]
max_vif_value <- max(vif_values)
# Print the results
cat("Variable with Maximum VIF:", max_vif_variable, "\n")
cat("Maximum VIF Value:", max_vif_value, "\n")



# Based on VIF Value selected variables 

Model_Final_lr <- glm(mny_lost_epayment ~ https_checking + bup_freq_epayment + bup_data_epayment + 
                        postgraduation + soft_keyboard + auth_method_securitypin + lessthan_two_lac +
                        os_update + age_41to60 + professional + knowledge_epayment + cyber_fraud_epayment
                      + morethan_twenty_lac + privacy_percp + policy_reading +
                        auth_method_password + service + cookies + female + auth_method_faceid,
                      data = train_data, family = binomial)
summary(Model_Final_lr)


# Print the summary of the model
summary_result <- summary(Model_Final_lr)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))


# Removing Proffesionals
Model_Final_lr1 <- glm(mny_lost_epayment ~ https_checking + bup_freq_epayment + bup_data_epayment + 
                         postgraduation + soft_keyboard + auth_method_securitypin + lessthan_two_lac +
                         os_update + age_41to60 +knowledge_epayment + cyber_fraud_epayment
                       + morethan_twenty_lac + privacy_percp + policy_reading +
                         auth_method_password + service + cookies + female + auth_method_faceid,
                       data = train_data, family = binomial)

# Print the summary of the model
summary_result <- summary(Model_Final_lr1)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))

#Removing back up data

Model_Final_lr2 <- glm(mny_lost_epayment ~ https_checking  +  bup_freq_epayment +
                         postgraduation + soft_keyboard + auth_method_securitypin + lessthan_two_lac +
                         os_update + age_41to60 +knowledge_epayment + cyber_fraud_epayment
                       + morethan_twenty_lac + privacy_percp + policy_reading +
                         auth_method_password + service + cookies + female + auth_method_faceid,
                       data = train_data, family = binomial)

# Print the summary of the model
summary_result <- summary(Model_Final_lr2)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))


# Removing Face id


Model_Final_lr3 <- glm(mny_lost_epayment ~ https_checking +  bup_freq_epayment + 
                         postgraduation + soft_keyboard + auth_method_securitypin + lessthan_two_lac +
                         os_update + age_41to60 +knowledge_epayment + cyber_fraud_epayment
                       + morethan_twenty_lac + privacy_percp + policy_reading +
                         auth_method_password + service + cookies + female ,
                       data = train_data, family = binomial)

# Print the summary of the model
summary_result <- summary(Model_Final_lr3)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))


# Removing Postgraduation


Model_Final_lr4 <- glm(mny_lost_epayment ~ https_checking +  bup_freq_epayment + soft_keyboard + auth_method_securitypin + lessthan_two_lac +
                         os_update + age_41to60 +knowledge_epayment + cyber_fraud_epayment
                       + morethan_twenty_lac + privacy_percp + policy_reading +
                         auth_method_password + service + cookies + female ,
                       data = train_data, family = binomial)

# Print the summary of the model
summary_result <- summary(Model_Final_lr4)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))

# Removing knowledge epayment


Model_Final_lr5 <- glm(mny_lost_epayment ~ https_checking +  bup_freq_epayment + 
                         auth_method_securitypin + lessthan_two_lac +
                         os_update + age_41to60  + soft_keyboard+ cyber_fraud_epayment
                       + morethan_twenty_lac + privacy_percp + policy_reading +
                         auth_method_password + service + cookies + female ,
                       data = train_data, family = binomial)

# Print the summary of the model
summary_result <- summary(Model_Final_lr5)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))

# Removing Policy Reading


Model_Final_lr6 <- glm(mny_lost_epayment ~ https_checking +  bup_freq_epayment + 
                         auth_method_securitypin + lessthan_two_lac +
                         os_update + age_41to60  + soft_keyboard+ cyber_fraud_epayment
                       + morethan_twenty_lac + privacy_percp +
                         auth_method_password + service + cookies + female ,
                       data = train_data, family = binomial)

# Print the summary of the model
summary_result <- summary(Model_Final_lr6)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))


# Removing soft keyboard


Model_Final_lr7 <- glm(mny_lost_epayment ~ https_checking +  bup_freq_epayment + 
                         auth_method_securitypin + lessthan_two_lac +
                         os_update + age_41to60  + cyber_fraud_epayment
                       + morethan_twenty_lac + privacy_percp +
                         auth_method_password + service + cookies + female ,
                       data = train_data, family = binomial)

# Print the summary of the model
summary_result <- summary(Model_Final_lr7)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))

# Removing cookies


Model_Final_lr8 <- glm(mny_lost_epayment ~ https_checking +  bup_freq_epayment + 
                         auth_method_securitypin + lessthan_two_lac +
                         os_update + age_41to60  + cyber_fraud_epayment
                       + morethan_twenty_lac + privacy_percp +
                         auth_method_password + service + female ,
                       data = train_data, family = binomial)

# Print the summary of the model
summary_result <- summary(Model_Final_lr8)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))


# Removing service


Model_Final_lr9 <- glm(mny_lost_epayment ~ https_checking +  bup_freq_epayment + 
                         auth_method_securitypin + lessthan_two_lac +
                         os_update + age_41to60  + cyber_fraud_epayment
                       + morethan_twenty_lac + privacy_percp +
                         auth_method_password  + female ,
                       data = train_data, family = binomial)

# Print the summary of the model
summary_result <- summary(Model_Final_lr9)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))

# Removing auth method pass


Model_Final_lr10 <- glm(mny_lost_epayment ~ https_checking +  bup_freq_epayment + 
                          auth_method_securitypin + lessthan_two_lac +
                          os_update + age_41to60  + cyber_fraud_epayment
                        + morethan_twenty_lac + privacy_percp +
                          female ,
                        data = train_data, family = binomial)

# Print the summary of the model
summary_result <- summary(Model_Final_lr10)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))

# Removing backup freq


Model_Final_lr11 <- glm(mny_lost_epayment ~ https_checking + 
                          auth_method_securitypin + lessthan_two_lac +
                          os_update + age_41to60  + cyber_fraud_epayment
                        + morethan_twenty_lac + privacy_percp +
                          female ,
                        data = train_data, family = binomial)

# Print the summary of the model
summary_result <- summary(Model_Final_lr11)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))

# Removing backup freq


Model_Final_lr12 <- glm(mny_lost_epayment ~ https_checking + 
                          auth_method_securitypin + lessthan_two_lac +
                          os_update + age_41to60  + cyber_fraud_epayment
                        + morethan_twenty_lac + privacy_percp ,
                        data = train_data, family = binomial)

# Print the summary of the model
summary_result <- summary(Model_Final_lr12)
# Find the variable with the highest p-value
max_p_value_index <- which.max(summary_result$coefficients[, "Pr(>|z|)"])
# Print the variable with the highest p-value
variable_with_max_p_value <- rownames(summary_result$coefficients)[max_p_value_index]
print(paste("Variable with Highest P-Value:", variable_with_max_p_value))
# Print the p-value of the variable with the highest p-value
max_p_value <- summary_result$coefficients[max_p_value_index, "Pr(>|z|)"]
print(paste("P-Value:", max_p_value))


#Final Model Opted. # F I N A L 

Model_Final_logistic <- glm(mny_lost_epayment ~ https_checking + 
                              auth_method_securitypin + lessthan_two_lac +
                              os_update + age_41to60  + cyber_fraud_epayment
                            + morethan_twenty_lac + privacy_percp ,
                            data = train_data, family = binomial)


# train data validation data test data # DEVIDING DATA INTO TRAIN , VALIDATION AND TESTING 

library(pROC)

split_index_valid_test <- sample.split(temp_data$mny_lost_epayment, SplitRatio = 0.5)
valid_data <- subset(temp_data, split_index_valid_test == TRUE)
test_data <- subset(temp_data, split_index_valid_test == FALSE)

Model_Final_logistic <- glm(mny_lost_epayment ~ https_checking + 
                              auth_method_securitypin + lessthan_two_lac +
                              os_update + age_41to60  + cyber_fraud_epayment
                            + morethan_twenty_lac + privacy_percp ,
                            data = train_data, family = binomial)


summary(Model_Final_logistic)

# Calculate probabilities based on odds ratios
probability_intercept <- exp(2.9649) / (1 + exp(2.9649))

probability_https_checking <- exp(-2.2101) / (1 + exp(-2.2101))
probability_auth_method_securitypin <- exp(-1.7063) / (1 + exp(-1.7063))
probability_lessthan_two_lac <- exp(-3.5897) / (1 + exp(-3.5897))
probability_os_update <- exp(-1.4738) / (1 + exp(-1.4738))
probability_age_41to60 <- exp(-3.0069) / (1 + exp(-3.0069))
probability_cyber_fraud_epayment <- exp(1.6918) / (1 + exp(1.6918))
probability_morethan_twenty_lac <- exp(-1.2433) / (1 + exp(-1.2433))
probability_privacy_percp <- exp(-2.0627) / (1 + exp(-2.0627))

# Print the probabilities
print("Probability Intercept:")
print(probability_intercept)

print("Probabilities for Predictors:")
print(probability_https_checking)
print(probability_auth_method_securitypin)
print(probability_lessthan_two_lac)
print(probability_os_update)
print(probability_age_41to60)
print(probability_cyber_fraud_epayment)
print(probability_morethan_twenty_lac)
print(probability_privacy_percp)












# Predict on validation data
valid_pred_probs <- predict(Model_Final_logistic, newdata = valid_data, type = "response")
# Create a ROC curve for validation data
roc_valid <- roc(valid_data$mny_lost_epayment, valid_pred_probs)
print(roc_valid)
library(ggplot2)
# Extract ROC coordinates
roc_coords <- coords(roc_valid)

# Plot ROC curve with ggplot2 for enhanced customization
roc_plot_gg <- ggplot(roc_coords, aes(1 - specificity, sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  theme_minimal() +
  labs(
    title = "ROC Curve - Validation Data",
    x = "1 - Specificity",
    y = "Sensitivity"
  )

# Display the plot
print(roc_plot_gg)
# Find the optimal threshold for validation data based on F1 score
coords_valid <- coords(roc_valid, "best", maximize = "f1")
optimal_threshold_valid <- coords_valid$threshold
# Print the optimal threshold for validation data
cat("Optimal Threshold for Validation Data (based on F1 score):", optimal_threshold_valid, "\n")





# Apply the selected threshold on test data

test_pred_probs <- predict(Model_Final_logistic, newdata = test_data, type = "response")
test_pred_class <- ifelse(test_pred_probs > optimal_threshold_valid, 1, 0)


# Calculate performance metrics for training data
conf_matrix_train <- table(Actual = train_data$mny_lost_epayment, Predicted = ifelse(predict(Model_lgR, newdata = train_data, type = "response") > optimal_threshold_valid, 1, 0))
accuracy_train <- sum(diag(conf_matrix_train)) / sum(conf_matrix_train)
precision_train <- conf_matrix_train[2, 2] / sum(conf_matrix_train[, 2])
recall_train <- conf_matrix_train[2, 2] / sum(conf_matrix_train[2, ])
f1_score_train <- 2 * (precision_train * recall_train) / (precision_train + recall_train)

# Apply the selected threshold on test data
test_pred_probs <- predict(Model_lgR, newdata = test_data, type = "response")
test_pred_class <- ifelse(test_pred_probs > optimal_threshold_valid, 1, 0)

# Calculate performance metrics for test data
conf_matrix_test <- table(Actual = test_data$mny_lost_epayment, Predicted = test_pred_class)
accuracy_test <- sum(diag(conf_matrix_test)) / sum(conf_matrix_test)
precision_test <- conf_matrix_test[2, 2] / sum(conf_matrix_test[, 2])
recall_test <- conf_matrix_test[2, 2] / sum(conf_matrix_test[2, ])
f1_score_test <- 2 * (precision_test * recall_test) / (precision_test + recall_test)

# Create a data frame to store the results
results_df <- data.frame(
  Dataset = c("Training", "Test"),
  Accuracy = c(accuracy_train, accuracy_test),
  Precision = c(precision_train, precision_test),
  Recall = c(recall_train, recall_test),
  F1_Score = c(f1_score_train, f1_score_test)
)

# Print the results in tabular form
cat("Optimal Threshold for Validation Data (based on F1 score):", optimal_threshold_valid, "\n")
print("Performance Metrics:")
print(results_df)

# Print confusion matrix for test data
conf_matrix_test <- table(Actual = test_data$mny_lost_epayment, Predicted = test_pred_class)
print("Confusion Matrix - Test Data:")
print(conf_matrix_test)


# Apply the selected threshold on test data
test_pred_probs <- predict(Model_Final_logistic, newdata = test_data, type = "response")
test_pred_class <- ifelse(test_pred_probs > optimal_threshold_valid, 1, 0)

# Perform Hosmer-Lemeshow test on test data
hoslem_test <- residuals.lrm(Model_Final_logistic, type = "gof")

# Print the results of the Hosmer-Lemeshow test
print("Hosmer-Lemeshow Test - Test Data:")
print(hoslem_test)


# End of Coding
