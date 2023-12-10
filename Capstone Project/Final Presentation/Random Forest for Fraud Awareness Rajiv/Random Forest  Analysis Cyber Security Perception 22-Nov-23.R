
# Library and Data Load ---------------------------------------------------

setwd("c:/users/rajiv.trivedi/downloads")
library("readxl")
library(randomForest)

data_hr=cyber_security_data <- read_excel("Random Forest Analysis of Cyber Security Perception.xlsx", sheet = "cyber_security_data")


# converting response and explanatory variable to numeric -----------------

# status
data_hr$fraud_awareness <- as.factor(data_hr$fraud_awareness)
levels(data_hr$fraud_awareness) <- c(0, 1)



# Data split and removing null values from data --------------------------------------------------------------

set.seed(123)

samp <- sample(nrow(data_hr), 0.8 * nrow(data_hr))

train <- data_hr[samp, ]

test <- data_hr[-samp, ]
train_no_missing_rows <- na.omit(train)
test_no_missing_rows <- na.omit(test)

# variables and formulas ---------------------------------------------------------------

response_variable <- "fraud_awareness"


# Specify the explanatory variables
explanatory_variables <- c("doing_transaction","transaction_method","concerned","os_update","public_network","information_security_knowledge","data_backup","data_backup_scheule","network_security","lost_money","two_factor","gender","Age","Education","Occupation","income")

# Create a formula for the random forest model
formula <- as.formula(paste(response_variable, "~", paste(explanatory_variables, collapse = "+")))
formula



# model -------------------------------------------------------------------

# Train the random forest model
rf_model <- randomForest(formula, data = train_no_missing_rows,
                         mtry=4, 
                         ntree=500)

# Make predictions on the test set (as probabilities)
predicted_classes <- predict(rf_model, newdata = test_no_missing_rows, type = "response")
rf_model

# Create a data frame with predictions and actual values
results <- data.frame(
  Predicted_Class = predicted_classes,
  Actual_Class = test_no_missing_rows$fraud_awareness
)
results
# Display the confusion matrix
conf_matrix <- table(results$Predicted_Class, results$Actual_Class)
print("Confusion Matrix:")
print(conf_matrix)

# Check accuracy
accuracy <- sum(results$Predicted_Class == test_no_missing_rows$fraud_awareness) / nrow(test)

# Display accuracy
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

# accuracy on all the data ------------------------------------------------

data_hr_all<-data_hr

predicted_classes_all <- predict(rf_model, newdata =data_hr_all , type = "response")

results_all <- data.frame(
  Predicted_Class = predicted_classes_all,
  Actual_Class = data_hr_all$fraud_awareness
)
results_all
# Display the confusion matrix
conf_matrix_all <- table(results_all$Predicted_Class, results_all$Actual_Class)
print("Confusion Matrix:")
print(conf_matrix_all)

# Check accuracy
accuracy_all <- sum(results_all$Predicted_Class == data_hr_all$fraud_awareness) / nrow(data_hr_all)


# Display accuracy
print(paste("Accuracy:", round(accuracy_all * 100, 2), "%"))

# Variable importance -----------------------------------------------------
# Get variable importance
importance <- rf_model$importance
importance
# Order the variables by importance
sorted_importance <- importance[order(importance[, 1], decreasing = TRUE), ]
sorted_importance
# Plot variable importance
barplot(sorted_importance, col = "lightblue", main = "Variable Importance", cex.names = 0.7)
sorted_importance


# rough code --------------------------------------------------------------

# Creating a data frame
variable_names <- c("doing_transaction", "data_backup", "information_security_knowledge", 
                    "network_security", "two_factor", "public_network", "os_update", 
                    "income", "concerned", "Occupation", "gender", "data_backup_scheule", 
                    "Age", "Education", "lost_money", "transaction_method")

values <- c(2.6276533, 1.7421563, 1.6587152, 1.5731396, 1.3864813, 1.3621695, 
            1.3223002, 0.8293752, 0.7476711, 0.5489599, 0.5349777, 0.3762079, 
            0.2551030, 0.2469639, 0.2108870, 0.1792693)

table_data <- data.frame(Variable = variable_names, Value = values)

# Print the table
print(table_data)

