#!/usr/bin/env python
# coding: utf-8

# In[2]:


get_ipython().system('pip uninstall imbalanced-learn -y')
get_ipython().system('pip install imbalanced-learn')


# In[1]:


conda install -c conda-forge imbalanced-learn


# In[1]:


import pandas as pd
from imblearn.over_sampling import SMOTE
from collections import Counter

# Load your dataset (replace 'your_file_path.xlsx' with the actual file path)
file_path = r'C:\Users\Jay Patel\Desktop\IIMA\Capstone Project\Capstone Project\epayment_analysis_dummy_r1.xlsx'
raw_data = pd.read_excel(file_path)

# Check the initial distribution of the target variable
print("Original class distribution:", Counter(raw_data['mny_lost_epayment']))

# Separate features and target variable
X = raw_data.drop('mny_lost_epayment', axis=1)
y = raw_data['mny_lost_epayment']

# Initialize SMOTE
smote = SMOTE(random_state=42)

# Apply SMOTE to generate synthetic samples
X_balanced, y_balanced = smote.fit_resample(X, y)

# Check the balanced class distribution
print("Balanced class distribution:", Counter(y_balanced))


# In[53]:


import os

# Output file path for SMOTE data
output_file_path = r'C:\Users\Jay Patel\Desktop\IIMA\Capstone Project\Capstone Project\SMOTE_data.xlsx'

# Combine original and SMOTE-generated data
smote_data = pd.concat([X_balanced, y_balanced], axis=1)

# Save the SMOTE data to an Excel file
smote_data.to_excel(output_file_path, index=False)

print("SMOTE data saved to:", output_file_path)


# In[54]:


## Workin on SMOTE data ##


# In[64]:


import pandas as pd
import numpy as np
from imblearn.over_sampling import SMOTE
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import RandomizedSearchCV
from sklearn.metrics import accuracy_score, classification_report, confusion_matrix
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt

# Load SMOTE data
smote_data_path = r'C:\Users\Jay Patel\Desktop\IIMA\Capstone Project\Capstone Project\MNY_LOST AS RESPONSE\SMOTE DATA\SMOTE_data.xlsx'
smote_data = pd.read_excel(smote_data_path)

# Separate features and target variable
X_smote = smote_data.drop('mny_lost_epayment', axis=1)
y_smote = smote_data['mny_lost_epayment']

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X_smote, y_smote, test_size=0.2, random_state=42)

# Initialize the Random Forest model
rf_model = RandomForestClassifier(oob_score=True, random_state=42)

# Define the hyperparameter grid for tuning
param_grid_rf = {
    'n_estimators': [50, 100, 150],
    'max_depth': [None, 10, 20, 30],
    'min_samples_split': [2, 5, 10],
    'min_samples_leaf': [1, 2, 4],
    'max_features': ['auto', 'sqrt', 'log2', None]  # Updated max_features options
}

# Initialize RandomizedSearchCV for Random Forest
random_search_rf = RandomizedSearchCV(estimator=rf_model, param_distributions=param_grid_rf, n_iter=50, 
                                       cv=5, scoring='accuracy', n_jobs=-1, random_state=42)

# Fit the model with the best hyperparameters on the training data
random_search_rf.fit(X_train, y_train)

# Get the best Random Forest model
best_rf_model = random_search_rf.best_estimator_

# Make predictions on the training data
y_pred_train_rf = best_rf_model.predict(X_train)

# Evaluate the Random Forest model on training data
print("Accuracy on Training Data:", accuracy_score(y_train, y_pred_train_rf))

# Make predictions on the test data
y_pred_rf = best_rf_model.predict(X_test)

# Evaluate the Random Forest model on test data
print("Best Hyperparameters for Random Forest:", random_search_rf.best_params_)
print("Accuracy on Test Data:", accuracy_score(y_test, y_pred_rf))
print("Classification Report:\n", classification_report(y_test, y_pred_rf))
print("Confusion Matrix:\n", confusion_matrix(y_test, y_pred_rf))
print("Out-of-Bag (OOB) Error Rate:", 1 - best_rf_model.oob_score_)

# Plot feature importance
importances = best_rf_model.feature_importances_
indices = np.argsort(importances)[::-1]

plt.figure(figsize=(15, 8))

# Choose a colormap (e.g., 'viridis', 'plasma', 'viridis', 'cividis', etc.)
colormap = plt.get_cmap('viridis')

# Normalize importance values to map to colors
norm = plt.Normalize(importances.min(), importances.max())

# Create a horizontal bar plot with colored bars
bars = plt.barh(range(X_train.shape[1]), importances[indices], color=colormap(norm(importances[indices])))

# Add a colorbar for reference
sm = plt.cm.ScalarMappable(cmap=colormap, norm=norm)
sm.set_array([])  # Dummy array for the data range
cbar = plt.colorbar(sm, orientation='vertical')
cbar.set_label('Importance')

# Set y-axis ticks and labels
plt.yticks(range(X_train.shape[1]), X_train.columns[indices])

# Reverse the y-axis to have the most important features at the top
plt.gca().invert_yaxis()

plt.xlabel("Importance")
plt.ylabel("Feature")
plt.title("Feature Importance - Random Forest")
plt.show()

# Extract the most important features contributing to 80%
cumulative_importance = np.cumsum(importances[indices])
index_80_percent = np.where(cumulative_importance >= 0.8)[0][0] + 1
selected_features_rf = X_train.columns[indices[:index_80_percent]].tolist()

# Create a new dataset with selected features and target variable
selected_data_rf = X_smote[selected_features_rf].copy()
selected_data_rf['mny_lost_epayment'] = y_smote

# Save the selected features data to an Excel file
output_file_path_rf = r'C:\Users\Jay Patel\Desktop\IIMA\Capstone Project\Capstone Project\MNY_LOST AS RESPONSE\selected_features_rf.xlsx'
selected_data_rf.to_excel(output_file_path_rf, index=False)

print("Selected features data saved to:", output_file_path_rf)


# In[56]:


### Using selcted features file to apply Decision Tree ###


# In[59]:


import pandas as pd
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, classification_report

# Load the selected features data
selected_features_path = r'C:\Users\Jay Patel\Desktop\IIMA\Capstone Project\Capstone Project\MNY_LOST AS RESPONSE\selected_features_rf.xlsx'
selected_data = pd.read_excel(selected_features_path)

# Separate features and target variable
X = selected_data.drop('mny_lost_epayment', axis=1)
y = selected_data['mny_lost_epayment']

# Split the data into training and testing sets
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Initialize the Decision Tree model
dt_model = DecisionTreeClassifier(random_state=42)

# Train the model
dt_model.fit(X_train, y_train)

# Make predictions on the test data
y_pred = dt_model.predict(X_test)

# Evaluate the Decision Tree model
print("Accuracy:", accuracy_score(y_test, y_pred))
print("Classification Report:\n", classification_report(y_test, y_pred))

# Visualize the Decision Tree (optional)
from sklearn.tree import plot_tree
import matplotlib.pyplot as plt

# Convert Index to a list
feature_names_list = X.columns.tolist()

plt.figure(figsize=(15, 10))
plot_tree(dt_model, filled=True, feature_names=feature_names_list, class_names=['0', '1'], rounded=True)
plt.title("Decision Tree Plot")
plt.show()


# In[60]:


### Extracting Rules ###


# In[61]:


def tree_to_rules(tree, feature_names, class_names, node_index=0):
    node = tree.tree_

    # Get feature name and threshold for the current node
    feature_index = node.feature[node_index]
    threshold = node.threshold[node_index]

    # Get feature name
    feature_name = feature_names[feature_index]

    # Get class name if the node is a leaf
    if node.children_left[node_index] == node.children_right[node_index]:
        class_index = np.argmax(node.value[node_index])
        class_name = class_names[class_index]
        return [f"If {feature_name} <= {threshold}, then class = {class_name}"]

    # Recursive call for left and right children
    left_rules = tree_to_rules(tree, feature_names, class_names, node.children_left[node_index])
    right_rules = tree_to_rules(tree, feature_names, class_names, node.children_right[node_index])

    # Combine rules
    rules = [f"If {feature_name} <= {threshold}, then:"] + left_rules + [f"Else, {feature_name} > {threshold}, then:"] + right_rules

    return rules

# Extract rules from the decision tree
rules = tree_to_rules(dt_model, feature_names=feature_names_list, class_names=['0', '1'])

# Print the rules
for rule in rules:
    print(rule)


# In[62]:


def extract_rules(tree, feature_names):
    tree_ = tree.tree_
    feature_name = [
        feature_names[i] if i != _tree.TREE_UNDEFINED else "undefined!"
        for i in tree_.feature
    ]

    def recurse(node):
        if tree_.feature[node] != _tree.TREE_UNDEFINED:
            name = feature_name[node]
            threshold = tree_.threshold[node]
            print(f"If {name} <= {threshold}:")
            recurse(tree_.children_left[node])

            print(f"Else, {name} > {threshold}:")
            recurse(tree_.children_right[node])
        else:
            print(f"Class = {np.argmax(tree_.value[node][0])}")

    recurse(0)

# Example usage:
extract_rules(dt_model, X_train.columns)


# In[ ]:





# In[ ]:




