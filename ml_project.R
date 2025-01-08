# Install required libraries
install.packages("ggplot2")        # For visualization
install.packages("corrplot")       # For correlation heatmaps
install.packages("reshape2")       # For data transformation (melt function)
install.packages("caret")          # For machine learning and model training
install.packages("glmnet")         # For Logistic Regression
install.packages("e1071")          # For SVM
install.packages("rpart")          # For Decision Tree
install.packages("randomForest")   # For Random Forest
install.packages("xgboost")        # For XGBoost
install.packages("dplyr")
install.packages("pROC")


# Load the libraries
library(pROC)
library(dplyr)
library(ggplot2)       # Data visualization
library(corrplot)      # Correlation matrix heatmap
library(reshape2)      # Data transformation
library(caret)         # Machine learning utilities
library(glmnet)        # Logistic regression
library(e1071)         # Support Vector Machine
library(rpart)         # Decision tree
library(randomForest)  # Random forest
library(xgboost)       # XGBoost



# Read the CSV file
data <- read.csv("/Users/furkanozdal/Downloads/alzheimers_disease_data.csv", header = TRUE, stringsAsFactors = FALSE)
df <- data
head(data)

summary(data)

# Remove columns using column names
data <- data[, !(names(data) %in% c("PatientID", "DoctorInCharge"))]

is.na(data)



# Custom labels for each categorical column
custom_labels <- list(
  Gender = c("Male", "Female"),
  Ethnicity = c("Caucasian", "African American", "Asian", "Other"),
  EducationLevel = c("None", "High School", "Bachelor's", "Higher"),
  Smoking = c("No", "Yes"),
  FamilyHistoryAlzheimers = c("No", "Yes"),
  CardiovascularDisease = c("No", "Yes"),
  Diabetes = c("No", "Yes"),
  Depression = c("No", "Yes"),
  HeadInjury = c("No", "Yes"),
  Hypertension = c("No", "Yes"),
  MemoryComplaints = c("No", "Yes"),
  BehavioralProblems = c("No", "Yes"),
  Confusion = c("No", "Yes"),
  Disorientation = c("No", "Yes"),
  PersonalityChanges = c("No", "Yes"),
  DifficultyCompletingTasks = c("No", "Yes"),
  Forgetfulness = c("No", "Yes")
)

# List of categorical columns
categorical_columns <- names(custom_labels)

# Loop through each categorical column and plot count plots
for (column in categorical_columns) {
  # Create the count plot
  p <- ggplot(data = df, aes_string(x = column)) +
    geom_bar(fill = "skyblue", color = "black") +
    labs(title = paste("Countplot of", column), x = column, y = "Count") +
    scale_x_discrete(labels = custom_labels[[column]]) +  # Set custom labels
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust text
  
  # Display the plot
  print(p)
}


numerical_columns <- c("Age", "BMI", "AlcoholConsumption","PhysicalActivity", "DietQuality","SleepQuality","SystolicBP","DiastolicBP","CholesterolTotal","CholesterolLDL","CholesterolHDL","CholesterolTriglycerides","MMSE","FunctionalAssessment","ADL")  # Replace with actual column names

# Loop through each numerical column and plot histograms
for (column in numerical_columns) {
  # Create the histogram with density curve
  p <- ggplot(df, aes_string(x = column)) +
    geom_histogram(aes(y = ..density..), bins = 20, fill = "skyblue", color = "black") +  # Histogram
    geom_density(color = "red", size = 1) +  # KDE line
    labs(title = paste("Distribution of", column), x = column, y = "Density") +
    theme_minimal()
  
  # Display the plot
  print(p)
}

# Calculate the correlation matrix
cor_matrix <- cor(df %>% select_if(is.numeric), use = "complete.obs")  # Select numeric columns only

# Draw the heatmap
corrplot(cor_matrix, 
         method = "color",         # Use colored tiles
         col = colorRampPalette(c("blue", "white", "red"))(200),  # Coolwarm color scheme
         type = "full",            # Show full matrix
         tl.col = "black",         # Text color
         tl.srt = 90,              # Rotate text for better readability
         tl.cex = 0.6)  



# Subset the dataframe to include only numeric columns
numeric_df <- df[sapply(df, is.numeric)]

# Compute Pearson correlation coefficients with the 'Diagnosis' column
cor_matrix <- cor(numeric_df, use = "complete.obs")  # Calculate the correlation matrix
correlations <- cor_matrix[, "Diagnosis"]   # Extract correlation with 'Diagnosis'
correlations <- correlations[-length(correlations)]  # Remove 'Diagnosis' correlation with itself

# Sort the correlations in ascending order
correlations <- sort(correlations, decreasing = FALSE)  # Sorting in ascending order

# Display the sorted correlations
print(correlations)

# Create a data frame for plotting
correlations_df <- data.frame(Feature = names(correlations), Correlation = correlations)

# Plot the correlations using ggplot2
ggplot(correlations_df, aes(x = Feature, y = Correlation)) +
  geom_bar(stat = "identity", width = 0.7, fill = "steelblue") +  # Create bar plot
  ylim(-1, 1) +  # Set y-axis limits
  labs(title = "Pearson Correlation with Diagnosis", x = "Features", y = "Pearson Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
        plot.title = element_text(hjust = 0.5))  # Center plot title




#########Most corred


# Subset the required columns for boxplots
boxplot_data <- df[, c("FunctionalAssessment", "ADL", "MMSE")]

# Convert data to long format for ggplot2
long_data <- melt(boxplot_data, variable.name = "Feature", value.name = "Value")

# Create boxplots
ggplot(long_data, aes(x = Feature, y = Value, fill = Feature)) +
  geom_boxplot() +
  labs(title = "Boxplots of FunctionalAssessment, ADL, and MMSE",
       x = "Features", y = "Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Pastel1")  # Optional: Set color palette



# Subset the required columns
countplot_data <- df[, c("BehavioralProblems", "MemoryComplaints")]

# Convert to long format for ggplot2
long_data <- melt(countplot_data, variable.name = "Feature", value.name = "Value")

# Create countplots using ggplot2
ggplot(long_data, aes(x = Value, fill = Feature)) +
  geom_bar(position = "dodge") +
  facet_wrap(~Feature, scales = "free_x") +
  labs(title = "Countplots of BehavioralProblems and MemoryComplaints",
       x = "Categories", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")  # Optional: Use Set2 color palette

#Countplot of target feature

#Create a countplot for the Diagnosis column
ggplot(df, aes(x = Diagnosis, fill = Diagnosis)) +
  geom_bar() +
  labs(title = "Countplot of Diagnosis",
       x = "Diagnosis Categories", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")  # Optional: Use a color palette

#Outlier analysis
# Subset numerical columns
exclude_columns <- c(
  "PatientID", "Gender", "Ethnicity", "EducationLevel", "Smoking", 
  "FamilyHistoryAlzheimers", "CardiovascularDisease", "Diabetes", 
  "Depression", "HeadInjury", "Hypertension", "MemoryComplaints", 
  "BehavioralProblems", "Confusion", "Disorientation", 
  "PersonalityChanges", "DifficultyCompletingTasks", "Forgetfulness"
)

# Subset numerical columns excluding the ones in exclude_columns
numeric_df <- df[sapply(df, is.numeric)]
numeric_df <- numeric_df[, setdiff(colnames(numeric_df), exclude_columns)]

# Initialize an empty list to store IQR results
iqr_results <- list()

# Loop through each numerical column to calculate IQR and bounds
for (feature in colnames(numeric_df)) {
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(numeric_df[[feature]], 0.25, na.rm = TRUE)
  Q3 <- quantile(numeric_df[[feature]], 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  
  # Define lower and upper bounds
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  outlier_count <- sum(numeric_df[[feature]] < lower_bound | numeric_df[[feature]] > upper_bound, na.rm = TRUE)
  
  # Store the results in a list
  iqr_results[[feature]] <- list(Outlier_Count = outlier_count  )
  cat("Feature:", feature, "\n")
  cat(" - Outlier Count:", outlier_count, "\n\n")
}

# View the complete IQR results as a list (optional)
print(iqr_results)

df <- df[, !colnames(df) %in% c("PatientID", "DoctorInCharge")]

#Normalize numerical columns
columns_to_normalize <- c(
  "Age", "BMI", "AlcoholConsumption", "PhysicalActivity", 
  "DietQuality", "SleepQuality", "SystolicBP", "DiastolicBP", 
  "CholesterolTotal", "CholesterolLDL", "CholesterolHDL", 
  "CholesterolTriglycerides", "MMSE", "FunctionalAssessment", "ADL"
)

# Normalize each column using min-max scaling
df[columns_to_normalize] <- lapply(df[columns_to_normalize], function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
})
df$Diagnosis <- as.factor(df$Diagnosis)
#Train Test Split
# Set a seed for reproducibility
set.seed(42)

# Create a train-test split (80% train, 20% test)
trainIndex <- createDataPartition(df$Diagnosis, p = 0.8, list = FALSE)
trainData <- df[trainIndex, ]
testData <- df[-trainIndex, ]

# Separate target variable
trainX <- trainData[, -which(names(trainData) == "Diagnosis")]
trainY <- trainData$Diagnosis
testX <- testData[, -which(names(testData) == "Diagnosis")]
testY <- testData$Diagnosis


# Set grid for Logistic Regression
log_grid <- expand.grid(alpha = c(0, 0.5, 1), lambda = seq(0.001, 0.1, length.out = 5))

# Train Logistic Regression
log_model <- train(Diagnosis ~ ., data = trainData, method = "glmnet", 
                   trControl = trainControl(method = "cv", number = 5), 
                   tuneGrid = log_grid)

# Print the results
print(log_model)

# Make predictions
log_preds <- predict(log_model, newdata = testData)

# Evaluate accuracy
log_accuracy <- mean(log_preds == testData$Diagnosis)
cat("Logistic Regression Accuracy: ", log_accuracy, "\n")

# Set grid for KNN
knn_grid <- expand.grid(k = seq(1, 20, by = 1))

# Train KNN
knn_model <- train(Diagnosis ~ ., data = trainData, method = "knn", 
                   trControl = trainControl(method = "cv", number = 5), 
                   tuneGrid = knn_grid)

# Print the results
print(knn_model)

# Make predictions
knn_preds <- predict(knn_model, newdata = testData)

# Evaluate accuracy
knn_accuracy <- mean(knn_preds == testData$Diagnosis)
cat("KNN Accuracy: ", knn_accuracy, "\n")

# Set grid for SVM
svm_grid <- expand.grid(sigma = c(0.01, 0.1, 1), C = c(0.1, 1, 10))

# Train SVM
svm_model <- train(Diagnosis ~ ., data = trainData, method = "svmRadial", 
                   trControl = trainControl(method = "cv", number = 5), 
                   tuneLength = 10, probability = TRUE)

# Print the results
print(svm_model)

# Make predictions
svm_preds <- predict(svm_model, newdata = testData)

# Evaluate accuracy
svm_accuracy <- mean(svm_preds == testData$Diagnosis)
cat("SVM Accuracy: ", svm_accuracy, "\n")

# Set grid for Decision Tree
tree_grid <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))

# Train Decision Tree
tree_model <- train(Diagnosis ~ ., data = trainData, method = "rpart", 
                    trControl = trainControl(method = "cv", number = 5), 
                    tuneGrid = tree_grid)

# Print the results
print(tree_model)

# Make predictions
tree_preds <- predict(tree_model, newdata = testData)

# Evaluate accuracy
tree_accuracy <- mean(tree_preds == testData$Diagnosis)
cat("Decision Tree Accuracy: ", tree_accuracy, "\n")

# Set grid for Random Forest
rf_grid <- expand.grid(mtry = seq(2, 10, by = 1))

# Train Random Forest
rf_model <- train(Diagnosis ~ ., data = trainData, method = "rf", 
                  trControl = trainControl(method = "cv", number = 5), 
                  tuneGrid = rf_grid)

# Print the results
print(rf_model)

# Make predictions
rf_preds <- predict(rf_model, newdata = testData)

# Evaluate accuracy
rf_accuracy <- mean(rf_preds == testData$Diagnosis)
cat("Random Forest Accuracy: ", rf_accuracy, "\n")



# Set grid for XGBoost with all required parameters
xgb_grid <- expand.grid(
  nrounds = c(100, 200), 
  max_depth = c(3, 6),
  eta = c(0.01, 0.1), 
  gamma = c(0, 1),
  colsample_bytree = c(0.5, 1), 
  min_child_weight = c(1, 5),
  subsample = c(0.7, 1)  # Added the subsample parameter
)

# Train XGBoost
xgb_model <- train(Diagnosis ~ ., data = trainData, method = "xgbTree", 
                   trControl = trainControl(method = "cv", number = 5),
                   tuneGrid = xgb_grid)

# Print the results
print(xgb_model)

# Make predictions
xgb_preds <- predict(xgb_model, newdata = testData)

# Evaluate accuracy
xgb_accuracy <- mean(xgb_preds == testData$Diagnosis)
cat("XGBoost Accuracy: ", xgb_accuracy, "\n")







# Function to evaluate and plot confusion matrix, AUC-ROC, and classification report
evaluate_and_plot <- function(model, testData) {
  # Make predictions
  preds <- predict(model, newdata = testData)
  prob_preds <- predict(model, newdata = testData, type = "prob")  # For AUC-ROC
  
  # Confusion Matrix
  cm <- confusionMatrix(preds, testData$Diagnosis)
  
  # Classification Report (Precision, Recall, F1-Score)
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1_score <- cm$byClass["F1"]
  accuracy <- cm$overall["Accuracy"]
  
  # Print Classification Report
  cat("\nClassification Report for", model$method, ":\n")
  cat("Precision: ", precision, "\n")
  cat("Recall: ", recall, "\n")
  cat("F1-Score: ", f1_score, "\n")
  cat("Accuracy: ", accuracy, "\n")
  
  # Plot Confusion Matrix
  cm_data <- as.data.frame(cm$table)
  cm_plot <- ggplot(cm_data, aes(x = Prediction, y = Reference)) + 
    geom_tile(aes(fill = Freq), color = "white") + 
    geom_text(aes(label = Freq), vjust = 1) + 
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_minimal() + 
    labs(title = paste("Confusion Matrix -", model$method), x = "Predicted", y = "Actual")
  
  # Print Confusion Matrix Plot
  print(cm_plot)
  
  # AUC-ROC
  roc_curve <- roc(testData$Diagnosis, prob_preds[,2])  # Use the second column for positive class probability
  auc_value <- auc(roc_curve)
  
  # Plot AUC-ROC Curve
  auc_plot <- ggroc(roc_curve) + 
    ggtitle(paste("AUC-ROC -", model$method)) + 
    theme_minimal() + 
    theme(legend.position = "none")
  print(auc_plot)
  
  # Return results
  return(list(cm = cm, auc = auc_value, precision = precision, recall = recall, f1_score = f1_score, accuracy = accuracy))
}

# Evaluate each model and plot
log_results <- evaluate_and_plot(log_model, testData)
knn_results <- evaluate_and_plot(knn_model, testData)
#svm_results <- evaluate_and_plot(svm_model, testData)
tree_results <- evaluate_and_plot(tree_model, testData)
rf_results <- evaluate_and_plot(rf_model, testData)
xgb_results <- evaluate_and_plot(xgb_model, testData)

# Print model accuracy
cat("Logistic Regression Accuracy: ", log_results$accuracy, "\n")
cat("KNN Accuracy: ", knn_results$accuracy, "\n")
#cat("SVM Accuracy: ", svm_results$accuracy, "\n")
cat("Decision Tree Accuracy: ", tree_results$accuracy, "\n")
cat("Random Forest Accuracy: ", rf_results$accuracy, "\n")
cat("XGBoost Accuracy: ", xgb_results$accuracy, "\n")