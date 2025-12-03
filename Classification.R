# Load necessary libraries
# -------------------------------------------------------------------------
# Check for missing packages and install them automatically
required_packages <- c("caret", "e1071", "rpart", "randomForest", "xgboost", "dplyr", "fastshap")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
library(caret)
library(e1071)
library(rpart)
library(randomForest)
library(xgboost)
library(dplyr)
library(fastshap) 

# ==========================================
# 1. Data Loading and Preprocessing
# ==========================================

# Load the dataset
data <- read.csv("trp2024income.csv", stringsAsFactors = FALSE)

# clean ACSIncome: Remove non-numeric characters if any, and convert to numeric
data$ACSIncome <- as.numeric(as.character(data$ACSIncome))

# Impute ACSIncome: Fill with 67929 if NA or 0
data$ACSIncome[is.na(data$ACSIncome) | data$ACSIncome == 0] <- 67929

# Create Target Variable: 1 if HiAtHome > 0, else 0
data$Target <- ifelse(data$HiAtHome > 0, 1, 0)
data$Target <- as.factor(data$Target) # Convert to factor for classification

# Convert NAICS2 to categorical (factor)
# This ensures it is treated as a category in trees/RF and dummy-encoded for XGBoost
data$NAICS2 <- as.factor(data$NAICS2)

# Remove rows with NAs (before selecting columns to ensure row consistency)
data <- na.omit(data)

# --- STRICT VARIABLE SELECTION ---
# Only selecting variables requested by user
# Correcting typos: NumPartTimeWorekrs -> NumPartTimeWorkers, HWTRime -> HWTRTime
selected_features <- c(
  "NumVehicles", 
  "NumDrivers", 
  "NumFullTimeWorkers", 
  "NumPartTimeWorkers", 
  "PickupOrDropoffUnder18", 
  "PickupOrDropoff18Plus", 
  "NAICS2", 
  "HWTRTime", 
  "HWDist", 
  "ACSIncome",
  "Target" # Must include the target variable
)

# Subset data to only these columns
model_data <- data %>% select(all_of(selected_features))

cat("Selected Variables for Modeling:\n")
print(names(model_data))
cat("--------------------------------------------------------\n")

# Split into Training (70%) and Testing (30%)
set.seed(123)
trainIndex <- createDataPartition(model_data$Target, p = .7, 
                                  list = FALSE, 
                                  times = 1)
train_data <- model_data[ trainIndex,]
test_data  <- model_data[-trainIndex,]

# Calculate the Target Proportion in Training Data
# We will enforce this proportion on the Test predictions
prop_positive <- mean(train_data$Target == 1)

cat(sprintf("Proportion of HiAtHome > 0 in Training Data: %.4f\n", prop_positive))
cat("--------------------------------------------------------\n")

# Helper function to apply the proportion constraint
# This finds the probability threshold that ensures the top X% are predicted as 1
apply_threshold <- function(probabilities, target_prop) {
  # We want the top (target_prop) of values to be 1
  # So the quantile cut off is 1 - target_prop
  cutoff <- quantile(probabilities, 1 - target_prop)
  preds <- ifelse(probabilities > cutoff, 1, 0)
  return(factor(preds, levels = c("0", "1")))
}

# Container for results
results <- data.frame(
  Model = character(), 
  Accuracy = numeric(), 
  Sensitivity = numeric(), 
  Specificity = numeric(), 
  Precision = numeric(), 
  F1_Score = numeric(),
  stringsAsFactors = FALSE
)

# Helper function to extract metrics and add to results
add_results <- function(model_name, cm, current_results) {
  new_row <- data.frame(
    Model = model_name,
    Accuracy = as.numeric(cm$overall['Accuracy']),
    Sensitivity = as.numeric(cm$byClass['Sensitivity']),
    Specificity = as.numeric(cm$byClass['Specificity']),
    Precision = as.numeric(cm$byClass['Pos Pred Value']),
    F1_Score = as.numeric(cm$byClass['F1']),
    stringsAsFactors = FALSE
  )
  return(rbind(current_results, new_row))
}

# ==========================================
# 2. Probit Regression
# ==========================================
cat("\n--- Probit Regression (Test Set) ---\n")
probit_model <- glm(Target ~ ., data = train_data, family = binomial(link = "probit"))
prob_probit <- predict(probit_model, newdata = test_data, type = "response")
pred_probit <- apply_threshold(prob_probit, prop_positive)

cm_probit <- confusionMatrix(pred_probit, test_data$Target, positive = "1")
print(cm_probit) 
results <- add_results("Probit", cm_probit, results)

# ==========================================
# 3. Support Vector Machine (SVM)
# ==========================================
cat("\n--- Support Vector Machine (Test Set) ---\n")
svm_model <- svm(Target ~ ., data = train_data, probability = TRUE)
svm_pred_obj <- predict(svm_model, newdata = test_data, probability = TRUE)
prob_svm <- attr(svm_pred_obj, "probabilities")[, "1"]
pred_svm <- apply_threshold(prob_svm, prop_positive)

cm_svm <- confusionMatrix(pred_svm, test_data$Target, positive = "1")
print(cm_svm)
results <- add_results("SVM", cm_svm, results)

# ==========================================
# 4. Decision Tree
# ==========================================
cat("\n--- Decision Tree (Test Set) ---\n")
tree_model <- rpart(Target ~ ., data = train_data, method = "class")
prob_tree <- predict(tree_model, newdata = test_data, type = "prob")[, 2]
pred_tree <- apply_threshold(prob_tree, prop_positive)

cm_tree <- confusionMatrix(pred_tree, test_data$Target, positive = "1")
print(cm_tree)
results <- add_results("Decision Tree", cm_tree, results)

# ==========================================
# 5. Random Forest
# ==========================================
cat("\n--- Random Forest (Test Set) ---\n")
rf_model <- randomForest(Target ~ ., data = train_data, ntree = 100)
prob_rf <- predict(rf_model, newdata = test_data, type = "prob")[, 2]
pred_rf <- apply_threshold(prob_rf, prop_positive)

cm_rf <- confusionMatrix(pred_rf, test_data$Target, positive = "1")
print(cm_rf)
results <- add_results("Random Forest", cm_rf, results)

# ==========================================
# 6. XGBoost
# ==========================================
cat("\n--- XGBoost (Test Set) ---\n")
# Prepare matrices
# Since NAICS2 is now a factor, dummyVars will automatically one-hot encode it
dummies <- dummyVars(" ~ .", data = model_data %>% select(-Target))
d_x <- predict(dummies, newdata = model_data)
train_x <- d_x[trainIndex, ]
test_x <- d_x[-trainIndex, ]
train_y <- as.numeric(as.character(train_data$Target)) 
test_y <- as.numeric(as.character(test_data$Target))
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)

# Train
params <- list(objective = "binary:logistic", eta = 0.1, max_depth = 6, eval_metric = "error")
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, verbose = 0)

# Predict
prob_xgb <- predict(xgb_model, dtest)
pred_xgb <- apply_threshold(prob_xgb, prop_positive)

cm_xgb <- confusionMatrix(pred_xgb, as.factor(test_y), positive = "1")
print(cm_xgb)
results <- add_results("XGBoost", cm_xgb, results)

# ==========================================
# 7. Final Comprehensive Comparison
# ==========================================
cat("\n=================================================================================\n")
cat("Final Model Comparison on Testing Set (Sorted by Accuracy)\n")
cat("=================================================================================\n")

results <- results[order(-results$Accuracy), ]
results_formatted <- results %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(results_formatted)

best_model <- results[1, 1]
cat(paste0("\nThe best performing classification method is: ", best_model, "\n"))

# ==========================================
# 8. SHAP Score Analysis (Variable Importance)
# ==========================================
cat("\n=================================================================================\n")
cat("Calculating SHAP Scores (Variable Importance) for Each Model\n")
cat("=================================================================================\n")
cat("Note: Using a subset of test data (n=100) for SHAP calculation speed.\n")

# Use a subset of test data for SHAP speed 
X_shap <- test_data %>% select(-Target)
if(nrow(X_shap) > 100) {
  X_shap <- X_shap[1:100, ] 
}

# --- 1. Probit SHAP ---
pfun_probit <- function(object, newdata) {
  predict(object, newdata = newdata, type = "response")
}
shap_probit <- explain(probit_model, X = X_shap, pred_wrapper = pfun_probit, nsim = 10)
imp_probit <- data.frame(Variable = colnames(X_shap), 
                         Importance = colMeans(abs(shap_probit))) %>%
  arrange(desc(Importance))

cat("\n--- Probit Variable Importance (Mean |SHAP| Score) ---\n")
print(head(imp_probit, 10))

# --- 2. SVM SHAP ---
pfun_svm <- function(object, newdata) {
  attr(predict(object, newdata = newdata, probability = TRUE), "probabilities")[, "1"]
}
shap_svm <- explain(svm_model, X = X_shap, pred_wrapper = pfun_svm, nsim = 10)
imp_svm <- data.frame(Variable = colnames(X_shap), 
                      Importance = colMeans(abs(shap_svm))) %>%
  arrange(desc(Importance))

cat("\n--- SVM Variable Importance (Mean |SHAP| Score) ---\n")
print(head(imp_svm, 10))

# --- 3. Decision Tree SHAP ---
pfun_tree <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")[, 2]
}
shap_tree <- explain(tree_model, X = X_shap, pred_wrapper = pfun_tree, nsim = 10)
imp_tree <- data.frame(Variable = colnames(X_shap), 
                       Importance = colMeans(abs(shap_tree))) %>%
  arrange(desc(Importance))

cat("\n--- Decision Tree Variable Importance (Mean |SHAP| Score) ---\n")
print(head(imp_tree, 10))

# --- 4. Random Forest SHAP ---
pfun_rf <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")[, 2]
}
shap_rf <- explain(rf_model, X = X_shap, pred_wrapper = pfun_rf, nsim = 10)
imp_rf <- data.frame(Variable = colnames(X_shap), 
                     Importance = colMeans(abs(shap_rf))) %>%
  arrange(desc(Importance))

cat("\n--- Random Forest Variable Importance (Mean |SHAP| Score) ---\n")
print(head(imp_rf, 10))

# --- 5. XGBoost SHAP ---
pfun_xgb <- function(object, newdata) {
  matrix_data <- predict(dummies, newdata = newdata)
  dmatrix <- xgb.DMatrix(data = matrix_data)
  predict(object, dmatrix)
}

shap_xgb <- explain(xgb_model, X = X_shap, pred_wrapper = pfun_xgb, nsim = 10)
imp_xgb <- data.frame(Variable = colnames(X_shap), 
                      Importance = colMeans(abs(shap_xgb))) %>%
  arrange(desc(Importance))

cat("\n--- XGBoost Variable Importance (Mean |SHAP| Score) ---\n")
print(head(imp_xgb, 10))