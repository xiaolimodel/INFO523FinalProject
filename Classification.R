# Load necessary libraries
# -------------------------------------------------------------------------
# Check for missing packages and install them automatically
# Added 'pROC' for ROC/AUC calculations
required_packages <- c("caret", "e1071", "rpart", "randomForest", "xgboost", "dplyr", "fastshap", "ggplot2", "pROC")
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
library(ggplot2)
library(pROC)
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

# Container for results (Added AUC)
results <- data.frame(
  Model = character(), 
  Accuracy = numeric(), 
  Sensitivity = numeric(), 
  Specificity = numeric(), 
  Precision = numeric(), 
  F1_Score = numeric(),
  AUC = numeric(), # Added AUC
  stringsAsFactors = FALSE
)

# Storage for ROC plotting data
all_roc_data <- data.frame()

# Helper function to extract metrics and add to results
# Now includes AUC and ROC processing
add_results <- function(model_name, cm, current_results, roc_obj) {
  # Add metrics to results table
  new_row <- data.frame(
    Model = model_name,
    Accuracy = as.numeric(cm$overall['Accuracy']),
    Sensitivity = as.numeric(cm$byClass['Sensitivity']),
    Specificity = as.numeric(cm$byClass['Specificity']),
    Precision = as.numeric(cm$byClass['Pos Pred Value']),
    F1_Score = as.numeric(cm$byClass['F1']),
    AUC = as.numeric(auc(roc_obj)),
    stringsAsFactors = FALSE
  )
  
  # Return updated results
  return(rbind(current_results, new_row))
}

# Helper to add ROC plotting data
add_roc_plot_data <- function(model_name, roc_obj, current_plot_data) {
  roc_df <- data.frame(
    Sensitivity = roc_obj$sensitivities,
    Specificity = roc_obj$specificities,
    Model = model_name
  )
  return(rbind(current_plot_data, roc_df))
}

# ==========================================
# 2. Probit Regression
# ==========================================
cat("\n--- Probit Regression (Test Set) ---\n")
probit_model <- glm(Target ~ ., data = train_data, family = binomial(link = "probit"))
prob_probit <- predict(probit_model, newdata = test_data, type = "response")
pred_probit <- apply_threshold(prob_probit, prop_positive)

# Compute ROC/AUC
roc_probit <- roc(test_data$Target, prob_probit, levels = c("0", "1"), direction = "<")
cm_probit <- confusionMatrix(pred_probit, test_data$Target, positive = "1")
print(cm_probit) 

results <- add_results("Probit", cm_probit, results, roc_probit)
all_roc_data <- add_roc_plot_data("Probit", roc_probit, all_roc_data)

# ==========================================
# 2.5 Naive Bayes
# ==========================================
cat("\n--- Naive Bayes (Test Set) ---\n")
nb_model <- naiveBayes(Target ~ ., data = train_data)
# type="raw" returns probabilities for each class
prob_nb <- predict(nb_model, newdata = test_data, type = "raw")[, "1"]
pred_nb <- apply_threshold(prob_nb, prop_positive)

roc_nb <- roc(test_data$Target, prob_nb, levels = c("0", "1"), direction = "<")
cm_nb <- confusionMatrix(pred_nb, test_data$Target, positive = "1")
print(cm_nb)

results <- add_results("Naive Bayes", cm_nb, results, roc_nb)
all_roc_data <- add_roc_plot_data("Naive Bayes", roc_nb, all_roc_data)

# ==========================================
# 3. Support Vector Machine (SVM)
# ==========================================
cat("\n--- Support Vector Machine (Test Set) ---\n")
svm_model <- svm(Target ~ ., data = train_data, probability = TRUE)
svm_pred_obj <- predict(svm_model, newdata = test_data, probability = TRUE)
prob_svm <- attr(svm_pred_obj, "probabilities")[, "1"]
pred_svm <- apply_threshold(prob_svm, prop_positive)

roc_svm <- roc(test_data$Target, prob_svm, levels = c("0", "1"), direction = "<")
cm_svm <- confusionMatrix(pred_svm, test_data$Target, positive = "1")
print(cm_svm)

results <- add_results("SVM", cm_svm, results, roc_svm)
all_roc_data <- add_roc_plot_data("SVM", roc_svm, all_roc_data)

# ==========================================
# 4. Decision Tree
# ==========================================
cat("\n--- Decision Tree (Test Set) ---\n")
tree_model <- rpart(Target ~ ., data = train_data, method = "class")
prob_tree <- predict(tree_model, newdata = test_data, type = "prob")[, 2]
pred_tree <- apply_threshold(prob_tree, prop_positive)

roc_tree <- roc(test_data$Target, prob_tree, levels = c("0", "1"), direction = "<")
cm_tree <- confusionMatrix(pred_tree, test_data$Target, positive = "1")
print(cm_tree)

results <- add_results("Decision Tree", cm_tree, results, roc_tree)
all_roc_data <- add_roc_plot_data("Decision Tree", roc_tree, all_roc_data)

# ==========================================
# 5. Random Forest
# ==========================================
cat("\n--- Random Forest (Test Set) ---\n")
rf_model <- randomForest(Target ~ ., data = train_data, ntree = 100)
prob_rf <- predict(rf_model, newdata = test_data, type = "prob")[, 2]
pred_rf <- apply_threshold(prob_rf, prop_positive)

roc_rf <- roc(test_data$Target, prob_rf, levels = c("0", "1"), direction = "<")
cm_rf <- confusionMatrix(pred_rf, test_data$Target, positive = "1")
print(cm_rf)

results <- add_results("Random Forest", cm_rf, results, roc_rf)
all_roc_data <- add_roc_plot_data("Random Forest", roc_rf, all_roc_data)

# ==========================================
# 6. XGBoost
# ==========================================
cat("\n--- XGBoost (Test Set) ---\n")
# Prepare matrices
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

roc_xgb <- roc(test_data$Target, prob_xgb, levels = c("0", "1"), direction = "<")
cm_xgb <- confusionMatrix(pred_xgb, as.factor(test_y), positive = "1")
print(cm_xgb)

results <- add_results("XGBoost", cm_xgb, results, roc_xgb)
all_roc_data <- add_roc_plot_data("XGBoost", roc_xgb, all_roc_data)

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
# 7.5 ROC Curve Plot
# ==========================================
cat("\n=================================================================================\n")
cat("Generating Combined ROC Curve Plot\n")
cat("=================================================================================\n")

p_roc <- ggplot(all_roc_data, aes(x = 1 - Specificity, y = Sensitivity, color = Model)) +
  geom_path(linewidth = 0.8) +
  geom_abline(linetype = "dashed", color = "gray") +
  coord_fixed() +
  labs(title = "ROC Curves for All Classification Models",
       x = "1 - Specificity (False Positive Rate)",
       y = "Sensitivity (True Positive Rate)") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p_roc)

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

# --- 2. Naive Bayes SHAP ---
pfun_nb <- function(object, newdata) {
  predict(object, newdata = newdata, type = "raw")[, "1"]
}
shap_nb <- explain(nb_model, X = X_shap, pred_wrapper = pfun_nb, nsim = 10)
imp_nb <- data.frame(Variable = colnames(X_shap), 
                     Importance = colMeans(abs(shap_nb))) %>%
  arrange(desc(Importance))

cat("\n--- Naive Bayes Variable Importance (Mean |SHAP| Score) ---\n")
print(head(imp_nb, 10))

# --- 3. SVM SHAP ---
pfun_svm <- function(object, newdata) {
  attr(predict(object, newdata = newdata, probability = TRUE), "probabilities")[, "1"]
}
shap_svm <- explain(svm_model, X = X_shap, pred_wrapper = pfun_svm, nsim = 10)
imp_svm <- data.frame(Variable = colnames(X_shap), 
                      Importance = colMeans(abs(shap_svm))) %>%
  arrange(desc(Importance))

cat("\n--- SVM Variable Importance (Mean |SHAP| Score) ---\n")
print(head(imp_svm, 10))

# --- 4. Decision Tree SHAP ---
pfun_tree <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")[, 2]
}
shap_tree <- explain(tree_model, X = X_shap, pred_wrapper = pfun_tree, nsim = 10)
imp_tree <- data.frame(Variable = colnames(X_shap), 
                       Importance = colMeans(abs(shap_tree))) %>%
  arrange(desc(Importance))

cat("\n--- Decision Tree Variable Importance (Mean |SHAP| Score) ---\n")
print(head(imp_tree, 10))

# --- 5. Random Forest SHAP ---
pfun_rf <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")[, 2]
}
shap_rf <- explain(rf_model, X = X_shap, pred_wrapper = pfun_rf, nsim = 10)
imp_rf <- data.frame(Variable = colnames(X_shap), 
                     Importance = colMeans(abs(shap_rf))) %>%
  arrange(desc(Importance))

cat("\n--- Random Forest Variable Importance (Mean |SHAP| Score) ---\n")
print(head(imp_rf, 10))

# --- 6. XGBoost SHAP ---
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