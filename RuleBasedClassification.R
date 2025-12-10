# Load libraries
if(!require(arules)) install.packages("arules")
if(!require(rCBA)) install.packages("rCBA")
if(!require(caret)) install.packages("caret")
if(!require(tidyverse)) install.packages("tidyverse")

library(arules)
library(rCBA)
library(caret)
library(tidyverse)

# 1. Load and Prepare Data
df <- read.csv("trp2024income.csv")

# clean ACSIncome: Remove non-numeric characters if any, and convert to numeric
df$ACSIncome <- as.numeric(as.character(df$ACSIncome))

# Impute ACSIncome: Fill with 67929 if NA or 0
df$ACSIncome[is.na(df$ACSIncome) | df$ACSIncome == 0] <- 67929

# 2. Preprocessing
# Filter valid rows and create target variable
df_clean <- df %>%
  filter(!is.na(ACSIncome) & !is.na(HWTRTime)) %>%
  mutate(
    WorkFromHome = factor(ifelse(HiAtHome > 0, "True", "False")),
    NAICS2 = as.factor(NAICS2)
  ) %>%
  select(NAICS2, ACSIncome, HWTRTime, WorkFromHome)

# 3. Discretization
# rCBA requires all columns to be categorical (factors)
df_clean$ACSIncome_Bin <- discretize(df_clean$ACSIncome, method = "frequency", categories = 4)
df_clean$HWTRTime_Bin <- discretize(df_clean$HWTRTime, method = "frequency", categories = 4)

# Remove original numeric columns for the final dataset
data_model <- df_clean %>% select(-ACSIncome, -HWTRTime)

data_model <- sapply(data_model,as.factor)
data_model <- data.frame(data_model, check.names=FALSE)

# 4. Split Data (60% Train, 40% Test)
train_rows = sample(1:nrow(data_model), 0.6*nrow(data_model))
train <- data_model[train_rows,]
test <- data_model[-train_rows,]

# --- PART 2: FP-Growth Classification ---

# NOTE: rCBA::fpgrowth requires a data.frame, not a 'transactions' object.
# It also mines ALL rules first, so we filter for 'consequent' manually below.

# 1. Mine Rules
print("Mining rules with FP-Growth...")
train_dataf <- sapply(train_data,as.factor)
train_dataf <- data.frame(train_dataf, check.names=FALSE)

txns <- as(train,"transactions")

rules = rCBA::fpgrowth(txns, support=0.01, confidence=0.03, maxLength=2, consequent="WorkFromHome",
                       parallel=FALSE)
sort_order <- order(quality(rules)$lift, decreasing = TRUE)
rules_sorted <- rules[sort_order]
# --- 7. Inspect the Results ---
cat("Top 6 Rules sorted by Lift:\n")
inspect(head(rules, 6))

predictions <- rCBA::classification(test,rules)
table(predictions)
sum(as.character(test$WorkFromHome)==as.character(predictions),na.rm=TRUE)/length(predictions)

prunedRules <- rCBA::pruning(train, rules, method="m2cba", parallel=FALSE)
predictions <- rCBA::classification(test, prunedRules)
table(predictions)
sum(as.character(test$WorkFromHome)==as.character(predictions),na.rm=TRUE)/length(predictions)

inspect(prunedRules)
