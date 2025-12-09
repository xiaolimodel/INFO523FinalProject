# --- Required Library ---
library(arules)
library(ggplot2)
library(dplyr)

# --- Define Parameters (Final Settings) ---
MIN_SUPPORT <- 0.6    # 60% support
MIN_CONFIDENCE <- 0.8 # 80% confidence
MIN_RULE_LENGTH <- 2  # Excludes empty LHS: forces predictive rules

# --- 1. Load and Transform the Data ---
data <- read.csv("WorkActivity.csv")

activity_counts <- data %>%
  count(WorkActivity, sort = TRUE, name = "Count")

# --- 3. Create the Bar Chart using ggplot2 ---
ggplot(activity_counts, aes(x = reorder(WorkActivity, -Count), y = Count)) +
  # Create the bars
  geom_col(fill = "skyblue", color = "darkblue") +
  
  # Add labels and title
  labs(title = "Count Distribution of Each Work Activity",
       x = "Work Activity",
       y = "Count") +
  
  # Rotate x-axis text for readability
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, size = 16)) +
  
  # Ensure the entire plot fits well
  coord_flip() # Optional: Flips the coordinates for horizontal bars, improving label readability

# --- 2. Filter out "Other" (New Step) ---
# We remove any rows where the WorkActivity is exactly "Other"
cat("Rows before removing 'Other':", nrow(data), "\n")
data <- subset(data, WorkActivity != "Other")
cat("Rows after removing 'Other':", nrow(data), "\n")

# --- 3. Remove Duplicates (Deduplication Step) ---
# We ensure that if a company has "Retail" 50 times, it counts as 1.
cat("Rows before deduplication:", nrow(data), "\n")
data_unique <- unique(data[, c("CompanyName", "WorkActivity")])
cat("Rows after deduplication (Final Dataset):", nrow(data_unique), "\n")

# --- 4. Transform to Transaction Data ---
cat("Converting unique data to transaction format...\n")
transaction_list <- split(x = data_unique$WorkActivity, f = data_unique$CompanyName)
transactions <- as(transaction_list, "transactions")

# Optional: Inspect transaction summary
# summary(transactions)

# --- 5. Run Apriori Algorithm ---
cat("\n--- Running Apriori Algorithm (No 'Other', Unique Data) ---\n")
rules <- apriori(transactions, 
                 parameter = list(supp = MIN_SUPPORT, 
                                  conf = MIN_CONFIDENCE,
                                  minlen = MIN_RULE_LENGTH,
                                  target = "rules"))

# --- 6. Robust Sorting by Lift ---
cat("\n--- Sorting Rules by Lift (Highest to Lowest) ---\n")

# Sort using the order() function on the quality slot
sort_order <- order(quality(rules)$lift, decreasing = TRUE)
rules_sorted <- rules[sort_order]

# --- 7. Inspect the Results ---
cat("Top 6 Rules sorted by Lift:\n")
inspect(head(rules_sorted, 6))

# install.packages("arulesViz") # Run this line once if you haven't installed the package
library(arulesViz)

# ------------------------------------------------------------------
# --- 2. Visualization Steps ---

# Step 2a: Default Scatter Plot
# This plot shows Confidence vs. Support, with Lift encoded by color.
# It's the most common and useful visualization for identifying strong rules.
cat("\nGenerating Scatter Plot...\n")
plot(rules_apriori, 
     main = "Apriori Association Rules: Confidence vs. Support (Lift Color)", 
     engine = "htmlwidget") # Use htmlwidget engine for interactive viewing

# Step 2b: Grouped Matrix Plot
# This plot groups rules by similar items and is useful for seeing item patterns.
cat("\nGenerating Grouped Matrix Plot...\n")
plot(rules_apriori, 
     method = "grouped", 
     main = "Grouped Matrix Plot of Association Rules",
     engine = "htmlwidget")

# Step 2c: Network Graph Plot (Best for small rule sets)
# The graph shows items as nodes and rules as arrows. 
# We restrict the number of rules to 10 for readability, as graphs get cluttered easily.
cat("\nGenerating Network Graph Plot (Top 10 Rules)...\n")
plot(rules_apriori, 
     method = "graph", 
     limit = 10, # Limits plot to the first 10 rules
     control = list(type = "items"),
     main = "Network Graph of Top 10 Association Rules")
