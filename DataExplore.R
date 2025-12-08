library(ggplot2)

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
data$NAICS2 <- as.factor(data$NAICS2)

# Remove rows with NAs (before selecting columns to ensure row consistency)
#data <- na.omit(data)

# ==========================================
# 1.1 Exploratory Data Analysis (EDA)
# ==========================================
cat("\n=================================================================================\n")
cat("1.1 Exploratory Data Analysis (EDA)\n")
cat("=================================================================================\n")

# 1. Dataset Overview
cat("\n--- Dataset Dimensions ---\n")
print(dim(data))

# 2. Travel Mode Summary Statistics (Replaces Box Plots)
cat("\n--- Calculating Summary Statistics for Travel Modes ---\n")

travel_cols <- c("HiDrive", "HiCpool", "HiRideHail", "HiBike", "HiWalk", "HiBus", "HiCatTran", "HiStreetCar", "HiAtHome")
existing_cols <- intersect(travel_cols, names(data))

if(length(existing_cols) > 0) {
  # Calculate statistics using sapply
  stats_matrix <- sapply(data[, existing_cols], function(x) {
    c(Mean = mean(x, na.rm = TRUE),
      Median = median(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE))
  })
  
  # Transpose to have variables as rows and statistics as columns
  stats_df <- as.data.frame(t(stats_matrix))
  
  # Round for readability
  stats_df <- round(stats_df, 2)
  
  print(stats_df)
} else {
  cat("Warning: None of the requested travel mode columns were found in the dataset.\n")
}


# 2. Target Variable Distribution
cat("\n--- Target Distribution (HiAtHome > 0) ---\n")
target_counts <- table(data$Target)
print(target_counts)
cat(sprintf("Percentage Positive: %.2f%%\n", prop.table(target_counts)[2] * 100))

# 2. Selected Variable Distributions
cat("\n--- Analyzing Distributions of Selected Variables ---\n")

selected_eda_vars <- c("NumVehicles", "NumDrivers", "NumFullTimeWorkers", 
                       "NumPartTimeWorkers", "PickupOrDropoffUnder18", 
                       "PickupOrDropoff18Plus", "NAICS2", "HWTRTime", 
                       "HWDist", "ACSIncome")

# Filter to those present in the dataset
existing_eda_cols <- intersect(selected_eda_vars, names(data))

# Separate Numeric and Factor variables for appropriate analysis
numeric_eda_vars <- existing_eda_cols[sapply(data[, existing_eda_cols], is.numeric)]
factor_eda_vars <- existing_eda_cols[sapply(data[, existing_eda_cols], is.factor)]

# A. Numeric Summary
if(length(numeric_eda_vars) > 0) {
  cat("\n[Numeric Variables] Summary Statistics:\n")
  stats_matrix <- sapply(data[, numeric_eda_vars], function(x) {
    c(Mean = mean(x, na.rm = TRUE),
      Median = median(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE))
  })
  
  # Transpose for readability
  stats_df <- as.data.frame(t(stats_matrix))
  stats_df <- round(stats_df, 2)
  print(stats_df)
}

# B. Categorical Frequency
if(length(factor_eda_vars) > 0) {
  cat("\n[Categorical Variables] Frequency Distributions:\n")
  for(v in factor_eda_vars) {
    cat(paste0("\n--- Distribution for: ", v, " ---\n"))
    print(table(data[[v]]))
  }
}
# B. Categorical Frequency (Special handling for NAICS2)
if("NAICS2" %in% factor_eda_vars) {
  cat("\n--- NAICS2 Sector Distribution & HiAtHome Analysis ---\n")
  
  # Create a mapping dataframe for NAICS 2-digit codes
  naics_map <- data.frame(
    Code = c("11", "21", "22", "23", "31", "32", "33", "42", "44", "45", 
             "48", "49", "51", "52", "53", "54", "55", "56", "61", "62", 
             "71", "72", "81", "92"),
    Sector = c("Agriculture/Forestry/Fishing", "Mining/Oil & Gas", "Utilities", "Construction", 
               "Manufacturing", "Manufacturing", "Manufacturing", "Wholesale Trade", 
               "Retail Trade", "Retail Trade", "Transportation/Warehousing", "Transportation/Warehousing", 
               "Information", "Finance & Insurance", "Real Estate/Rental", 
               "Professional/Scientific/Tech", "Management of Companies", 
               "Admin/Support/Waste Mgmt", "Educational Services", "Health Care/Social Assistance", 
               "Arts/Entertainment/Recreation", "Accommodation/Food Services", 
               "Other Services", "Public Administration")
  )
  
  # Ensure strict character type for joining
  data$NAICS2_Char <- as.character(data$NAICS2)
  
  # Join data with mapping
  data_naics <- left_join(data, naics_map, by = c("NAICS2_Char" = "Code"))
  
  # Fill NA sectors with "Unknown" or original code if map missing
  data_naics$Sector <- ifelse(is.na(data_naics$Sector), paste("Code", data_naics$NAICS2_Char), data_naics$Sector)
  
  # Summarize Count and Percentage of Positive Target by Sector
  sector_summary <- data_naics %>%
    group_by(Sector) %>%
    summarise(
      Count = n(),
      Pct_HiAtHome = mean(Target == "1") * 100
    ) %>%
    arrange(desc(Pct_HiAtHome)) # Sort by highest Work from Home percentage
  
  # Print the detailed table
  cat("\n[Table] NAICS2 Sectors: Count and % HiAtHome > 0\n")
  print(as.data.frame(sector_summary))
  
  # Plot 1: Distribution of Industries (Count)
  p_naics_count <- ggplot(sector_summary, aes(x = reorder(Sector, Count), y = Count)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() + 
    labs(title = "Distribution of Industries (NAICS Sectors)",
         x = "Sector",
         y = "Frequency (Count)") +
    theme_minimal()
  print(p_naics_count)
  
  # Plot 2: Percentage of HiAtHome > 0 by Industry
  p_naics_pct <- ggplot(sector_summary, aes(x = reorder(Sector, Pct_HiAtHome), y = Pct_HiAtHome)) +
    geom_bar(stat = "identity", fill = "darkorange") +
    coord_flip() + 
    labs(title = "Telecommute % by NAICS Sector",
         x = "Sector",
         y = "Percentage (%)") +
    theme_minimal()
  print(p_naics_pct)
}
# 3. Target Variable Distribution
cat("\n--- Target Distribution (HiAtHome > 0) ---\n")
target_counts <- table(data$Target)
print(target_counts)
cat(sprintf("Percentage Positive: %.2f%%\n", prop.table(target_counts)[2] * 100))

# 4. Numeric Correlations
# Select only numeric columns for correlation (exclude KeyID if present)
numeric_vars <- data %>% select(-one_of("KeyID")) %>% select_if(is.numeric)
cat("\n--- Correlation Matrix (Top correlations with ACSIncome) ---\n")
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Show what correlates most with Income
if("ACSIncome" %in% colnames(cor_matrix)) {
  income_cor <- sort(cor_matrix[,"ACSIncome"], decreasing = TRUE)
  print(head(income_cor, 10))
}

# 5. Visualizations (Generated as R plots)
# Plot 1: Boxplot of Income vs Target
p1 <- ggplot(data, aes(x = Target, y = ACSIncome, fill = Target)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "ACSIncome Distribution by Target (HiAtHome > 0)",
       x = "HiAtHome > 0 (0=No, 1=Yes)",
       y = "ACS Income") +
  theme_minimal()
print(p1)

# Plot 2: Correlation Plot
if(ncol(numeric_vars) > 1) {
  corrplot(cor_matrix, method = "circle", type = "upper", 
           tl.cex = 0.6, title = "Correlation of Numeric Variables", mar=c(0,0,1,0))
}

cat("\n(EDA complete. Proceeding to variable selection and modeling...)\n")

# 3. Numeric Correlations
# Select only numeric columns for correlation (exclude KeyID if present)
numeric_vars <- data %>% select(-one_of("KeyID")) %>% select_if(is.numeric)
cat("\n--- Correlation Matrix (Top correlations with ACSIncome) ---\n")
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Show what correlates most with Income
if("ACSIncome" %in% colnames(cor_matrix)) {
  income_cor <- sort(cor_matrix[,"ACSIncome"], decreasing = TRUE)
  print(head(income_cor, 10))
}
# C. Scatter Plots (HiAtHome vs Numeric Variables)
cat("\n--- Scatter Plots: HiAtHome vs Selected Variables ---\n")
scatter_vars <- c("NumVehicles", "NumDrivers", "NumFullTimeWorkers", 
                  "NumPartTimeWorkers", "PickupOrDropoffUnder18", 
                  "PickupOrDropoff18Plus", "HWTRTime", 
                  "HWDist", "ACSIncome")

# Filter to available variables
valid_scatter_vars <- intersect(scatter_vars, names(data))

if(length(valid_scatter_vars) > 0) {
  for(v in valid_scatter_vars) {
    p_scatter <- ggplot(data, aes(x = .data[[v]], y = HiAtHome)) +
      geom_point(alpha = 0.4, color = "purple") +
      geom_smooth(method = "lm", color = "black", se = FALSE) + # Add trend line
      labs(title = paste("Scatter Plot: HiAtHome vs", v),
           x = v, 
           y = "HiAtHome") +
      theme_minimal()
    print(p_scatter)
  }
} else {
  cat("Warning: No valid variables found for scatter plots.\n")
}

# Print other factors if any
other_factors <- setdiff(factor_eda_vars, "NAICS2")
if(length(other_factors) > 0) {
  cat("\n[Other Categorical Variables] Frequency Distributions:\n")
  for(v in other_factors) {
    cat(paste0("\n--- Distribution for: ", v, " ---\n"))
    print(table(data[[v]]))
  }
}
# 4. Visualizations (Generated as R plots)
# Plot 1: Boxplot of Income vs Target
p1 <- ggplot(data, aes(x = Target, y = ACSIncome, fill = Target)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "ACSIncome Distribution by Target (Telecommute)",
       x = "Telecommute (0=No, 1=Yes)",
       y = "ACS Income") +
  theme_minimal()
print(p1)

boxplot_vars <- c("NumVehicles", "NumDrivers", "NumFullTimeWorkers", 
                  "NumPartTimeWorkers", "PickupOrDropoffUnder18", 
                  "PickupOrDropoff18Plus", "HWTRTime", "HWDist")

valid_boxplot_vars <- intersect(boxplot_vars, names(data))

if(length(valid_boxplot_vars) > 0) {
  cat("\n--- Generating Additional Boxplots by Target ---\n")
  for(v in valid_boxplot_vars) {
    p_box <- ggplot(data, aes(x = Target, y = .data[[v]], fill = Target)) +
      geom_boxplot() +
      labs(title = paste(v, "Distribution by Target (Telecommute)"),
           x = "Telecommute (0=No, 1=Yes)",
           y = v) +
      theme_minimal()
    print(p_box)
  }
}

# Plot 2: Correlation Plot
if(ncol(numeric_vars) > 1) {
  corrplot(cor_matrix, method = "circle", type = "upper", 
           tl.cex = 0.6, title = "Correlation of Numeric Variables", mar=c(0,0,1,0))
}

# --- Calculate Average Values for Telecommute vs Non-Telecommute ---
cat("\n=================================================================================\n")
cat("Average Values: Telecommute (HiAtHome > 0) vs Non-Telecommute (HiAtHome = 0)\n")
cat("=================================================================================\n")

avg_vars <- c("NumVehicles", "NumDrivers", "NumFullTimeWorkers", 
              "NumPartTimeWorkers", "PickupOrDropoffUnder18", 
              "PickupOrDropoff18Plus", "HWTRTime", "HWDist", "ACSIncome")

# Filter to available variables
valid_avg_vars <- intersect(avg_vars, names(data))

if(length(valid_avg_vars) > 0) {
  # Calculate means
  avg_summary <- data %>%
    group_by(Target) %>%
    summarise(across(all_of(valid_avg_vars), mean, na.rm = TRUE))
  
  # Define column names based on Target
  target_names <- ifelse(avg_summary$Target == "1", "Telecommute (1)", "Non-Telecommute (0)")
  
  # Transpose
  avg_summary_t <- t(avg_summary[, -1]) # Exclude Target column
  colnames(avg_summary_t) <- target_names
  
  print(as.data.frame(avg_summary_t))
} else {
  cat("Warning: None of the requested variables for averaging were found.\n")
}


# Plot 2: Correlation Plot
cat("\n--- Correlation Matrix (Selected Variables) ---\n")
corr_vars_list <- c("NumVehicles", "NumDrivers", "NumFullTimeWorkers", 
                    "NumPartTimeWorkers", "PickupOrDropoffUnder18", 
                    "PickupOrDropoff18Plus", "HWTRTime", 
                    "HWDist", "ACSIncome", "HiAtHome")

# Ensure variables exist in the dataset
existing_corr_vars <- intersect(corr_vars_list, names(data))

numeric_vars <- data %>% select(all_of(existing_corr_vars))
cor_matrix <- cor(numeric_vars, use = "complete.obs")
# Plot 2: Correlation Plot (Replaced corrplot with ggplot2 to fix missing package error)
if(ncol(numeric_vars) > 1) {
  # Convert correlation matrix to long format for ggplot
  cor_melt <- as.data.frame(as.table(cor_matrix))
  names(cor_melt) <- c("Var1", "Var2", "Correlation")
  
  p_corr <- ggplot(cor_melt, aes(Var1, Var2, fill = Correlation)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 10, hjust = 1)) +
    coord_fixed() +
    labs(title = "Correlation of Numeric Variables")
  print(p_corr)
}

cat("\n(EDA complete. Proceeding to variable selection and modeling...)\n")