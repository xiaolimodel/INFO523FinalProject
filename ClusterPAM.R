# R Code for Optimal k Determination, PAM Clustering, and Plotting
# -----------------------------------------------------------------------------

# 1. Install and load necessary packages (if not already installed)
# You may need to run this line in your R console first:
# install.packages(c("cluster", "factoextra", "ggplot2", "ggrepel"))

# LOAD ALL REQUIRED LIBRARIES
library(cluster)
library(factoextra) 
library(ggplot2)
library(ggrepel) 

# 2. Load and Prepare Data
data <- read.csv("TRPCompanies.csv")

# Select the relevant columns
clustering_data <- data[, c("Average.of.HiAtHome", "Average.of.HWTRTime")]

# Remove any rows with missing values and get indices of kept rows
rows_kept <- which(!is.na(clustering_data$Average.of.HiAtHome) & !is.na(clustering_data$Average.of.HWTRTime))
clustering_data <- na.omit(clustering_data)

# Scale the data (essential for clustering)
# The scaled_data object stores the mean (center) and SD (scale)
scaled_data <- scale(clustering_data)

# --------------------------------------------------------
# PART 1: Find the Optimal Number of Clusters (k) using Silhouette
# --------------------------------------------------------

# Set a seed for reproducibility
set.seed(42)

print("--- Step 1: Generating Silhouette Plot to Find Optimal k ---")

# Plot the silhouette width for k=2 to 10.
optimal_k_plot <- fviz_nbclust(
  scaled_data,
  pam,
  method = "silhouette",
  k.max = 10
) +
  labs(
    title = "Optimal k for PAM Clustering (Silhouette Method)",
    subtitle = "Highest average silhouette width is the optimal k"
  )

# Print/display the plot to find the optimal k
print(optimal_k_plot)

# ==============================================================================
# !!! STOP HERE !!!
# Inspect the plot printed above to determine the optimal k (usually 2 or 3).
# Then, update the OPTIMAL_K variable below with that value, and continue running the rest of the script.
# ==============================================================================

# --- USER ACTION REQUIRED: Replace the value below with your determined optimal k ---
# Example: If the optimal k is 3, set OPTIMAL_K <- 3
OPTIMAL_K <- 2 

# --------------------------------------------------------
# PART 2: Perform PAM Clustering and Plot Results
# --------------------------------------------------------

print(paste("--- Step 2: Performing PAM Clustering with k =", OPTIMAL_K, "---"))

# Perform PAM Clustering
set.seed(42) # for reproducibility
pam_result <- pam(scaled_data, k = OPTIMAL_K)

# Add cluster assignment to the original data frame (only for rows used in clustering)
data_clustered <- data[rows_kept, ]
data_clustered$Cluster <- factor(pam_result$clustering)

# Add the medoid (cluster center) to the clustered data for plotting aesthetics
medoids_scaled <- pam_result$medoids

# --------------------------------------------------------
# MANUAL DE-SCALING FIX: Reverses standardization without 'unscale' function
# --------------------------------------------------------

# Extract center (mean) and scale (SD) attributes from the scaled data
center_vals <- attr(scaled_data, "scaled:center")
scale_vals <- attr(scaled_data, "scaled:scale")

# Manually unscale the medoids using base R formula: Unscaled = (Scaled * SD) + Mean
medoids_unscaled <- medoids_scaled 
for (i in 1:ncol(medoids_scaled)) {
  medoids_unscaled[, i] <- (medoids_unscaled[, i] * scale_vals[i]) + center_vals[i]
}

# Convert back to data frame and set names
medoids_unscaled <- as.data.frame(medoids_unscaled)
colnames(medoids_unscaled) <- colnames(clustering_data)
medoids_unscaled$Cluster <- factor(1:OPTIMAL_K)

# --------------------------------------------------------
# 3. Identify and Format Points to Label
# --------------------------------------------------------

# Condition: Average of HiAtHome > 1.5 AND Average of HWTRTime > 30
data_to_label <- subset(
  data_clustered,
  Average.of.HiAtHome > 1.5 | Average.of.HWTRTime > 30
)

# Create the label text: Company name and NAICS2 (Use '\n' for a line break)
data_to_label$LabelText <- paste(
  data_to_label$Company, "\n(", data_to_label$Average.of.NAICS2, ")",
  sep = ""
)

# 4. Visualize the Clusters
cluster_plot <- ggplot(data_clustered, aes(x = Average.of.HiAtHome, y = Average.of.HWTRTime, color = Cluster)) +
  # Points for each company
  geom_point(size = 3, alpha = 0.7) +
  # Medoid points (cluster centers)
  geom_point(data = medoids_unscaled, aes(x = Average.of.HiAtHome, y = Average.of.HWTRTime),
             size = 6, shape = 4, stroke = 2, color = "black") + # 'x' shape for medoids
  # Label the specified points using ggrepel for better placement
  geom_text_repel(
    data = data_to_label,
    aes(label = LabelText),
    color = "black",
    size = 3,
    max.overlaps = 10,
    segment.color = 'grey50'
  ) +
  labs(
    title = paste("PAM Clustering of TRP Companies (k=", OPTIMAL_K, ")"),
    x = "Average of HiAtHome (Proportion of Trips at Home)",
    y = "Average of HWTRTime (Home-Work Travel Time in Minutes)",
    caption = "Medoids are marked with a black 'x'. Red dashed lines show the labeling thresholds."
  ) +
  theme_minimal() +
  # Add lines for the labeling thresholds (HiAtHome > 1.5 and HWTRTime > 30)
  geom_hline(yintercept = 30, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "red", alpha = 0.5)

# Print/display the cluster plot
print(cluster_plot)

# 5. Output Summary and CSV
print("Cluster Medoids (Cluster Centers in Original Scale):")
print(medoids_unscaled)

# Output the data with cluster assignments to a new CSV file
write.csv(data_clustered, "TRPCompanies_PAM_Clustered.csv", row.names = FALSE)
print("Data with cluster assignments saved to 'TRPCompanies_PAM_Clustered.csv'.")