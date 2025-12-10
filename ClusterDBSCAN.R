# R Code for DBSCAN Clustering, Parameter Finding, and Plotting
# -----------------------------------------------------------------------------

# 1. Install and load necessary packages (if not already installed)
# You may need to run this line in your R console first:
# install.packages(c("dbscan", "ggplot2", "ggrepel"))

# LOAD ALL REQUIRED LIBRARIES
library(dbscan) 
library(ggplot2)
library(ggrepel) 

# 2. Load and Prepare Data
data <- read.csv("TRPCompanies.csv")

# Select the relevant columns
clustering_data <- data[, c("Average.of.HiAtHome", "Average.of.HWTRTime")]

# Remove any rows with missing values and get indices of kept rows
rows_kept <- which(!is.na(clustering_data$Average.of.HiAtHome) & !is.na(clustering_data$Average.of.HWTRTime))
clustering_data <- na.omit(clustering_data)

# Scale the data (essential for distance-based clustering like DBSCAN)
scaled_data <- scale(clustering_data)

# --------------------------------------------------------
# PART 1: Find Optimal DBSCAN Parameters (eps)
# --------------------------------------------------------

# Set MinPts: A common heuristic for 2D data is MinPts = 2 * dimensions.
MIN_PTS <- 4 

print("--- Step 1: Generating k-Distance Plot to Find Optimal eps ---")

# Plot the distance of the 4th nearest neighbor for every point.
# The 'knee' of the curve suggests an optimal 'eps' value.
k_distance_plot <- kNNdistplot(scaled_data, k = MIN_PTS)

# Print/display the plot to find the optimal eps
# Look for the distance value on the y-axis where the curve sharply bends/knees.
print(k_distance_plot)

# ==============================================================================
# !!! STOP HERE !!!
# Inspect the plot printed above to determine the optimal epsilon (eps).
# The optimal eps is the distance (y-axis value) at the 'knee' or bend in the curve.
# Then, update the EPSILON variable below with that value, and continue running the rest of the script.
# ==============================================================================

# --- USER ACTION REQUIRED: Replace the value below with your determined optimal eps ---
# Example: If the knee is at 0.5, set EPSILON <- 0.5
EPSILON <- 0.5 

# --------------------------------------------------------
# PART 2: Perform DBSCAN Clustering and Plot Results
# --------------------------------------------------------

print(paste("--- Step 2: Performing DBSCAN Clustering with eps =", EPSILON, "and MinPts =", MIN_PTS, "---"))

# Perform DBSCAN Clustering
set.seed(42) # for reproducibility
dbscan_result <- dbscan(scaled_data, eps = EPSILON, minPts = MIN_PTS)

# Add cluster assignment to the original data frame (only for rows used in clustering)
data_clustered <- data[rows_kept, ]
data_clustered$Cluster <- factor(dbscan_result$cluster)
# Cluster 0 represents noise/outliers in DBSCAN

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
    title = paste("DBSCAN Clustering of TRP Companies (eps=", EPSILON, ", MinPts=", MIN_PTS, ")"),
    x = "Average of HiAtHome (Proportion of Trips at Home)",
    y = "Average of HWTRTime (Home-Work Travel Time in Minutes)",
    caption = "Cluster 0 points are classified as noise."
  ) +
  theme_minimal() +
  # Add lines for the labeling thresholds (HiAtHome > 1.5 and HWTRTime > 30)
  geom_hline(yintercept = 30, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_vline(xintercept = 1.5, linetype = "dashed", color = "red", alpha = 0.5) +
  # Use a color palette that clearly distinguishes clusters from noise (Cluster 0)
  scale_color_brewer(palette = "Set1")

# Print/display the cluster plot
print(cluster_plot)

# 5. Output Summary and CSV
print("Summary of Cluster Sizes (0 is Noise):")
print(table(data_clustered$Cluster))

# Output the data with cluster assignments to a new CSV file
write.csv(data_clustered, "TRPCompanies_DBSCAN_Clustered.csv", row.names = FALSE)
print("Data with cluster assignments saved to 'TRPCompanies_DBSCAN_Clustered.csv'.")