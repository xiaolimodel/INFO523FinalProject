# --------------------------------------------------
# TRP Company Data Clustering Analysis
# --------------------------------------------------

# load package
library(dplyr)
library(cluster)
library(ggplot2)

# data 
trp_df <- read.csv(
  "TRPCompanies.csv",
  stringsAsFactors = FALSE
)
# process column names
names(trp_df) <- gsub("^Average.of.", "", names(trp_df))

# extract numerical variables for clustering 
numeric_cols <- trp_df %>%
  select(-Company, -`NAICS2`) %>%
  select(where(is.numeric))

# remove the zero-variance column
zero_var <- apply(numeric_cols, 2, var) == 0
if (any(zero_var)) {
  numeric_cols <- numeric_cols[, !zero_var]
  cat("removed ", sum(zero_var), "zero variance columns. \n")
}

# multi-element Outlier Detection: Mahalanobis Distance
mahal_dist <- mahalanobis(
  x = numeric_cols,
  center = colMeans(numeric_cols),
  cov = cov(numeric_cols)
)
p_val <- pchisq(mahal_dist, df = ncol(numeric_cols), lower.tail = FALSE)

# anomaly index 
outlier_idx <- which(p_val < 0.001)

# If there are no multivariate exceptions, fall back to simple rules
if (length(outlier_idx) == 0) {
  outlier_idx <- which(trp_df$HWDist < 1 & trp_df$HWTRTime < 1)
}

cat("detected ", length(outlier_idx), "outliers. \n")
if (length(outlier_idx) > 0) {
  print(trp_df[outlier_idx, c("Company", "HWDist", "HWTRTime", "NumVehicles")])
}

# clean dataset 
clean_numeric <- numeric_cols[-outlier_idx, , drop = FALSE]
clean_trp_df <- trp_df[-outlier_idx, ]  # 用于后续合并标签

# optimal contour coefficient K
k_range <- 2:8
sil_scores <- sapply(k_range, function(k) {
  pam(clean_numeric, k = k, stand = TRUE)$silinfo$avg.width
})

# plot
plot(k_range, sil_scores, type = "b",
     xlab = "Number of clusters K", ylab = "Average contour coefficient",
     main = "PAM contour coefficient selection K")

# optimal_k
optimal_k <- k_range[which.max(sil_scores)]
cat("✅ optimal K =", optimal_k, "\n")

# fit the final PAM model
final_pam <- pam(clean_numeric, k = optimal_k, stand = TRUE)

# add the clustering labels to the clean data frame
clean_trp_df$pam_cluster <- final_pam$clustering

# group Summary
summary_pam <- clean_trp_df %>%
  group_by(pam_cluster) %>%
  summarise(
    `The number of companies` = n(),
    `Average commuting distance` = round(mean(HWDist, na.rm = TRUE), 2),
    `Average commuting time` = round(mean(HWTRTime, na.rm = TRUE), 2),
    `The average number of vehicles` = round(mean(NumVehicles, na.rm = TRUE), 2),
    .groups = "drop"
  )

# result
print("===== summary of PAM clustering results  =====")
print(summary_pam)

clean_numeric_scaled <- scale(clean_numeric)
pca_result <- prcomp(clean_numeric_scaled, center = TRUE, scale. = TRUE)


pca_df <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Cluster = as.factor(clean_trp_df$pam_cluster),
  Company = clean_trp_df$Company
)


cluster_centers <- pca_df %>%
  group_by(Cluster) %>%
  summarise(
    PC1_center = mean(PC1),
    PC2_center = mean(PC2),
    Cluster_Size = n(),
    .groups = "drop"
  )


pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster, shape = Cluster)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_point(data = cluster_centers, 
             aes(x = PC1_center, y = PC2_center, color = Cluster),
             size = 8, shape = 18, alpha = 0.8) +
  geom_text(data = cluster_centers,
            aes(x = PC1_center, y = PC2_center, 
                label = paste0("C", Cluster, "\n(n=", Cluster_Size, ")")),
            color = "black", size = 3.5, fontface = "bold") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    title = "PAM result",
    subtitle = paste0("K = ", optimal_k),
    x = paste0("PC1 (", round(summary(pca_result)$importance[2,1] * 100, 1), "%)"),
    y = paste0("PC2 (", round(summary(pca_result)$importance[2,2] * 100, 1), "%)")
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right"
  )

print(pca_plot)