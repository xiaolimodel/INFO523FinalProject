# Clustering Practice: Traditional Methods for University Datasets (PAM, K-means, Hierarchical Clustering)

# load package  
library(dplyr)
library(ISLR)
library(cluster)
library(Rtsne)
library(ggplot2)

set.seed(1234)  # random seed

# data cleaning and feature engineering
raw_college <- College
clean_uni <- raw_college %>%
  mutate(
    uni_name = rownames(.),
    accept_pct = Accept / Apps,
    elite_flag = cut(Top10perc, breaks = c(0, 50, 100), labels = c("Not Elite", "Elite"))
  ) %>%
  mutate(elite_flag = factor(elite_flag)) %>%
  select(uni_name, accept_pct, Outstate, Enroll, Grad.Rate, Private, elite_flag)

# calculate the Gower distance of mixed type data
dist_gower <- daisy(clean_uni[, -1], metric = "gower", type = list(logratio = 3))
dist_matrix <- as.matrix(dist_gower)

# PAM clustering, divided around the central point
# select the best K using the contour coefficient
k_range <- 2:10
sil_scores <- numeric(length(k_range))

for (idx in seq_along(k_range)) {
  k_val <- k_range[idx]
  pam_fit <- pam(dist_matrix, diss = TRUE, k = k_val)
  sil_scores[idx] <- pam_fit$silinfo$avg.width
}

# draw the contour coefficient diagram to find the peak value
plot(k_range, sil_scores, type = "b",
     xlab = "The number K of clusters", ylab = "Average contour coefficient",
     main = "The PAM profile coefficient is selected as K")

# select K=3
final_pam <- pam(dist_gower, diss = TRUE, k = 3)
clean_uni$pam_group <- final_pam$clustering

# view the statistical summaries of each cluster
summary_by_pam <- clean_uni %>%
  select(-uni_name) %>%
  group_by(pam_group) %>%
  summarise_all(~ mean(., na.rm = TRUE), .groups = "drop")

print("Summary of the mean values of each cluster of PAM:")
print(summary_by_pam)

# t-SNE visualization
tsne_out <- Rtsne(dist_gower, is_distance = TRUE)
tsne_df <- data.frame(
  tsne_x = tsne_out$Y[, 1],
  tsne_y = tsne_out$Y[, 2],
  cluster_id = factor(clean_uni$pam_group),
  name = clean_uni$uni_name
)

ggplot(tsne_df, aes(x = tsne_x, y = tsne_y, color = cluster_id)) +
  geom_point() + ggtitle("PAM clustering results (t-SNE dimensionality reduction)")

# K-means cluster
# prepare pure numerical data
num_uni <- College %>%
  mutate(name = rownames(.), acc_rate = Accept / Apps) %>%
  select(name, acc_rate, Outstate, Enroll, Grad.Rate, Private, Top10perc)

num_uni$Private <- ifelse(num_uni$Private == "Yes", 1, 0)  # Yes/No -> 1/0

# standardization（Z-score）
scaled_data <- scale(num_uni[, -1])

# K-means (K=3)
kmeans_res <- kmeans(scaled_data, centers = 3, nstart = 25)
num_uni$kmeans_label <- kmeans_res$cluster

# Hierarchical clustering
hier_model <- hclust(dist_gower, method = "average")
hier_3_groups <- cutree(hier_model, k = 3)
clean_uni$hier_label <- hier_3_groups

# result comparison
comp_table <- table(PAM = clean_uni$pam_group, Hierarchical = clean_uni$hier_label)
print("PAM vs Hierarchical Clustering consistency table:")
print(comp_table)

# Density-based Clustering: DBSCAN and OPTICS Demonstration

# load package
library(dbscan)  
library(dplyr)
library(ISLR)
library(cluster)

# demonstrate the principle of the algorithm with simulated data
set.seed(2)
n_pts <- 400
# generate four slightly overlapping two-dimensional Gaussian clusters
fake_data <- cbind(
  x_coord = runif(4, 0, 1) + rnorm(n_pts, sd = 0.1),
  y_coord = runif(4, 0, 1) + rnorm(n_pts, sd = 0.1)
)
true_labels <- rep(1:4, each = 100)

# draw the true distribution
plot(fake_data, col = true_labels, pch = true_labels,
     main = "simulated data (real cluster labels)")

# --- DBSCAN cluster ---
# select eps using the k-NN distance graph (minPts = dim + 1 = 3)
kNNdistplot(fake_data, k = 3)
abline(h = 0.05, col = "red", lty = 2)  # elbow point

# DBSCAN
dbscan_result <- dbscan(fake_data, eps = 0.05, minPts = 3)

# visualization
plot(fake_data, col = dbscan_result$cluster + 1L, pch = dbscan_result$cluster + 1L,
     main = "DBSCAN clustering results")
hullplot(fake_data, dbscan_result)

# OPTICS
optics_raw <- optics(fake_data, eps = 10, minPts = 10)

# accessibility map
plot(optics_raw, main = "OPTICS accessibility map")
abline(h = 0.065, col = "red", lty = 2)  # cutting line, to extract clusters

# extract clusters from OPTICS
# Method 1: simulate DBSCAN (fixed density)
optics_db <- extractDBSCAN(optics_raw, eps_cl = 0.065)
plot(optics_db)
hullplot(fake_data, optics_db)

# Method 2: hierarchical extraction (variable density, using xi threshold)
optics_xi <- extractXi(optics_raw, xi = 0.05)
plot(optics_xi)
hullplot(fake_data, optics_xi)

# convert to a tree diagram
optics_dend <- as.dendrogram(optics_xi)
plot(optics_dend, ylab = "accessibility distance", leaflab = "none",
     main = "OPTICS hierarchical clustering tree diagram")

# applying OPTICS to University data
data(College, package = "ISLR")

# data cleaning
uni_clean <- College %>%
  mutate(school = rownames(.),
         accept_ratio = Accept / Apps,
         is_elite = cut(Top10perc, c(0, 50, 100), labels = c("No", "Yes"))) %>%
  mutate(is_elite = factor(is_elite)) %>%
  select(school, accept_ratio, Outstate, Enroll, Grad.Rate, Private, is_elite)

# calculate Gower distance, for mixed data
gower_d <- daisy(uni_clean[, -1], metric = "gower", type = list(logratio = 3))

# OPTICS
optics_on_college <- optics(gower_d, eps = 10, minPts = 10)
plot(optics_on_college, main = "University data OPTICS Accessibility plot")

# Extracting Clusters using the xi method, to discover small and dense groups
college_clusters <- extractXi(optics_on_college, xi = 0.05)
print("cluster intervals discovered by University data:")
print(college_clusters$clusters_xi)

# visualization cluster
plot(college_clusters)
college_dendro <- as.dendrogram(college_clusters)
plot(college_dendro, ylab = "accessibility distance", leaflab = "none")