library(dbscan)
library(dplyr)
library(cluster)

set.seed(1234)

# data
trp_df <- read.csv("TRPCompanies.csv", stringsAsFactors = FALSE)

# process column names
names(trp_df) <- gsub("^Average.of.", "", names(trp_df))

# extract numerical variables
numeric_cols <- trp_df %>%
  select(-Company, -`NAICS2`) %>%
  select(where(is.numeric))

zero_var <- apply(numeric_cols, 2, var) == 0
if (any(zero_var)) {
  numeric_cols <- numeric_cols[, !zero_var]
  cat("removed ", sum(zero_var), "zero variance columns. \n")
}
trp_scaled <- numeric_cols

#  DBSCAN
#  use k-NN distplot
kNNdistplot(trp_scaled, k = 10)
abline(h = 0.8, col = "red", lty = 2)

# DBSCAN
dbscan_res <- dbscan(trp_scaled, eps = 1.5, minPts = 10)
print(table(dbscan_res$cluster)) # view cluster distribution

# OPTICS
optics_res <- optics(trp_scaled, minPts = 10)
plot(optics_res, main = "TRP Companies: OPTICS Reachability Plot")



# try multiple xi values
xi_list <- c(0.001, 0.005, 0.01, 0.02)
results <- list()

for (xi in xi_list) {
  clust <- extractXi(optics_res, xi = xi)
  labels <- integer(nrow(trp_scaled))
  labels[optics_res$order] <- clust$cluster_ids
  results[[as.character(xi)]] <- table(labels)
}

# result
lapply(results, print)



#  xi = 0.005
optics_clusters <- extractXi(optics_res, xi = 0.05)
print(optics_clusters$clusters_xi)

# Visualization 
pca_res <- prcomp(trp_scaled)
pca_df <- data.frame(
  PC1 = pca_res$x[,1],
  PC2 = pca_res$x[,2],
  cluster = factor(optics_clusters$cluster)
)

library(ggplot2)
ggplot(pca_df, aes(x=PC1, y=PC2, color=cluster)) +
  geom_point() + ggtitle("TRP Companies: OPTICS Clusters (PCA)")