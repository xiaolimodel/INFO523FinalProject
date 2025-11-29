# Libraries used:
install.packages("dbscan")
library("dbscan")
library(dplyr) # for data cleaning
library(ISLR) # for college dataset
# The package dbscan provides high performance code for DBSCAN and OPTICS

# use simulated data to show the use of dbscan
set.seed(2)
n <- 400
# generate four slightly overlapping Gaussians in 2-D space with 100 points each.

x <- cbind(
  # points (x,y): random normally distributed data adding uniform
  # distribution noises (runif)
  # runif(4, 0, 1) generate 4 uniform distributed data values in [0, 1]
  x = runif(4, 0, 1) + rnorm(n, sd = 0.1),
  y = runif(4,0,1)+rnorm(n, sd=0.1)
)

# cluster labels
true_clusters <- rep(1:4, time = 100)
# take a look at the data, 4 colors and symbols to indicate true clusters for the points
plot(x, col = true_clusters, pch= true_clusters) # col: color, pch:symbols

# next, we need to decide on minPts and eps.
# The rule of thumb for minPts = # of dimensions of the dataset + 1, this is
# 3 in our case
# eps can be determined by plotting the points' kNN distances (ie. the
# distance to the kth nearest neighbor)
# in increasing order and look for a knee in the plot.
# distance of each data point to its 3 nearest neighbor is plotted (3x400
# = 1200 distances)

kNNdistplot(x, k=3)
# distance 0.05 is the "knee", adding a reference line to show this
abline(h=0.05, col ="red", lty=2)

# now perform DBSCAN
res <- dbscan(x, eps = 0.05, minPts=3)
# points with cluster id=0 are noise.
res
# dbscan alsp provide a plot that adds convect cluster hulls to the scatter
# plot.
plot(x, col=res$cluster + 1L, pch=res$cluster+1L)
hullplot(x, res)
# to see the cluster assignment of the original data points
predict(res, x [1:25,], data=x) # the first 25 data points
predict(res, x, data=x)

# Clustering with OPTICS
# To benefit from the flexibility of OPTICS algorithm, we select larger eps
# and minPts to run OPTICS
# minPts = 10 means the core-distance is defined as the distance to the 9th
# nearest neighbor
res_op <- optics(x, eps=10, minPts = 10)
# Some additional parameters you will see in the result:
# eps_cl: another threshold on eps may be used for identify final clusters,
# eps_cl<=eps
# xi: if use optics or hierarchical clustering, xi is the steepness threshold
# to identify clusters hierarchically using the reachability plot.
res_op
# to get the computed order of the points
# The order says that data point 1 in the data set is the first in the order,
# data point 363 is the second and so forth
head(res_op$order, n=50)

# This density-based order produced by OPTICS can be directly used to plot a
# reachability plot
# 4 Valleys represent potential clusters separated by peaks. Very high peaks
# may indicate noise points.
plot(res_op)
abline (h=0.065, col ="red", lty=2) # add a reference cutting line (lty=2
# draws a dashed line) for extracting the clusters later

# We can also visualize the order on the original datasets by plotting a polygon on top of the original data points using the order
# if you run a smaller example, combining the order information with the graph, you will be able see clearly the order of the visit of the points in the dataset.
plot(x, col="red")
polygon(x[res_op$order,])

# ------------------------------------------------------------------------------
# Part 4: Extracting Clusters from OPTICS
# ------------------------------------------------------------------------------
# Now we can extract a DBSCAN-type clustering based on the reachability plot
# Because this method extracts clusters like DBSCAN, it cannot identify partitions that exhibit very significant differences in density
# As shown above, eps_cl = 0.065 will result in 4 clusters

res_op_c <- extractDBSCAN(res_op, eps_cl=0.065)
plot(res_op_c)
hullplot(x, res_op_c)

res_op_c # we can see 92 points were not included in the clusters
# this can be a reliable way to identify tight clusters in a dataset and tight clusters can be used as a summary of the dataset.

# We can also extract a hierarchical cluster structure using extractXi
# xi is the steepness threshold, describes the relative magnitude of the change of cluster density (i.e., reachability)
# Significant changes in relative reachability allow for clusters to manifest themselves hierarchically as 'dents' in the ordering structure.
# The hierarchical representation ExtractXi can, as opposed to the ExtractDBSCAN method, produce clusters of varying densities.
# Clusters are represented as contiguous ranges in the reachability plot and are available in the field clusters_xi

(res_op_h <- extractXi(res_op, xi=0.05))
res_op_h$clusters_xi

# We can visualize the clusters in the reachability plot
# clusters represented using colors and vertical bars below the plot.
plot(res_op_h)
hullplot(x, res_op_h)

# To represent the hierarchical clustering as a dendrogram
(dend <- as.dendrogram(res_op_h))
plot(dend, ylab="Reachability distance", leaflab="none")

# ------------------------------------------------------------------------------
# Part 5: College Dataset Exercise
# ------------------------------------------------------------------------------
# Run OPTICS on the College dataset, using gower_dist matrix
set.seed(1680) # for reproducibility
data(College, package="ISLR")

# Data cleaning
college_clean <- College %>%
  mutate(name = row.names(.),
         accept_rate = Accept/Apps,
         isElite = cut(Top10perc,
                       breaks = c(0, 50, 100),
                       labels = c("Not Elite", "Elite"),
                       include.lowest = TRUE)) %>%
  mutate(isElite = factor(isElite)) %>%
  select(name, accept_rate, Outstate, Enroll, Grad.Rate, Private, isElite)

# Note: 'daisy' requires the 'cluster' package (implied by the function call)
# library(cluster) # implied requirement
gower_dist <- daisy(college_clean[, -1],
                    metric = "gower",
                    type = list(logratio = 3))

dim(college_clean)

# 7 dimensions, so set k=8
kNNdistplot(gower_dist, k=8)
abline(h=0.05, col='red', lty=2)

(res_col <- optics(gower_dist, eps = 10, minPts = 10))
plot(res_col)

# 3 clusters, 78 noise points
(res_col_d <- extractDBSCAN(res_col, eps_cl=0.05))

# 4 clusters, 2 noise points
(res_col_h <- extractXi(res_col, xi=0.05))

res_col_h$clusters_xi # cluster 4 is tiny, what is in there?

plot(res_col_h)
dend_col <- as.dendrogram(res_col_h)
plot(dend_col)

# 12 Universities in cluster 4, almost exactly match the smallest cluster produced by PAM (University of Michigan at Ann Arbor not in the current list)
college_clean[res_col_h$order[765:776],]

# which cluster is UMich at Ann Arbor?
college_clean[college_clean$name=="University of Michigan at Ann Arbor", ]

match(638, res_col_h$order) # Michigan is 777 in the order, by checking res_col_h$clusters_xi, it is one of the two outliers

# by comparing the results from OPTICS with PAM and see they are similar, our confidence in the clusters identified increases.