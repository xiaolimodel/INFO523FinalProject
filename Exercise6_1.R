# Libraries needed for this exercise
library(dplyr) # for data cleaning
install.packages("ISLR")
library(ISLR)
# for college dataset
library(cluster) # for gower similarity, pam, and diana (a divisive hierarchical method).
# clara() is also included, which is basically pam by sampling
install.packages("Rtsne")
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
# Clustering requires
# 1. Calculating distance among the observations
# 2. Choosing a clustering algorithm
# 3. Selecting the number of clusters
# distance measures for continuous variables: dist()
# generate a 5x10 matrix with random data
set.seed(2345) # to get repeatable results
randDat <- matrix(rnorm(50), nrow=5)
# default dist(): Euclidean distance
dist(randDat)
# Manhattan distance
dist(randDat, method="manhattan")
dist(randDat, method="minkowski", p=4)
dist(randDat, method="minkowski", p=2)
dist(randDat, method="minkowski", p=1)
library(dplyr) # for data cleaning
library(ISLR)
# for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

set.seed(1680) # for reproducibility
data(College, package="ISLR") # with mixed typed variables
# A data frame with 777 observations on the following 18 variables.
# Private A factor with levels No and Yes indicating private or public university
# Apps Number of applications received
# Accept Number of applications accepted
# Enroll Number of new students enrolled
# Top 10 Perc Pct. new students from top 10% of H.S. class
# Top 25 Perc Pct. new students from top 25% of H.S. class
# F.Undergrad Number of full time undergraduates
# P.Undergrad Number of part time undergraduates
# Outstate Out-of-state tuition
# Room. Board Room and board costs
# Books Estimated book costs
# Personal Estimated personal spending
# PhD Pct. of faculty with Ph.D.'s
# Terminal Pct. of faculty with terminal degree
# S.F.Ratio Student/faculty ratio
# perc.alumni Pct. alumni who donate
# Expend Instructional expenditure per student
# Grad. Rate Graduation rate

glimpse (College)

# data transformation
# Acceptance rate is created by diving the number of acceptances by the number of applications
# isElite is created by labeling colleges with more than 50% of their new students who were in the top 10% of their high school class as elite
# mutate() adds new variables and preserves existing;
college_clean <- College %>%
  mutate (name = row.names(.),
          accept_rate = Accept/Apps,
          isElite = cut (Top10perc,
                         breaks = c(0, 50, 100),
                         labels = c("Not Elite", "Elite"),
                         include.lowest = TRUE)) %>%
  mutate(isElite = factor (isElite)) %>%
  select(name, accept_rate, Outstate, Enroll,
         Grad.Rate, Private, isElite)

glimpse(college_clean)

# calculating distance the Gower distance for mixed variable types in package cluster
# Gower distance: For each variable type, a particular distance metric that works well for that type is used and scaled to fall between 0 and 1.
# Then, a linear combination using user-specified weights (most simply an average) is calculated to create the final distance matrix.
# The metrics used for each data type are described below:

# quantitative (interval): range-normalized Manhattan distance
# ordinal: variable is first ranked, then Manhattan distance is used with a special adjustment for ties
# nominal: variables of k categories are first converted into k binary columns and then the Dice coefficient is used

# pros: Intuitive to understand and straightforward to calculate
# cons: Sensitive to non-normality and outliers present in continuous variables, so transformations as a pre-processing step might be necessary.
# Also requires an NxN distance matrix to be calculated, which is computationally intensive to keep in-memory for large samples
# Note that due to positive skew in the Enroll variable, a log transformation is conducted internally via the type argument.
# Instructions to perform additional transformations, like for factors that could be considered as asymmetric binary (such as rare events), can be seen in ?daisy.

# Remove college name before clustering
# Use daisy() to compute the distance matrix using 'gower'
gower_dist <- daisy(college_clean[, -1],
                    metric = "gower",
                    type = list(logratio = 3))

# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called,
# the type remains coded as "I"

summary(gower_dist)
gower_mat <- as.matrix(gower_dist)

# Output most similar pair, excluding dissimilar scores of 0 (one compared to itself)
college_clean [
  which(gower_mat == min(gower_mat [gower_mat != min(gower_mat)]),
        arr.ind = TRUE) [1,],]
college_clean [which(gower_mat==max(gower_mat), arr.ind = TRUE) [1,],]
# check if the pair has the highest dissimilarity score
max(gower_mat)
gower_mat [673, 460]
# making sense of the nested matrix and which() function used above by checking each individual function using a simpler matrix
m = matrix(1:10,nrow=5)
m
m != 10
m[m!=10]
class (m[m!=10])
min(m[m!=10])
m==min(m[m!=10])
which(m==min(m[m!=10]), arr.ind = FALSE)
which(m==min(m[m!=10]), arr.ind = TRUE)

which(m == 5, arr.ind = TRUE)

# Use PAM (partitioning around medoids) to perform the clustering
# Calculate Silhouette width for 2 to 10 clusters using PAM

sil <- c(NA)

for(i in 2:10){
  pam_fit <- pam(gower_mat, diss = TRUE, k=i)
  sil[i] <- pam_fit$silinfo$avg.width
}

# Plot silhouette width (higher is better, clusters = 3 has the highest sil value)

plot(1:10, sil,
     xlab= "Number of clusters",
     ylab= "Silhouette Width")
lines (1:10, sil)

# Cluster Interpretation: via Descriptive Statistics
pam_fit <- pam(gower_dist, diss = TRUE, k=3)
# add cluster labels to the data. We will use result1 later
college_clean <- data.frame(college_clean, pam_fit$cluster)
# show clustering results by college
result1 <- college_clean %>% dplyr::select(name,pam_fit.cluster)
View(result1)
# group_by cluster and then compute the summary data (means, median, etc) for each cluster
pam_results <- college_clean %>%
  dplyr::select(-name) %>%
  mutate(cluster = pam_fit$clustering) %>% # add the cluster column
  group_by(cluster) %>% # group universities by its cluster
  do(the_summary = summary(.)) # do: summarize by group/cluster, add the_summary column

pam_results$the_summary

# The results suggest
# Cluster 1, is mainly Private/Not Elite with medium levels of out of state tuition and smaller levels of enrollment.
# Cluster 2, is mainly Private/Elite with lower levels of acceptance rates, high levels of out of state tuition, and high graduation rat
# Cluster 3, is mainly Public/Not Elite with the lowest levels of tuition, largest levels of enrollment, and lowest graduation rate.

# Output medoids: representative university in each cluster:
college_clean[pam_fit$medoids, ]

## Cluster Interpretation: via visualization
# One way to visualize many variables in a lower dimensional space is with t-distributed stochastic neighborhood embedding, or t-SNE.
# This method is a dimension reduction technique that tries to preserve local structure so as to make clusters visible in a 2D or 3D visualization.
# it is a powerful but also sometimes puzzling technique, more see https://distill.pub/2016/misread-tsne/

tsne_obj <- Rtsne (gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = college_clean$name)

ggplot(aes(x = X, y=Y), data = tsne_data) +
  geom_point(aes (color = cluster))

# What is in the little cluster between blue and green?
tsne_data %>%
  filter (X > 10 & X < 20,
          Y > -5 & Y < 5) %>%
  left_join(college_clean, by = "name") %>%
  collect %>%
  .[["name"]]

# cluster the same data set using kmeans
# different implementations of kmeans are provided throught the algorithm argument to
# kmeans() method: "Hartigan-Wong", "Lloyd", "Forgy", "MacQueen". The default H-W implementation works well in most cases.
# To know more about the differences see https://core.ac.uk/download/pdf/27210461.pdf
# kmeans() take numeric data,
college_clean_n <- College %>%
  mutate(name = row.names(.),
         accept_rate = Accept/Apps) %>%
  select(name, accept_rate, Outstate, Enroll,
         Grad.Rate, Private, Top10perc)

# turn 'yes' 'no' to 1 and 0
college_clean_n$Private<-as.integer(college_clean_n$Private)-1L
# z-score transformation to scale the variables. Scaling is needed because we will use euclidean distant
college_clean_n %>% mutate_at(scale, .vars=vars(-name))
# get distance matrix, excluding first column: name
# note: nstart is the parameter that allows the user to try multiple sets of initial centroids.
# You should use a nstart > 1, for example, nstart = 25, this will run kmeans nstart number of times, each time with a different set of initial centroids.
# kmeans will then select the one with the lowest within cluster variation.
dist_mat <- dist(college_clean_n[, -1], method="euclidean")
avgS <- c() # initiate an empty vector
for(k in 2:10){
  kmeans_cl <- kmeans (college_clean_n[,-1], centers=k, iter.max = 500, nstart=1)
  s <- silhouette(kmeans_cl$cluster, dist_mat)
  avgS <- c(avgS, mean (s[,3])) # take the mean of sil_width of all observations, and save it in the avgS vector
}
data.frame(nClus = 2:10, Silh=avgS)
plot(2:10, avgS,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines (2:10, avgS)
# 2 clusters seem to be the best
kmeans_fit <- kmeans (college_clean_n[,-1], 2)
# get cluster means
aggregate (college_clean_n[,-1], by=list(kmeans_fit$cluster), FUN=mean)
# append cluster assignment to data
college_clean_n <- data.frame(college_clean_n, kmeans_fit$cluster)
## Cluster Interpretation: via visualization
tsne_obj <- Rtsne(dist_mat, is_distance = TRUE)
# TSNE is a powerful but sometimes puzzling technique More see https://distill.pub/2016/misread-tsne/
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate (cluster = factor(kmeans_fit$cluster),
          name = college_clean_n$name)
ggplot(aes (x=X, y=Y), data = tsne_data) +
  geom_point(aes (color = cluster))

# TSNE is a powerful but sometimes puzzling technique More see https://distill.pub/2016/misread-tsne/
# To compare to PAM (3-cluster result), we use k=3 for kmeans
kmeans_fit <- kmeans (college_clean_n[,-1], 3)
# append cluster assignment to data
college_clean_n <- data.frame(college_clean_n, kmeans_fit$cluster)
# college_clean_n
result2 <- college_clean_n %>% dplyr::select(name, kmeans_fit.cluster)
View(result2)
# note: clustering generated from different algorithms may or may not match.
# but PAM and Kmeans often have good overlap
# using the mediods from PAM results (assuming these are good mediods), we can tell
# cluster 1 from PAM (result 1) => cluster 2 from kmeans (result2)
# cluster 2 from PAM => cluster 1 from kmeans
# cluster 3 from PAM => cluster 3 from kmeans
# so, basically switch clusters 1 and 2 in kmeans results
# using 0 below is necessary to make the correct changes.
result2$kmeans_fit.cluster [result2$kmeans_fit.cluster==2] <- 0
result2$kmeans_fit.cluster [result2$kmeans_fit.cluster == 1] <- 2
result2$kmeans_fit.cluster [result2$kmeans_fit.cluster == 0] <- 1
# now create a confusion table to see the differences in the two clusterings
# 285+51+190=526 universities were in the same clusters
# 16+16+87+128+2+2=251 universities were in different clusters
table(result1$pam_fit.cluster, result2$kmeans_fit.cluster)

# build a hierarchical cluster using College data
# method argument takes the agglomeration method to be used.
# This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA),
# "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
h <- hclust(gower_dist, method="average")
# plot the dendrogram, #hang decides where to start labels on axis.
# -0.1 is negative, so the x-axis labels will hang down from 0
plot(h, hang=-0.1)

# If we want three clusters
clus3 <- cutree(h, 3)
# clus3 is a vector with the number of the cluster to which each observation was assigned.
# Observations keep the original order as in the dissimilar matrix.
clus3
# 492 is the index medoid for the 1st cluster, 38 is the index of the medoid for the 2nd/3rd clusters using PAM
clus3 [492]
# [1] 1
clus3[38]
# [1] 2
clus3[234]
# [1] 3
# compare with pam 3-cluter results (we verified that the cluster labels 1, 2, 3 match PAM's mediods result)
table(result1$pam_fit.cluster, clus3)

# use DIANA in the cluster package for divisive hierarchical clustering
# because we provided a dissimilarity matrix, we don't need to supply other arguments to this function.
# dissimilarity matrix incorporated the distance measures and data normalization
d <- diana(gower_dist)
d3 <- cutree(d, 3)
table(result1$pam_fit.cluster, d3) # (we verified that the cluster labels 1, 2, 3 match PAM mediods result)
# 492 is the index medoid for the 1st cluster, 38 is the index of the medoid for the 2nd/3rd clusters using PAM
d3 [492]
# [1] 1
d3 [38]
# [1] 2
d3 [234]

