# Load necessary library
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# 1. Load the data
df <- read.csv("TRPCompanies.csv")

# 2. Prepare Data and Cluster
features_scaled <- scale(df[, c("Average.of.HiAtHome", "Average.of.HWTRTime")])
set.seed(42)
km_res <- kmeans(features_scaled, centers = 3, nstart = 25)
df$Cluster <- as.factor(km_res$cluster)

# 3. Prepare NAICS Labels
# Default: NAICS code
df$NaicsLabel <- as.character(df$Average.of.NAICS2)
# Hide NAICS for dense region
cond_hide <- (df$Average.of.HWTRTime < 25) & (df$Average.of.HiAtHome < 0.8)
df$NaicsLabel[cond_hide] <- ""

# 4. Prepare Company Labels and Alignment
df$CompanyLabel <- NA
df$HJustVal <- NA

# Condition A: HWTRTime > 25 -> Label to the RIGHT
cond_right <- (df$Average.of.HWTRTime > 25)
df$CompanyLabel[cond_right] <- df$Company[cond_right]
df$HJustVal[cond_right] <- 0   # hjust=0 aligns text to the right of the point (left-justified text)

# Condition B: HiAtHome > 1 -> Label to the LEFT
# This overwrites Condition A if a point meets both, which is good 
# because high HiAtHome points are on the far right edge of the plot.
cond_left <- (df$Average.of.HiAtHome > 1)
df$CompanyLabel[cond_left] <- df$Company[cond_left]
df$HJustVal[cond_left] <- 1    # hjust=1 aligns text to the left of the point (right-justified text)

# 5. Plot
ggplot(df, aes(x = Average.of.HiAtHome, y = Average.of.HWTRTime)) +
  # Points
  geom_point(aes(color = Cluster), size = 3, alpha = 0.8) +
  
  # Layer 1: NAICS Labels (Below the point)
  geom_text(aes(label = NaicsLabel), vjust = 2, size = 3, color = "darkgrey", check_overlap = FALSE) +
  
  # Layer 2: Company Labels (Left or Right based on HJustVal)
  # We subset only rows that have a company label to avoid warnings with NA hjust
  geom_text(data = subset(df, !is.na(CompanyLabel)),
            aes(label = CompanyLabel, hjust = HJustVal),
            nudge_x = ifelse(subset(df, !is.na(CompanyLabel))$HJustVal == 0, 0.05, -0.05), # Small nudge for spacing
            size = 3, fontface = "bold") +
  
  # Titles and Theme
  labs(title = "K-Means Clustering (k=3)",
       #subtitle = "Company Labels: Left if HiAtHome > 1, Right if HWTRTime > 25",
       x = "Average of Telecommute Days",
       y = "Average of Home to Work Travel Time") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")
# Clustering Practice: Traditional Methods for University Datasets (PAM, K-means, Hierarchical Clustering)
