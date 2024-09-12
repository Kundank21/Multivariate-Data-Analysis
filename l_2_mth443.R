# Required libraries

library(factoextra)
library(ggplot2)

# Perform PCA and keep case tags

pca_result <- prcomp(data, scale = TRUE)

# Get principal component projection (first two PCs for visualization)
pc_projection <- data.frame(pca_result$x[, 1:2])
pc_projection$case_tag <- rownames(data)

# Visualize the principal component projection with case tags
library(ggplot2)
ggplot(pc_projection, aes(PC1, PC2, label = case_tag)) +
  geom_point(color = "blue") +
  geom_text(aes(label = case_tag), vjust = -0.5, size = 3) +
  ggtitle("Principal Component Projection with Case Tags") +
  theme_minimal()


# Proportion of variance explained by each principal component
variance_explained <- summary(pca_result)$importance[2, ]

# Print proportion of variance explained
variance_explained

# Plot cumulative proportion of variance explained
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100)) +
  ggtitle("Proportion of Total Variation Explained by Each PC")


# Scree plot to observe elbow formation
fviz_screeplot(pca_result, ncp = 10, addlabels = TRUE, ylim = c(0, 50)) +
  ggtitle("Scree Plot of Principal Components")





# Identify potential outliers based on the PCA projections
library(factoextra)
outliers <- get_pca_ind(pca_result)$coord[, 1:2] # Get the coordinates of individuals

# Detect outliers using Mahalanobis distance
distances <- mahalanobis(outliers, colMeans(outliers), cov(outliers))
threshold <- quantile(distances, 0.975) # Use 97.5% quantile for threshold
outlier_indices <- which(distances > threshold)

# Highlight outliers on PCA plot
pc_projection$outlier <- ifelse(1:nrow(pc_projection) %in% outlier_indices, "Outlier", "Normal")

ggplot(pc_projection, aes(PC1, PC2, color = outlier)) +
  geom_point() +
  ggtitle("Outliers Detection in PCA Projection") +
  theme_minimal()



# Perform clustering using k-means (for 3 clusters as an example)
set.seed(123)
kmeans_result <- kmeans(pca_result$x[, 1:2], centers = 3)

# Add cluster information to PCA projection data
pc_projection$cluster <- factor(kmeans_result$cluster)

# Visualize clusters on PCA projection
ggplot(pc_projection, aes(PC1, PC2, color = cluster)) +
  geom_point(size = 3) +
  ggtitle("Clusters Identified from PCA Projection") +
  theme_minimal()

# Get cluster characteristics
cluster_summary <- aggregate(data, by = list(Cluster = kmeans_result$cluster), mean)
cluster_summary

