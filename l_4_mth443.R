library(ggplot2)
library(cluster)
library(dendextend)

data <- read.csv("wine_data.csv")  # Update with your actual file name
data <- data[,-which(names(data) == "Type")]  # Remove 'Type' column
data <- scale(data)  # Standardize the data



#Hierarchical Clustering:
# Compute distance matrix
dist_matrix <- dist(data)

# Complete linkage hierarchical clustering
hc_complete <- hclust(dist_matrix, method = "complete")
plot(hc_complete, main = "Hierarchical Clustering - Complete Linkage")

# Single linkage hierarchical clustering
hc_single <- hclust(dist_matrix, method = "single")
plot(hc_single, main = "Hierarchical Clustering - Single Linkage")


#K-Means Clustering:

set.seed(123)  # For reproducibility

# Perform k-means clustering with K=3
kmeans_results <- list()
for (i in 1:5) {  # Try 5 different initial configurations
  kmeans_results[[i]] <- kmeans(data, centers = 3, nstart = 20)
}

# Print the clustering results for each configuration
for (i in 1:5) {
  cat("K-means result configuration", i, ":\n")
  print(kmeans_results[[i]])
}


#Calculate Total Within-Cluster Point Scatter


# Function to calculate total within-cluster sum of squares
total_within_ss <- function(clustering_result, data) {
  cluster_assignments <- clustering_result$cluster
  cluster_centers <- clustering_result$centers
  total_ss <- 0
  
  for (i in 1:length(cluster_centers)) {
    cluster_points <- data[cluster_assignments == i, ]
    total_ss <- total_ss + sum(rowSums((cluster_points - cluster_centers[i, ])^2))
  }
  
  return(total_ss)
}

# Calculate total within-cluster sum of squares for hierarchical clustering
hc_clusters_complete <- cutree(hc_complete, k = 3)
hc_clusters_single <- cutree(hc_single, k = 3)

# Creating a data frame with clusters assigned from hierarchical clustering
data_hc_complete <- cbind(data, cluster = hc_clusters_complete)
data_hc_single <- cbind(data, cluster = hc_clusters_single)

# K-means results
kmeans_ss_list <- sapply(kmeans_results, function(res) total_within_ss(res, data))

# Calculate and print within-cluster sum of squares for each method
hc_ss_complete <- total_within_ss(list(cluster = hc_clusters_complete, centers = kmeans_results[[1]]$centers), data)
hc_ss_single <- total_within_ss(list(cluster = hc_clusters_single, centers = kmeans_results[[1]]$centers), data)

cat("Total within-cluster sum of squares (Hierarchical Complete):", hc_ss_complete, "\n")
cat("Total within-cluster sum of squares (Hierarchical Single):", hc_ss_single, "\n")
cat("Total within-cluster sum of squares (K-means):", kmeans_ss_list, "\n")



#Validate the Clustering Results with the "Type" Variable

# Add clusters to original data
data_with_clusters_complete <- data
data_with_clusters_complete$Cluster_HC_Complete <- hc_clusters_complete

data_with_clusters_single <- data
data_with_clusters_single$Cluster_HC_Single <- hc_clusters_single

data_with_clusters_kmeans <- data
data_with_clusters_kmeans$Cluster_KMeans <- kmeans_results[[1]]$cluster

# Add the "Type" variable back for comparison
data_with_clusters_complete$Type <- read.csv("wine_data.csv")$Type
data_with_clusters_single$Type <- read.csv("wine_data.csv")$Type
data_with_clusters_kmeans$Type <- read.csv("wine_data.csv")$Type

# Compare clustering results with "Type"
library(dplyr)

# Cross-tabulate clustering results with "Type" variable
table(data_with_clusters_complete$Cluster_HC_Complete, data_with_clusters_complete$Type)
table(data_with_clusters_single$Cluster_HC_Single, data_with_clusters_single$Type)
table(data_with_clusters_kmeans$Cluster_KMeans, data_with_clusters_kmeans$Type)

