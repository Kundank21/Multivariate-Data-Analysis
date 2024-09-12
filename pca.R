
library(factoextra)
library(ggplot2)
data <- read.csv("eco_dev_data_fd8eb272-89ff-4ae6-9508-2e32b6b0d048.csv")
colnames(data) <- data[1,]
data <- data[-1,]
rownames(data) <- data[,1]
data <- data[,-1]

for (i in 1:dim(data)[2]) {
  data[,i] <- as.numeric(data[,i]) 
}

data <- scale(data)
pr <- prcomp(data)
pp <- pr$x[,1:3]
# Visualize PCA results to identify outliers
library(plotly)
ggplot2::last_plot()

####
fig <- plot_ly(x = pp[, 1], y = pp[, 2], z = pp[, 3], 
               type = 'scatter3d', 
               mode = 'markers', 
               marker = list(size = 5, color = mode, colorscale = 'Viridis', showscale = TRUE),
               text = paste("Point", 1:nrow(pp))) %>%
  layout(scene = list(xaxis = list(title = 'PC1'),
                      yaxis = list(title = 'PC2'),
                      zaxis = list(title = 'PC3')),
         title = '3D PCA Plot with Outliers Highlighted')

# Display the plot
fig

# Detect outliers using Mahalanobis distance


md <- mahalanobis(pp, colMeans(pp), cov(pp))
cutoff <- qchisq(0.975, df = ncol(pp))  # 97.5% cutoff for Chi-squared distribution
outliers <- which(md > cutoff)
data_cleaned <- data[-outliers, ]
pr_1 <- prcomp(data_cleaned)
country_ranking <- data.frame(country=rownames(pr_1$x),pc1 = pr_1$x[,1])
country_ranking <- country_ranking[order(-country_ranking$pc1),]


loadings <- pr_1$rotation
heatmap(abs(loadings))


##Assess normality for each principal component using Q-Q plots
par(mfrow = c(2, 2))
for (i in 1:4) {  # Plot for first four principal components
  qqnorm(pr_1$x[, i], main = paste("Q-Q Plot for PC", i))
  qqline(pr_1$x[, i])
}
library(MVN)
# Multivariate normality test using Mardia's test
mardia_test <- mvn(data, mvnTest = "mardia")
print(mardia_test$multivariateNormality)  # Display Mardia's test results





