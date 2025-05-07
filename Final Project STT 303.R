#Clustering


# Load necessary packages
install.packages("dplyr")
install.packages("tidyr")

library(dplyr)
library(tidyr)

#upload dataset in file named "GTEx_dataset.gct"
# Read the data
data <- GTEx_dataset
head(data)


colnames(data) <- data[2, ]
data <- data[-c(1, 2), ]
data <- data.frame(lapply(data, function(x) type.convert(as.character(x), as.is = TRUE)))
summary(data)

#scale data
data <- scale(data)

#remove first two columns in data
data <- data[, -c(1, 2)]
# Convert the data to numeric
data <- data.frame(lapply(data, function(x) type.convert(as.character(x), as.is = TRUE)))


install.packages("kknn")
install.packages("ggplot2")
library(kknn)
library(ggplot2)


# Elbow Method to determine optimal number of clusters
wcss <- sapply(1:10, function(k) {
  kmeans(data, centers = k, nstart = 10)$tot.withinss
})

plot(1:10, wcss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters K",
     ylab = "Total within-clusters sum of squares")
#6 clusters identified


# Perform k-means clustering using kmeans.knn
#changed to 4, find best amount of clusters
set.seed(123) # For reproducibility
kmeans_result <- kmeans(data, centers = 6)

# Create a data frame for visualization
data$cluster <- as.factor(kmeans_result$cluster)
#scale result of the data
pca_result <- prcomp(data[, -ncol(data)], scale. = TRUE)
pca_data <- data.frame(pca_result$x, cluster = data$cluster)

# Plot the clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point() +
  labs(title = "PCA of GTEx Data with K-means Clusters")



