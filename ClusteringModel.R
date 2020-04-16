#Clustering

df1 <- read.csv('/Users/Sunny/Github/CSP571_Movie_Profits_Project/final_dataset.csv', stringsAsFactors = T)


df1$Success_2_to_1 <- 0
df1[,'Success_2_to_1'][df1$earnings_percent > 0] <- 1

summary(df1)

##Extract categorical variables
df2 <- df1[,c('release_date', 'Success_2_to_1', 'quarter', 'actorRank', 'drama', 
              'thriller', 'nonfiction', 'action', 'amusement')]

##Standardize numerical columns
df3 <- scale(df1[, c('runtime', 'ProductionBudget')])

##Merge the two dataframes
df4 <- cbind(df2, df3)

##Convert date column and sort by date
df4$release_date <- as.Date(df4$release_date)
df5 <- df4[order(df4$release_date), -1]

##Convert quarter from factor to numeric
df5$quarter <- as.numeric(df5$quarter)

##Partition into 75%, and 25% test, constrained to its chronological ordering
splitRow <- as.integer(nrow(df5)*0.8)
training <- df5[c(1:splitRow), ]
train_set <- training[, -1]
colnames(train_set)
test <- df5[c((splitRow+1):nrow(df5)), ]
test_set <- test[, -1]
test_label <- test[, 1]

##Selecting K, using elbow graph and within cluster sum-of-squares
optimal_k <- function (data){
  ss_within <- data.frame(0,0)
  for (i in 1:100){
    kmeans_model <- kmeans(x = data, centers = i, nstart = 3, iter.max = 15)
    ss_within <- rbind(ss_within, c(i, kmeans_model$tot.withinss))
  }
  return(ss_within)
}

num_centers <- optimal_k(train_set)
plot(num_centers[-1,], type = 'l', main = 'Total Within SS for K clusters',
     xlab = 'K # of Clusters', ylab = 'Total Within SS')
##We see from the elbow method that after 7 clusters, additional clusters do not lower Within
##Cluster Sum of Squares significantly, meaning each additional cluster after k=7
##explains less and less of the variance within the data.

##Implementing Final Kmeans model
final_kmeans <- kmeans(x = train_set, centers = 12, nstart = 3, iter.max = 15)
training[, 'Cluster'] <- final_kmeans$cluster
final_kmeans$betweenss

##Labeling the clusters
centers <- data.frame(final_kmeans$centers)
center_labels <- data.frame(0)
for (i in 1:nrow(centers)){
  lab <- sum(training[training$Cluster == i, 'Success_2_to_1']) / nrow(training[training$Cluster == i, ])
  center_labels <- rbind(center_labels, lab)
}
centers[, 'Success_prob'] <- center_labels[-1,]
centers[, 'Label'] <- round(centers$Success_prob)

##Understanding our data
centers
##According to this one particular cluster has more successful movies than the others.
##This cluster suggests that movies that have a higher probability of success have
##long run-times, are of the nonfiction and amusement genres, release around the second quarter,
##and do not need well-known actors/actresses.

##Assign the Test Data with function
centroid_assignment <- function (data){
  label <- data.frame(0)
  for (i in 1:nrow(data)){
    value_matrix <- rbind(data[i,], centers[, -c(10:11)])
    dist_matrix <- data.frame(as.table(dist(value_matrix)))
    min_distance <- min(dist_matrix[1:nrow(value_matrix), 2])
    assign_cluster <- which(dist_matrix[, 2] == min_distance)
    cluster_label <- centers[assign_cluster, 'Label']
    label <- rbind(label, cluster_label)
  }
  return(label[-1,])
}

##Evaluate Results with Confusion Matrix
y_predict <- centroid_assignment(test_set)
true_pred_df <- data.frame(cbind(test_label, y_predict))
true_pred_df$test_label <- as.factor(test_label)
true_pred_df$y_predict <- as.factor(y_predict)
                           
confusionMatrix(true_pred_df$y_predict, reference = true_pred_df$test_label)
##The Performance is TERRIBLE!

##Testing with PCA for dimensionality reduction
library('corrplot')
str(training)
df5_cor <- cor(df5)
corrplot(df5_cor)
##Not many strong correlations at first glance. Will try PCA anyways

##Convert datasets using PCA
pca_train_set <- princomp(train_set)
pca_test_set <- princomp(test_set)


##First four components explain 83% of the variance
plot(pca_train_set)
summary(pca_train_set)


##Plot against each component
train_comp <- data.frame(pca_training$scores[, 1:4])
plot(comp, pch = 16)

##Take the first 4 components
train_comp <- pca_train_set$scores[,1:4]
test_comp <- pca_test_set$scores[,1:4]

##Find optimal K
num_centers <- optimal_k(train_comp)
plot(num_centers[-1,], type = 'l', main = 'Total Within SS for K clusters',
     xlab = 'K # of Clusters', ylab = 'Total Within SS')

##Optimal K = 7

##Training final model
final_kmeans <- kmeans(x = train_comp, centers = 7, nstart = 3, iter.max = 15)
training[, 'Cluster'] <- final_kmeans$cluster
final_kmeans$betweenss

##Labeling the clusters
centers <- data.frame(final_kmeans$centers)
center_labels <- data.frame(0)
for (i in 1:nrow(centers)){
  lab <- sum(training[training$Cluster == i, 'Success_2_to_1']) / nrow(training[training$Cluster == i, ])
  center_labels <- rbind(center_labels, lab)
}
centers[, 'Success_prob'] <- center_labels[-1,]
centers[, 'Label'] <- round(centers$Success_prob)

##Understanding our data
centers
##According to this one particular cluster has more successful movies than the others.
##This cluster suggests that movies that have a higher probability of success have
##long run-times, are of the nonfiction and amusement genres, release around the second quarter,
##and do not need well-known actors/actresses.

##Assign the Test Data with function
centroid_assignment <- function (data){
  label <- data.frame(0)
  for (i in 1:nrow(data)){
    value_matrix <- rbind(data[i,], centers[, -c(10:11)])
    dist_matrix <- data.frame(as.table(dist(value_matrix)))
    min_distance <- min(dist_matrix[1:nrow(value_matrix), 2])
    assign_cluster <- which(dist_matrix[, 2] == min_distance)
    cluster_label <- centers[assign_cluster, 'Label']
    label <- rbind(label, cluster_label)
  }
  return(label[-1,])
}

##Evaluate Results with Confusion Matrix
y_predict <- centroid_assignment(test_set)
true_pred_df <- data.frame(cbind(test_label, y_predict))
true_pred_df$test_label <- as.factor(test_label)
true_pred_df$y_predict <- as.factor(y_predict)

confusionMatrix(true_pred_df$y_predict, reference = true_pred_df$test_label)
