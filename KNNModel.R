#KNN
rm(list = ls())
library('caret')

df1 <- read.csv('/Users/Sunny/Github/CSP571_Movie_Profits_Project/5_merged_with_holidays.csv', stringsAsFactors = T)


##Convert classification to factors
df1$success_1_to_1 <- as.factor(df1$success_1_to_1)

##Extract categorical variables and other variables that we do not want to standardize
df2 <- df1[,c(32, 7:24, 30, 34:53)]

##Standardize numerical columns
df3 <- scale(df1[, c(5:6, 27:29, 31, 33)])

##Merge the two dataframes
df4 <- cbind(df2, df3)

##Partition into 80%, and 20% test, constrained to its chronological ordering
inTrain <- createDataPartition(y = df4[,'success_1_to_1'], list = FALSE, p = .8)
train <- df4[inTrain,]
test <- df4[-inTrain,]

train_set <- train[, -c(1)]
train_label_class <- train[, 1]
test <- df5[c((splitRow+1):nrow(df5)),]
test_set <- test[, -c(1)]
test_label_class <- test[, 1]

##Error Check for row equality
stopifnot(nrow(train) + nrow(test) == nrow(df4))

##Training the model using the caret package and finding the optimal k for # of neighbors.
##trControl allows the user to control the resampling criteria. We are using 10-fold cross validation.
##tuneGrid allows the user to specify the range of k's to test for the best model

#Weighted KNN Algorithms

##Rectangular / Non-weighted
model_rectangular <- train(
  success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:50,
                         distance = 2,
                         kernel = c('rectangular')))
##K=15 is optimal
plot(model_rectangular)
model_rectangular$bestTune

##Triangular
model_triangular <- train(
  success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:50,
                         distance = 2,
                         kernel = c('triangular')))
##K=33 is optimal
plot(model_triangular)
model_triangular$bestTune

##Gaussian
model_gaussian <- train(
  success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:50,
                         distance = 2,
                         kernel = c('gaussian')))
##K=16 is optimal
plot(model_gaussian)
model_gaussian$bestTune

##Epanechnikov
model_epan <- train(
  success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:50,
                         distance = 2,
                         kernel = c('epanechnikov')))
##K=26 is optimal
plot(model_epan)
model_epan$bestTune

##Which model has the lowest RMSE?
mean(model_unweighted$results$Accuracy)
mean(model_rectangular$results$Accuracy)

min(model_triangular$results$Accuracy)
min(model_gaussian$results$Accuracy)
min(model_epan$results$Accuracy)
##Epanechnikov with 26 has the highest accuracy. We will use it

##Let's predict using the test set
pred_unweighted <- predict.train(object = model_unweighted, test_set)
pred_rectangular <- predict.train(object = model_rectangular, test_set)
pred_triangular <- predict.train(object = model_triangular, test_set)
pred_gaussian <- predict.train(object = model_gaussian, test_set)
pred_epan <- predict.train(object = model_epan, test_set)

##Let's evaluate each model's performance
test_label_factor <- as.factor(test_label_class) ##Factorize the test labels so it can be used
df_unweighted <- as.factor(round(pred_unweighted))
df_rectangular <- as.factor(round(pred_rectangular))
df_triangular <- as.factor(round(pred_triangular))
df_gaussian <- as.factor(round(pred_gaussian))
df_epan <- as.factor(round(pred_epan))

##NO INFO RATE = 0.5083
confusionMatrix(pred_unweighted, reference = test_label_factor, positive = '1') ##0.7044
confusionMatrix(pred_rectangular, reference = test_label_factor, positive = '1') ##0.6943
confusionMatrix(pred_triangular, reference = test_label_factor, positive = '1') ##0.6993
confusionMatrix(pred_gaussian, reference = test_label_factor, positive = '1') ##0.6926
confusionMatrix(pred_epan, reference = test_label_factor, positive = '1') ##0.6976

##Gaussian, indeed, has best performance

##Let's further tune the distance hyperparameter for model(weight = Gaussian, K = 136)
gauss_dist <- train(
  Success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 135:140,
                         distance = 1:4,
                         kernel = c('gaussian')))
##L2 Norm, K=136 is optimal
plot(gauss_dist)
gauss_dist$bestTune


################################################################################
##Selecting K, determine each K's accuracy
library('class')
k_accuracy <- function(trainSet, trainLab, testSet, testLab){
  k_acc <- data.frame(0,0)
  for(i in 1:100){
    knn_predict <- knn(train = trainSet, test = testSet, cl = trainLab, k = i)
    knn_accuracy <- sum(testLab == knn_predict) / nrow(testSet)
    k_acc <- rbind(k_acc, c(i, knn_accuracy))
  }
  return(k_acc)
}

##Input train and validation sets into function to find optimal K
k_acc_values <- k_accuracy(train_set, train_label, validation_set, validation_label)
plot(k_acc_values, type = 'l')
k_acc_values[k_acc_values$X0.1 == max(k_acc_values$X0.1),]
##K = 35 is best. Afterward, the improvement in accuracy is very little. 


##Using the caret package
library('caret')
model <- train(
  Success_1_to_1 ~., 
  data = training, 
  method = "knn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(k = c(1:100))
)
plot(model)
model$bestTune


predictions <- predict.train(object = model, test_set)
prediction_df <- data.frame(round(predictions))
test_predict <- as.factor(prediction_df$round.predictions.)
summary(test_predict)
summary(knn_predict)

test_label_factor <- as.factor(test_label)
confusionMatrix(test_predict, reference = test_label_factor)

knn_predict <- knn(train = training[, -1], test = test_set, cl = training[, 1], k = 20)

##Evaluate KNN model
knn_predict <- as.factor(knn_predict)
test_label_factor <- as.factor(test_label)
confusionMatrix(knn_predict, reference = test_label_factor)
##TERRIBLE
summary(df2)
##################################################################################

##Testing with PCA for dimensionality reduction
library('corrplot')
str(train_set)
df5_cor <- cor(df5)
corrplot(df5_cor)
##Not many strong correlations at first glance. Will try PCA anyways

##Convert datasets using PCA
pca_train_set <- princomp(train_set)
pca_test_set <- princomp(test_set)


##First seven components explain 83% of the variance
plot(pca_train_set)
summary(pca_train_set)


##Plot against each component
train_comp <- data.frame(pca_train_set$scores[, 1:7])
plot(train_comp, pch = 16)

##Take the first 4 components
train_comp <- cbind(train_label, pca_train_set$scores[,1:7])
test_comp <- pca_test_set$scores[,1:7]

##Train model using PC
model_pca_unweighted <- train(
  train_label ~., 
  data = train_comp, 
  method = "knn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(k = c(15:80)))
##K=62 is optimal
plot(model_pca_unweighted)
model_pca_unweighted$bestTune

##Evaluate PCA and compare to unweighted with PCA
pred_pca_unweighted <- predict.train(object = model_pca_unweighted, test_comp)
df_pca_unweighted <- as.factor(round(pred_pca_unweighted))
confusionMatrix(df_pca_unweighted, reference = test_label_factor, positive = '1')
##TERRIBLE - Even worse than not having performed PCA


##############################################################
##Relief Algorithm
#install.packages("CORElearn")
library(CORElearn)
colnames(train)
optimalVars <- attrEval(success_1_to_1 ~ ., data = train, estimator = "ReliefFequalK")

install.packages("dprep")
