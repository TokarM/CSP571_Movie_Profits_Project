#KNN

df1 <- read.csv('/Users/Sunny/Github/CSP571_Movie_Profits_Project/final_dataset.csv', stringsAsFactors = T)

##Extract categorical variables
df2 <- df1[,c('release_date', 'Success_1_to_1', 'quarter', 'actorRank', 'drama', 
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

##Partition into 80%, and 20% test, constrained to its chronological ordering
splitRow <- as.integer(nrow(df5)*0.8)
train <- df5[c(1:splitRow), ]
train_set <- train[, -1]
train_label <- train[, 1]
test <- df5[c((splitRow+1):nrow(df5)), ]
test_set <- test[, -1]
test_label <- test[, 1]

##Error Check for row equality
stopifnot(nrow(train_set) + nrow(test_set) == nrow(df5))

##Using the caret package
library('caret')

##Training the model using the caret package and finding the optimal k for # of neighbors.
##trControl allows the user to control the resampling criteria. We are using 10-fold cross validation.
##tuneGrid allows the user to specify the range of k's to test for the best model
model_unweighted <- train(
  Success_1_to_1 ~., 
  data = train, 
  method = "knn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(k = c(15:80)))
##K=24 is optimal
plot(model_unweighted)
model_unweighted$bestTune

#Weighted KNN Algorithms

##Rectangular
model_rectangular <- train(
  Success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:150,
                         distance = 2,
                         kernel = c('rectangular')))
##K=93 is optimal
plot(model_rectangular)
model_rectangular$bestTune

##Triangular
model_triangular <- train(
  Success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:150,
                         distance = 2,
                         kernel = c('triangular')))
##K=132 is optimal
plot(model_triangular)
model_triangular$bestTune

##Gaussian
model_gaussian <- train(
  Success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:150,
                         distance = 2,
                         kernel = c('gaussian')))
##K=136 is optimal
plot(model_gaussian)
model_gaussian$bestTune

##Epanechnikov
model_epan <- train(
  Success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:150,
                         distance = 2,
                         kernel = c('epanechnikov')))
##K=115 is optimal
plot(model_epan)
model_epan$bestTune

##Which model has the lowest RMSE?
min(model_unweighted$results$RMSE)
min(model_rectangular$results$RMSE)
min(model_triangular$results$RMSE)
min(model_gaussian$results$RMSE)
min(model_epan$results$RMSE)
##Gaussian weighted with K=136

##Let's predict using the test set
pred_unweighted <- predict.train(object = model_unweighted, test_set)
pred_rectangular <- predict.train(object = model_rectangular, test_set)
pred_triangular <- predict.train(object = model_triangular, test_set)
pred_gaussian <- predict.train(object = model_gaussian, test_set)
pred_epan <- predict.train(object = model_epan, test_set)

##Let's evaluate each model's performance
test_label_factor <- as.factor(test_label) ##Factorize the test labels so it can be used
df_unweighted <- as.factor(round(pred_unweighted))
df_rectangular <- as.factor(round(pred_rectangular))
df_triangular <- as.factor(round(pred_triangular))
df_gaussian <- as.factor(round(pred_gaussian))
df_epan <- as.factor(round(pred_epan))

confusionMatrix(df_unweighted, reference = test_label_factor, positive = '1') ##0.5598
confusionMatrix(df_rectangular, reference = test_label_factor, positive = '1') ##0.5598
confusionMatrix(df_triangular, reference = test_label_factor, positive = '1') ##0.5598
confusionMatrix(df_gaussian, reference = test_label_factor, positive = '1') ##0.5646
confusionMatrix(df_epan, reference = test_label_factor, positive = '1') ##0.555

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

##But performance still isn't very good. Let us reduce 

model5 <- train(
  Success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 101:110,
                         distance = 2,
                         kernel = c('rectangular', 'triangular', 'gaussian', 'epanechnikov')
  )
)
plot(model5)
model5$bestTune

model6 <- train(
  Success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 111:120,
                         distance = 2,
                         kernel = c('rectangular', 'triangular', 'gaussian', 'epanechnikov')
  )
)
plot(model6)
model6$bestTune


min(model$results$RMSE)
min(model1$results$RMSE)
min(model2$results$RMSE)
min(model3$results$RMSE)
min(model4$results$RMSE)
min(model5$results$RMSE)
min(model6$results$RMSE)

model_weighted <- train(
  Success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 20:130,
                         distance = 2,
                         kernel = c('rectangular', 'triangular', 'gaussian', 'epanechnikov')
  )
)


predictions <- predict.train(object = model, test_set)
prediction_df <- data.frame(round(predictions))
test_label_factor <- as.factor(test_label)
confusionMatrix(test_predict, reference = test_label_factor, positive = '1')


predictions1 <- predict.train(object = model1, test_set)
prediction_df1 <- data.frame(round(predictions))
test_predict1 <- as.factor(prediction_df1$round.predictions.)
confusionMatrix(test_predict1, reference = test_label_factor, positive = '1')

predictions2 <- predict.train(object = model2, test_set)
prediction_df2 <- data.frame(round(predictions))
test_predict2 <- as.factor(prediction_df2$round.predictions.)
confusionMatrix(test_predict2, reference = test_label_factor, positive = '1')

predictions3 <- predict.train(object = model3, test_set)
prediction_df3 <- data.frame(round(predictions))
test_predict3 <- as.factor(prediction_df3$round.predictions.)
confusionMatrix(test_predict3, reference = test_label_factor, positive = '1')


model2 <- train(
  Success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:40,
                         distance = 2:3,
                         kernel = c('rectangular', 'triangular', 'gaussian', 'epanechnikov')
  )
)
plot(model1)



##From the model and plot, we see that k = 22 gives the best result. 
model$bestTune































##Further partition training set into 75% train set and 25% validation set, stratified
library('caret')
inTrain <- createDataPartition(y = training[,'Success_1_to_1'], list = FALSE, p = .75)
train <- training[inTrain,]
training_set <- training[, -1]
validation <- training[-inTrain,]
train_set <- train[, -1]
train_label <- train[, 1]
validation_set <- validation[, -1]
validation_label <- validation[, 1]



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

##Testing with PCA for dimensionality reduction
library('corrplot')
str(training)
df5_cor <- cor(df5)
corrplot(df5_cor)
##Not many strong correlations at first glance. Will try PCA anyways

##Convert datasets using PCA
pca_training <- princomp(training[, -1])
pca_train_set <- princomp(train_set)
pca_validation_set <- princomp(validation_set)
pca_test_set <- princomp(test_set)


##First four components explain 83% of the variance
plot(pca_train_set)
summary(pca_train_set)


##Plot against each component
train_comp <- data.frame(pca_training$scores[, 1:4])
plot(comp, pch = 16)

##Take the first 4 components
training_comp <- pca_training$scores[,1:4]
train_comp <- pca_train_set$scores[,1:4]
validation_comp <- pca_validation_set$scores[,1:4]
test_comp <- pca_test_set$scores[,1:4]

##Find optimal K
k_acc_values <- k_accuracy(train_comp, train_label, validation_comp, validation_label)
plot(k_acc_values, type = 'l')
k_acc_values[k_acc_values$X0.1 == max(k_acc_values$X0.1),]

##Optimal K = 10


##Train model
knn_pca <- knn_predict <- knn(train = training_comp, test = test_comp, cl = training[, 1], k = 10)

##Evaluate
knn_pca_predict <- as.factor(knn_pca)
test_label_factor <- as.factor(test_label)
confusionMatrix(knn_pca_predict, reference = test_label_factor)
##TERRIBLE
