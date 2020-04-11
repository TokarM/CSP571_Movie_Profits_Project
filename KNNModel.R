#KNN

df1 <- read.csv('/Users/Sunny/Github/CSP571_Movie_Profits_Project/final_dataset.csv', stringsAsFactors = T)

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

##Partition into 80%, and 20% test, constrained to its chronological ordering
splitRow <- as.integer(nrow(df5)*0.8)
training <- df5[c(1:splitRow), ]
test <- df5[c((splitRow+1):nrow(df5)), ]
test_set <- test[, -1]
test_label <- test[, 1]

##Further partition training set into 75% train set and 25% validation set, stratified
library('caret')
inTrain <- createDataPartition(y = training[,'Success_2_to_1'], list = FALSE, p = .75)
train <- training[inTrain,]
validation <- training[-inTrain,]
train_set <- train[, -1]
train_label <- train[, 1]
validation_set <- validation[, -1]
validation_label <- validation[, 1]


##Error Check for row equality
stopifnot(nrow(train) + nrow(validation) +nrow(test) == nrow(df5))


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
##K = 32 is best. Afterward, the improvement in accuracy is very little. 


##Using the caret package
library('caret')
model <- train(
  Success_2_to_1 ~., 
  data = training, 
  method = "knn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(k = c(1:100))
)
plot(model)
model$bestTune

predictions <- predict.train(object = model, test_set)
table(predictions)
confusionMatrix(predictions, test_label)

knn_predict <- knn(train = training[, -1], test = test_set, cl = training[, 1], k = 31)

##Evaluate KNN model
knn_predict <- as.factor(knn_predict)
test_label_factor <- as.factor(test_label)
confusionMatrix(knn_predict, reference = test_label_factor)
##TERRIBLE


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
