#KNN using the full set of variables. NOT OUR FINAL KNN MODEL.
library('caret')
set.seed(1000)

df1 <- read.csv('/Users/Sunny/Github/CSP571_Movie_Profits_Project/5_merged_with_holidays.csv', stringsAsFactors = T)


##Convert classification to factors
df1$success_1_to_1 <- as.factor(df1$success_1_to_1)

##Extract categorical variables and other variables that we do not want to standardize
df2 <- df1[,c(32, 7:24, 30, 34:53)]

##Standardize numerical columns
df3 <- scale(df1[, c(5:6, 27:29, 31, 33)])

##Merge the two dataframes
df4 <- cbind(df2, df3)

##Partition into 80%, and 20% test
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

##Rectangular / Non-weighted
model_rectangular <- train(
  success_1_to_1 ~., 
  data = train, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:50,
                         distance = 2,
                         kernel = c('rectangular')))
##See best model and its results on the validation set
plot(model_rectangular)
model_rectangular$bestTune
max(model_rectangular$results$Accuracy)
