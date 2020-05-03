#KNN using elastic net variable set. KNN FINAL MODEL.
library('caret')
set.seed(1000)

##Read in data set with only the elastic net variables remaining
df1_elastic <- read.csv('/Users/Sunny/Github/CSP571_Movie_Profits_Project/elastic_net_final_dataset.csv', stringsAsFactors = T)
df1_elastic <- df1_elastic[2:20]

##Convert classification to factors
df1_elastic$success_1_to_1 <- as.factor(df1_elastic$success_1_to_1)

##Separate the categorical variables
df2_elastic <- df1_elastic[, c(19, 3:11, 15, 17:18)]

##Separate and scale the numerical variables
df3_elastic <- scale(df1_elastic[, c(1:2, 12:14, 16)])

##Recombine the two dataframes
df4_elastic <- cbind(df2_elastic, df3_elastic)

##Partition the train/test set
inTrain <- createDataPartition(y = df4_elastic[,'success_1_to_1'], list = FALSE, p = .8)
train_elastic <- df4_elastic[inTrain,]
test_elastic <- df4_elastic[-inTrain,]

##First model with only k tuning, distance and kernel are set to their defaults. This is for comparision
##against the base model with the full set of variables and with the set of top 15 most significant variables.
model_rectangular_elastic <- train(
  success_1_to_1 ~., 
  data = train_elastic, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:50,
                         distance = 2,
                         kernel = c('rectangular')))
##See the results on the validation set
plot(model_rectangular_elastic)
model_rectangular_elastic$bestTune
max(model_rectangular_elastic$results$Accuracy)

##Train model with tuning on K and kernel
model_kernel_elastic <- train(
  success_1_to_1 ~., 
  data = train_elastic, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:50,
                         distance = 2,
                         kernel = c('rectangular', 'triangular', 'gaussian', 'epanechnikov')))
##See which hyperparameter values did best and what the results on the validation set were
plot(model_kernel_elastic)
model_kernel_elastic$bestTune
mean(model_kernel_elastic$results$Accuracy)

##Train the model using the best K and kernel hyperparameters (found above) and this time tune distance
##Note: This could all tuning could have bee done in one training model but it was divided up into two
##pieces because runtime was over two hours. This makes it more manageable.
model_gaussian_elastic <- train(
  success_1_to_1 ~., 
  data = train_elastic, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 38,
                         distance = c(1:5),
                         kernel = c('gaussian')))
##See which distance value did best and what the results on the validation set were.
plot(model_gaussian_elastic)
model_gaussian_elastic$bestTune
max(model_gaussian_elastic$results$Accuracy)

##Evaluate the final model on the test set and see its performance statistics.
pred_gaussian_elastic <- predict.train(object = model_gaussian_elastic, test_elastic)
confusionMatrix(pred_gaussian_elastic, reference = test_elastic$success_1_to_1, positive = '1')
