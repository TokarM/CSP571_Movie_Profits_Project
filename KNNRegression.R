df1_elastic <- read.csv('/Users/Sunny/Github/CSP571_Movie_Profits_Project/elastic_net_final_dataset.csv', stringsAsFactors = T)
df1_elastic <- df1_elastic[2:20]

##Convert classification to factors
df1_elastic$success_1_to_1 <- as.factor(df1_elastic$success_1_to_1)

df2_elastic <- df1_elastic[, c(19, 3:11, 15, 17:18)]
df3_elastic <- scale(df1_elastic[, c(1:2, 12:14, 16)])
df4_elastic <- cbind(df2_elastic, df3_elastic)

inTrain <- createDataPartition(y = df4_elastic[,'success_1_to_1'], list = FALSE, p = .8)
train_elastic <- df4_elastic[inTrain,]
test_elastic <- df4_elastic[-inTrain,]

model_rectangular_elastic <- train(
  success_1_to_1 ~., 
  data = train_elastic, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:50,
                         distance = 2,
                         kernel = c('rectangular')))
##K=15 is optimal
plot(model_rectangular_elastic)
model_rectangular_elastic$bestTune
max(model_rectangular_elastic$results$Accuracy)

model_kernel_elastic <- train(
  success_1_to_1 ~., 
  data = train_elastic, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:50,
                         distance = 2,
                         kernel = c('rectangular', 'triangular', 'gaussian', 'epanechnikov')))
plot(model_kernel_elastic)
model_kernel_elastic$bestTune
mean(model_kernel_elastic$results$Accuracy)

model_gaussian_elastic <- train(
  success_1_to_1 ~., 
  data = train_elastic, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 38,
                         distance = c(1:5),
                         kernel = c('gaussian')))
plot(model_gaussian_elastic)
model_gaussian_elastic$bestTune
mean(model_gaussian_elastic$results$Accuracy)

pred_gaussian_elastic <- predict.train(object = model_gaussian_elastic, test_elastic)
confusionMatrix(pred_gaussian_elastic, reference = test_elastic$success_1_to_1, positive = '1')