install.packages("randomForest")
library(randomForest)
movie <- read.csv('/Users/kayinho/Documents/CSP571_Movie_Profits_Project/final_dataset.csv', header = TRUE, stringsAsFactors = FALSE)
head(movie)
set.seed(123)
library('caret')
movie$quarter <- ifelse(movie$quarter == "Q1",1, ifelse(movie$quarter =="Q2", 2, ifelse(movie$quarter =="Q3", 3, ifelse(movie$quarter == "Q4", 4,movie$quarter))))
movie$quarter <- as.numeric(movie$quarter)
movie$Success_2_to_1 <- as.factor(movie$Success_2_to_1)

splitVar <- 'Success_2_to_1'

movieRF <- movie[,c('actorRank', 'runtime','ProductionBudget', 'quarter', 'drama', 'thriller', 'nonfiction', 'action', 'amusement', 'Success_2_to_1')]
movieRF_inTrain <- createDataPartition(y = movieRF[,splitVar], list = FALSE, p = .8)
movieRF_train <- movieRF[movie_inTrain,]
movieRF_test <- movieRF[-movie_inTrain,]

movieRF_model1 <- randomForest(Success_2_to_1 ~ ., data = movieRF_train, importance = TRUE, proximity=TRUE, do.trace = 100)
movieRF_model1
#Call:
#  randomForest(formula = Success_2_to_1 ~ ., data = movieRF_train,      importance = TRUE, proximity = TRUE, do.trace = 100) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 3

#OOB estimate of  error rate: 23.76%
#Confusion matrix:
#  0  1 class.error
#0 678 25  0.03556188
#1 191 15  0.92718447 <- high error

#finetune the model parameter
#ntree: 1000; mtry: 3 give the best accuracy: 0.7048458
movieRF_model2 <- randomForest(Success_2_to_1 ~ ., data = movieRF_train, ntree = 1000, mtry = 3, importance = TRUE, proximity=TRUE, do.trace = 100)
movieRF_model2
#Call:
#  randomForest(formula = Success_2_to_1 ~ ., data = movieRF_train,      ntree = 1000, mtry = 3, importance = TRUE, proximity = TRUE,      do.trace = 100) 
#Type of random forest: classification
#Number of trees: 1000
#No. of variables tried at each split: 3

#OOB estimate of  error rate: 23.1%
#Confusion matrix:
#  0  1 class.error
#0 682 21  0.02987198
#1 189 17  0.91747573 <- high error


predTrain <- predict(movie_model2, movieRF_train, type = "class")
# Checking classification accuracy
table(predTrain, movieRF_train$Success_2_to_1)
#predTrain             0 1
#0.00634250826014772 1 0
#0.00689442990343765 1 0
#0.00696027229758088 1 0
#0.00713663629450846 1 0
#...
#Don't know why the rows are not {0,1}

predTest <- predict(movie_model2, movieRF_test, type = "class")
# Checking classification accuracy
mean(predTest == movie_test$Success_2_to_1)   #0 <- most movies are not successful?              
table(predTest,movie_test$Success_2_to_1)
#same problem as above
#predTest             0 1
#0.0118822864096765 0 1
#0.0211945533111119 1 0
#0.0242563201430816 1 0
#0.0253642016606116 1 0
#...

importance(movie_model2)        
varImpPlot(movie_model2)
#                     IncMSE IncNodePurity
#actorRank        -3.050793      5.136868
#runtime          11.938531     34.108992
#ProductionBudget 40.712307     39.409708
#quarter          -0.110570     10.004709
#drama            13.435345      4.284179
#thriller          4.140650      3.483759
#nonfiction        6.542595      3.117175
#action           14.151061      3.924420
#amusement         6.972070      4.071059
#Any idea how to interpret it?

#function to find the best paramaters
a=c()
i=5
for (i in 3:9) {
  model3 <- randomForest(Success_2_to_1 ~ ., data = movieRF_train, ntree = 1000, mtry = i, importance = TRUE)
  predTest <- predict(model3, movieRF_test, type = "class")
  a[i-2] = mean(predTest == movieRF_test$Success_2_to_1)
}

a
plot(3:9,a)