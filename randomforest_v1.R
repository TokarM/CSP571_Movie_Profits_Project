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
movieRF_train <- movieRF[movieRF_inTrain,]
movieRF_test <- movieRF[-movieRF_inTrain,]

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

predTrain <- predict(movieRF_model2, movieRF_train, type = "class")
confusionMatrix(predTrain, movieRF_train$Success_2_to_1)
# Checking classification accuracy
table(predTrain, movieRF_train$Success_2_to_1)
#predTrain   0   1
#        0 693  37
#        1   0 179

plot(movieRF_model2)
tune <- tuneRF(movieRF_train[,-10], movieRF_train[,10], stepFactor = 0.05, plot = TRUE, ntreeTry = 200, trace = TRUE, improve = 0.05)

predTest <- predict(movieRF_model2, movieRF_test, type = "class")
confusionMatrix(predTest, movieRF_test$Success_2_to_1)
hist(treesize(movieRF_model2))
     
# Checking classification accuracy
mean(predTest == movieRF_test$Success_2_to_1)             
#0.7444934 (accuracy)
table(predTest,movieRF_test$Success_2_to_1)
#predTest   0   1
#       0 166  51
#       1   7   3

importance(movieRF_model2)        
varImpPlot(movieRF_model2)
#                          0           1 MeanDecreaseAccuracy MeanDecreaseGini
#actorRank        -7.1781476  3.79517865          -4.59061668        13.828418
#runtime          -0.0378661 -0.08332498          -0.03114362        80.485687
#ProductionBudget 12.8477950 10.88192729          17.14969898        92.994194
#quarter          -3.0645311 -2.65150970          -4.03158775        25.012813
#drama            11.1042583 -2.12511177           9.18549258        10.600449
#thriller         -1.6064444  7.28097621           2.15491026         8.854674
#nonfiction       -2.7103323  5.01407009           0.70272315         7.066349
#action           -1.3137347  8.31262629           2.67185665         9.437411
#amusement        -0.6588842 -4.16097994          -2.83307362        10.055188

#IncMSE: tests how worse the model performs without each variable (higher rank means more important)
#IncNodePurity: measures how pure the nodes at the end of the tree without each variable (higher rank means higher contribution as paramters)

varUsed(movieRF_model2)
#actorRank runtime ProductionBudget quarter drama thriller nonfiction action amusement
#12596     45913   48058            21968   9367  7917     7287       5247   10553

#partial dependence plot
partialPlot(movieRF_model2, movieRF_train, actorRank, "1")
partialPlot(movieRF_model2, movieRF_train, runtime, "1")
partialPlot(movieRF_model2, movieRF_train, ProductionBudget, "1")
partialPlot(movieRF_model2, movieRF_train, quarter, "1")
partialPlot(movieRF_model2, movieRF_train, drama, "1")
partialPlot(movieRF_model2, movieRF_train, thriller, "1")
partialPlot(movieRF_model2, movieRF_train, nonfiction, "1")
partialPlot(movieRF_model2, movieRF_train, action, "1")
partialPlot(movieRF_model2, movieRF_train, amusement, "1")

#read single tree
getTree(movieRF_model2, 1, labelVar = TRUE)

#multi-dimensional scaling plot of proximity matrix
MDSplot(movieRF_model2, movieRF_train$Success_2_to_1)

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