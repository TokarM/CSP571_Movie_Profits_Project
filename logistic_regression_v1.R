#Reading data from csv file
movie <- read.csv('D:/3rd Sem Courses/Data Preparation and Analysis/Project/CSP571_Movie_Profits_Project-master/final_dataset.csv', header = TRUE, stringsAsFactors = FALSE)
head(movie)

#movie$ScaledProductionBudget <- scale(movie$ProductionBudget)

#movie$Success_2_to_1 <- 0
#movie[,'Success_2_to_1'][movie$earnings_percent > 0] <- 1

#Ranking the quarter variable 
set.seed(123)
library('caret')
movie$quarter <- ifelse(movie$quarter == "Q1",1, ifelse(movie$quarter =="Q2", 2, ifelse(movie$quarter =="Q3", 3, ifelse(movie$quarter == "Q4", 4,movie$quarter))))
movie$quarter <- as.numeric(movie$quarter)

#Converting Scuccess_2_to_1 to factor
movie$Success_2_to_1 <- as.factor(movie$Success_2_to_1)

#We'll do stratified sampling to split our data into training and test sets
targetVar <- 'Success_2_to_1'
movieLR <- movie[, c('actorRank','runtime','ProductionBudget','quarter', 'drama', 'thriller', 'nonfiction', 'action', 'amusement', 'Success_2_to_1')]
inTrain <- createDataPartition(y = movieLR[,targetVar], list = FALSE, p = .8)
trainData <- movieLR[inTrain,]
testData <- movieLR[-inTrain,]
stopifnot(nrow(trainData) + nrow(testData) == nrow(movieLR))

#Logistic Regression on train dataset
logitModel <- glm(Success_2_to_1 ~ .,family=binomial(link='logit'),data=trainData)

#Predict on test data
predicted <- predict(logitModel, testData, type="response")

#Histogram of prediction
hist(predicted)

#testData[,'predicted'] <- predicted

#Deciding optimal prediction probability cutoff for the model
install.packages("InformationValue")
library(InformationValue)
optCutOff <- optimalCutoff(testData$Success_2_to_1, predicted)[1] 

#Model Diagnostics
summary(logitModel)

#Classification of Success or not.
success.pred <- ifelse(predicted > 0.5,1,0)

optCutOff.success.pred <- ifelse(predicted > optCutOff,1,0)

#Claculating the mean of prediction
mean(success.pred)
mean(optCutOff.success.pred)

# Checking classification accuracy
mean(success.pred == testData$Success_2_to_1)

mean(optCutOff.success.pred == testData$Success_2_to_1)  

#Confusion matrix to evaluate how good our results are
confusionMatrix(testData$Success_2_to_1, success.pred)


confusionMatrix(testData$Success_2_to_1, optCutOff.success.pred)


#Missclassification Error
misClassError(testData$Success_2_to_1, predicted, threshold = optCutOff)

#If we take optcutoff/thershold as 0.5
misClassError(testData$Success_2_to_1, predicted, threshold = 0.5)

#The ROC curve
plotROC(testData$Success_2_to_1, predicted)

#install.packages("ROCR")
#library(ROCR)
#pr <- prediction(predicted, testData$Success_2_to_1)
#prf <- performance(pr, measure = "tpr", x.measure = "fpr")
#plot(prf)

#auc <- performance(pr, measure = "auc")
#auc <- auc@y.values[[1]]
#auc

#Calcuting Concordance
Concordance(testData$Success_2_to_1, predicted)

#Ploting precision recall curves
install.packages('DMwR')
library('DMwR')
PRcurve(preds = predicted, trues = testData$Success_2_to_1)

#The deviance
llcomponents <- function(y, predicted.y){
  return(y*log(predicted.y) + (1-y)*log(1-predicted.y))
}

xVars <- c('actorRank', 'runtime','ProductionBudget', 'quarter', 'drama', 'thriller', 'nonfiction', 'action', 'amusement')
y <- trainData[,targetVar]
predicted.y <- predict(logitModel, newdata = trainData[,xVars], type='response')

deviance <- sign(as.numeric(y) - predicted.y)*sqrt(-2*llcomponents(as.numeric(y), predicted.y))

summary(deviance)

# Extract the AIC
aic<- 2 * length(logitModel$coefficients) - 2*logLik(logitModel)
aic
AIC(logitModel)
