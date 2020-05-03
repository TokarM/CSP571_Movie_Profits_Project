#install.packages("InformationValue")
#install.packages('DMwR')
set.seed(123)
library('caret')
library('tidyverse')
library(e1071)
library(InformationValue)
library('DMwR')
library(MASS)


#Reading data from csv file
movie <- read.csv('/Users/swath/CSP571_Movie_Profits_Project/elastic_net_final_dataset.csv', header = TRUE, stringsAsFactors = FALSE)
head(movie)
movie$X <- NULL
movie$X.1 <- NULL


#Scaling
df_continuous <- movie[c("runtime", "budget")]
df_scaled_cont <- scale(df_continuous) 
movie[c("runtime", "budget")] <- df_scaled_cont


#We'll do stratified sampling to split our data into training and test sets
targetVar <- 'success_1_to_1'

inTrain <- createDataPartition(y = movie[,targetVar], list = FALSE, p = .8)
trainData <- movie[inTrain,]
testData <- movie[-inTrain,]
stopifnot(nrow(trainData) + nrow(testData) == nrow(movie))


#Logistic Regression on train dataset
#logitModel <- glm(success_1_to_1 ~ .,family=binomial(link='logit'),data=trainData) %>% stepAIC(trace=FALSE)
logitModel <- glm(success_1_to_1 ~ .,family=binomial(link='logit'),data=trainData)

#Model Diagnostics
summary(logitModel)

#Predict on test data
predicted <- predict(logitModel, testData, type="response")

#Histogram of prediction
hist(predicted)


#Deciding optimal prediction probability cutoff for the model
optCutOff <- optimalCutoff(testData$success_1_to_1, predicted)[1] 
print(paste0("Optimal cutoff for the model: ", optCutOff))

#Classification of Success or not.
success.pred <- ifelse(predicted > 0.5,1,0)

optCutOff.success.pred <- ifelse(predicted > optCutOff,1,0)

#Claculating the mean of prediction
mean(success.pred)
mean(optCutOff.success.pred)

# Checking classification accuracy
mean(success.pred == testData$success_1_to_1)

mean(optCutOff.success.pred == testData$success_1_to_1)  

#Confusion matrix to evaluate how good our results are
check_acc <- as.data.frame(as.factor(testData$success_1_to_1))
check_acc['actual'] <- as.data.frame(as.factor(testData$success_1_to_1))
check_acc['prediction'] <- as.factor(success.pred)
caret::confusionMatrix(data=check_acc$prediction, reference=check_acc$actual, positive = "1")


check_acc['opt_prediction'] <- as.factor(optCutOff.success.pred)
caret::confusionMatrix(data=check_acc$opt_prediction, reference=check_acc$actual,  positive = "1")


#Missclassification Error
misClassError(testData$success_1_to_1, predicted, threshold = optCutOff)

#If we take optcutoff/thershold as 0.5
misClassError(testData$success_1_to_1, predicted, threshold = 0.5)

#Precision, Recall and F1-Score claculation for thershold 0.5
confusion_0.5 <- caret::confusionMatrix(data=check_acc$prediction, reference=check_acc$actual, positive = "1")
print(paste0("Precision for 0.5 thershold: ", confusion_0.5$byClass[5]))

print(paste0("Recall for 0.5 thershold: ", confusion_0.5$byClass[6]))

print(paste0("F1-Score for 0.5 thershold: ", confusion_0.5$byClass[7]))


#Precision, Recall and F1-Score claculation for thershold optCutOff
confusion_optCutoff <- caret::confusionMatrix(data=check_acc$opt_prediction, reference=check_acc$actual, positive = "1")
print(paste0("Presicion for optimal cutoff thershold: ", confusion_optCutoff$byClass[5]))

print(paste0("Recall for optimal cutoff thershold: ", confusion_optCutoff$byClass[6]))

print(paste0("F1-Score for optimal cutoff thershold: ", confusion_optCutoff$byClass[7]))


#The ROC curve
plotROC(testData$success_1_to_1, predicted)


#Calcuting Concordance
Concordance(testData$success_1_to_1, predicted)

#Ploting precision recall curves

PRcurve(preds = predicted, trues = testData$success_1_to_1)

#The deviance
llcomponents <- function(y, predicted.y){
  return(y*log(predicted.y) + (1-y)*log(1-predicted.y))
}

xVars <- names(movie)
y <- trainData[,targetVar]
predicted.y <- predict(logitModel, newdata = trainData[,xVars], type='response')

deviance <- sign(as.numeric(y) - predicted.y)*sqrt(-2*llcomponents(as.numeric(y), predicted.y))

summary(deviance)

# Extract the AIC
aic<- 2 * length(logitModel$coefficients) - 2*logLik(logitModel)
aic
AIC(logitModel)