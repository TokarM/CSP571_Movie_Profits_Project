#Reading data from csv file
movie <- read.csv('/Users/swathi/CSP571_Movie_Profits_Project/5_merged_with_holidays.csv', header = TRUE, stringsAsFactors = FALSE)
head(movie)

#movie$ScaledProductionBudget <- scale(movie$ProductionBudget)

#Ranking the quarter variable 
set.seed(123)
library('caret')
#movie$quarter <- ifelse(movie$quarter == "Q1",1, ifelse(movie$quarter =="Q2", 2, ifelse(movie$quarter =="Q3", 3, ifelse(movie$quarter == "Q4", 4,movie$quarter))))
#movie$quarter <- as.numeric(movie$quarter)

#Checking for outliers
#boxplot(movie$revenue)
#boxplot(movie$budget)
#boxplot(movie$runtime)
#boxplot(movie$directorEarnings)

#Removing outliers
#movie <- movie[-which(movie$revenue %in% boxplot(movie$revenue, plot = FALSE)$out),]
#movie <- movie[-which(movie$budget %in% boxplot(movie$budget, plot = FALSE)$out),]
#movie <- movie[-which(movie$runtime %in% boxplot(movie$runtime, plot = FALSE)$out),]
#movie <- movie[-which(movie$directorEarnings %in% boxplot(movie$directorEarnings, plot = FALSE)$out),]

#Converting Scuccess_1_to_1 to factor
movie$success_1_to_1 <- as.factor(movie$success_1_to_1)

#Scaling
df_continuous <- movie[c("runtime", "revenue", "budget", "directorEarnings")]
df_scaled_cont <- scale(df_continuous) 
movie[c("runtime", "revenue", "budget", "directorEarnings")] <- df_scaled_cont

##Convert date column and sort by date
movie$release_date <- as.Date(movie$release_date)
movie <- movie[order(movie$release_date), -1]

#We'll do stratified sampling to split our data into training and test sets
targetVar <- 'success_1_to_1'
movieLR <- movie[, c('release_date', 'revenue', 'budget', 'runtime', 'animation', 'comedy', 'family', 'adventure', 'fantasy', 'drama', 'romance', 'action', 'crime', 'thriller', 'history', 'mystery', 'music', 'horror', 'war', 'documentary', 'western', 'scifi', 'actorMovieCount', 'directorMovieCount', 'domestic', 'quarter', 'success_1_to_1', 'numOfHolidays', 'newyearsday', 'martinlutherkingjrday', 'valentinesday', 'presidentsday', 'goodfriday', 'mothersday', 'memorialday', 'fathersday', 'independenceday', 'laborday', 'columbusday', 'veteransday', 'thanksgivingday', 'christmaseve', 'christmasday', 'independencedayobserved', 'christmasdayobserved', 'newyearsdayobserved', 'christmaseveobserved')]

inTrain <- createDataPartition(y = movieLR[,targetVar], list = FALSE, p = .8)
trainData <- movieLR[inTrain,]
testData <- movieLR[-inTrain,]
stopifnot(nrow(trainData) + nrow(testData) == nrow(movieLR))

#Logistic Regression on train dataset
logitModel <- glm(success_1_to_1 ~ .,family=binomial(link='logit'),data=trainData, maxit = 100)

#Predict on test data
predicted <- predict(logitModel, testData, type="response")

#Histogram of prediction
hist(predicted)


#Deciding optimal prediction probability cutoff for the model
#install.packages("InformationValue")
library(InformationValue)
optCutOff <- optimalCutoff(testData$success_1_to_1, predicted)[1] 
print(paste0("Optimal cutoff for the model: ", optCutOff))

#Model Diagnostics
summary(logitModel)

#Finding significant variables
importance <- varImp(logitModel, scale = FALSE)
print(importance)

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
check_acc <- as.data.frame(testData$success_1_to_1)
check_acc['actual'] <- as.data.frame(testData$success_1_to_1)
check_acc['prediction'] <- as.factor(success.pred)
caret::confusionMatrix(data=check_acc$prediction, reference=check_acc$actual)

check_acc['opt_prediction'] <- as.factor(optCutOff.success.pred)
caret::confusionMatrix(data=check_acc$opt_prediction, reference=check_acc$actual)

#Missclassification Error
misClassError(testData$success_1_to_1, predicted, threshold = optCutOff)

#If we take optcutoff/thershold as 0.5
misClassError(testData$success_1_to_1, predicted, threshold = 0.5)

#Precision, Recall and F1-Score claculation for thershold 0.5
precision_0.5 <- specificity(testData$success_1_to_1, predicted, threshold = 0.5)
print(paste0("Precision for 0.5 thershold: ", precision_0.5))

recall_0.5 <- sensitivity(testData$success_1_to_1, predicted, threshold = 0.5)
print(paste0("Recall for 0.5 thershold: ", recall_0.5))

F1_score_0.5 <- (2 * precision_0.5 * recall_0.5)/(precision_0.5 + recall_0.5)
print(paste0("F1-Score for 0.5 thershold: ", F1_score_0.5))


#Precision, Recall and F1-Score claculation for thershold optCutOff
precision_optCutOff <- specificity(testData$success_1_to_1, predicted, threshold = optCutOff)
print(paste0("Presicion for optimal cutoff thershold: ", precision_optCutOff))

recall_optCutOff <- sensitivity(testData$success_1_to_1, predicted, threshold = optCutOff)
print(paste0("Recall for optimal cutoff thershold: ", recall_optCutOff))

F1_score_optCutOff <- (2 * precision_optCutOff * recall_optCutOff)/(precision_optCutOff + recall_optCutOff)
print(paste0("F1-Score for optimal cutoff thershold: ", F1_score_optCutOff))

#The ROC curve
plotROC(testData$success_1_to_1, predicted)

#install.packages("ROCR")
#library(ROCR)
#pr <- prediction(predicted, testData$Success_2_to_1)
#prf <- performance(pr, measure = "tpr", x.measure = "fpr")
#plot(prf)

#auc <- performance(pr, measure = "auc")
#auc <- auc@y.values[[1]]
#auc

#Calcuting Concordance
Concordance(testData$success_1_to_1, predicted)

#Ploting precision recall curves
#install.packages('DMwR')
library('DMwR')
PRcurve(preds = predicted, trues = testData$success_1_to_1)

#The deviance
llcomponents <- function(y, predicted.y){
  return(y*log(predicted.y) + (1-y)*log(1-predicted.y))
}

xVars <- c('release_date', 'revenue', 'budget', 'runtime', 'animation', 'comedy', 'family', 'adventure', 'fantasy', 'drama', 'romance', 'action', 'crime', 'thriller', 'history', 'mystery', 'music', 'horror', 'war', 'documentary', 'western', 'scifi', 'actorMovieCount', 'directorMovieCount', 'domestic', 'quarter', 'success_1_to_1', 'numOfHolidays', 'newyearsday', 'martinlutherkingjrday', 'valentinesday', 'presidentsday', 'goodfriday', 'mothersday', 'memorialday', 'fathersday', 'independenceday', 'laborday', 'columbusday', 'veteransday', 'thanksgivingday', 'christmaseve', 'christmasday', 'independencedayobserved', 'christmasdayobserved', 'newyearsdayobserved', 'christmaseveobserved')
y <- trainData[,targetVar]
predicted.y <- predict(logitModel, newdata = trainData[,xVars], type='response')

deviance <- sign(as.numeric(y) - predicted.y)*sqrt(-2*llcomponents(as.numeric(y), predicted.y))

summary(deviance)

# Extract the AIC
aic<- 2 * length(logitModel$coefficients) - 2*logLik(logitModel)
aic
AIC(logitModel)

