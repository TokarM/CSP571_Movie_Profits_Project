movie <- read.csv('/Users/nick/Desktop/CSP571_Movie_Profits_Project/final_dataset.csv', header = TRUE, stringsAsFactors = FALSE)
head(movie)

#Ranking the quarter variable 
set.seed(123)
library('caret')

#Converting Scuccess_2_to_1 to factor
movie$Success_1_to_1 <- as.factor(movie$Success_1_to_1)

#We'll do stratified sampling to split our data into training and test sets
targetVar <- 'Success_1_to_1'
movieLR <- movie[, c('runtime','ProductionBudget', 'drama', 'action', 'amusement', 'Success_1_to_1')]
inTrain <- createDataPartition(y = movieLR[,targetVar], list = FALSE, p = .8)
trainData <- movieLR[inTrain,]
testData <- movieLR[-inTrain,]
stopifnot(nrow(trainData) + nrow(testData) == nrow(movieLR))

#Logistic Regression on train dataset
logitModel <- glm(Success_1_to_1 ~ .,family=binomial(link='logit'),data=trainData)

save(logitModel, file = '/Users/nick/Desktop/CSP571_Movie_Profits_Project/logit_model.rda')
