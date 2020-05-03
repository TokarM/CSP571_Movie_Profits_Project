movie <- read.csv('/Users/nick/Desktop/CSP571_Movie_Profits_Project/5_merged_with_holidays.csv', header = TRUE, stringsAsFactors = FALSE)
head(movie)

boxplot(movie$runtime)
id1 <- boxplot.stats(movie$runtime, 2)
id1$stats

movie <- movie[which(movie$runtime < 163),]
movie <- movie[which(movie$runtime > 53),]

boxplot(movie$budget)
id2 <- boxplot.stats(movie$budget, 2.0)
id2$stats

movie <- movie[which(movie$budget < 57000000),]
movie <- movie[which(movie$budget > 1),]

#movie['EarningPercent'] <- (movie$revenue - movie$budget) / movie$budget
#movie$directorEarnings <- NULL

movie$X <- NULL
movie$id <- NULL
movie$release_date <- NULL
movie$Actor <- NULL
movie$Director <- NULL
movie$revenue <- NULL
movie$related_holiday <- NULL


#Variable Selection

library('caret')
library('tidyverse')
library('glmnet')
library('foreach')
library('pROC')
library('doParallel')
registerDoParallel(cores = 4)

targetVar <- 'success_1_to_1'

inTrain <- createDataPartition(y = movie[,targetVar], list = FALSE, p = .8)
trainData <- movie[inTrain,]
testData <- movie[-inTrain,]
stopifnot(nrow(trainData) + nrow(testData) == nrow(movie))

mdlY <- as.factor(as.matrix(trainData[targetVar]))
mdlX <- as.matrix(trainData[setdiff(colnames(movie), c(targetVar))])

newY <- as.factor(as.matrix(testData[targetVar]))
newX <- as.matrix(testData[setdiff(colnames(movie), c(targetVar))])

#LASSO
cv1 <- cv.glmnet(mdlX, mdlY, family = "binomial", nfold = 10, type.measure = "deviance", paralle = TRUE, alpha = 1)
md1 <- glmnet(mdlX, mdlY, family = "binomial", lambda = cv1$lambda.1se, alpha = 1)
coef(md1)

roc(newY, as.numeric(predict(md1, newX, type = "response")))

#RIDGE
cv2 <- cv.glmnet(mdlX, mdlY, family = "binomial", nfold = 10, type.measure = "deviance", paralle = TRUE, alpha = 0)
md2 <- glmnet(mdlX, mdlY, family = "binomial", lambda = cv2$lambda.1se, alpha = 0)
coef(md2)
roc(newY, as.numeric(predict(md2, newX, type = "response")))

#ELASTIC NET WITH 0 < ALPHA < 1
a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
  cv <- cv.glmnet(mdlX, mdlY, family = "binomial", nfold = 10, type.measure = "deviance", paralle = TRUE, alpha = i)
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.1se], lambda.1se = cv$lambda.1se, alpha = i)
}
cv3 <- search[search$cvm == min(search$cvm), ]
md3 <- glmnet(mdlX, mdlY, family = "binomial", lambda = cv3$lambda.1se, alpha = cv3$alpha)
coef(md3)

roc(newY, as.numeric(predict(md3, newX, type = "response")))

imp_feature = movie[c('budget', 'runtime', 'comedy', 'family', 'adventure', 'fantasy', 'drama','action', 'horror','documentary', 'scifi', 'actorMovieCount', 'directorMovieCount', 'directorEarnings',
                      'domestic', 'quarter', 'newyearsday', 'christmaseve', 'success_1_to_1')]

write.csv(imp_feature, '/Users/nick/Desktop/CSP571_Movie_Profits_Project/elastic_net_final_dataset.csv')


