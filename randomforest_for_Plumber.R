#install.packages("randomForest")
library(randomForest)
library(caret)
library(e1071)
set.seed(123)

setwd('/Users/nick/Desktop/CSP571_Movie_Profits_Project')
movie <- read.csv("elastic_net_final_dataset.csv", header = TRUE, stringsAsFactors = FALSE)
movie$success_1_to_1 <- as.factor(movie$success_1_to_1)
splitVar <- 'success_1_to_1'

#######classification#########
movieRF <- subset(movie, select = -c(X))
movieRF_inTrain <- createDataPartition(y = movieRF[,splitVar], list = FALSE, p = .8)
movieRF_train <- movieRF[movieRF_inTrain,]
movieRF_test <- movieRF[-movieRF_inTrain,]

#finetune the model parameter
#ntree: 500; mtry: 3 give the best accuracy: 0.7512690 
movieRF_model <- randomForest(success_1_to_1 ~ ., data = movieRF_train, ntree = 500, mtry = 3, importance = TRUE, proximity=TRUE, do.trace = 100)
movieRF_model

#Model creation file
#saveRDS(movieRF_model, '/Users/nick/Desktop/CSP571_Movie_Profits_Project/rf_model.rds')
plot(movieRF_model)

###########classfication###############
predTrain <- predict(movieRF_model, movieRF_train, type = "class")
confusionMatrix(predTrain, movieRF_train$success_1_to_1, positive = "1") 
# Checking classification accuracy
table(predTrain, movieRF_train$success_1_to_1)

predTest <- predict(movieRF_model, movieRF_test)
confusionMatrix(predTest, movieRF_test$success_1_to_1, positive = "1") 
hist(treesize(movieRF_model1))

# Checking classification accuracy
mean(predTest == movieRF_test$success_1_to_1)             

#F-score: 0.7197232
precision(predTest, movieRF_test$success_1_to_1, relevant = levels(movieRF_test$success_1_to_1)[2]) 
recall(predTest, movieRF_test$success_1_to_1, relevant = levels(movieRF_test$success_1_to_1)[2]) 
F_meas(predTest, movieRF_test$success_1_to_1, relevant = levels(movieRF_test$success_1_to_1)[2]) 
?precision

require(pROC)
rf.roc<-roc(movieRF_train$success_1_to_1,movieRF_model$votes[,2])
plot(rf.roc)

importance(movieRF_model)
varImpPlot(movieRF_model)
#IncMSE: tests how worse the model performs without each variable (higher rank means more important)
#IncNodePurity: measures how pure the nodes at the end of the tree without each variable (higher rank means higher contribution as paramters)


#Plot variable importance (based on MeanDecreaseGini)
library(dplyr)
library(ggplot2)

var_importance <- data.frame(variable=setdiff(colnames(movieRF_train),'success_1_to_1'),
                             importance=as.vector(importance(movieRF_model)[,4]))

var_importance <- arrange(var_importance, importance)
var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)

p <- ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
p <- p + geom_bar() + ggtitle("Top 20 Variables Importance from Random Forest")
p <- p + xlab("Variable Importance (Mean Decrease in Gini Index)") + ylab("Movie features") 
p <- p + scale_fill_discrete(name="Variable Name")
p <- p + coord_flip()
p <- p + theme(plot.title = element_text(hjust = 0.5))
p + theme(axis.text.x=element_blank(),
          axis.text.y=element_text(size=18),
          axis.title=element_text(size=16),
          plot.title=element_text(size=20),
          legend.title=element_text(size=15),
          legend.text=element_text(size=12))

#multi-dimensional scaling plot of proximity matrix
MDSplot(movieRF_model, movieRF_test$success_1_to_1)

#function to find the best paramaters
#hyperparameter tuning - mtry
xVars <- colnames(movieRF)
regVar <- 'success_1_to_1'
xVars <- xVars[!xVars %in% regVar]

rfCV<- train(x = movieRF_train[,xVars]
               , y = movieRF_train[,splitVar]
               , method = "rf",
               tuneLength=18
)
rfCV
plot(rfCV, main = "Random Forest mtry Selection")

#hyperparameter tuning - ntree
set.seed(123)
b=c(100,200,300,400,500,1000,1500,2000,2500)
a=c()
tuningDF <- data.frame()
for (j in c(1:10)){
  for (i in 1:length(b)) {
    print(b[i])
    model3 <- randomForest(success_1_to_1 ~ ., data = movieRF_train, ntree = b[i], mtry = 3, importance = TRUE)
    predTest <- predict(model3, movieRF_test, type = "class")
    a[i] = mean(predTest == movieRF_test$success_1_to_1)
  }
  tuningDF <- tuningDF(trialDF, a)
}
trialDF <- addmargins(trialDF, 2, mean)
trialDF

# Random Search for hyperparamter
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
mtry <- sqrt(ncol(movieRF))
rf_random <- train(success_1_to_1~., data=movieRF_train, method="rf", metric='Accuracy', tuneLength=18, trControl=control)
print(rf_random)
plot(rf_random)

#variable selection in random forest
#method 1: VSURF
install.packages('VSURF')
library(VSURF)
set.seed(123)
movieVar <- VSURF(success_1_to_1 ~., data = movieRF_train, na.action = na.omit)
summary(movieVar)
plot(movieVar)
plot(movieVar, step = 'thres', imp.sd = F, var.names = T)
movieVar$varselect.thres

str(movieVar)
#movieVar$terms
varChoice <- c( 'budget', 'runtime', 'animation', 'comedy', 'family', 
                'adventure', 'fantasy', 'drama', 'romance', 'action', 'crime', 'thriller', 
                'history', 'mystery', 'music', 'horror', 'war', 'documentary', 'western', 
                'scifi', 'actorMovieCount', 'directorMovieCount', 'directorEarnings', 
                'domestic', 'quarter', 'numOfHolidays', 'newyearsday', 'martinlutherkingjrday', 
                'valentinesday', 'presidentsday', 'goodfriday', 'mothersday', 'memorialday', 
                'fathersday', 'independenceday', 'laborday', 'columbusday', 'veteransday', 
                'thanksgivingday', 'christmaseve', 'christmasday', 'independencedayobserved', 
                'christmasdayobserved', 'newyearsdayobserved', 'christmaseveobserved', 
                'veteransdayobserved')

varChoice[movieVar$varselect.thres] 
varChoice[movieVar$varselect.interp]
varChoice[movieVar$varselect.pred]

#Method 2: permutation variable selection in random forest
install.packages('vita')
library(vita)
system.time(pimp.varImp.cl<-PIMP(movieRF_train, movieRF_train$success_1_to_1,movieRF_model, parallel=TRUE, ncores=4))
varMX <- pimp.varImp.cl[[2]]
varPerm <- data.frame()
for (i in 1:nrow(varMX)){
  varPerm <- rbind(varPerm, addmargins(varMX, 2, mean)[i,101])
}
varPerm <- cbind(rownames(varMX), varPerm)
varPerm

