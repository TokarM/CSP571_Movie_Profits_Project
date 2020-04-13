library('plumber')

load(file = '/Users/nick/Desktop/CSP571_Movie_Profits_Project/logit_model.rda')


runtime <- 100
ProductionBudget <- 6000000
drama <- 0
action <- 1
amusement <- 1


#* @post /predict
predict <- function(runtime, ProductionBudget, drama, action, amusement){
  runtime <- as.numeric(runtime)
  ProductionBudget <- as.numeric(ProductionBudget)
  drama <- as.numeric(drama)
  action <- as.numeric(action)
  amusement <- as.numeric(amusement)
  
  data <- data.frame(runtime, ProductionBudget, drama, action, amusement)
  yhat <-predict.glm(logitModel, data, type="response")
  optCutOff <- 0.4906
  pred <- ifelse(yhat > optCutOff,1,0)
  string <- paste("Probability of success is ", yhat, " , Model prediction is ", pred)
  return(string)
}