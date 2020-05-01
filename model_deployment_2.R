
library('plumber')

# Here is a path to actual model, I pushed it git as well
RF_model <- readRDS('/Users/nick/Desktop/CSP571_Movie_Profits_Project/rf_model.rds')
library('randomForest')

name <- "Movie"
budget <- 6000000
runtime <- 100
comedy <- 1
family <- 0
adventure <- 0
fantasy <- 0
drama <- 0
action <- 1
horror <- 0
documentary <- 0
scifi <- 1
actorMovieCount <- 5
directorMovieCount <- 3
directorEarnings <- 300000
domestic <- 0
quarter <- 2
newyearsday <- 0
christmaseve <- 0

#* @post /predict
prediction <- function(name,budget, runtime, comedy, family, adventure, fantasy, drama, action, horror, documentary, scifi, actorMovieCount, directorMovieCount, directorEarnings, domestic, quarter,newyearsday,christmaseve)
  {
  budget <- as.numeric(budget)
  runtime <- as.numeric(runtime)
  comedy <- as.numeric(comedy)
  family <- as.numeric(family)
  adventure <- as.numeric(adventure)
  fantasy <- as.numeric(fantasy)
  drama <- as.numeric(drama)
  action <- as.numeric(action)
  horror <- as.numeric(horror)
  documentary <- as.numeric(documentary)
  scifi <- as.numeric(scifi)
  actorMovieCount <- as.numeric(actorMovieCount)
  directorMovieCount <- as.numeric(directorMovieCount)
  directorEarnings <- as.numeric(directorEarnings)
  domestic <- as.numeric(domestic)
  quarter <- as.numeric(quarter)
  newyearsday <- as.numeric(newyearsday)
  christmaseve <- as.numeric(christmaseve)
  
  df <- data.frame(budget, runtime, comedy, family, adventure, fantasy, drama, action, horror, documentary,
                     scifi, actorMovieCount, directorMovieCount, directorEarnings, domestic, quarter, newyearsday, christmaseve)
  
  yhat <- predict(RF_model, df)
  
  string <- paste("Model prediction is ", yhat)
  if(as.numeric(yhat) == 2){
    string = " is going to be Successfull"
  }
  else
  {
    string = " is not going to be Successfull"
  }
  return(paste(name, string))
}
