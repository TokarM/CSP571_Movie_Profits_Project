# Start the server

library(plumber)
r <- plumb("/Users/nick/Desktop/CSP571_Movie_Profits_Project/model_deployment_2.R")  # Where 'plumber.R' is the location of the file shown above
r$run(port=8000)

# curl --data "runtime=100&ProductionBudget=600000&drama=0&action=1&amusement=0" "http://localhost:8000/predict"