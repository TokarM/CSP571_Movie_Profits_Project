# Start the server
#Run these lines to start a server, do not forgot to link it with model_deployment 2 file
library('plumber')
r <- plumb("/Users/nick/Desktop/CSP571_Movie_Profits_Project/model_deployment_2.R")  # Where 'plumber.R' is the location of the file shown above
r$run(port=8000)

# After starting a server run the following line in terminal

#curl --data "budget=2500000&runtime=118&comedy=1&family=0&adventure=0&fantasy=0&drama=1&action=0&horror=0&documentary=0&scifi=0&actorMovieCount=0&directorMovieCount=1&directorEarnings=16178959&domestic=0&quarter=3&newyearsday=0&christmaseve=0" "http://localhost:8000/predict"


# Do not worry about this part

# curl --data "budget=runtime=100&ProductionBudget=600000&drama=0&action=1&amusement=0" "http://localhost:8000/predict"

new_movies <- read.csv("/Users/nick/Desktop/CSP571_Movie_Profits_Project/new_movie_2019.csv")

generate_curl = function(row){
  paste("curl --data ",'"name=',row$original_title, "&budget=",row$budget,"&runtime=",row$runtime,"&comedy=",row$comedy,"&family=",row$family,"&adventure=", row$adventure,"&fantasy=", row$fantasy,"&drama=", row$drama,
        "&action=", row$action, "&horror=", row$horror, "&documentary=", row$documentary,"&scifi=", row$scifi, "&actorMovieCount=", row$actorMovieCount, "&directorMovieCount=", row$directorMovieCount,
        "&directorEarnings=", row$directorEarnings, "&domestic=", row$domestic, "&quarter=", row$quarter, "&newyearsday=", row$newyearsday, "&christmaseve=", row$christmaseve,'" "http://localhost:8000/predict"', sep='')
}

generate_curl(new_movies[1:10,])


