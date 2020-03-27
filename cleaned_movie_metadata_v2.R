install.packages('tidyr')
library('tidyr')
movieMaster <- read.csv('/Users/Sunny/Github/CSP571_Movie_Profits_Project/movies_metadata.csv'
              ,header = T, sep = ",", stringsAsFactors = F)
movieMaster <- movieMaster[,c('id','original_title','genres','release_date','revenue','budget','runtime')]
head(movieMaster, 1)

##Convert date and limit movies to those released between 2000 and 2017
movieMaster$release_date <- as.Date(movieMaster$release_date)
movieMaster <- movieMaster[(movieMaster$release_date > "1999-12-31"),]
movieMaster <- movieMaster[(movieMaster$release_date < "2018-01-01"),]


##List of all unique genres
library(stringr)

extract_genre = function(x, output) {
  str_match_all(x, "name': '(.*?)'")
}

list_of_genres <- sapply(movieMaster$genres, FUN = extract_genre)
all_genres_df <- as.data.frame(list_of_genres[[1]], stringsAsFactors = F)
for (i in 2:length(list_of_genres)){
  all_genres_df <- rbind(all_genres_df, 
  as.data.frame(list_of_genres[[i]], stringsAsFactors = F))
}

all_genres_df <- na.omit(all_genres_df)
unique_genres <- unique(all_genres_df$V2)
unique_genres

##Grouping the genres into broader categories, to be used as dummy variables
drama <- c("Drama", "Romance", "Crime", "Mystery")
thriller <- c("Horror", "Thriller", "Mystery")
nonfiction <- c("Documentary", "Music", "History", "War")
action <- c("Action", "Adventure", "Western", "War")
amusement <- c("Family", "Science Fiction", "Fantasy", "Adventure", "Animation", "Comedy")

##Dummy Variables for Genres
movieMaster$drama <- 0
movieMaster$thriller <- 0
movieMaster$nonfiction <- 0
movieMaster$action <- 0
movieMaster$amusement <- 0

##Setting the Dummy Variables to 1 based on the movie's genres

##Drama Dummy Variable
for (row in 1:length(movieMaster)){
  for (genre in drama){
    if (grepl(genre, movieMaster$genres[[row]])){
      movieMaster$drama[row] <- 1
      break
    }
  }
}

##Thriller Dummy Variable
for (row in 1:length(movieMaster)){
  for (genre in thriller){
    if (grepl(genre, movieMaster$genres[[row]])){
      movieMaster$thriller[row] <- 1
      break
    }
  }
}

##Nonfiction Dummy Variable
for (row in 1:length(movieMaster)){
  for (genre in nonfiction){
    if (grepl(genre, movieMaster$genres[[row]])){
      movieMaster$nonfiction[row] <- 1
      break
    }
  }
}

##Action Dummy Variable
for (row in 1:length(movieMaster)){
  for (genre in action){
    if (grepl(genre, movieMaster$genres[[row]])){
      movieMaster$action[row] <- 1
      break
    }
  }
}

##Amusement Dummy Variable
for (row in 1:length(movieMaster)){
  for (genre in amusement){
    if (grepl(genre, movieMaster$genres[[row]])){
      movieMaster$amusement[row] <- 1
      break
    }
  }
}


movieMaster <- separate(movieMaster, 'production_companies', c("start","producation comp","others"), "'name': '")


movieMaster <- separate(movieMaster, release_date, c('Year', 'Month', 'Date')
         , sep='-'
         , remove=FALSE)


write.csv(movieMaster, file = "cleaned_movies_metadata_v2.csv")

