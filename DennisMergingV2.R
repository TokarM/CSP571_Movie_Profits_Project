setwd('/Users/Sunny/Github/CSP571_Movie_Profits_Project')
budgets <- read.csv('budgets.csv', stringsAsFactors = F)
primary <- read.csv('cleaned_movies_metadata_v3.csv', stringsAsFactors = F)
cast <- read.csv('cast.csv', stringsAsFactors = F)
ranking <- read.csv('celebrity_ranking.csv', stringsAsFactors = F)

library(tidyverse)

##Cleaning the Names
searchString <- ' '
replacementString <- ''
colnames(ranking) <- c("X", "Name", "actorRank", "FirstName", "LastName")


budgets$MovieName <- trimws(budgets$MovieName)
budgets$MovieName <- gsub(searchString, replacementString, budgets$MovieName)

ranking$Name <- trimws(ranking$Name)
ranking$Name <- gsub(searchString, replacementString, ranking$Name)

cast$Actor <- trimws(cast$Actor)
cast$Actor <- gsub(searchString, replacementString, cast$Actor)

cast$Director <- trimws(cast$Director)
cast$Director <- gsub(searchString, replacementString, cast$Director)

v1_movie_meta<-merge(primary,cast, by='id')
v2_movie_meta<-merge(v1_movie_meta,budgets,by.x='cleaned.title',by.y='MovieName', all=FALSE)

#remove duplicated movies
v3_movie_meta<-v2_movie_meta[!duplicated(v2_movie_meta$cleaned.title), ]
v3_movie_meta$release_date <- as.Date(v3_movie_meta$release_date)
##Sort the DF
v3_sorted <- v3_movie_meta[order(v3_movie_meta$release_date), ]
##Add new column
v3_sorted$actorMovieCount <- 0
v3_sorted$directorMovieCount <- 0
##Remove entries with no actors
v4_sorted <- v3_sorted[!is.na(v3_sorted$Actor), ]
v4_sorted <- v4_sorted[!is.na(v4_sorted$Director), ]


for (actor in unique(v4_sorted$Actor)){
  actMovie <- v4_sorted[which(v4_sorted$Actor == actor),]
  for (i in 1:nrow(actMovie)){
    num <- nrow(actMovie) - i
    v4_sorted[which(v4_sorted$Actor == actor),]$actorMovieCount[i] <- num
  }
}

for (dir in unique(v4_sorted$Director)){
  dirMovie <- v4_sorted[which(v4_sorted$Director == dir),]
  for (i in 1:nrow(dirMovie)){
    num <- nrow(dirMovie) - i
    v4_sorted[which(v4_sorted$Director == dir),]$directorMovieCount[i] <- num
  }
}

##Convert date and limit movies to those released between 2000 and 2017
v5 <- v4_sorted[(v4_sorted$release_date > "1999-12-31"),]


#Replacing the '0' values from DomesticGross and Runtime with the mean for those columns
v5$runtime[v5$runtime == 0] <- mean(v5$runtime)
v5$DomesticGross[v5$DomesticGross == 0] <- mean(v5$DomesticGross)
hist(v5$runtime)
hist(v5$DomesticGross)


sapply(v5, function(y) sum(length(which(is.na(y)))))
colnames(v5)

final_dataset <- subset(v5, select = c("original_title", "id", "release_date", "Actor", "Director", "actorMovieCount",
                                       "directorMovieCount","runtime", "ProductionBudget", "DomesticGross",
                                       "quarter", "animation", "comedy", "family", "adventure", "fantasy", 
                                       "romance", "drama","action", "crime", "thriller", "horror", "history",
                                       "mystery", "war", "music", "documentary", "western", "scifi"))

write.csv(final_dataset,"near_final_dataset.csv")
