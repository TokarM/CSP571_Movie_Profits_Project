setwd('/Users/Sunny/Github/CSP571_Movie_Profits_Project')
budgets <- read.csv('budgets.csv', stringsAsFactors = F)
primary <- read.csv('cleaned_movies_metadata_v2.csv', stringsAsFactors = F)
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


v1_movie_meta<-merge(primary,cast, by='id')
v2_movie_meta<-merge(v1_movie_meta,budgets,by.x='cleaned.title',by.y='MovieName', all=FALSE)
v3_movie_meta<-merge(v2_movie_meta,ranking,by.x='Actor',by.y='Name', all.x=TRUE)


#remove duplicated movies
v4_movie_meta<-v3_movie_meta[!duplicated(v3_movie_meta$cleaned.title), ]

#Add rank 3 actors
v4_movie_meta[is.na(v4_movie_meta$actorRank), 'actorRank'] <- 3



final_dataset <- subset(v4_movie_meta, select = c("original_title", "id", "actorRank", "Director", 
                                                  "runtime", "ProductionBudget", "DomesticGross", "release_date",
                                                  "quarter", "drama","thriller", "nonfiction", "action", "amusement"))
write.csv(final_dataset,"final_dataset.csv")

