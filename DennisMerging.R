setwd('/Users/Dennis/Documents/GitFolder/CSP571_Movie_Profits_Project')
budgets <- read.csv('budgets.csv')
movie_meta <- read.csv('cleaned_movies_metadata_1.csv')
cast <- read.csv('cast.csv')
ranking <- read.csv('celebrity_ranking.csv')

library(tidyverse)

budgets$MovieName<-trimws(budgets$MovieName)
searchString <- ' '
replacementString <- ''
budgets$MovieName <- gsub(searchString,replacementString,budgets$MovieName)


v2_movie_meta<-merge(movie_meta,cast, by='id')
v3_movie_meta<-merge(v2_movie_meta,budgets,by.x='cleaned.title',by.y='MovieName', all=FALSE)
v4_movie_meta<-merge(v3_movie_meta,ranking,by.x='Actor',by.y='Name', all.x=TRUE)

#remove duplicated movies
v4_movie_meta<-v4_movie_meta[!duplicated(v4_movie_meta$cleaned.title), ]


final_dataset <- subset(v4_movie_meta, select = c("original_title", "id", "Actor", "Director", "budget", "X1st.genre",
                                                   "X1st.production.company", "runtime", "ProductionBudget", "DomesticGross", "release_date"))

