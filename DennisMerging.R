setwd('/Users/Dennis/Documents/GitFolder/CSP571_Movie_Profits_Project')
budgets <- read.csv('budgets.csv')
initial_movie_meta <- read.csv('cleaned_movies_metadata_1.csv')
second_movie_meta<-read.csv('cleaned_movies_metadata_v2.csv')
cast <- read.csv('cast.csv')
ranking <- read.csv('celebrity_ranking.csv')

library(tidyverse)

budgets$MovieName<-trimws(budgets$MovieName)
searchString <- ' '
replacementString <- ''
budgets$MovieName <- gsub(searchString,replacementString,budgets$MovieName)

v1_movie_meta<-merge(initial_movie_meta,second_movie_meta,by='id')
v2_movie_meta<-merge(v1_movie_meta,cast, by='id')
v3_movie_meta<-merge(v2_movie_meta,budgets,by.x='cleaned.title',by.y='MovieName', all=FALSE)
v4_movie_meta<-merge(v3_movie_meta,ranking,by.x='Actor',by.y='Name', all.x=TRUE)

#remove duplicated movies
v5_movie_meta<-v4_movie_meta[!duplicated(v4_movie_meta$cleaned.title), ]


final_dataset <- subset(v5_movie_meta, select = c("original_title.x", "id", "Actor", "Director", "budget.x",
                                                   "X1st.production.company", "runtime.x", "ProductionBudget", "DomesticGross", "release_date.x",
                                                  "drama","thriller", "nonfiction", "action", "amusement"))

write.csv(final_dataset,"final_dataset.csv")