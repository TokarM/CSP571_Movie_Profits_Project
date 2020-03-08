setwd('/Users/nick/Desktop/CSP571_Movie_Profits_Project')
budgets <- read.csv('budgets.csv')
movie_meta <- read.csv('cleaned_movies_metadata_1.csv')
cast <- read.csv('cast.csv')
ranking <- read.csv('celebrity_ranking.csv')

library(tidyverse)

ranking <- ranking %>% rename(Actor = Name)

actors_directors <- merge(cast, ranking, by='Actor', all.x=TRUE, no.dups=TRUE )
actors_directors_no_na <- drop_na(actors_directors)

movie_meta_actors_directors <- merge(actors_directors_no_na, movie_meta, by='id', no.dups=TRUE)
movie_meta_actors_directors <- movie_meta_actors_directors %>% rename(MovieName = original_title)

final_dataset <- merge(movie_meta_actors_directors, budgets, by='MovieName', no.dups=TRUE)

final_dataset_ <- subset(final_dataset, select = c("MovieName", "id", "Actor", "Director", "budget", "X1st.genre",
                                                   "X1st.production.company", "runtime", "ProductionBudget", "DomesticGross", "release_date"))

final_dataset_no_dup <- unique(final_dataset_)