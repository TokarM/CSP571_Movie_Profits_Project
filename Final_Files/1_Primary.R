##install.packages('tidyr')
library('tidyr')
movieMaster <- read.csv('/Users/Sunny/Github/CSP571_Movie_Profits_Project/movies_metadata.csv'
              ,header = T, sep = ",", stringsAsFactors = F)
movieMaster <- movieMaster[,c('id','original_title','genres','release_date','revenue','budget','runtime')]


##Remove movies that lack our dependent variable, Revenue
movieMaster <- movieMaster[movieMaster$revenue != 0,]


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


all_genres <- data.frame(unique_genres, stringsAsFactors = F)
filtered_genres <-  tolower(c(all_genres[-c(12, 18, 20),], 'scifi'))

##Grouping the genres into broader categories, to be used as dummy variables


##Dummy Variables for Genres
movieMaster[, filtered_genres] <-0

##Drama
for (row in 1:nrow(movieMaster)){
  if (grepl('Drama', movieMaster$genres[[row]])){
      movieMaster$drama[row] <- 1
  }
}

##Romance
for (row in 1:nrow(movieMaster)){
  if (grepl('Romance', movieMaster$genres[[row]])){
    movieMaster$romance[row] <- 1
  }
}

##Comedy
for (row in 1:nrow(movieMaster)){
  if (grepl('Comedy', movieMaster$genres[[row]])){
    movieMaster$comedy[row] <- 1
  }
}

##Horror
for (row in 1:nrow(movieMaster)){
  if (grepl('Horror', movieMaster$genres[[row]])){
    movieMaster$horror[row] <- 1
  }
}

##Documentary
for (row in 1:nrow(movieMaster)){
  if (grepl('Documentary', movieMaster$genres[[row]])){
    movieMaster$documentary[row] <- 1
  }
}

##Action
for (row in 1:nrow(movieMaster)){
  if (grepl('Action', movieMaster$genres[[row]])){
    movieMaster$action[row] <- 1
  }
}

##Thriller
for (row in 1:nrow(movieMaster)){
  if (grepl('Thriller', movieMaster$genres[[row]])){
    movieMaster$thriller[row] <- 1
  }
}

##Crime
for (row in 1:nrow(movieMaster)){
  if (grepl('Crime', movieMaster$genres[[row]])){
    movieMaster$crime[row] <- 1
  }
}

##Family
for (row in 1:nrow(movieMaster)){
  if (grepl('Family', movieMaster$genres[[row]])){
    movieMaster$family[row] <- 1
  }
}

##Music
for (row in 1:nrow(movieMaster)){
  if (grepl('Music', movieMaster$genres[[row]])){
    movieMaster$music[row] <- 1
  }
}

##Mystery
for (row in 1:nrow(movieMaster)){
  if (grepl('Mystery', movieMaster$genres[[row]])){
    movieMaster$mystery[row] <- 1
  }
}

##Adventure
for (row in 1:nrow(movieMaster)){
  if (grepl('Adventure', movieMaster$genres[[row]])){
    movieMaster$adventure[row] <- 1
  }
}

##Animation
for (row in 1:nrow(movieMaster)){
  if (grepl('Animation', movieMaster$genres[[row]])){
    movieMaster$animation[row] <- 1
  }
}

##History
for (row in 1:nrow(movieMaster)){
  if (grepl('History', movieMaster$genres[[row]])){
    movieMaster$history[row] <- 1
  }
}

##War
for (row in 1:nrow(movieMaster)){
  if (grepl('War', movieMaster$genres[[row]])){
    movieMaster$war[row] <- 1
  }
}

##Western
for (row in 1:nrow(movieMaster)){
  if (grepl('Western', movieMaster$genres[[row]])){
    movieMaster$western[row] <- 1
  }
}

##Fantasy
for (row in 1:nrow(movieMaster)){
  if (grepl('Fantasy', movieMaster$genres[[row]])){
    movieMaster$fantasy[row] <- 1
  }
}

##Scifi
for (row in 1:nrow(movieMaster)){
  if (grepl('Science Fiction', movieMaster$genres[[row]])){
    movieMaster$scifi[row] <- 1
  }
}

##Check if it worked
sapply(movieMaster[, 8:25], sum)

##Standardizing movie title
movieMaster$original_title <- tolower(movieMaster$original_title)
movieMaster$original_title <- gsub('[[:punct:] ]+',' ',movieMaster$original_title)
movieMaster$original_title <- gsub(' ', '', movieMaster$original_title)


write.csv(movieMaster, file = "1_primary.csv")

