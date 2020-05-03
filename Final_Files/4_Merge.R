#library(tidyverse)

setwd('/Users/Sunny/Github/CSP571_Movie_Profits_Project')
primary <- read.csv('1_primary.csv', stringsAsFactors = F)
domestic <- read.csv('3_domestic.csv', stringsAsFactors = F)
cast <- read.csv('cast.csv', stringsAsFactors = F)

##Cleaning the Names from Domestic and Casts
searchString <- ' '
replacementString <- ''

cast$Actor <- trimws(cast$Actor)
cast$Actor <- gsub(searchString, replacementString, cast$Actor)

cast$Director <- trimws(cast$Director)
cast$Director <- gsub(searchString, replacementString, cast$Director)

domestic$movieName <- trimws(domestic$movieName)
domestic$movieName <- gsub(searchString, replacementString, domestic$movieName)

##Removing duplicate IDs from Primary and from Cast
primary_cleaned <- primary[!duplicated(primary$id),]
cast_cleaned <- cast[!duplicated(cast$id),]

##Merge
v1 <- merge(primary_cleaned, cast_cleaned, by='id')

##Remove duplicates created by merge
v1 <- v1[!duplicated(v1$id),]

##Remove rows where Actors or Directors is NA
v1 <- v1[!is.na(v1$Actor), ]
v1 <- v1[!is.na(v1$Director), ]

##Remove rows where Actor name contains a number
v2 <- v1
remove <- data.frame(grep('\\d', v2$Actor))
colnames(remove) <- "x"
remove <- sort(remove$x, decreasing = T)

for (i in remove){
  v2 <- v2[-i,]
}

##Remove rows where Director name contains a number
remove <- data.frame(grep('\\d', v2$Director))
colnames(remove) <- "x"
remove <- sort(remove$x, decreasing = T)

for (i in remove){
  v2 <- v2[-i,]
}

#Change Date Column to type Date()
v2$release_date <- as.Date(v2$release_date)

##Sort the Release Date
v3 <- v2[order(v2$release_date), ]

##Add 3 new columns
v3$actorMovieCount <- 0
v3$directorMovieCount <- 0
v3$directorEarnings <- 0

##Add how many movies an Actor starred in prior to the current movie
for (actor in unique(v3$Actor)){
  actMovie <- v3[which(v3$Actor == actor),]
  for (i in 1:nrow(actMovie)){
    num <- nrow(actMovie) - i
    v3[which(v3$Actor == actor),]$actorMovieCount[i] <- num
  }
}

##Add how many movies a Director directed prior to the current movie
##Add how much total revenue a Director has genereated from movies prior to the current movie
for (dir in unique(v3$Director)){
  dirMovie <- v3[which(v3$Director == dir),]
  dirEarnings <- dirMovie
  for (i in 1:nrow(dirMovie)){
    num <- nrow(dirMovie) - i
    dirEarnings <- dirEarnings[-1, ]
    v3[which(v3$Director == dir),]$directorMovieCount[i] <- num
    v3[which(v3$Director == dir),]$directorEarnings[i] <- sum(dirEarnings$revenue)
  }
}

##Identify Domestic Films / the Budgets Dataset consists of only domestic films and will be used as our reference.
v3$domestic <- 0

v4 <- merge(v3, domestic, by.x='original_title', by.y='movieName', all.x=TRUE)
v4 <- v4[!duplicated(v4$id),]

##Primary records that do not have matching entries with titles in Domestic will have a 0 (not domestic) label
v4$domestic.y[is.na(v4$domestic.y)] <- 0
v4$domestic <- v4$domestic.y

##Add quarters to the dataset
v4$month <- months.Date(v4$release_date)
v4$quarter <- factor(v4$month, levels = c('January', 'February', 'March', 'April', 'May',
                                          'June', 'July', 'August', 'September', 'October',
                                          'November', 'December'), labels = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4))



##Check for NAs
sapply(v4, function(y) sum(length(which(is.na(y)))))

##Distribution of runtime prior to mean imputation
hist(v4$runtime, xlab = "Runtime", main = "Histogram of Runtime Prior to Mean Imputation")
abline(v = mean(v4[!is.na(v4$runtime), 'runtime']),
       col = "royalblue", lwd = 2)

##Replace the NAs in runtime with 0
v4$runtime[is.na(v4$runtime)] <- 0

#Replacing the '0' values from Budget and Runtime with the mean for those columns
v5 <- v4
v5$runtime[v5$runtime == 0] <- mean(v5$runtime[v5$runtime > 0])
v5$budget[v5$budget == 0] <- mean(v5$budget[v5$budget > 0])
hist(v5$runtime)
hist(v5$budget)

##Adding Success Criterion for Classification purposes
v5$success_1_to_1 <- 0
for (i in 1:nrow(v5)){
  if(v5$revenue[i] > v5$budget[i]){
    v5$success_1_to_1[i] <- 1
  }
}

##Count Number of True Positives to calculate our precision threshold
true_pos <- sum(v5$success_1_to_1)
total_predicted_true <- nrow(v5)
precision_threshold <- true_pos / total_predicted_true

##Selecting our desired columns
colnames(v5)
final_dataset <- v5[, c(1:2, 5:26, 28:32, 36, 38:39)]

write.csv(final_dataset,"4_merge.csv")
