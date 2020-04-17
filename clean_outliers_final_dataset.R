movie <- read.csv('/Users/nick/Desktop/CSP571_Movie_Profits_Project/5_merged_with_holidays.csv', header = TRUE, stringsAsFactors = FALSE)
head(movie)

#Delete runtime outliers
boxplot(movie$runtime)
id1 <- boxplot.stats(movie$runtime, 2)
id1$stats

movie <- movie[which(movie$runtime < 163),]
movie <- movie[which(movie$runtime > 53),]

boxplot(movie$budget)
id2 <- boxplot.stats(movie$budget, 2)
id2$stats

movie <- movie[which(movie$budget < 57000000),]
movie <- movie[which(movie$budget > 1),]

#movie['EarningPercent'] <- (movie$revenue - movie$budget) / movie$budget

movie$X <- NULL
movie$id <- NULL
movie$release_date <- NULL
movie$Actor <- NULL
movie$Director <- NULL
movie$revenue <- NULL
movie$veteransdayobserved <- NULL
movie$related_holiday <- NULL

write.csv(movie, '/Users/nick/Desktop/CSP571_Movie_Profits_Project/no_outliers_final_dataset.csv')


