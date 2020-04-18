movie <- read.csv('/Users/nick/Desktop/CSP571_Movie_Profits_Project/5_merged_with_holidays.csv', header = TRUE, stringsAsFactors = FALSE)
head(movie)

#Delete runtime outliers
boxplot(movie$runtime)
id1 <- boxplot.stats(movie$runtime, 1.5)
id1$stats

movie <- movie[which(movie$runtime < 151),]
movie <- movie[which(movie$runtime > 62),]

boxplot(movie$budget)
id2 <- boxplot.stats(movie$budget, 1.5)
id2$stats

movie <- movie[which(movie$budget < 48000000),]
movie <- movie[which(movie$budget > 1),]

#movie['EarningPercent'] <- (movie$revenue - movie$budget) / movie$budget
#movie$directorEarnings <- NULL

movie$X <- NULL
movie$id <- NULL
movie$release_date <- NULL
movie$Actor <- NULL
movie$Director <- NULL
movie$revenue <- NULL
movie$veteransdayobserved <- NULL
movie$related_holiday <- NULL
movie$christmasdayobserved <- NULL
movie$independencedayobserved <- NULL
movie$columbusday <- NULL
movie$crismaseve <- NULL
movie$mystery <- NULL
movie$martinlutherkingjrday <- NULL
movie$goodfriday <- NULL
movie$fathersday <- NULL
movie$christmaseve <- NULL
movie$valentinesday <- NULL
movie$veteransday <- NULL
movie$numOfHolidays <- NULL
movie$animation <- NULL
movie$comedy <- NULL
movie$war <- NULL
movie$christmaseveobserved <- NULL
movie$presidentsday <- NULL
movie$laborday <- NULL

write.csv(movie, '/Users/nick/Desktop/CSP571_Movie_Profits_Project/no_outliers_final_dataset.csv')


