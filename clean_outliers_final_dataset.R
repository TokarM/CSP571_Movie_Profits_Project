movie <- read.csv('/Users/nick/Desktop/CSP571_Movie_Profits_Project/final_dataset.csv', header = TRUE, stringsAsFactors = FALSE)
head(movie)

#Delete runtime outliers
boxplot(movie$runtime)
id1 <- boxplot.stats(movie$runtime, 2)
id1$stats

movie <- movie[which(movie$runtime < 157),]
movie <- movie[which(movie$runtime > 58),]


boxplot(movie$ProductionBudget)
id2 <- boxplot.stats(movie$ProductionBudget, 2)
id2$stats

movie <- movie[which(movie$ProductionBudget < 103000000),]
movie <- movie[which(movie$ProductionBudget > 7000),]

write.csv(movie, '/Users/nick/Desktop/CSP571_Movie_Profits_Project/final_dataset.csv')
