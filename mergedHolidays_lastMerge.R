setwd('/Users/Sunny/Github/CSP571_Movie_Profits_Project')
secondary <- read.csv('near_final_dataset_director_holidays.csv', stringsAsFactors = F)
primary <- read.csv('final_with_holidays_sunny.csv', stringsAsFactors = F)

colnames(secondary)
tertiary <- secondary[, c(2, 31:51)]

nrow(primary)
nrow(secondary)
colnames(primary)
head(secondary, 1)

merged <-merge(primary,tertiary,by.x='id',by.y='id', all.x=TRUE)

sapply(merged, function(y) sum(length(which(is.na(y)))))
colnames(merged)



final <- merged[, c(33,1,4:32,34:54)]

write.csv(final,"true_final_dataset.csv")
