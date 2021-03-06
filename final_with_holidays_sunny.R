setwd('/Users/Sunny/Github/CSP571_Movie_Profits_Project')
primary <- read.csv('near_final_dataset.csv', stringsAsFactors = F)
holidays <- read.csv('US_holidays_2000_2017.csv', stringsAsFactors = F)

str(primary)
primary$release_date <-as.Date(primary$release_date)

str(holidays)
holidays$FullDate <- as.Date(holidays$FullDate)

primary$oneMonth <- primary$release_date + 30

hist(final$holidays)
primary$holidays <- 0

for(i in 1:nrow(primary)){
  temp <- holidays[which(holidays$FullDate >= primary[i, 'release_date']),]
  temp2 <- temp[which(temp$FullDate <= primary[i, 'oneMonth']),]
  primary$holidays[i] <- nrow(temp2)
}

final <- primary[, -30]
head(final, 1)

final$success_1_to_1 <- 0
for (i in 1:nrow(final)){
  if(final$DomesticGross[i] > final$ProductionBudget[i]){
    final$success_1_to_1[i] <- 1
  }
}
colnames(final)
final <- final[, c(1:29, 31:32)]

write.csv(final,"final_with_holidays_sunny.csv")
