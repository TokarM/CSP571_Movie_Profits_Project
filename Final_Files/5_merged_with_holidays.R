setwd('/Users/Sunny/Github/CSP571_Movie_Profits_Project')
primary <- read.csv('4_merge.csv', stringsAsFactors = F)
holidays <- read.csv('US_holidays_2000_2017.csv', stringsAsFactors = F)

primary$release_date <-as.Date(primary$release_date)

holidays$FullDate <- as.Date(holidays$FullDate)

primary$oneMonth <- primary$release_date + 30

primary$numOfHolidays <- 0

for(i in 1:nrow(primary)){
  temp <- holidays[which(holidays$FullDate >= primary[i, 'release_date']),]
  temp2 <- temp[which(temp$FullDate <= primary[i, 'oneMonth']),]
  primary$numOfHolidays[i] <- nrow(temp2)
}

final <- primary

##library(tidyverse)

##cleaning
holidays$HolidayName <- tolower(holidays$HolidayName)
holidays$HolidayName <- gsub('[[:punct:]]+',' ',holidays$HolidayName)
searchString <- ' '
replacementString <- ''

holidays$HolidayName <- trimws(holidays$HolidayName)
holidays$HolidayName <- gsub(searchString, replacementString, holidays$HolidayName)

holidaylist <- unique(holidays$HolidayName)



##Create holiday columns
for(i in holidaylist){
  final[i] <- 0
}

##Populate the holiday fields
for (i in 1:nrow(holidays)){
  holidayname <- holidays$HolidayName[i]
  for (j in 1:nrow(final)){
    if (abs(final$release_date[j] - holidays$FullDate[i]) < 4){
      final[[holidayname]][j] <- 1
      final$related_holiday[j] <- holidayname
    } else {
      final$related_holiday[j] <- "None"
    }
  }
}

colnames(final)
final_df <- final[,c(3:33, 35:56)]

write.csv(final_df,"5_merged_with_holidays.csv")
