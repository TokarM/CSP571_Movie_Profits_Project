semi_final <- read.csv('near_final_dataset_director.csv', header = TRUE, stringsAsFactors = FALSE)
semi_final$release_date <- as.Date(semi_final$release_date)
holidays <- read.csv('US_holidays_2000_2017.csv', header = TRUE, stringsAsFactors = FALSE)
holidays$FullDate <- as.Date(holidays$FullDate)

holidaylist <- unique(holidays$HolidayName)
for(i in holidaylist){
  semi_final[i] <- 0
}

for (i in 1:nrow(holidays)){
  holidayname <- holidays$HolidayName[i]
  for (j in 1:nrow(semi_final)){
    print(j)
    if (abs(semi_final$release_date[j] - holidays$FullDate[i]) < 4){
      semi_final[[holidayname]][j] <- 1
      semi_final$related_holiday[j] <- holidayname
    } else {
      semi_final$related_holiday[j] <- "None"
    }
  }
}
#head(semi_final)
write.csv(semi_final, file = "near_final_dataset_director_holidays.csv")