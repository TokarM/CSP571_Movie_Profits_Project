library('rvest')
library('tidyr')
url = 'https://www.timeanddate.com/holidays/us/'
holidayAll <- data.frame()

for (page in c(2000:2017)){
  urlList<-paste0(url,page)
  holidayWebpage <- read_html(urlList)
  holidayTable <- html_nodes(holidayWebpage, css = 'table')
  holidayYearly <- html_table(holidayTable, fill = TRUE)[[1]]
  holidayYearly <- holidayYearly[-(1:2),]
  holidayYearly['Year'] <- page
  holidayAll<-rbind(holidayAll,holidayYearly)
}

holiday_clean <- subset(holidayAll, (holidayAll$Type == "Federal Holiday" | 
                                      holidayAll$Name == "Christmas Eve" | 
                                      holidayAll$Name == "Christmas Eve observed" |
                                      holidayAll$Name == "Mother's Day" |
                                      holidayAll$Name == "Father's Day" |
                                      holidayAll$Name == "Valentine's Day" |
                                      holidayAll$Name == "Good Friday"))
names(holiday_clean) <- c('Date', 'DayOfWeek', 'HolidayName', 'Type', 'Details', 'Year')
holiday_clean <- holiday_clean[,c('Year', 'Date', 'DayOfWeek', 'HolidayName', 'Type')]
holiday_clean$FullDate <- paste(holiday_clean$Date, holiday_clean$Year, sep = ", ")
holiday_clean$FullDate <- as.Date(holiday_clean$FullDate, "%b %d, %Y")
holiday_clean <- subset(holiday_clean, !duplicated(holiday_clean$FullDate))

write.csv(holiday_clean, file = 'US_holidays_2000_2017.csv')
