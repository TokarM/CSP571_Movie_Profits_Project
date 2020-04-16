library('rvest')
library('tidyr')

initialurl<-'https://www.the-numbers.com/movie/budgets/all'
MovieStats <- read_html(initialurl)
MovieStats <- html_nodes(MovieStats, css = 'table')
MovieStats <- html_table(MovieStats)[[1]]
MovieStats1<- data.frame(MovieStats)

for (page in c(1:60)){
  urlList<-paste0("https://www.the-numbers.com/movie/budgets/all/",page,"01")
  MovieStats <- read_html(urlList)
  MovieStats <- html_nodes(MovieStats, css = 'table')
  MovieStats <- html_table(MovieStats)[[1]]
  MovieStats<- data.frame(MovieStats)
  MovieStats1<-rbind(MovieStats1,MovieStats)
  
}

summary(MovieStats1)


names(MovieStats1) <- c("Number","ReleaseDate", "MovieName", "ProductionBudget","DomesticGross","WorldwideGross")

MovieStatsChart <- separate(MovieStats1, ReleaseDate, c("Month", "Day", "Year"), sep=' ', remove=TRUE)
MovieStatsChart$MovieName<-tolower(MovieStatsChart$MovieName)
MovieStatsChart$Month<-tolower(MovieStatsChart$Month)
head(MovieStatsChart)

library(dplyr)
df<-MovieStatsChart %>% select(`Month`,'Day', 'Year', `MovieName`,`ProductionBudget`,`DomesticGross`)
df<-na.omit(df)
df$Day<-as.factor(df$Day)
summary(df$Day)

df$Month<-as.factor(df$Month)
df$Day<-gsub('[[:punct:] ]+',' ',df$Day)
View(df)
df$Month<-factor(df$Month, levels = c("jan", "feb", "mar", "apr", "may","jun","jul","aug","sep","oct","nov","dec"), labels = c("01", "02", "03", "04", "05","06","07","08","09","10","11","12"))
df$Day<-gsub(' ','',df$Day)
df$Day<-factor(df$Day, levels = c("1", "2", "3", "4", "5","6","7","8","9","10","11","12","13","14","15","16",
                                  "17","18","19","20","21","22","23","24","25","26","27","28","29","30","31" ), 
               labels = c("01", "02", "03", "04", "05","06","07","08","09","10","11","12","13","14","15","16",
                          "17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"))

df$Date<-paste(df$Month, df$Day, df$Year)
df$Date<-as.Date(df$Date,"%m%d%y")


df$quarter<-factor(df$Month, levels = c("01", "02", "03", "04", "05","06","07","08","09","10","11","12"), labels = c("Q1", "Q1", "Q1", "Q2", "Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))
nrow(df)

df$ProductionBudget<-gsub("\\$","",df$ProductionBudget)
df$ProductionBudget<-as.numeric(gsub(",","",df$ProductionBudget))
df$DomesticGross<-gsub("\\$","",df$DomesticGross)
df$DomesticGross<-as.numeric(gsub(",","",df$DomesticGross))
summary(df)

df$MovieName<-gsub('[[:punct:] ]+',' ',df$MovieName)
df <- as.data.frame(df[, c('MovieName','ProductionBudget','DomesticGross','quarter','Date')], stringAsFactors = F)
df2 <- df[!is.na(df$quarter),]

df2

write.csv(df2,"budgets&ReleaseDate.csv")