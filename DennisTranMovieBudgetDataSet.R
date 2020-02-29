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
head(MovieStats)

head(MovieStatsChart)

MovieStatsChart <- separate(MovieStats1, ReleaseDate, c("Month", "Year", "Years"), sep=', ', remove=TRUE)
MovieStatsChart <- separate(MovieStatsChart, Month, c("Month", "Day"), sep=' ', remove=TRUE)
MovieStatsChart$MovieName<-tolower(MovieStatsChart$MovieName)
MovieStatsChart$Month<-tolower(MovieStatsChart$Month)
head(MovieStatsChart)

library(dplyr)
df<-MovieStatsChart %>% select(`Month`,'Year',`MovieName`,`ProductionBudget`,`DomesticGross`)
head(df)

df$Month<-as.factor(df$Month)
df$Month<-factor(df$Month, levels = c("jan", "feb", "mar", "apr", "may","jun","jul","aug","sep","oct","nov","dec"), labels = c("Q1", "Q1", "Q1", "Q2", "Q2","Q2","Q3","Q3","Q3","Q4","Q4","Q4"))
head(df)
df<-data.frame(df)
df$ProductionBudget<-gsub("\\$","",df$ProductionBudget)
df$ProductionBudget<-as.numeric(gsub(",","",df$ProductionBudget))
df$DomesticGross<-gsub("\\$","",df$DomesticGross)
df$DomesticGross<-as.numeric(gsub(",","",df$DomesticGross))

head(df)

df$Year<-as.numeric(df$Year)
df<-subset(df, df$Year>=2000)
summary(df)

df$MovieName<-gsub('[[:punct:] ]+',' ',df$MovieName)
head(df)

