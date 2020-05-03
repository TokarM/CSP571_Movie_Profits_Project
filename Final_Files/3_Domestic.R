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

names(MovieStats1) <- c("number","releaseDate", "movieName", "productionBudget","domesticGross","worldwideGross")

MovieStats1$movieName<-tolower(MovieStats1$movieName)


df<-na.omit(MovieStats1)


df$movieName<-gsub('[[:punct:]]+',' ',df$movieName)
df$domestic <- 1

df <- as.data.frame(df[, c('movieName','domestic')], stringAsFactors = F)

write.csv(df,"3_domestic.csv")
