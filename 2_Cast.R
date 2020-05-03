library(tidyr)
setwd('/Users/Sunny/Github/CSP571_Movie_Profits_Project')
credits <- read.csv("credits.csv")

#Actor cleaning
MovieStatsChart <- separate(credits, cast, c("1", "2", "3","4", "5","Actor"), sep=', ', remove=TRUE)
head(MovieStatsChart, 1)

df<-MovieStatsChart %>% select(`Actor`,'id')
head(df)

df <- separate(df, Actor, c("1", "Actor"), sep=': ', remove=TRUE)


df<-as.data.frame(df)

df$Actor<-gsub('[[:punct:] ]+',' ',df$Actor)
head(df)
Actors<-df %>% select(`Actor`,'id')
head(Actors)

#Director cleaning
MovieStatsChart <- separate(credits, crew, c("1", "2", "3","4", "5","6","7","8","Director"), sep=', ', remove=TRUE)
head(MovieStatsChart)

df<-MovieStatsChart %>% select('5',`6`,'id')
head(df)

df <- separate(df, '5', c("1", "Position"), sep=': ', remove=TRUE)
df <- separate(df, '6', c("1", "PositionName"), sep=': ', remove=TRUE)

df<-as.data.frame(df)


df$Position<-gsub('[[:punct:] ]+',' ',df$Position)
df$PositionName<-gsub('[[:punct:] ]+',' ',df$PositionName)
head(df)
Directors<-df %>% select(`Position`,'PositionName','id')
head(Directors)


Directors<-subset(Directors,Directors$Position==" Director ")
summary(Directors)


ActorDirector<-merge(Actors,Directors,by="id")
Cast<-ActorDirector %>% select(`Actor`,'PositionName','id')
names(Cast) <- c("Actor", "Director", "id")
head(Cast)

Cast$Actor<-tolower(Cast$Actor)
Cast$Director<-tolower(Cast$Director)
Cast$Actor <- trimws(Cast$Actor)
Cast$Director<-trimws(Cast$Director)


write.csv(Cast,"2_cast.csv")
