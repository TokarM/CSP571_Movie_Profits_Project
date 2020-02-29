install.packages('tidyr')
library('tidyr')
movieMaster <- read.csv('/Users/kayinho/Documents/IIT/Sem2 - 2020Spring/CSP571/git_project/movie_forecasting/CSP571_Movie_Profits_Project/the-movies-dataset/movies_metadata.csv'
              ,header = T, sep = ",")
movieMaster <- movieMaster[,c('id','budget','genres','original_title','production_companies','release_date','revenue', 'runtime')]

movieMaster$release_date <- as.Date(movieMaster$release_date)
movieMaster <- movieMaster[(movieMaster$release_date > "2000-01-01"),]
movieMaster <- movieMaster[(movieMaster$release_date < "2017-12-31"),]
head(movieMaster, n= 50)
movieMaster <- separate(movieMaster, genres, c("start","genre1"), "'name': '")
movieMaster <- separate(movieMaster, genre1, c("1st genre"), "'")
movieMaster <- movieMaster[,c('id','budget','1st genre','original_title','production_companies','release_date','revenue', 'runtime')]
movieMaster <- separate(movieMaster, 'production_companies', c("start","producation comp","others"), "'name': '")
movieMaster <- separate(movieMaster, 'producation comp', c('1st production company'), "', 'id': ")
movieMaster <- movieMaster[,c('id','budget','1st genre','original_title','1st production company','release_date','revenue', 'runtime')]

movieMaster$`1st genre` <- tolower(movieMaster$`1st genre`)
movieMaster$original_title <- tolower(movieMaster$original_title)
movieMaster$original_title<-gsub('[[:punct:] ]+',' ',movieMaster$original_title)
movieMaster$`1st production company` <- tolower(movieMaster$`1st production company`)

movieMaster <- separate(movieMaster, release_date, c('Year', 'Month', 'Date')
         , sep='-'
         , remove=FALSE)

write.csv(movieMaster, file = "/Users/kayinho/Documents/IIT/Sem2 - 2020Spring/CSP571/git_project/movie_forecasting/CSP571_Movie_Profits_Project/the-movies-dataset/cleaned_movies_metadata.csv")

head(movieMaster, n = 100)

