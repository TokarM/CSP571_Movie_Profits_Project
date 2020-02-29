library(stringr)
dfA <- read.csv("A-Listers of Hollywood.csv"
                , header = TRUE)
dfB <- read.csv("B-Listers Celebrities.csv"
                , header = TRUE)
dfA <- as.data.frame(dfA)
dfB <- as.data.frame(dfB)
dfA['Rank'] <- 1
dfB['Rank'] <- 2
df <- rbind(dfA[c('Name', 'Rank')], dfB[c('Name', 'Rank')])
temp <- str_split_fixed(df$Name, " ")
df['FirstName'] <- temp[,1]
df['LastName'] <- temp[,2]
