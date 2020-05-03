library(tidyverse)
library(ggplot2)
library(psych)

dataset<-elastic_net_final_dataset

summary(dataset)
describe(dataset)

hist(dataset$budget)

#making distribution of budget
ggplot(dataset, aes(x=budget)) +
  geom_histogram(binwidth=10000000, colour="black", fill="white") +
  geom_vline(aes(xintercept=mean(budget, na.rm=T)),   
             color="red", linetype="dashed", size=1)


#making the correlation matrix
cordataset <- subset(dataset, select = c("budget","runtime","comedy","family","adventure","fantasy","drama","action",
"horror","documentary","scifi"))
res<-cor(cordataset)
round(res,2)
?cor

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
