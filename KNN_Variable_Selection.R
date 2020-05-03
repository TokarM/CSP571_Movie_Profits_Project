#KNN using top 15 most significant variables per F-score criterion. NOT OUR FINAL KNN MODEL.
library('caret')
set.seed(1000)

df1 <- read.csv('/Users/Sunny/Github/CSP571_Movie_Profits_Project/5_merged_with_holidays.csv', stringsAsFactors = T)
colnames(df1)

##Select categorical columns
df2 <- df1[,c(32, 7:24, 30, 34:53)]

##Standardize numerical columns
df3 <- scale(df1[, c(5:6, 27:29, 31, 33)])

##Merge the two dataframes
colnames(df4)
df4 <- cbind(df2, df3)

##Separate the independent variables and the dependent variable
df_ind_col <- df4[,-c(1, 40)]
df_dep_col <- df4[, 1]

##Create a placeholder list to be populated later
var_fstat <- as.numeric(rep(NA, times = ncol(df_ind_col)))

##Names of each list item are the independent variable names
names(var_fstat) <- colnames(df_ind_col)

##Save the F-score of each model after the dependent variable is regressed on each independent variable
for (i in 1:ncol(df_ind_col)){
  var_fstat[i] <- summary(lm(substitute(success_1_to_1 ~ i, 
                                        list(i = as.name(names(var_fstat)[i]))), 
                             data = df4))$fstatistic[[1]]
}

##Sort the scores in descending order
sorted_df <- sort(unlist(var_fstat), decreasing = T)

###############################################################

##Take the top 15 scores to compare to the elastic net variable set
top_15 <- sorted_df[c(1:15)]
names(df_15)

##Select those 15 variables from the primary dataset
df_15 <- df4[, c(1:2, 20, 42:45, 4:7, 9, 15, 17, 19, 21)]
df_15$success_1_to_1 <- as.factor(df_15$success_1_to_1)

##Partition to train/test sets
inTrain <- createDataPartition(y = df_15[,'success_1_to_1'], list = FALSE, p = .8)
train_15 <- df_15[inTrain,]
test_15 <- df_15[-inTrain,]

##Train the model, only k is tuned, distance and kernel are set to their default values
model_rectangular_15 <- train(
  success_1_to_1 ~., 
  data = train_15, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:50,
                         distance = 2,
                         kernel = c('rectangular')))

##See results of the trained model and how it did on the validation set during cross validation
plot(model_rectangular_15)
model_rectangular_15$bestTune
max(model_rectangular_15$results$Accuracy)

