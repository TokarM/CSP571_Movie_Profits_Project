df1 <- read.csv('/Users/Sunny/Github/CSP571_Movie_Profits_Project/5_merged_with_holidays.csv', stringsAsFactors = T)
colnames(df1)
df2 <- df1[,c(32, 7:24, 30, 34:53)]

##Standardize numerical columns
df3 <- scale(df1[, c(5:6, 27:29, 31, 33)])

##Merge the two dataframes
df4 <- cbind(df2, df3)
df_ind_col <- df4[,-1]
df_dep_col <- df4[, 1]

var_fstat <- as.numeric(rep(NA, times = ncol(df_ind_col)))


createModelFormula <- function(xVars){
  modelForm <- as.formula(paste('targetVar', "~", paste(xVars, collapse = '+ ')))
  return(modelForm)
}

for (i in 1:ncol(df_ind_col)){
  var_fstat[i] <- summary(lm(substitute(i ~ success_1_to_1, 
                                        list(i = as.name(names(var_fstat)[i]))), 
                             data = df4))$fstatistic[1]
}

sorted_df <- sort(unlist(var_fstat), decreasing = T)

best_17 <- names(sorted_df)[1:17]
###############################################################

train_17 <- 
colnames(train_17)[1] <- 'success_1_to_1'

test_17 <- cbind(test[, 'success_1_to_1'], test[, best_17])
colnames(test_17)[1] <- 'success_1_to_1'

train_set <- train[, -c(1)]
train_label_class <- train[, 1]
test <- df5[c((splitRow+1):nrow(df5)),]
test_set <- test[, -c(1)]
test_label_class <- test[, 1]


model_rectangular_17 <- train(
  success_1_to_1 ~., 
  data = train_17, 
  method = "kknn",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(kmax = 15:50,
                         distance = 2,
                         kernel = c('rectangular')))
plot(model_rectangular_17)
model_rectangular_17$bestTune
max(model_rectangular_17$results$Accuracy)

