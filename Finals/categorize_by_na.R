train = read.csv("train.csv")

train$na = factor()

get_NA_category <- function(row) {
  NAs = ""
  for(i in 1:length(row)) {
    if( is.na(row[i]) || row[i] == "") {
      NAs = paste(NAs, names(row[i]), sep = " ")
      #NAs = NAs + names(row[i]) + " " 
    }
  }
  return(NAs)
}

train$na = apply(train, 1, get_NA_category)

NA_freq = table(train$na)
NA_freq_df = as.data.frame(NA_freq)

# Create dataframes out of top 3 categories
cat_1 = subset( train, na == NA_freq_df[60,1] )
cat_2 = subset( train, na == NA_freq_df[100,1] )
# cat_3 is rows that are not missing any data
cat_3 = subset( train, na == "" )

# Remove all NA columns or "" columns from these categories
cat_1 = cat_1[, colSums(is.na(cat_1)) != nrow(cat_1)]
cat_1 = cat_1[,-cat_1[,10]]

cat_2$v113 = NULL

#Remove "na" column
cat_1$na = NULL
cat_2$na = NULL
cat_3$na = NULL


# Create table of just remaining data
remaining = subset(train, na != NA_freq_df[60,1])
remaining = subset(remaining, na != NA_freq_df[100,1])
remaining = subset(remaining, na != "")


# Impute remaining data NAs with means, let "" be its own category
remain_filled = remaining
# For each column
for(i in 1:ncol(remain_filled)) {
  # If column is numeric
  if(is.numeric( remain_filled[,i] )) {
    median = median(remain_filled[,i], na.rm = TRUE)
    
    # Replace all values with medians
    for(j in 1:nrow(remain_filled)) {
      if( is.na(remain_filled[j,i]) ) {
        remain_filled[j,i] <- median 
      }
    }
  }
}

remain_filled$na = NULL
####################### ####################### ####################### #######################
########## Neural networks #######################
####################### ####################### #######################
install.packages("nnet")
require(nnet)

################# category 1 ##############################
# Remove categorical variable with absurd number of levels
#cat_1$v29 = NULL

# Make categorical vars numeric because R doesnt play nice with factors
for(i in 1:ncol(cat_1)) {
  if(is.factor(cat_1[,i])) {
    cat_1[,i] = as.numeric(cat_1[,i])
  }
}

#net_model = nnet(target~ ., data=train_prune, size=10, maxit=1000, decay=.001, MaxNWts=5000)
#31 input nodes, 1 output node. 1 layer between 1 and 31, use 16 b/c it is average
cat_1_net = nnet(target~ . - ID, data = cat_1, size=16, maxit=1000, decay=0.001)

# See accuracy of model
library(ROCR)
pred = prediction(predict(cat_1_net,newdata=cat_1,type="raw"),cat_1$target)
perf = performance(pred,"tpr","fpr")
plot(perf,lwd=2,col="blue",main="Neural Network for category 1")
abline(a=0,b=1)

cat_1[,'Predictions'] = predict(cat_1_net,newdata=cat_1,type="raw")

#Find prop of correct 1s
cutoff = 0.72
pp = predict(cat_1_net,newdata=cat_1,type="raw")

pp[ pp > cutoff ] = 1
pp[ pp <= cutoff ] = 0
cat_1[,'PredictedTarget'] = pp

overall = length( which(cat_1$PredictedTarget == cat_1$target))/nrow(cat_1)
corr1s = length( which(cat_1$PredictedTarget == 1 & 1 == cat_1$target ) ) / length(which(cat_1$target==1))
corr0s = length( which(cat_1$PredictedTarget == 0 & 0 == cat_1$target ) ) / length(which(cat_1$target==0))
print( paste("Overall: ", overall, "Corr 1s: ", corr1s, "Corr 0s: ", corr0s))

################################ category 2 ###################################

# Only run neural net on significant features foud from random forest code
cat_2_prune = cat_2[, c("target", "v114", "v36", "v43", "v58", "v105", "v101", "v59", "v12", "v47", "v10", "v131", "v57", "v81", "v34", "v24", "v37", "v14", "v66", "v50")]

# 19 input nodes, 1 output node. 1 layer between 1 and 19, use 10 bc it is average, converges at 910
cat_2_net = nnet(target~ . , data = cat_2_prune, size=10, maxit=1000, decay=0.001)

# See accuracy of model
library(ROCR)
pred = prediction(predict(cat_2_net,newdata=cat_2_prune,type="raw"),cat_2_prune$target)
perf = performance(pred,"tpr","fpr")
plot(perf,lwd=2,col="blue",main="Neural Network for pruned category 2")
abline(a=0,b=1)

cat_2_prune[,'Predictions'] = predict(cat_2_net,newdata=cat_2_prune,type="raw")

#Find prop of correct 1s
cutoff = 0.76
pp = predict(cat_2_net,newdata=cat_2_prune,type="raw")

pp[ pp > cutoff ] = 1
pp[ pp <= cutoff ] = 0
cat_2_prune[,'PredictedTarget'] = pp

overall = length( which(cat_2_prune$PredictedTarget == cat_2_prune$target))/nrow(cat_2_prune)
corr1s = length( which(cat_2_prune$PredictedTarget == 1 & 1 == cat_2_prune$target ) ) / length(which(cat_2_prune$target==1))
corr0s = length( which(cat_2_prune$PredictedTarget == 0 & 0 == cat_2_prune$target ) ) / length(which(cat_2_prune$target==0))
print( paste("Overall: ", overall, "Corr 1s: ", corr1s, "Corr 0s: ", corr0s))

########### category 3 #############################

# Only run neural net on significant features foud from random forest code
cat_3_prune = cat_3[, c("target", "v114", "v36", "v43", "v58", "v105", "v101", "v59", "v12", "v47", "v10", "v131", "v57", "v81", "v34", "v24", "v37", "v14", "v66", "v50")]

# 19 input nodes, 1 output node. 1 layer between 1 and 19, use 10 bc it is average, converges at 960
cat_3_net = nnet(target~ . , data = cat_3_prune, size=10, maxit=1000, decay=0.001)

# See accuracy of model
library(ROCR)
pred = prediction(predict(cat_3_net,newdata=cat_3_prune,type="raw"),cat_3_prune$target)
perf = performance(pred,"tpr","fpr")
plot(perf,lwd=2,col="blue",main="Neural Network for pruned category 3")
abline(a=0,b=1)

cat_3_prune[,'Predictions'] = predict(cat_3_net,newdata=cat_3_prune,type="raw")

#Find prop of correct 1s
cutoff = 0.69
pp = predict(cat_3_net,newdata=cat_3_prune,type="raw")

pp[ pp > cutoff ] = 1
pp[ pp <= cutoff ] = 0
cat_3_prune[,'PredictedTarget'] = pp

overall = length( which(cat_3_prune$PredictedTarget == cat_3_prune$target))/nrow(cat_3_prune)
corr1s = length( which(cat_3_prune$PredictedTarget == 1 & 1 == cat_3_prune$target ) ) / length(which(cat_3_prune$target==1))
corr0s = length( which(cat_3_prune$PredictedTarget == 0 & 0 == cat_3_prune$target ) ) / length(which(cat_3_prune$target==0))
print( paste("Overall: ", overall, "Corr 1s: ", corr1s, "Corr 0s: ", corr0s))

######################################## Remaining data ############################################################
# Only run neural net on significant features foud from random forest code
remain_prune = remain_filled[, c("target", "v114", "v36", "v43", "v58", "v105", "v101", "v59", "v12", "v47", "v10", "v131", "v57", "v81", "v34", "v24", "v37", "v14", "v66", "v50")]

# 19 input nodes, 1 output node. 1 layer between 1 and 19, use 10 bc it is average, converges at 960
remain_net = nnet(target~ . , data = remain_prune, size=10, maxit=10000, decay=0.001)

# See accuracy of model
library(ROCR)
pred = prediction(predict(remain_net,newdata=remain_prune,type="raw"),remain_prune$target)
perf = performance(pred,"tpr","fpr")
plot(perf,lwd=2,col="blue",main="Neural Network for pruned remaining data")
abline(a=0,b=1)

remain_prune[,'Predictions'] = predict(remain_net,newdata=remain_prune,type="raw")

#Find prop of correct 1s
cutoff = 0.75
pp = predict(remain_net,newdata=remain_prune,type="raw")

pp[ pp > cutoff ] = 1
pp[ pp <= cutoff ] = 0
remain_prune[,'PredictedTarget'] = pp

overall = length( which(remain_prune$PredictedTarget == remain_prune$target))/nrow(remain_prune)
corr1s = length( which(remain_prune$PredictedTarget == 1 & 1 == remain_prune$target ) ) / length(which(remain_prune$target==1))
corr0s = length( which(remain_prune$PredictedTarget == 0 & 0 == remain_prune$target ) ) / length(which(remain_prune$target==0))
print( paste("Overall: ", overall, "Corr 1s: ", corr1s, "Corr 0s: ", corr0s))

########### Now do neural net for everything ####################
train_impute = read.csv("train_imputed_numerical_only.csv")

# Only run neural net on significant features foud from random forest code
train_impute_prune = train_impute[, c("target", "v114", "v36", "v43", "v58", "v105", "v101", "v59", "v12", "v47", "v10", "v131", "v57", "v81", "v34", "v24", "v37", "v14", "v66", "v50")]

# 19 input nodes, 1 output node. 1 layer between 1 and 19, use 10 bc it is average, converges at 960
train_impute_net = nnet(target~ . , data = train_impute_prune, size=10, maxit=10000, decay=0.001)

# See accuracy of model
library(ROCR)
pred = prediction(predict(train_impute_net,newdata=train_impute_prune,type="raw"),train_impute_prune$target)
perf = performance(pred,"tpr","fpr")
plot(perf,lwd=2,col="blue",main="Neural Network for all data")
abline(a=0,b=1)

train_impute_prune[,'Predictions'] = predict(train_impute_net,newdata=train_impute_prune,type="raw")

#Find prop of correct 1s
cutoff = 0.73
pp = predict(train_impute_net,newdata=train_impute_prune,type="raw")

pp[ pp > cutoff ] = 1
pp[ pp <= cutoff ] = 0
train_impute_prune[,'PredictedTarget'] = pp

overall = length( which(train_impute_prune$PredictedTarget == train_impute_prune$target))/nrow(train_impute_prune)
corr1s = length( which(train_impute_prune$PredictedTarget == 1 & 1 == train_impute_prune$target ) ) / length(which(train_impute_prune$target==1))
corr0s = length( which(train_impute_prune$PredictedTarget == 0 & 0 == train_impute_prune$target ) ) / length(which(train_impute_prune$target==0))
print( paste("Overall: ", overall, "Corr 1s: ", corr1s, "Corr 0s: ", corr0s))
#############################################
# Put all categories together and see results
library(plyr)
comp = rbind.fill(remain_prune, cat_3_prune, cat_2_prune, cat_1)
