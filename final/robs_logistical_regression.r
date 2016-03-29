################################################################################
# Logistic Regression Attempts
################################################################################


# initial setup
library(rpart)



############### SELECT the data to use #########################################

# 1: Normal data
data = train
trainData = data.frame(data[1:100000,])
testData = data.frame(data[100001:114321,])

# 2: NAs replaced with -999
data = train
data[is.na(data)] = -999
trainData = data.frame(data[1:100000,])
testData = data.frame(data[100001:114321,])

# 3: Normal data w/ NAs removed
data = train[complete.cases(train),] # remove incomplete data
trainData = data.frame(data[1:50000,])
testData = data.frame(data[50001:62561,])

# 4: NAs replaced w/ median
trainData = data.frame(train_filled[1:100000,])
testData = data.frame(train_filled[100001:114321,])

# 5: NAs replaced w/ median (Mode for categorical)
if (ncol(train_imputed) > 133) train_imputed = train_imputed[,-1] # remove extra index col
trainData = data.frame(train_imputed[1:100000,])
testData = data.frame(train_imputed[100001:114321,])

# 6: NAs replaced w/ mean
data = train
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] = mean(x = data[,i], na.rm = TRUE)
}
trainData = data.frame(data[1:100000,])
testData = data.frame(data[100001:114321,])


#///////////// END: Select data ////////////////////////////////////////////////






############### START: columns with the best p-values trial ####################

pValues = c()

# Go through the data column-by-column and run logistic regression
for (i in 3:133) {
  if (i != 24 && i != 58) { # Skip v22
    mylogit <- glm(target ~ trainData[,i],data = trainData,family = "binomial")
    pValues[i-2] = summary(mylogit)$coefficients[2,4]
  }
}

# Narrow it down to only the significant pvalues and rerun regression
sigCol = which(pValues<0.001) + 2
newData = data.frame(trainData$target,trainData[,sigCol])
mylogit <- glm(as.factor(trainData.target) ~ ., data = newData , family = "binomial")

# Use the same columns from our cross validation data
newData2 = data.frame(testData[,sigCol])

# Get the log-odds for each value
predictions = predict(mylogit,newdata=newData2)

# Create a vector of the probabilities of getting a 1 using logit func
newProb = exp(predictions)/(exp(predictions)+1)

#///////////// END: columns with the best p-values trial //////////////////////







################ START: sig (TREE) columns attempt ########################
sigCol = c(50,52,12,125,112,10,66,114,40,14,34,21,107,91,24,31,30,113,110,79) + 2
newData = data.frame(trainData$target,trainData[,sigCol])
mylogit <- glm(as.factor(trainData.target) ~ ., data = newData , family = binomial(link = "logit"))

# Remove the same columns from our cross validation data
newData2 = data.frame(testData[,sigCol])

# Get the log-odds for each value
predictions = predict(mylogit,newdata=newData2)

# Create a vector of the probabilities of getting a 1 using logit func
newProb = exp(predictions)/(exp(predictions)+1)

#////////////// END: sig (TREE) columns attempt ///////////////////////////







################ START: all non-complex columns attempt ########################

mylogit <- glm(as.factor(target) ~ ., data = trainData[,-c(1,24,58)] , family = binomial(link = "logit"))

# Remove the same columns from our cross validation data
newData = data.frame(testData[,-c(1,24,58)])

# Get the log-odds for each value
predictions = predict(mylogit,newdata=newData)

# Create a vector of the probabilities of getting a 1 using logit func
newProb = exp(predictions)/(exp(predictions)+1)

#////////////// END: all non-complex columns attempt ///////////////////////////






############### START: Predictions #############################################

# OPTION 1: Randomly pick 1s and 0s using the probabilities discovered
otherPredictions = c()
for (i in 1:7561) {
  otherPredictions = c(otherPredictions,sample(c(0,1), size = 1, replace=TRUE, prob=c(1-newProb[i],newProb[i])))
}
finalPredict = otherPredictions

# OPTION 2: Assume prob above .5 are 1 and below are 0
newProb[newProb>=.7] = 1
newProb[newProb<0.7] = 0
finalPredict = newProb

#///////////// END: Predictions ////////////////////////////////////////////////







############## Start: Validation ###############################################

# Check 1s predicted compared actual 1s in cross validation set
length(which(finalPredict[which(testData$target==1)]==1)) / length(which(testData$target == 1))

# Check 0s predicted compared actual 0s in cross validation set
length(which(finalPredict[which(testData$target==0)]==0)) / length(which(testData$target == 0))

# Overall sucess rate
success = which(finalPredict == testData$target)
length(success) / nrow(testData)

#///////////// END: Validation /////////////////////////////////////////////////

