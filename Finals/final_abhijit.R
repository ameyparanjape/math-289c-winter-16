setwd("C:/Users/abhijit331/Desktop/289 Finals")
#install.packages("zoo")
library(zoo)
library(randomForest)
####
fillNA <- function(S) 
{
  L <- !is.na(S)
  c(S[L][1], S[L])[cumsum(L)+1]
}
####
train = read.csv("train.csv", header = TRUE)
train1 = train
fac = which(lapply(train,class)=="factor")
temp = c(24,58,127)
train = train[,-fac]
train = train[,-1]
train = data.frame(lapply(train,fillNA))
#figure out the columns which are factor 


train_train = train[1:100000,]
labels = train[100001:dim(train)[1],1]
train_test = train[100001:dim(train)[1],-1]

rf = randomForest(as.factor(target) ~ .,data = train_train )
pred = predict(rf,train_test)
length(which(pred == labels))/dim(train_test)[1]
# 78% correct classification on the Validation set. Data is trained on 100k datapoints, with 114 variables 
# included in the training set. 
test = read.csv("test.csv", header = T)
ttest = test
fac2 = which(lapply(test,class)=="factor")
temp = c(23,57,126)
test = test[,-temp]
test = test[,-1]
test = data.frame(lapply(test,fillNA))
test$target = -1
test$target = predict(rf,test)
