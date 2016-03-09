setwd("C:/Users/abhijit331/Dropbox/289 Finals")
library(randomForest)
train = read.csv("train.csv", header = TRUE)
train1 = train
train = train[complete.cases(train),]
#figure out the columns which are factor 
fac = which(lapply(train,class)=="factor")
temp = c(24,58,127)
train = train[,-temp]
train = train[,-1]
train_train = train[1:50000,]
labels = train[50001:dim(train)[1],1]
train_test = train[50001:dim(train)[1],-1]

rf = randomForest(as.factor(target) ~ .,data = train_train )
pred = predict(rf,train_test)
length(which(pred == labels))/dim(train_test)[1]
# 77% correct classification on the Validation set. Data is trained on 50k datapoints, with 130 variables 
# included in the training set. 
