#setwd("C:/Users/abhijit331/Desktop/289 Finals")
setwd("/home/abhijit331/Desktop/Math 289")
#install.packages("zoo")
#install.packages("doParallel")
library(doParallel)
library(zoo)
library(randomForest)
library(e1071)
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
factor = train[,fac]
factor = factor[,-c(2,8,19)]
train = train[,-fac]
train = train[,-1]
train = data.frame(lapply(train,fillNA))
# add back the factor terms
train = cbind(train,factor)


train_train = train[1:100000,]
labels = train[100001:dim(train)[1],1]
train_test = train[100001:dim(train)[1],-1]

ntree = 100; numCore = 4
rep <- ntree/numCore # tree / numCore
registerDoParallel(cores=numCore)
begTime <- Sys.time()
random.forest <- foreach(ntree=rep(rep, numCore), .combine=combine,
              .packages="randomForest") %dopar%
  randomForest(as.factor(target) ~ . , data = train_train,ntree = ntree,mtry = 10,importance = T,replace = F)
#       randomForest(X.sub, y.sub,
#                   ntree=ntree#                   mtry=10,
#                   importance=TRUE,
#                   na.action=na.roughfix, #can also use na.action = na.omit
#                   replace=FALSE)


#rf = randomForest(as.factor(target) ~ .,data = train_train )
pred = predict(random.forest,train_test)
length(which(pred == labels))/dim(train_test)[1]
runTime <- Sys.time()-begTime
runTime
varImpPlot(random.forest)
# 78% correct classification on the Validation set. Data is trained on 100k datapoints, with 114 variables 
# included in the training set. 
test = read.csv("test.csv", header = T)
ttest = test
fac2 = which(lapply(test,class)=="factor")
factor.test = test[,fac2]
factor.test = factor.test[,-c]
test = test[,-temp]
test = test[,-1]
test = data.frame(lapply(test,fillNA))
test$target = -1
test$target = predict(rf,test)

# SVM 
ntree = 100; numCore = 4
rep <- ntree/numCore # tree / numCore
registerDoParallel(cores=numCore)
begtimesvm <- Sys.time()
model.svm =foreach(ntree=rep(rep, numCore), .combine=combine,
                         .packages="e1071") %dopar%
  svm(as.factor(target) ~ . , data = train_train)
runtimesvm = Sys.time() - begtimesvm
#       randomForest(X.sub, y.sub,
#                   ntree=ntree#                   mtry=10,
#                   importance=TRUE,
#                   na.action=na.roughfix, #can also use na.action = na.omit
#                   replace=FALSE)



