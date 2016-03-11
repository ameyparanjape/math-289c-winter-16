#setwd("C:/Users/abhijit331/Desktop/289 Finals")
setwd("/home/abhijit331/Desktop/Math 289")
#install.packages("zoo")
#install.packages("doParallel")
library(doParallel)
library(zoo)
library(randomForest)
library(e1071)
library(parallelSVM)
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
pred = predict(random.forest,train_test)
length(which(pred == labels))/dim(train_test)[1]

# SVM 
begin = Sys.time()
svm.radial = svm(as.factor(target) ~ . , data = train_train,type = "C-classification",kernel = "radial" ) 
pred.svm = predict(svm.radial,train_test)
length(which(pred.svm == labels))/dim(train_test)[1]
end = Sys.time()-begin
end
# ~ 3 hours execution time. 

# Parallel shit
ntree = 100; numCore = 4
rep <- ntree/numCore # tree / numCore

z = c()
cl = makeCluster(4)
registerDoParallel(cl)
getDoParWorkers()
dummy = train_train[1:100000,]
begin = Sys.time()
model.svm = parallelSVM(as.factor(target) ~ .,data =dummy,numberCores = detectCores(),probability = T)
end = Sys.time() - begin
end
stopCluster(cl)
registerDoSEQ()
z =c(z,end*60)

train_test = train_train[100001:dim(train)[1],]
lab = train_train[100001:dim(train)[1],1]
predict.svm = predict(model.svm,train_test)
length(which(pred.svm == labels))/dim(train_test)[1]



cl = makeCluster(4)
split = sort(rank(1:nrow(train_test))%%4)
svm.prediction = foreach(i = unique(split),.combine = combine,.packages = c("e1071")) %dopar%{
  as.numeric(predict(model.svm,newdata=train_test[splits==i,]))
}
stopCluster(cl)
