#setwd("C:/Users/abhijit331/Desktop/289 Finals")
load("~/Desktop/Math 289/.RData")
setwd("/home/abhijit331/Desktop/Math 289")
#install.packages("zoo")
#install.packages("doParallel")
#install.packages("xgboost")
library(doParallel)
library(zoo)
library(xgboost)
library(MASS)
library(randomForest)
library(e1071)
library(parallelSVM)
####
replaceNA = function(s)
{
  ifelse(is.na(s),-999,s)
}
fillNA <- function(S) 
{
  L <- !is.na(S)
  c(S[L][1], S[L])[cumsum(L)+1]
}
####

counts = c()
for(i in 1:dim(train1)[1])
{
  counts = c(counts,length(which(is.na(train1[i,]) == TRUE)))
  
}
###
train = read.csv("train.csv", header = TRUE)
train1 = train
fac = which(lapply(train,class)=="factor")
factor = train[,fac]
factor = factor[,-c(2,8,19)]
train = train[,-fac]
train = train[,-1]
train = data.frame(lapply(train,replaceNA))
# add back the factor terms
train = cbind(train,factor)
train = cbind(train,counts)


train_train = train[1:10000,]
labels = train[10001:20000,1]
train_test = train[10001:20000,-1]

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
dummy = train[1:10000,]
begin = Sys.time()
svm.radial = svm(as.factor(target) ~ . , data = dummy,type = "C-classification",kernel = "radial" ) 
end = Sys.time()-begin
end
pred.svm = predict(svm.radial,train_test)
length(which(pred.svm == labels))/dim(train_test)[1]

# ~ 3 hours execution time. 

# Parallel SVM
ntree = 100; numCore = 4
rep <- ntree/numCore # tree / numCore

z = c()
cl = makeCluster(4)
registerDoParallel(cl)
getDoParWorkers()
dummy = train_train[1:10000,]
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


# Parallel Prediction SVM
cl = makeCluster(4)
registerDoParallel(cl)
split = sort(rank(1:nrow(train_test))%%4)
svm.prediction = foreach(i = unique(split),.combine = combine,.packages = c("e1071")) %dopar%
  {as.numeric(predict(model.svm,newdata=train_test[split==i,]))}
stopCluster(cl)
registerDoSEQ()
length(which(svm.prediction == labels))/dim(train_test)[1]



 
# Extreme Gradient Boosting
dum = train[1:100000,-1]
la = train[1:100000,1]
dumtest = train[100001:110000,-1]
spar2 = sparse.model.matrix(~.,data = dumtest)
latest = train[100001:110000,1]
spar = sparse.model.matrix(~.,data = dum)
xgb.model = xgboost(data = spar,label =as.factor(la),max_depth = 9,eta = 1 , nthread = 4,nround = 10,onjective = "binary:logistic")
impo =xgb.importance(feature_names = spar@Dimnames[[2]],model = xgb.model)
testp = predict(xgb.model,newdata = spar2)
testp=ifelse(testp > 0.5,1,0)
length(which(testp == latest))
head(testp)
head(latest)
