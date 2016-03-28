#install.packages("class")
library(class)
install.packages("FastKNN")
library(FastKNN)
library(dprep)
test = read.csv("test.csv")
train = read.csv("train.csv")
test = test[,-1]
train = train[,-1]
train = train[,-c(23, 57, 125)]
test = test[,-c(23, 57, 125)]
train0 = subset(train, subset = target==0)
train1 = subset(train, subset = target==1)
#a = complete.cases(train1)
classes = train[,1]

library(StatMatch)
#distmat = gower.dist(train[1,-1], train[,-1])
categorical = which(do.call("c",lapply(train[1,], class))=="factor")

library(ggplot2)
ggplot()+
  geom_histogram(aes(x=v50, y=..density..), data=train0, bins = 100, fill="red", alpha=0.5)+
  geom_histogram(aes(x=v50, y=..density..), data=train1, bins = 100, fill="blue", alpha=0.5)
# 4, 10, 14, 17, 21, 34, 36, 38, 40, 44, 45, 48, 50, 51, 55, 58, 62, 64, 101, 106, 123, 129
#####################################################
## k-nn testing
select.test.0 = sample(1:nrow(train0), size = 5000)
select.test.1 = sample(1:nrow(train1), size = 5000)

temp.train = rbind(train0[-select.test.0,2:ncol(train0)][1:25000,], train1[-select.test.1,2:ncol(train1)][1:25000,])
#temp.train = temp.train[1:50000,]
temp.test = rbind(train0[select.test.0,2:ncol(train0)], train1[select.test.1,2:ncol(train1)])
temp.test.class = c(train0[select.test.0,1], train1[select.test.1, 1])
classes = c(train0[-select.test.0,1][1:25000], train1[-select.test.1, 1][1:25000])

my_knn = function(train, test, cl, k, dist.metric = gower.dist){
  class.test.2 = rep(0, nrow(temp.test))
  #cl = makeCluster(4)
  #registerDoParallel(cl)
  for(i in 1:nrow(temp.test)){
    t = Sys.time()
    distvec = dist.metric(temp.test[i,], temp.train)[1,]
    closest.dist = sort(distvec)[1:10]
    closest = sapply(closest.dist, function(i) sample(which(distvec==i), size = 1))
    class.test.2[i] = as.numeric(names(sort(table(classes[closest]), decreasing = T)[1]))
    print(Sys.time()-t)
    if(i%%100==0){
      print(paste("i=",i))
    }
  }
  return(class.test)
}

fit = glm(class ~ ., data = data.frame(class = classes, temp.train), family = "binomial")
pred.out = predict(fit, newdata = temp.test)

class.test = rep(0, nrow(temp.test))
t1 = Sys.time()
cl = makeCluster(4)
registerDoParallel(cl)
  foreach(i=1:1000)%dopar%{
    t = Sys.time()
    distvec = dist.metric(temp.test[i,], temp.train)[1,]
    closest.dist = sort(distvec)[1:10]
    closest = sapply(closest.dist, function(i) sample(which(distvec==i), size = 1))
    class.test[i] = as.numeric(names(sort(table(classes[closest]), decreasing = T)[1]))
    print(Sys.time()-t)
  }
stopCluster(cl)
print(Sys.time() - t1)
#####################################################
# HANDLING NAs
# Delete rows with more than 40 NAs
na.count.0=apply(train0, 1, function(i) sum(is.na(i)))
train0 = train0[-which(na.count.0>=40),]
na.count.1=apply(train1, 1, function(i) sum(is.na(i)))
train1 = train1[-which(na.count.1>=40),]

#
#na.count = apply(test, 1, function(i) sum(is.na(i)))
#
train.numerical = train[sapply(train, is.numeric)]
#test.numerical = test[sapply(test, is.numeric)]
#temp = 1:nrow(train)
#temp = temp[complete.cases(train.numerical)]
#logit.model = glm()
temp = train.numerical[complete.cases(train.numerical),]
#exp(predictions)/(exp(predictions)+1)
select = sample(1:nrow(temp), size = 40000)
temp.train = temp[select,]
temp.test = temp[-select,]
knnout = knn(train = temp.train[,2:ncol(temp.train)], test = temp.test[,2:ncol(temp.test)], cl = temp.train$target, k = 5)

#####################################################

temp = t(apply(train.numerical, 1, function(i) {i[is.na(i)] <- -999; return(i)}))
temp = as.data.frame(temp)
colnames(temp) = colnames(train.numerical)
temp = cbind(temp, train[,categorical])

fit = glm(target ~ ., data = temp, family = "binomial")
step.func = function(fit){
  stepout = step(fit, direction = "backward")
  return(stepout)
}
stepout = step.func(fit)
fit = glm(target ~ v50, data=train[1:80000,],family='binomial')
predicted = predict(fit, newdata = data.frame(v50 = train[80001:nrow(train),"v50"]))
lab.test = train[80001:nrow(train),"target"]
length(predicted[!is.na(predicted)])
length(lab.test[!is.na(predicted)])
predicted[1:10]
plot(predicted)
sum(predicted[!is.na(predicted)]<0.5)
length(predicted[!is.na(predicted)])
lab.test[1:50]
predicted[head(which(lab.test==0), 10)]
pred.new = predicted[!is.na(predicted)]
pred.new[pred.new<1] = 0
pred.new[pred.new>=1] = 1
sum(pred.new==lab.test[!is.na(predicted)])

imp.col = c("v4", "v10", "v12", "v14", "v17", "v21", "v33", "v34", "v40", "v48", "v50", "v61", "v64", "v76", "v93", "v106", "v114", "v121", "v123", "v129", "v130", "v24", "v31", "v47", "v66", "v79","v110")
temp.train = train[select.train, imp.col]
temp.test = train[-select.train, imp.col]
temp.train.lab = train[select.train, "target"]
temp.test.lab = train[-select.train, "target"]


######################

temp = rbind(train0[sample(1:nrow(train0), 5000),],train1[sample(1:nrow(train1), 5000),])
fit = glm(as.factor(target) ~ ., data = temp, family = "binomial")

######################
sel = sample(1:nrow(temp), 7000)
temp.test = temp[-sel,2:ncol(temp)]
temp.train = temp[sel,2:ncol(temp)]
temp.train.lab = temp[sel, "target"]
temp.test.lab = temp[-sel, "target"]
sum(class.test.1 == temp.test.lab)/3000

corr.mat = cor(temp[complete.cases(temp), -categorical])
#temp.cor = temp[complete.cases(temp), -categorical]
#temp.cor
corr.mat = corr.mat[2:nrow(corr.mat), 2:ncol(corr.mat)]
summary(corr.mat)

corr.high = apply(corr.mat, 2, function(i) return(which(i>0.95)))
corr.high.list = lapply(corr.high, function(i) (length(i)>1))
corr.high.which = do.call("c", corr.high.list)
corr.high[corr.high.which]

corr.remove.var = c("v12", "v25", "v32", "v37","v41", "v43", "v46", "v53","v55", "v60", "v63","v64", "v65","v67", "v73", "v76","v77","v83","v89", "v95","v96", "v104", "v105","v111","v114","v118", "v121", "v128")