trainData <- read.csv("train.csv")

train <- trainData[complete.cases(trainData),]
del = array()
i = 1
for (j in 1:ncol(train)){
  if (class(train[,j]) == "factor"){
    del[i] = j
    i = i + 1
  }
}
X <- train[,-c(1,2,del)]
y <- as.factor(train[,2]) # make response variable a factor

index = 1:nrow(X)
samplesize = 40000
Sample = sample(index, size = samplesize)

X.sub <- X[Sample[1:(samplesize/2)],]
y.sub <- y[Sample[1:(samplesize/2)]]

X.test <- X[Sample[(samplesize/2+1):samplesize],]
y.test <- y[Sample[(samplesize/2+1):samplesize]]

install.packages("randomForest")
require("randomForest")

ntree=100
begTime <- Sys.time()
RF <- randomForest(X.sub, y.sub, importance = TRUE, ntree=ntree, mtry=10,
                   na.action=na.roughfix, #can also use na.action = na.omit
                   replace=FALSE)
runTime <- Sys.time()-begTime
runTime
# about 1.11 min

plot(RF)
print(RF)
RF$mtry
round(importance(RF), 2) # check importance
varImpPlot(RF)

pred <- as.vector(as.numeric(predict(RF, newdata = X.test)))
sum(as.numeric(y.test) - pred != 0)/(samplesize/2) # test error rate

# parallel RF
install.packages("doParallel")
require(doParallel)

ntree = 100; numCore = 4
rep <- ntree/numCore # tree / numCore
registerDoParallel(cores=numCore)

begTime <- Sys.time()
rf <- foreach(ntree=rep(rep, numCore), .combine=combine,
              .packages="randomForest") %dopar%
      randomForest(X.sub, y.sub,
                  ntree=ntree,
                  mtry=10,
                  importance=TRUE,
                  na.action=na.roughfix, #can also use na.action = na.omit
                  replace=FALSE)
runTime <- Sys.time()-begTime
runTime
# about 27 secs

plot(rf)
pred <- as.vector(as.numeric(predict(rf, newdata = X.test)))
sum(as.numeric(y.test) - pred != 0)/(samplesize/2) # error rate







install.packages("ada")
require("ada")
require("rpart")

adaboost <- ada(X, y, loss="logistic", type="real", iter=100, nu=0.1, bag.frac = 0.5,
                rpart.control(maxdepth=1,cp=-1,minsplit=0))