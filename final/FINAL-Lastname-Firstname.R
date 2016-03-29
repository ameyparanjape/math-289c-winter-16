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


####################################################################################


# The following code is not in R

import numpy as np
import pandas as pd
import sklearn
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestClassifier,ExtraTreesClassifier
import pickle
from sklearn.metrics import log_loss
from sklearn.utils import shuffle
import warnings
warnings.filterwarnings("ignore", category=DeprecationWarning)


plt.style.use('fivethirtyeight')

train1 = pd.read_csv('train.csv',index_col=[0])
test1 = pd.read_csv('test.csv',index_col=[0])

dropping_list= ['v98','v70','v117','v87','v36','v82','v102','v5','v81','v109','v108','v128','v120','v124','v115','v69','v78','v28','v16','v6','v131','v99','v1','v45','v89','v122','v57','v2','v58','v54','v37','v88','v100','v119','v9','v127','v8','v80','v63','v18','v35','v25','v105','v90','v126','v85','v97','v118','v46','v103','v53','v68','v44','v11','v42','v27','v59','v86','v94','v116','v7','v111','v101','v84','v20','v4','v26','v43','v15','v60','v39','v77','v61','v49','v32','v73','v104','v55','v51','v92','v13','v83','v19','v67','v41','v121','v95','v123','v65','v23','v29','v33','v96','v106','v130','v93','v17','v76','v48','v64','v38','v74','v3']

# print(len(dropping_list))
# quit()
ave_loss = []
ave_scores = []
#drop unrelated features
num_del = len(dropping_list)-10
for dro in list(reversed(range(num_del))):
  # for dro in range(1):
  train = train1.drop(dropping_list[dro-1:],axis=1)
test = test1.drop(dropping_list[dro-1:],axis=1)
num_missing = train1.isnull().sum(axis=1)
train['v132'] = num_missing
num_missing = test1.isnull().sum(axis=1)
test['v132'] = num_missing
target = train['target']
train = train.drop('target',axis=1)
tr_col = train.columns
# print(tr_col)
# quit()
for (train_name, train_series), (test_name, test_series) in zip(train.iteritems(),test.iteritems()):
  if train_series.dtype == 'O':
  #for objects: factorize
  train[train_name], tmp_indexer = pd.factorize(train[train_name])
test[test_name] = tmp_indexer.get_indexer(test[test_name])
#but now we have -1 values (NaN)
else:
  #for int or float: fill NaN
  tmp_len = len(train[train_series.isnull()])
if tmp_len>0:
  #print "mean", train_series.mean()
  train.loc[train_series.isnull(), train_name] = -999 
#and Test
tmp_len = len(test[test_series.isnull()])
if tmp_len>0:
  test.loc[test_series.isnull(), test_name] = -999

num_data = target.shape[0]
valid_num = int(num_data*8/10)

scores = []
clf_pred = []
for _ in range(7):
  
  train, target = shuffle(train,target,random_state=0)

y_train = target[:valid_num]
X_train = train[:valid_num]
y_test = target[valid_num:]
X_test = train[valid_num:]

m = int(len(train.columns)/2)

# estimator = ExtraTreesClassifier(n_estimators=2000,max_features= m,criterion= 'entropy',min_samples_split= 4,\
# max_depth= 35, min_samples_leaf= 2, n_jobs = -1)
estimator = RandomForestClassifier(n_estimators=200,max_features= m,criterion= 'entropy',min_samples_split= 4,\
                                   max_depth= 35, min_samples_leaf= 2, n_jobs = -1)
estimator.fit(X_train, y_train)
clf_pred.append(estimator.score(X_test,y_test))
clf_probs = estimator.predict_proba(X_test)
score = log_loss(y_test, clf_probs)
scores.append(score)
scores = np.array(scores)
clf_pred = np.array(clf_pred)
print("logloss: ",np.mean(score)," accuracy: ",np.mean(clf_pred))
importance = estimator.feature_importances_
indices = np.argsort(importance)[::-1]
ave_loss.append(np.mean(score))
ave_scores.append(np.mean(clf_pred))
# for f in range(train.shape[1]):
#     print("%d. feature %s (%f)" % (f + 1, tr_col[indices[f]], importance[indices[f]]))

# for f in range(train.shape[1]):
#     print("'%s'," % tr_col[indices[f]],end='')
print(1-dro/num_del)

fig,ax = plt.subplots(1,2)
ax[0].plot(ave_loss)
ax[0].set_title("Log_loss vs #features deleted")
ax[0].set_xlabel('# features deleted')
ax[0].set_ylabel('Log_loss')
ax[0].XTickLabelRotation=45
ax[1].plot(ave_scores)
ax[1].set_title("Accuracy vs #features deleted")
ax[1].set_xlabel('# features deleted')
ax[1].set_ylabel('Acuracy')
ax[1].XTickLabelRotation=45
plt.show()

quit()

plt.figure()
plt.title("Feature importances")
plt.bar(range(train.shape[1]), importance[indices],
        color="r", align="center")
plt.xticks(range(train.shape[1]), indices+1,fontsize=7)
plt.xlim([-1, train.shape[1]])
plt.show()

quit()
y_predict = estimator.predict_proba(test)[:,1]
output = pd.DataFrame(index=test.index)
output['PredictedProb'] = y_predict
output.to_csv("answers8.csv")

# pickle_out = open('extra_trees.pickle','wb')
# pickle.dump(estimator,pickle_out)
# pickle_out.close()


# importances = estimator.feature_importances_
# std = np.std([tree.feature_importances_ for tree in estimator.estimators_],
#              axis=0)
# indices = np.argsort(importances)[::-1]

# # Print the feature ranking
# print("Feature ranking:")

# for f in range(X_train.shape[1]):
#     print("%d. feature %s (%f)" % (f + 1, X_train.columns[indices[f]], importances[indices[f]]))

# # Plot the feature importances of the forest
# plt.figure()
# plt.title("Feature importances")
# plt.bar(range(X_train.shape[1]), importances[indices],
#        color="r", yerr=std[indices], align="center")
# plt.xticks(range(X_train.shape[1]), indices)
# plt.xlim([-1, X_train.shape[1]])
# plt.show()


####################################################################################


# Replace numerical columns with median
train_filled = train
# For each column
for(i in 1:ncol(train_filled)) {
  # If column is numeric
  if(is.numeric( train_filled[,i] )) {
    median = median(train_filled[,i], na.rm = TRUE)
    
    # Replace all values with medians
    for(j in 1:nrow(train_filled)) {
      if( is.na(train_filled[j,i]) ) {
        train_filled[j,i] <- median 
      }
    }
  }
}

train_filled_3 = read.csv("~/Desktop/train_filled.csv")
# Replace missing categorical variables with mode

# Turn missing values into NAs instead of ""s
replace_with_NA <- function(column){
  column[ column == ""] <- NA
  return(column)
}

train_filled_2 = train_filled
for(i in 1:ncol(train_filled_2)) {
  if ( is.factor(train_filled_2[,i]) ) {
    train_filled_2[,i] = replace_with_NA(train_filled_2[,i])
  }
}

# Define function to get mode of categorical column
replace.mode <- function(column){
  mode = names( sort(table(column),decreasing=TRUE)[1] )
  if(mode == "") {
    mode = names( sort(table(column),decreasing=TRUE)[2] )
  }
  
  column[ column == ""] = mode
  return(column)
}

# Replace ""s in each column with mode
#for(i in 1:ncol(train_filled_2)) {
for(i in 1:ncol(train_filled_3)) {
  if(is.factor(train_filled_3[,i])) {
    train_filled_3[,i] = replace.mode(train_filled_3[,i])
  }
}


####################################################################################


# The following code is not in R

{
  "cells": [
    {
      "cell_type": "code",
      "execution_count": 3,
      "metadata": {
        "collapsed": false
      },
      "outputs": [
        {
          "data": {
            "text/html": [
              "<div>\n",
              "<table border=\"1\" class=\"dataframe\">\n",
              "  <thead>\n",
              "    <tr style=\"text-align: right;\">\n",
              "      <th></th>\n",
              "      <th>ID</th>\n",
              "      <th>target</th>\n",
              "      <th>v1</th>\n",
              "      <th>v2</th>\n",
              "      <th>v3</th>\n",
              "      <th>v4</th>\n",
              "      <th>v5</th>\n",
              "      <th>v6</th>\n",
              "      <th>v7</th>\n",
              "      <th>v8</th>\n",
              "      <th>...</th>\n",
              "      <th>v122</th>\n",
              "      <th>v123</th>\n",
              "      <th>v124</th>\n",
              "      <th>v125</th>\n",
              "      <th>v126</th>\n",
              "      <th>v127</th>\n",
              "      <th>v128</th>\n",
              "      <th>v129</th>\n",
              "      <th>v130</th>\n",
              "      <th>v131</th>\n",
              "    </tr>\n",
              "  </thead>\n",
              "  <tbody>\n",
              "    <tr>\n",
              "      <th>0</th>\n",
              "      <td>3</td>\n",
              "      <td>1</td>\n",
              "      <td>1.335739e+00</td>\n",
              "      <td>8.727474</td>\n",
              "      <td>C</td>\n",
              "      <td>3.921026</td>\n",
              "      <td>7.915266</td>\n",
              "      <td>2.599278</td>\n",
              "      <td>3.176895</td>\n",
              "      <td>0.012941</td>\n",
              "      <td>...</td>\n",
              "      <td>8.000000</td>\n",
              "      <td>1.989780</td>\n",
              "      <td>3.575369e-02</td>\n",
              "      <td>AU</td>\n",
              "      <td>1.804126</td>\n",
              "      <td>3.113719</td>\n",
              "      <td>2.024285</td>\n",
              "      <td>0</td>\n",
              "      <td>0.636365</td>\n",
              "      <td>2.857144e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>1</th>\n",
              "      <td>4</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>9.191265</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>2.301630</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>5.988956e-01</td>\n",
              "      <td>AF</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>1.957825</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>2</th>\n",
              "      <td>5</td>\n",
              "      <td>1</td>\n",
              "      <td>9.438769e-01</td>\n",
              "      <td>5.310079</td>\n",
              "      <td>C</td>\n",
              "      <td>4.410969</td>\n",
              "      <td>5.326159</td>\n",
              "      <td>3.979592</td>\n",
              "      <td>3.928571</td>\n",
              "      <td>0.019645</td>\n",
              "      <td>...</td>\n",
              "      <td>9.333333</td>\n",
              "      <td>2.477596</td>\n",
              "      <td>1.345191e-02</td>\n",
              "      <td>AE</td>\n",
              "      <td>1.773709</td>\n",
              "      <td>3.922193</td>\n",
              "      <td>1.120468</td>\n",
              "      <td>2</td>\n",
              "      <td>0.883118</td>\n",
              "      <td>1.176472e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>3</th>\n",
              "      <td>6</td>\n",
              "      <td>1</td>\n",
              "      <td>7.974146e-01</td>\n",
              "      <td>8.304757</td>\n",
              "      <td>C</td>\n",
              "      <td>4.225930</td>\n",
              "      <td>11.627438</td>\n",
              "      <td>2.097700</td>\n",
              "      <td>1.987549</td>\n",
              "      <td>0.171947</td>\n",
              "      <td>...</td>\n",
              "      <td>7.018256</td>\n",
              "      <td>1.812795</td>\n",
              "      <td>2.267384e-03</td>\n",
              "      <td>CJ</td>\n",
              "      <td>1.415230</td>\n",
              "      <td>2.954381</td>\n",
              "      <td>1.990847</td>\n",
              "      <td>1</td>\n",
              "      <td>1.677108</td>\n",
              "      <td>1.034483e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>4</th>\n",
              "      <td>8</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>Z</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>5</th>\n",
              "      <td>9</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>8.856791</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0.359993</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>4.986116e-02</td>\n",
              "      <td>X</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>1.536222</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>6</th>\n",
              "      <td>12</td>\n",
              "      <td>0</td>\n",
              "      <td>8.998057e-01</td>\n",
              "      <td>7.312995</td>\n",
              "      <td>C</td>\n",
              "      <td>3.494148</td>\n",
              "      <td>9.946200</td>\n",
              "      <td>1.926070</td>\n",
              "      <td>1.770427</td>\n",
              "      <td>0.066251</td>\n",
              "      <td>...</td>\n",
              "      <td>3.476299</td>\n",
              "      <td>1.992594</td>\n",
              "      <td>8.375832e-02</td>\n",
              "      <td>BJ</td>\n",
              "      <td>3.276100</td>\n",
              "      <td>1.623298</td>\n",
              "      <td>2.266575</td>\n",
              "      <td>0</td>\n",
              "      <td>2.263736</td>\n",
              "      <td>9.708730e-01</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>7</th>\n",
              "      <td>21</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>BY</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>8</th>\n",
              "      <td>22</td>\n",
              "      <td>0</td>\n",
              "      <td>2.078651e+00</td>\n",
              "      <td>8.462619</td>\n",
              "      <td>NaN</td>\n",
              "      <td>3.739030</td>\n",
              "      <td>5.265636</td>\n",
              "      <td>1.573033</td>\n",
              "      <td>2.303371</td>\n",
              "      <td>0.015869</td>\n",
              "      <td>...</td>\n",
              "      <td>8.148148</td>\n",
              "      <td>1.875560</td>\n",
              "      <td>1.865950e-02</td>\n",
              "      <td>S</td>\n",
              "      <td>1.159637</td>\n",
              "      <td>5.582865</td>\n",
              "      <td>1.105283</td>\n",
              "      <td>0</td>\n",
              "      <td>1.170731</td>\n",
              "      <td>3.333334e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>9</th>\n",
              "      <td>23</td>\n",
              "      <td>1</td>\n",
              "      <td>1.144802e+00</td>\n",
              "      <td>5.880606</td>\n",
              "      <td>C</td>\n",
              "      <td>3.244469</td>\n",
              "      <td>9.538384</td>\n",
              "      <td>2.500001</td>\n",
              "      <td>1.559405</td>\n",
              "      <td>0.412610</td>\n",
              "      <td>...</td>\n",
              "      <td>7.325843</td>\n",
              "      <td>4.896617</td>\n",
              "      <td>8.943653e-03</td>\n",
              "      <td>E</td>\n",
              "      <td>1.344550</td>\n",
              "      <td>1.601176</td>\n",
              "      <td>1.928009</td>\n",
              "      <td>0</td>\n",
              "      <td>3.174603</td>\n",
              "      <td>1.000000e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>10</th>\n",
              "      <td>24</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>AR</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>11</th>\n",
              "      <td>27</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>AM</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>12</th>\n",
              "      <td>28</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>Z</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>13</th>\n",
              "      <td>30</td>\n",
              "      <td>1</td>\n",
              "      <td>1.400267e+00</td>\n",
              "      <td>5.367204</td>\n",
              "      <td>C</td>\n",
              "      <td>4.122155</td>\n",
              "      <td>8.137188</td>\n",
              "      <td>2.983080</td>\n",
              "      <td>2.640249</td>\n",
              "      <td>0.211851</td>\n",
              "      <td>...</td>\n",
              "      <td>8.817203</td>\n",
              "      <td>2.096062</td>\n",
              "      <td>5.459061e-07</td>\n",
              "      <td>AQ</td>\n",
              "      <td>1.731656</td>\n",
              "      <td>6.102516</td>\n",
              "      <td>1.388117</td>\n",
              "      <td>4</td>\n",
              "      <td>1.220911</td>\n",
              "      <td>1.878453e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>14</th>\n",
              "      <td>31</td>\n",
              "      <td>1</td>\n",
              "      <td>2.260036e+00</td>\n",
              "      <td>14.693263</td>\n",
              "      <td>C</td>\n",
              "      <td>5.150750</td>\n",
              "      <td>8.554136</td>\n",
              "      <td>1.954626</td>\n",
              "      <td>2.931936</td>\n",
              "      <td>0.041446</td>\n",
              "      <td>...</td>\n",
              "      <td>5.038168</td>\n",
              "      <td>1.836667</td>\n",
              "      <td>5.973979e-03</td>\n",
              "      <td>Z</td>\n",
              "      <td>3.242128</td>\n",
              "      <td>0.818063</td>\n",
              "      <td>2.400050</td>\n",
              "      <td>0</td>\n",
              "      <td>1.166666</td>\n",
              "      <td>2.857143e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>15</th>\n",
              "      <td>32</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>BY</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>16</th>\n",
              "      <td>33</td>\n",
              "      <td>1</td>\n",
              "      <td>6.228961e-01</td>\n",
              "      <td>7.024732</td>\n",
              "      <td>C</td>\n",
              "      <td>4.193688</td>\n",
              "      <td>6.288177</td>\n",
              "      <td>2.132436</td>\n",
              "      <td>3.198654</td>\n",
              "      <td>0.407525</td>\n",
              "      <td>...</td>\n",
              "      <td>6.086957</td>\n",
              "      <td>2.812624</td>\n",
              "      <td>2.474223e-01</td>\n",
              "      <td>BJ</td>\n",
              "      <td>1.865530</td>\n",
              "      <td>3.345960</td>\n",
              "      <td>0.796533</td>\n",
              "      <td>0</td>\n",
              "      <td>1.389474</td>\n",
              "      <td>6.060615e-01</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>17</th>\n",
              "      <td>34</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>Z</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>2</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>18</th>\n",
              "      <td>35</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>E</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>19</th>\n",
              "      <td>36</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>AZ</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>20</th>\n",
              "      <td>37</td>\n",
              "      <td>1</td>\n",
              "      <td>9.438780e-01</td>\n",
              "      <td>5.927194</td>\n",
              "      <td>C</td>\n",
              "      <td>4.404372</td>\n",
              "      <td>9.045057</td>\n",
              "      <td>2.551021</td>\n",
              "      <td>2.693878</td>\n",
              "      <td>0.085665</td>\n",
              "      <td>...</td>\n",
              "      <td>6.632125</td>\n",
              "      <td>2.419037</td>\n",
              "      <td>1.847926e-02</td>\n",
              "      <td>U</td>\n",
              "      <td>1.884567</td>\n",
              "      <td>2.602041</td>\n",
              "      <td>1.959776</td>\n",
              "      <td>1</td>\n",
              "      <td>1.121212</td>\n",
              "      <td>1.351351e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>21</th>\n",
              "      <td>39</td>\n",
              "      <td>1</td>\n",
              "      <td>1.289841e+00</td>\n",
              "      <td>4.788645</td>\n",
              "      <td>C</td>\n",
              "      <td>4.283417</td>\n",
              "      <td>10.719571</td>\n",
              "      <td>3.107570</td>\n",
              "      <td>1.962151</td>\n",
              "      <td>0.080812</td>\n",
              "      <td>...</td>\n",
              "      <td>5.968993</td>\n",
              "      <td>2.169826</td>\n",
              "      <td>4.919843e-03</td>\n",
              "      <td>CD</td>\n",
              "      <td>1.460793</td>\n",
              "      <td>1.531375</td>\n",
              "      <td>1.964652</td>\n",
              "      <td>1</td>\n",
              "      <td>2.111675</td>\n",
              "      <td>1.346155e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>22</th>\n",
              "      <td>40</td>\n",
              "      <td>1</td>\n",
              "      <td>7.288239e-01</td>\n",
              "      <td>4.073244</td>\n",
              "      <td>C</td>\n",
              "      <td>4.130054</td>\n",
              "      <td>9.032563</td>\n",
              "      <td>4.149705</td>\n",
              "      <td>1.917269</td>\n",
              "      <td>2.767934</td>\n",
              "      <td>...</td>\n",
              "      <td>3.903742</td>\n",
              "      <td>3.401924</td>\n",
              "      <td>7.107372e-01</td>\n",
              "      <td>AE</td>\n",
              "      <td>1.840384</td>\n",
              "      <td>3.779547</td>\n",
              "      <td>1.798435</td>\n",
              "      <td>0</td>\n",
              "      <td>1.986302</td>\n",
              "      <td>8.275852e-01</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>23</th>\n",
              "      <td>42</td>\n",
              "      <td>1</td>\n",
              "      <td>3.944563e+00</td>\n",
              "      <td>5.718516</td>\n",
              "      <td>C</td>\n",
              "      <td>2.205080</td>\n",
              "      <td>5.340648</td>\n",
              "      <td>2.010356</td>\n",
              "      <td>1.657021</td>\n",
              "      <td>3.233160</td>\n",
              "      <td>...</td>\n",
              "      <td>6.404715</td>\n",
              "      <td>10.691157</td>\n",
              "      <td>3.065766e+00</td>\n",
              "      <td>AU</td>\n",
              "      <td>1.813419</td>\n",
              "      <td>1.233628</td>\n",
              "      <td>0.626694</td>\n",
              "      <td>2</td>\n",
              "      <td>4.250001</td>\n",
              "      <td>2.422145e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>24</th>\n",
              "      <td>43</td>\n",
              "      <td>1</td>\n",
              "      <td>4.045725e+00</td>\n",
              "      <td>3.992607</td>\n",
              "      <td>C</td>\n",
              "      <td>3.598096</td>\n",
              "      <td>7.946330</td>\n",
              "      <td>1.709742</td>\n",
              "      <td>2.365804</td>\n",
              "      <td>7.827175</td>\n",
              "      <td>...</td>\n",
              "      <td>5.226130</td>\n",
              "      <td>5.259272</td>\n",
              "      <td>2.690857e+00</td>\n",
              "      <td>BD</td>\n",
              "      <td>1.447090</td>\n",
              "      <td>2.559641</td>\n",
              "      <td>1.509052</td>\n",
              "      <td>0</td>\n",
              "      <td>3.260504</td>\n",
              "      <td>2.268042e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>25</th>\n",
              "      <td>46</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>CG</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>26</th>\n",
              "      <td>51</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>J</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>27</th>\n",
              "      <td>52</td>\n",
              "      <td>1</td>\n",
              "      <td>2.920282e-01</td>\n",
              "      <td>12.715811</td>\n",
              "      <td>C</td>\n",
              "      <td>4.999724</td>\n",
              "      <td>5.998656</td>\n",
              "      <td>2.020520</td>\n",
              "      <td>2.620363</td>\n",
              "      <td>0.058548</td>\n",
              "      <td>...</td>\n",
              "      <td>7.796611</td>\n",
              "      <td>1.697821</td>\n",
              "      <td>7.351065e-03</td>\n",
              "      <td>N</td>\n",
              "      <td>1.491973</td>\n",
              "      <td>4.202842</td>\n",
              "      <td>1.306053</td>\n",
              "      <td>0</td>\n",
              "      <td>0.722892</td>\n",
              "      <td>6.666657e-01</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>28</th>\n",
              "      <td>54</td>\n",
              "      <td>1</td>\n",
              "      <td>5.172412e-01</td>\n",
              "      <td>8.528544</td>\n",
              "      <td>C</td>\n",
              "      <td>4.075372</td>\n",
              "      <td>5.726834</td>\n",
              "      <td>2.534949</td>\n",
              "      <td>3.643990</td>\n",
              "      <td>0.749568</td>\n",
              "      <td>...</td>\n",
              "      <td>5.188680</td>\n",
              "      <td>1.788132</td>\n",
              "      <td>8.594683e-02</td>\n",
              "      <td>AR</td>\n",
              "      <td>2.105971</td>\n",
              "      <td>0.524231</td>\n",
              "      <td>0.785667</td>\n",
              "      <td>0</td>\n",
              "      <td>0.603581</td>\n",
              "      <td>1.016950e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>29</th>\n",
              "      <td>55</td>\n",
              "      <td>0</td>\n",
              "      <td>2.415670e+00</td>\n",
              "      <td>14.960392</td>\n",
              "      <td>C</td>\n",
              "      <td>4.356980</td>\n",
              "      <td>8.520510</td>\n",
              "      <td>2.132753</td>\n",
              "      <td>2.023938</td>\n",
              "      <td>0.035822</td>\n",
              "      <td>...</td>\n",
              "      <td>5.585586</td>\n",
              "      <td>2.451680</td>\n",
              "      <td>6.640126e-03</td>\n",
              "      <td>Z</td>\n",
              "      <td>1.442224</td>\n",
              "      <td>2.244287</td>\n",
              "      <td>2.373578</td>\n",
              "      <td>0</td>\n",
              "      <td>1.634408</td>\n",
              "      <td>3.157894e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>...</th>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "      <td>...</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114291</th>\n",
              "      <td>228655</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>BM</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114292</th>\n",
              "      <td>228657</td>\n",
              "      <td>1</td>\n",
              "      <td>2.768086e-01</td>\n",
              "      <td>7.055455</td>\n",
              "      <td>C</td>\n",
              "      <td>3.823010</td>\n",
              "      <td>9.619069</td>\n",
              "      <td>2.194513</td>\n",
              "      <td>1.825436</td>\n",
              "      <td>0.328563</td>\n",
              "      <td>...</td>\n",
              "      <td>7.116105</td>\n",
              "      <td>3.000683</td>\n",
              "      <td>9.536287e-02</td>\n",
              "      <td>BM</td>\n",
              "      <td>1.528000</td>\n",
              "      <td>2.777432</td>\n",
              "      <td>1.985969</td>\n",
              "      <td>0</td>\n",
              "      <td>1.639345</td>\n",
              "      <td>3.999997e-01</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114293</th>\n",
              "      <td>228658</td>\n",
              "      <td>1</td>\n",
              "      <td>1.983914e+00</td>\n",
              "      <td>11.296659</td>\n",
              "      <td>C</td>\n",
              "      <td>6.459140</td>\n",
              "      <td>9.193006</td>\n",
              "      <td>1.849866</td>\n",
              "      <td>3.217159</td>\n",
              "      <td>0.055774</td>\n",
              "      <td>...</td>\n",
              "      <td>6.478874</td>\n",
              "      <td>1.535538</td>\n",
              "      <td>1.466965e-01</td>\n",
              "      <td>G</td>\n",
              "      <td>2.781526</td>\n",
              "      <td>1.131032</td>\n",
              "      <td>2.950252</td>\n",
              "      <td>0</td>\n",
              "      <td>0.800000</td>\n",
              "      <td>3.333334e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114294</th>\n",
              "      <td>228659</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>X</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114295</th>\n",
              "      <td>228660</td>\n",
              "      <td>1</td>\n",
              "      <td>8.743610e-01</td>\n",
              "      <td>8.529843</td>\n",
              "      <td>C</td>\n",
              "      <td>4.391163</td>\n",
              "      <td>9.575062</td>\n",
              "      <td>2.646711</td>\n",
              "      <td>2.103190</td>\n",
              "      <td>1.300537</td>\n",
              "      <td>...</td>\n",
              "      <td>6.937500</td>\n",
              "      <td>3.171537</td>\n",
              "      <td>1.309991e+00</td>\n",
              "      <td>AR</td>\n",
              "      <td>1.386343</td>\n",
              "      <td>2.880071</td>\n",
              "      <td>2.140204</td>\n",
              "      <td>2</td>\n",
              "      <td>1.722846</td>\n",
              "      <td>1.043479e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114296</th>\n",
              "      <td>228663</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>Z</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114297</th>\n",
              "      <td>228666</td>\n",
              "      <td>1</td>\n",
              "      <td>1.603274e+00</td>\n",
              "      <td>11.493653</td>\n",
              "      <td>C</td>\n",
              "      <td>4.328798</td>\n",
              "      <td>8.055434</td>\n",
              "      <td>1.848820</td>\n",
              "      <td>2.532499</td>\n",
              "      <td>0.171407</td>\n",
              "      <td>...</td>\n",
              "      <td>9.122806</td>\n",
              "      <td>2.250368</td>\n",
              "      <td>4.670769e-02</td>\n",
              "      <td>AF</td>\n",
              "      <td>1.527338</td>\n",
              "      <td>3.231824</td>\n",
              "      <td>1.246700</td>\n",
              "      <td>0</td>\n",
              "      <td>1.581749</td>\n",
              "      <td>1.730770e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114298</th>\n",
              "      <td>228668</td>\n",
              "      <td>1</td>\n",
              "      <td>9.477454e-01</td>\n",
              "      <td>8.941986</td>\n",
              "      <td>C</td>\n",
              "      <td>4.578888</td>\n",
              "      <td>14.882313</td>\n",
              "      <td>2.868852</td>\n",
              "      <td>2.402663</td>\n",
              "      <td>0.157079</td>\n",
              "      <td>...</td>\n",
              "      <td>6.666667</td>\n",
              "      <td>1.856606</td>\n",
              "      <td>9.564098e-03</td>\n",
              "      <td>S</td>\n",
              "      <td>1.586184</td>\n",
              "      <td>2.737578</td>\n",
              "      <td>2.785707</td>\n",
              "      <td>0</td>\n",
              "      <td>1.049040</td>\n",
              "      <td>1.626015e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114299</th>\n",
              "      <td>228670</td>\n",
              "      <td>1</td>\n",
              "      <td>9.688237e-07</td>\n",
              "      <td>6.163454</td>\n",
              "      <td>C</td>\n",
              "      <td>3.502739</td>\n",
              "      <td>7.202643</td>\n",
              "      <td>1.741765</td>\n",
              "      <td>2.165847</td>\n",
              "      <td>2.424658</td>\n",
              "      <td>...</td>\n",
              "      <td>6.339869</td>\n",
              "      <td>6.239218</td>\n",
              "      <td>3.054839e+00</td>\n",
              "      <td>V</td>\n",
              "      <td>1.291664</td>\n",
              "      <td>5.253692</td>\n",
              "      <td>1.026424</td>\n",
              "      <td>0</td>\n",
              "      <td>2.615385</td>\n",
              "      <td>5.482204e-07</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114300</th>\n",
              "      <td>228671</td>\n",
              "      <td>0</td>\n",
              "      <td>1.539335e+00</td>\n",
              "      <td>4.845475</td>\n",
              "      <td>C</td>\n",
              "      <td>4.445304</td>\n",
              "      <td>8.605211</td>\n",
              "      <td>1.906204</td>\n",
              "      <td>2.382753</td>\n",
              "      <td>1.142149</td>\n",
              "      <td>...</td>\n",
              "      <td>8.505747</td>\n",
              "      <td>2.364559</td>\n",
              "      <td>6.571835e-01</td>\n",
              "      <td>CG</td>\n",
              "      <td>1.380594</td>\n",
              "      <td>7.233358</td>\n",
              "      <td>1.495398</td>\n",
              "      <td>0</td>\n",
              "      <td>1.511110</td>\n",
              "      <td>1.848739e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114301</th>\n",
              "      <td>228673</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>BJ</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114302</th>\n",
              "      <td>228677</td>\n",
              "      <td>1</td>\n",
              "      <td>2.093352e+00</td>\n",
              "      <td>5.696141</td>\n",
              "      <td>C</td>\n",
              "      <td>5.105297</td>\n",
              "      <td>8.836584</td>\n",
              "      <td>2.734559</td>\n",
              "      <td>2.772276</td>\n",
              "      <td>0.195443</td>\n",
              "      <td>...</td>\n",
              "      <td>6.986301</td>\n",
              "      <td>2.601368</td>\n",
              "      <td>1.805020e-02</td>\n",
              "      <td>AK</td>\n",
              "      <td>1.649315</td>\n",
              "      <td>3.818954</td>\n",
              "      <td>2.049363</td>\n",
              "      <td>1</td>\n",
              "      <td>1.469388</td>\n",
              "      <td>2.222222e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114303</th>\n",
              "      <td>228680</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>J</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>2</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114304</th>\n",
              "      <td>228682</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>H</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114305</th>\n",
              "      <td>228683</td>\n",
              "      <td>1</td>\n",
              "      <td>1.030641e+00</td>\n",
              "      <td>13.003754</td>\n",
              "      <td>C</td>\n",
              "      <td>4.512371</td>\n",
              "      <td>8.928965</td>\n",
              "      <td>2.548746</td>\n",
              "      <td>2.583565</td>\n",
              "      <td>1.756440</td>\n",
              "      <td>...</td>\n",
              "      <td>6.331360</td>\n",
              "      <td>1.345799</td>\n",
              "      <td>2.027626e-01</td>\n",
              "      <td>BO</td>\n",
              "      <td>1.535785</td>\n",
              "      <td>2.950904</td>\n",
              "      <td>1.790912</td>\n",
              "      <td>1</td>\n",
              "      <td>1.110512</td>\n",
              "      <td>1.553399e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114306</th>\n",
              "      <td>228684</td>\n",
              "      <td>1</td>\n",
              "      <td>2.051611e+00</td>\n",
              "      <td>8.259554</td>\n",
              "      <td>C</td>\n",
              "      <td>4.466166</td>\n",
              "      <td>11.291191</td>\n",
              "      <td>2.269140</td>\n",
              "      <td>3.510343</td>\n",
              "      <td>0.215781</td>\n",
              "      <td>...</td>\n",
              "      <td>5.898438</td>\n",
              "      <td>3.043102</td>\n",
              "      <td>7.758699e-03</td>\n",
              "      <td>BM</td>\n",
              "      <td>2.048144</td>\n",
              "      <td>3.214972</td>\n",
              "      <td>1.805540</td>\n",
              "      <td>0</td>\n",
              "      <td>1.307412</td>\n",
              "      <td>1.933085e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114307</th>\n",
              "      <td>228688</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>CD</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114308</th>\n",
              "      <td>228691</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>BD</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114309</th>\n",
              "      <td>228695</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>A</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114310</th>\n",
              "      <td>228697</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>CB</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114311</th>\n",
              "      <td>228699</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>O</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114312</th>\n",
              "      <td>228701</td>\n",
              "      <td>1</td>\n",
              "      <td>1.142478e+00</td>\n",
              "      <td>8.477138</td>\n",
              "      <td>C</td>\n",
              "      <td>3.827255</td>\n",
              "      <td>8.539653</td>\n",
              "      <td>2.276136</td>\n",
              "      <td>1.905602</td>\n",
              "      <td>3.199958</td>\n",
              "      <td>...</td>\n",
              "      <td>7.437501</td>\n",
              "      <td>2.859732</td>\n",
              "      <td>3.258223e+00</td>\n",
              "      <td>AP</td>\n",
              "      <td>0.958445</td>\n",
              "      <td>2.382003</td>\n",
              "      <td>1.601009</td>\n",
              "      <td>1</td>\n",
              "      <td>1.851852</td>\n",
              "      <td>1.399999e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114313</th>\n",
              "      <td>228702</td>\n",
              "      <td>1</td>\n",
              "      <td>1.364308e+00</td>\n",
              "      <td>9.621454</td>\n",
              "      <td>C</td>\n",
              "      <td>4.342517</td>\n",
              "      <td>8.648951</td>\n",
              "      <td>2.772861</td>\n",
              "      <td>3.480826</td>\n",
              "      <td>2.208724</td>\n",
              "      <td>...</td>\n",
              "      <td>8.170212</td>\n",
              "      <td>2.427557</td>\n",
              "      <td>5.664891e-01</td>\n",
              "      <td>CI</td>\n",
              "      <td>2.091074</td>\n",
              "      <td>4.300331</td>\n",
              "      <td>1.738246</td>\n",
              "      <td>0</td>\n",
              "      <td>0.855932</td>\n",
              "      <td>1.980199e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114314</th>\n",
              "      <td>228705</td>\n",
              "      <td>0</td>\n",
              "      <td>3.633647e+00</td>\n",
              "      <td>3.052302</td>\n",
              "      <td>C</td>\n",
              "      <td>2.055147</td>\n",
              "      <td>7.770079</td>\n",
              "      <td>1.929975</td>\n",
              "      <td>2.459437</td>\n",
              "      <td>0.932377</td>\n",
              "      <td>...</td>\n",
              "      <td>6.842105</td>\n",
              "      <td>12.727869</td>\n",
              "      <td>5.673148e-01</td>\n",
              "      <td>CD</td>\n",
              "      <td>1.660675</td>\n",
              "      <td>3.074295</td>\n",
              "      <td>1.383798</td>\n",
              "      <td>0</td>\n",
              "      <td>2.958333</td>\n",
              "      <td>2.159624e+00</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114315</th>\n",
              "      <td>228707</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>AE</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114316</th>\n",
              "      <td>228708</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>AL</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114317</th>\n",
              "      <td>228710</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>E</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114318</th>\n",
              "      <td>228711</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>10.069277</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0.323324</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>1.567642e-01</td>\n",
              "      <td>Q</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>2.417606</td>\n",
              "      <td>2</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114319</th>\n",
              "      <td>228712</td>\n",
              "      <td>1</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>C</td>\n",
              "      <td>NaN</td>\n",
              "      <td>10.106144</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>0.309226</td>\n",
              "      <td>...</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>4.906576e-01</td>\n",
              "      <td>BW</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "      <td>3.526650</td>\n",
              "      <td>0</td>\n",
              "      <td>NaN</td>\n",
              "      <td>NaN</td>\n",
              "    </tr>\n",
              "    <tr>\n",
              "      <th>114320</th>\n",
              "      <td>228713</td>\n",
              "      <td>1</td>\n",
              "      <td>1.619763e+00</td>\n",
              "      <td>7.932978</td>\n",
              "      <td>C</td>\n",
              "      <td>4.640085</td>\n",
              "      <td>8.473141</td>\n",
              "      <td>2.351470</td>\n",
              "      <td>2.826766</td>\n",
              "      <td>3.479754</td>\n",
              "      <td>...</td>\n",
              "      <td>7.936508</td>\n",
              "      <td>2.944285</td>\n",
              "      <td>3.135205e+00</td>\n",
              "      <td>V</td>\n",
              "      <td>1.943149</td>\n",
              "      <td>4.385553</td>\n",
              "      <td>1.604493</td>\n",
              "      <td>0</td>\n",
              "      <td>1.787610</td>\n",
              "      <td>1.386138e+00</td>\n",
              "    </tr>\n",
              "  </tbody>\n",
              "</table>\n",
              "<p>114321 rows × 133 columns</p>\n",
              "</div>"
              ],
            "text/plain": [
              "            ID  target            v1         v2   v3        v4         v5  \\\n",
              "0            3       1  1.335739e+00   8.727474    C  3.921026   7.915266   \n",
              "1            4       1           NaN        NaN    C       NaN   9.191265   \n",
              "2            5       1  9.438769e-01   5.310079    C  4.410969   5.326159   \n",
              "3            6       1  7.974146e-01   8.304757    C  4.225930  11.627438   \n",
              "4            8       1           NaN        NaN    C       NaN        NaN   \n",
              "5            9       0           NaN        NaN    C       NaN   8.856791   \n",
              "6           12       0  8.998057e-01   7.312995    C  3.494148   9.946200   \n",
              "7           21       1           NaN        NaN    C       NaN        NaN   \n",
              "8           22       0  2.078651e+00   8.462619  NaN  3.739030   5.265636   \n",
              "9           23       1  1.144802e+00   5.880606    C  3.244469   9.538384   \n",
              "10          24       1           NaN        NaN    C       NaN        NaN   \n",
              "11          27       1           NaN        NaN    C       NaN        NaN   \n",
              "12          28       0           NaN        NaN    C       NaN        NaN   \n",
              "13          30       1  1.400267e+00   5.367204    C  4.122155   8.137188   \n",
              "14          31       1  2.260036e+00  14.693263    C  5.150750   8.554136   \n",
              "15          32       1           NaN        NaN    C       NaN        NaN   \n",
              "16          33       1  6.228961e-01   7.024732    C  4.193688   6.288177   \n",
              "17          34       1           NaN        NaN    C       NaN        NaN   \n",
              "18          35       1           NaN        NaN    C       NaN        NaN   \n",
              "19          36       1           NaN        NaN    C       NaN        NaN   \n",
              "20          37       1  9.438780e-01   5.927194    C  4.404372   9.045057   \n",
              "21          39       1  1.289841e+00   4.788645    C  4.283417  10.719571   \n",
              "22          40       1  7.288239e-01   4.073244    C  4.130054   9.032563   \n",
              "23          42       1  3.944563e+00   5.718516    C  2.205080   5.340648   \n",
              "24          43       1  4.045725e+00   3.992607    C  3.598096   7.946330   \n",
              "25          46       1           NaN        NaN    C       NaN        NaN   \n",
              "26          51       1           NaN        NaN    C       NaN        NaN   \n",
              "27          52       1  2.920282e-01  12.715811    C  4.999724   5.998656   \n",
              "28          54       1  5.172412e-01   8.528544    C  4.075372   5.726834   \n",
              "29          55       0  2.415670e+00  14.960392    C  4.356980   8.520510   \n",
              "...        ...     ...           ...        ...  ...       ...        ...   \n",
              "114291  228655       1           NaN        NaN    C       NaN        NaN   \n",
              "114292  228657       1  2.768086e-01   7.055455    C  3.823010   9.619069   \n",
              "114293  228658       1  1.983914e+00  11.296659    C  6.459140   9.193006   \n",
              "114294  228659       1           NaN        NaN    C       NaN        NaN   \n",
              "114295  228660       1  8.743610e-01   8.529843    C  4.391163   9.575062   \n",
              "114296  228663       1           NaN        NaN    C       NaN        NaN   \n",
              "114297  228666       1  1.603274e+00  11.493653    C  4.328798   8.055434   \n",
              "114298  228668       1  9.477454e-01   8.941986    C  4.578888  14.882313   \n",
              "114299  228670       1  9.688237e-07   6.163454    C  3.502739   7.202643   \n",
              "114300  228671       0  1.539335e+00   4.845475    C  4.445304   8.605211   \n",
              "114301  228673       1           NaN        NaN    C       NaN        NaN   \n",
              "114302  228677       1  2.093352e+00   5.696141    C  5.105297   8.836584   \n",
              "114303  228680       1           NaN        NaN    C       NaN        NaN   \n",
              "114304  228682       1           NaN        NaN    C       NaN        NaN   \n",
              "114305  228683       1  1.030641e+00  13.003754    C  4.512371   8.928965   \n",
              "114306  228684       1  2.051611e+00   8.259554    C  4.466166  11.291191   \n",
              "114307  228688       1           NaN        NaN    C       NaN        NaN   \n",
              "114308  228691       1           NaN        NaN    C       NaN        NaN   \n",
              "114309  228695       0           NaN        NaN    C       NaN        NaN   \n",
              "114310  228697       1           NaN        NaN    C       NaN        NaN   \n",
              "114311  228699       1           NaN        NaN    C       NaN        NaN   \n",
              "114312  228701       1  1.142478e+00   8.477138    C  3.827255   8.539653   \n",
              "114313  228702       1  1.364308e+00   9.621454    C  4.342517   8.648951   \n",
              "114314  228705       0  3.633647e+00   3.052302    C  2.055147   7.770079   \n",
              "114315  228707       1           NaN        NaN    C       NaN        NaN   \n",
              "114316  228708       1           NaN        NaN    C       NaN        NaN   \n",
              "114317  228710       1           NaN        NaN    C       NaN        NaN   \n",
              "114318  228711       1           NaN        NaN    C       NaN  10.069277   \n",
              "114319  228712       1           NaN        NaN    C       NaN  10.106144   \n",
              "114320  228713       1  1.619763e+00   7.932978    C  4.640085   8.473141   \n",
              "\n",
              "              v6        v7        v8      ...           v122       v123  \\\n",
              "0       2.599278  3.176895  0.012941      ...       8.000000   1.989780   \n",
              "1            NaN       NaN  2.301630      ...            NaN        NaN   \n",
              "2       3.979592  3.928571  0.019645      ...       9.333333   2.477596   \n",
              "3       2.097700  1.987549  0.171947      ...       7.018256   1.812795   \n",
              "4            NaN       NaN       NaN      ...            NaN        NaN   \n",
              "5            NaN       NaN  0.359993      ...            NaN        NaN   \n",
              "6       1.926070  1.770427  0.066251      ...       3.476299   1.992594   \n",
              "7            NaN       NaN       NaN      ...            NaN        NaN   \n",
              "8       1.573033  2.303371  0.015869      ...       8.148148   1.875560   \n",
              "9       2.500001  1.559405  0.412610      ...       7.325843   4.896617   \n",
              "10           NaN       NaN       NaN      ...            NaN        NaN   \n",
              "11           NaN       NaN       NaN      ...            NaN        NaN   \n",
              "12           NaN       NaN       NaN      ...            NaN        NaN   \n",
              "13      2.983080  2.640249  0.211851      ...       8.817203   2.096062   \n",
              "14      1.954626  2.931936  0.041446      ...       5.038168   1.836667   \n",
              "15           NaN       NaN       NaN      ...            NaN        NaN   \n",
              "16      2.132436  3.198654  0.407525      ...       6.086957   2.812624   \n",
              "17           NaN       NaN       NaN      ...            NaN        NaN   \n",
              "18           NaN       NaN       NaN      ...            NaN        NaN   \n",
              "19           NaN       NaN       NaN      ...            NaN        NaN   \n",
              "20      2.551021  2.693878  0.085665      ...       6.632125   2.419037   \n",
              "21      3.107570  1.962151  0.080812      ...       5.968993   2.169826   \n",
              "22      4.149705  1.917269  2.767934      ...       3.903742   3.401924   \n",
              "23      2.010356  1.657021  3.233160      ...       6.404715  10.691157   \n",
              "24      1.709742  2.365804  7.827175      ...       5.226130   5.259272   \n",
              "25           NaN       NaN       NaN      ...            NaN        NaN   \n",
              "26           NaN       NaN       NaN      ...            NaN        NaN   \n",
              "27      2.020520  2.620363  0.058548      ...       7.796611   1.697821   \n",
              "28      2.534949  3.643990  0.749568      ...       5.188680   1.788132   \n",
              "29      2.132753  2.023938  0.035822      ...       5.585586   2.451680   \n",
              "...          ...       ...       ...      ...            ...        ...   \n",
              "114291       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114292  2.194513  1.825436  0.328563      ...       7.116105   3.000683   \n",
              "114293  1.849866  3.217159  0.055774      ...       6.478874   1.535538   \n",
              "114294       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114295  2.646711  2.103190  1.300537      ...       6.937500   3.171537   \n",
              "114296       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114297  1.848820  2.532499  0.171407      ...       9.122806   2.250368   \n",
              "114298  2.868852  2.402663  0.157079      ...       6.666667   1.856606   \n",
              "114299  1.741765  2.165847  2.424658      ...       6.339869   6.239218   \n",
              "114300  1.906204  2.382753  1.142149      ...       8.505747   2.364559   \n",
              "114301       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114302  2.734559  2.772276  0.195443      ...       6.986301   2.601368   \n",
              "114303       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114304       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114305  2.548746  2.583565  1.756440      ...       6.331360   1.345799   \n",
              "114306  2.269140  3.510343  0.215781      ...       5.898438   3.043102   \n",
              "114307       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114308       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114309       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114310       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114311       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114312  2.276136  1.905602  3.199958      ...       7.437501   2.859732   \n",
              "114313  2.772861  3.480826  2.208724      ...       8.170212   2.427557   \n",
              "114314  1.929975  2.459437  0.932377      ...       6.842105  12.727869   \n",
              "114315       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114316       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114317       NaN       NaN       NaN      ...            NaN        NaN   \n",
              "114318       NaN       NaN  0.323324      ...            NaN        NaN   \n",
              "114319       NaN       NaN  0.309226      ...            NaN        NaN   \n",
              "114320  2.351470  2.826766  3.479754      ...       7.936508   2.944285   \n",
              "\n",
              "                v124  v125      v126      v127      v128  v129      v130  \\\n",
              "0       3.575369e-02    AU  1.804126  3.113719  2.024285     0  0.636365   \n",
              "1       5.988956e-01    AF       NaN       NaN  1.957825     0       NaN   \n",
              "2       1.345191e-02    AE  1.773709  3.922193  1.120468     2  0.883118   \n",
              "3       2.267384e-03    CJ  1.415230  2.954381  1.990847     1  1.677108   \n",
              "4                NaN     Z       NaN       NaN       NaN     0       NaN   \n",
              "5       4.986116e-02     X       NaN       NaN  1.536222     0       NaN   \n",
              "6       8.375832e-02    BJ  3.276100  1.623298  2.266575     0  2.263736   \n",
              "7                NaN    BY       NaN       NaN       NaN     0       NaN   \n",
              "8       1.865950e-02     S  1.159637  5.582865  1.105283     0  1.170731   \n",
              "9       8.943653e-03     E  1.344550  1.601176  1.928009     0  3.174603   \n",
              "10               NaN    AR       NaN       NaN       NaN     0       NaN   \n",
              "11               NaN    AM       NaN       NaN       NaN     0       NaN   \n",
              "12               NaN     Z       NaN       NaN       NaN     0       NaN   \n",
              "13      5.459061e-07    AQ  1.731656  6.102516  1.388117     4  1.220911   \n",
              "14      5.973979e-03     Z  3.242128  0.818063  2.400050     0  1.166666   \n",
              "15               NaN    BY       NaN       NaN       NaN     0       NaN   \n",
              "16      2.474223e-01    BJ  1.865530  3.345960  0.796533     0  1.389474   \n",
              "17               NaN     Z       NaN       NaN       NaN     2       NaN   \n",
              "18               NaN     E       NaN       NaN       NaN     1       NaN   \n",
              "19               NaN    AZ       NaN       NaN       NaN     1       NaN   \n",
              "20      1.847926e-02     U  1.884567  2.602041  1.959776     1  1.121212   \n",
              "21      4.919843e-03    CD  1.460793  1.531375  1.964652     1  2.111675   \n",
              "22      7.107372e-01    AE  1.840384  3.779547  1.798435     0  1.986302   \n",
              "23      3.065766e+00    AU  1.813419  1.233628  0.626694     2  4.250001   \n",
              "24      2.690857e+00    BD  1.447090  2.559641  1.509052     0  3.260504   \n",
              "25               NaN    CG       NaN       NaN       NaN     0       NaN   \n",
              "26               NaN     J       NaN       NaN       NaN     0       NaN   \n",
              "27      7.351065e-03     N  1.491973  4.202842  1.306053     0  0.722892   \n",
              "28      8.594683e-02    AR  2.105971  0.524231  0.785667     0  0.603581   \n",
              "29      6.640126e-03     Z  1.442224  2.244287  2.373578     0  1.634408   \n",
              "...              ...   ...       ...       ...       ...   ...       ...   \n",
              "114291           NaN    BM       NaN       NaN       NaN     0       NaN   \n",
              "114292  9.536287e-02    BM  1.528000  2.777432  1.985969     0  1.639345   \n",
              "114293  1.466965e-01     G  2.781526  1.131032  2.950252     0  0.800000   \n",
              "114294           NaN     X       NaN       NaN       NaN     0       NaN   \n",
              "114295  1.309991e+00    AR  1.386343  2.880071  2.140204     2  1.722846   \n",
              "114296           NaN     Z       NaN       NaN       NaN     0       NaN   \n",
              "114297  4.670769e-02    AF  1.527338  3.231824  1.246700     0  1.581749   \n",
              "114298  9.564098e-03     S  1.586184  2.737578  2.785707     0  1.049040   \n",
              "114299  3.054839e+00     V  1.291664  5.253692  1.026424     0  2.615385   \n",
              "114300  6.571835e-01    CG  1.380594  7.233358  1.495398     0  1.511110   \n",
              "114301           NaN    BJ       NaN       NaN       NaN     0       NaN   \n",
              "114302  1.805020e-02    AK  1.649315  3.818954  2.049363     1  1.469388   \n",
              "114303           NaN     J       NaN       NaN       NaN     2       NaN   \n",
              "114304           NaN     H       NaN       NaN       NaN     0       NaN   \n",
              "114305  2.027626e-01    BO  1.535785  2.950904  1.790912     1  1.110512   \n",
              "114306  7.758699e-03    BM  2.048144  3.214972  1.805540     0  1.307412   \n",
              "114307           NaN    CD       NaN       NaN       NaN     0       NaN   \n",
              "114308           NaN    BD       NaN       NaN       NaN     0       NaN   \n",
              "114309           NaN     A       NaN       NaN       NaN     0       NaN   \n",
              "114310           NaN    CB       NaN       NaN       NaN     0       NaN   \n",
              "114311           NaN     O       NaN       NaN       NaN     0       NaN   \n",
              "114312  3.258223e+00    AP  0.958445  2.382003  1.601009     1  1.851852   \n",
              "114313  5.664891e-01    CI  2.091074  4.300331  1.738246     0  0.855932   \n",
              "114314  5.673148e-01    CD  1.660675  3.074295  1.383798     0  2.958333   \n",
              "114315           NaN    AE       NaN       NaN       NaN     0       NaN   \n",
              "114316           NaN    AL       NaN       NaN       NaN     0       NaN   \n",
              "114317           NaN     E       NaN       NaN       NaN     1       NaN   \n",
              "114318  1.567642e-01     Q       NaN       NaN  2.417606     2       NaN   \n",
              "114319  4.906576e-01    BW       NaN       NaN  3.526650     0       NaN   \n",
              "114320  3.135205e+00     V  1.943149  4.385553  1.604493     0  1.787610   \n",
              "\n",
              "                v131  \n",
              "0       2.857144e+00  \n",
              "1                NaN  \n",
              "2       1.176472e+00  \n",
              "3       1.034483e+00  \n",
              "4                NaN  \n",
              "5                NaN  \n",
              "6       9.708730e-01  \n",
              "7                NaN  \n",
              "8       3.333334e+00  \n",
              "9       1.000000e+00  \n",
              "10               NaN  \n",
              "11               NaN  \n",
              "12               NaN  \n",
              "13      1.878453e+00  \n",
              "14      2.857143e+00  \n",
              "15               NaN  \n",
              "16      6.060615e-01  \n",
              "17               NaN  \n",
              "18               NaN  \n",
              "19               NaN  \n",
              "20      1.351351e+00  \n",
              "21      1.346155e+00  \n",
              "22      8.275852e-01  \n",
              "23      2.422145e+00  \n",
              "24      2.268042e+00  \n",
              "25               NaN  \n",
              "26               NaN  \n",
              "27      6.666657e-01  \n",
              "28      1.016950e+00  \n",
              "29      3.157894e+00  \n",
              "...              ...  \n",
              "114291           NaN  \n",
              "114292  3.999997e-01  \n",
              "114293  3.333334e+00  \n",
              "114294           NaN  \n",
              "114295  1.043479e+00  \n",
              "114296           NaN  \n",
              "114297  1.730770e+00  \n",
              "114298  1.626015e+00  \n",
              "114299  5.482204e-07  \n",
              "114300  1.848739e+00  \n",
              "114301           NaN  \n",
              "114302  2.222222e+00  \n",
              "114303           NaN  \n",
              "114304           NaN  \n",
              "114305  1.553399e+00  \n",
              "114306  1.933085e+00  \n",
              "114307           NaN  \n",
              "114308           NaN  \n",
              "114309           NaN  \n",
              "114310           NaN  \n",
              "114311           NaN  \n",
              "114312  1.399999e+00  \n",
              "114313  1.980199e+00  \n",
              "114314  2.159624e+00  \n",
              "114315           NaN  \n",
              "114316           NaN  \n",
              "114317           NaN  \n",
              "114318           NaN  \n",
              "114319           NaN  \n",
              "114320  1.386138e+00  \n",
              "\n",
              "[114321 rows x 133 columns]"
              ]
          },
          "execution_count": 3,
          "metadata": {},
          "output_type": "execute_result"
        }
        ],
      "source": [
        "# required packages: keras, pandas\n",
        "import pandas as pd\n",
        "\n",
        "train = pd.read_csv(\"train.csv\")"
        ]
    },
    {
      "cell_type": "code",
      "execution_count": 5,
      "metadata": {
        "collapsed": false
      },
      "outputs": [],
      "source": [
        "# Remove large categorical data on column v22\n",
        "train_clean = train.drop('v22', 1)"
        ]
    },
    {
      "cell_type": "code",
      "execution_count": 14,
      "metadata": {
        "collapsed": false
      },
      "outputs": [
        {
          "ename": "AssertionError",
          "evalue": "It looks like like your version of Theano is out of date. Install the latest version with:\npip install git+git://github.com/Theano/Theano.git --upgrade --no-deps",
          "output_type": "error",
          "traceback": [
            "\u001b[0;31m---------------------------------------------------------------------------\u001b[0m",
            "\u001b[0;31mAssertionError\u001b[0m                            Traceback (most recent call last)",
            "\u001b[0;32m<ipython-input-14-b1835b03a244>\u001b[0m in \u001b[0;36m<module>\u001b[0;34m()\u001b[0m\n\u001b[1;32m     13\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m     14\u001b[0m model.compile(loss='binary_crossentropy',\n\u001b[0;32m---> 15\u001b[0;31m               optimizer='rmsprop')\n\u001b[0m",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/models.pyc\u001b[0m in \u001b[0;36mcompile\u001b[0;34m(self, optimizer, loss, class_mode, sample_weight_mode)\u001b[0m\n\u001b[1;32m    465\u001b[0m         \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mX_test\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_input\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mFalse\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    466\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 467\u001b[0;31m         \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0my_train\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mTrue\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    468\u001b[0m         \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0my_test\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mFalse\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    469\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/layers/containers.pyc\u001b[0m in \u001b[0;36mget_output\u001b[0;34m(self, train)\u001b[0m\n\u001b[1;32m    126\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    127\u001b[0m     \u001b[0;32mdef\u001b[0m \u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mFalse\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 128\u001b[0;31m         \u001b[0;32mreturn\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mlayers\u001b[0m\u001b[0;34m[\u001b[0m\u001b[0;34m-\u001b[0m\u001b[0;36m1\u001b[0m\u001b[0;34m]\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    129\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    130\u001b[0m     \u001b[0;32mdef\u001b[0m \u001b[0mset_input\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/layers/core.pyc\u001b[0m in \u001b[0;36mget_output\u001b[0;34m(self, train)\u001b[0m\n\u001b[1;32m    968\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    969\u001b[0m     \u001b[0;32mdef\u001b[0m \u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mFalse\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 970\u001b[0;31m         \u001b[0mX\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_input\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    971\u001b[0m         \u001b[0moutput\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mactivation\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mK\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mdot\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mX\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mW\u001b[0m\u001b[0;34m)\u001b[0m \u001b[0;34m+\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mb\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    972\u001b[0m         \u001b[0;32mreturn\u001b[0m \u001b[0moutput\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/layers/core.pyc\u001b[0m in \u001b[0;36mget_input\u001b[0;34m(self, train)\u001b[0m\n\u001b[1;32m    173\u001b[0m                 \u001b[0;32mif\u001b[0m \u001b[0mprevious_layer_id\u001b[0m \u001b[0;32min\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mlayer_cache\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    174\u001b[0m                     \u001b[0;32mreturn\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mlayer_cache\u001b[0m\u001b[0;34m[\u001b[0m\u001b[0mprevious_layer_id\u001b[0m\u001b[0;34m]\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 175\u001b[0;31m             \u001b[0mprevious_output\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mprevious\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    176\u001b[0m             \u001b[0;32mif\u001b[0m \u001b[0mhasattr\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0;34m'layer_cache'\u001b[0m\u001b[0;34m)\u001b[0m \u001b[0;32mand\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mcache_enabled\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    177\u001b[0m                 \u001b[0mprevious_layer_id\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0;34m'%s_%s'\u001b[0m \u001b[0;34m%\u001b[0m \u001b[0;34m(\u001b[0m\u001b[0mid\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mprevious\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/layers/core.pyc\u001b[0m in \u001b[0;36mget_output\u001b[0;34m(self, train)\u001b[0m\n\u001b[1;32m    643\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    644\u001b[0m     \u001b[0;32mdef\u001b[0m \u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mFalse\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 645\u001b[0;31m         \u001b[0mX\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_input\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    646\u001b[0m         \u001b[0;32mif\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mp\u001b[0m \u001b[0;34m>\u001b[0m \u001b[0;36m0.\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    647\u001b[0m             \u001b[0;32mif\u001b[0m \u001b[0mtrain\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/layers/core.pyc\u001b[0m in \u001b[0;36mget_input\u001b[0;34m(self, train)\u001b[0m\n\u001b[1;32m    173\u001b[0m                 \u001b[0;32mif\u001b[0m \u001b[0mprevious_layer_id\u001b[0m \u001b[0;32min\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mlayer_cache\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    174\u001b[0m                     \u001b[0;32mreturn\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mlayer_cache\u001b[0m\u001b[0;34m[\u001b[0m\u001b[0mprevious_layer_id\u001b[0m\u001b[0;34m]\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 175\u001b[0;31m             \u001b[0mprevious_output\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mprevious\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    176\u001b[0m             \u001b[0;32mif\u001b[0m \u001b[0mhasattr\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0;34m'layer_cache'\u001b[0m\u001b[0;34m)\u001b[0m \u001b[0;32mand\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mcache_enabled\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    177\u001b[0m                 \u001b[0mprevious_layer_id\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0;34m'%s_%s'\u001b[0m \u001b[0;34m%\u001b[0m \u001b[0;34m(\u001b[0m\u001b[0mid\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mprevious\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/layers/core.pyc\u001b[0m in \u001b[0;36mget_output\u001b[0;34m(self, train)\u001b[0m\n\u001b[1;32m    968\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    969\u001b[0m     \u001b[0;32mdef\u001b[0m \u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mFalse\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 970\u001b[0;31m         \u001b[0mX\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_input\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    971\u001b[0m         \u001b[0moutput\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mactivation\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mK\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mdot\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mX\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mW\u001b[0m\u001b[0;34m)\u001b[0m \u001b[0;34m+\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mb\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    972\u001b[0m         \u001b[0;32mreturn\u001b[0m \u001b[0moutput\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/layers/core.pyc\u001b[0m in \u001b[0;36mget_input\u001b[0;34m(self, train)\u001b[0m\n\u001b[1;32m    173\u001b[0m                 \u001b[0;32mif\u001b[0m \u001b[0mprevious_layer_id\u001b[0m \u001b[0;32min\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mlayer_cache\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    174\u001b[0m                     \u001b[0;32mreturn\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mlayer_cache\u001b[0m\u001b[0;34m[\u001b[0m\u001b[0mprevious_layer_id\u001b[0m\u001b[0;34m]\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 175\u001b[0;31m             \u001b[0mprevious_output\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mprevious\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    176\u001b[0m             \u001b[0;32mif\u001b[0m \u001b[0mhasattr\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0;34m'layer_cache'\u001b[0m\u001b[0;34m)\u001b[0m \u001b[0;32mand\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mcache_enabled\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    177\u001b[0m                 \u001b[0mprevious_layer_id\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0;34m'%s_%s'\u001b[0m \u001b[0;34m%\u001b[0m \u001b[0;34m(\u001b[0m\u001b[0mid\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mprevious\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/layers/core.pyc\u001b[0m in \u001b[0;36mget_output\u001b[0;34m(self, train)\u001b[0m\n\u001b[1;32m    643\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    644\u001b[0m     \u001b[0;32mdef\u001b[0m \u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mFalse\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 645\u001b[0;31m         \u001b[0mX\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_input\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    646\u001b[0m         \u001b[0;32mif\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mp\u001b[0m \u001b[0;34m>\u001b[0m \u001b[0;36m0.\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    647\u001b[0m             \u001b[0;32mif\u001b[0m \u001b[0mtrain\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/layers/core.pyc\u001b[0m in \u001b[0;36mget_input\u001b[0;34m(self, train)\u001b[0m\n\u001b[1;32m    173\u001b[0m                 \u001b[0;32mif\u001b[0m \u001b[0mprevious_layer_id\u001b[0m \u001b[0;32min\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mlayer_cache\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    174\u001b[0m                     \u001b[0;32mreturn\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mlayer_cache\u001b[0m\u001b[0;34m[\u001b[0m\u001b[0mprevious_layer_id\u001b[0m\u001b[0;34m]\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 175\u001b[0;31m             \u001b[0mprevious_output\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mprevious\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    176\u001b[0m             \u001b[0;32mif\u001b[0m \u001b[0mhasattr\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0;34m'layer_cache'\u001b[0m\u001b[0;34m)\u001b[0m \u001b[0;32mand\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mcache_enabled\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    177\u001b[0m                 \u001b[0mprevious_layer_id\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0;34m'%s_%s'\u001b[0m \u001b[0;34m%\u001b[0m \u001b[0;34m(\u001b[0m\u001b[0mid\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mprevious\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/layers/core.pyc\u001b[0m in \u001b[0;36mget_output\u001b[0;34m(self, train)\u001b[0m\n\u001b[1;32m    969\u001b[0m     \u001b[0;32mdef\u001b[0m \u001b[0mget_output\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mself\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mtrain\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mFalse\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    970\u001b[0m         \u001b[0mX\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mget_input\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mtrain\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 971\u001b[0;31m         \u001b[0moutput\u001b[0m \u001b[0;34m=\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mactivation\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mK\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mdot\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mX\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mW\u001b[0m\u001b[0;34m)\u001b[0m \u001b[0;34m+\u001b[0m \u001b[0mself\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mb\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m    972\u001b[0m         \u001b[0;32mreturn\u001b[0m \u001b[0moutput\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    973\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/activations.pyc\u001b[0m in \u001b[0;36mrelu\u001b[0;34m(x, alpha, max_value)\u001b[0m\n\u001b[1;32m     25\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m     26\u001b[0m \u001b[0;32mdef\u001b[0m \u001b[0mrelu\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mx\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0malpha\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0;36m0.\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mmax_value\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mNone\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m---> 27\u001b[0;31m     \u001b[0;32mreturn\u001b[0m \u001b[0mK\u001b[0m\u001b[0;34m.\u001b[0m\u001b[0mrelu\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mx\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0malpha\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0malpha\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mmax_value\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mmax_value\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0m\u001b[1;32m     28\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m     29\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;32m/Users/massoudmaher/anaconda/lib/python2.7/site-packages/keras/backend/theano_backend.pyc\u001b[0m in \u001b[0;36mrelu\u001b[0;34m(x, alpha, max_value)\u001b[0m\n\u001b[1;32m    499\u001b[0m \u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    500\u001b[0m \u001b[0;32mdef\u001b[0m \u001b[0mrelu\u001b[0m\u001b[0;34m(\u001b[0m\u001b[0mx\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0malpha\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0;36m0.\u001b[0m\u001b[0;34m,\u001b[0m \u001b[0mmax_value\u001b[0m\u001b[0;34m=\u001b[0m\u001b[0mNone\u001b[0m\u001b[0;34m)\u001b[0m\u001b[0;34m:\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[0;32m--> 501\u001b[0;31m     assert hasattr(T.nnet, 'relu'), ('It looks like like your version of '\n\u001b[0m\u001b[1;32m    502\u001b[0m                                      \u001b[0;34m'Theano is out of date. '\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n\u001b[1;32m    503\u001b[0m                                      \u001b[0;34m'Install the latest version with:\\n'\u001b[0m\u001b[0;34m\u001b[0m\u001b[0m\n",
            "\u001b[0;31mAssertionError\u001b[0m: It looks like like your version of Theano is out of date. Install the latest version with:\npip install git+git://github.com/Theano/Theano.git --upgrade --no-deps"
            ]
        }
        ],
      "source": [
        "from keras.models import Sequential\n",
        "from keras.layers.core import Dense, Activation, Dropout\n",
        "\n",
        "model = Sequential()\n",
        "\n",
        "# Set up neural network model for binary classification, source: http://keras.io/examples/#mlp-for-binary-classification \n",
        "model = Sequential()\n",
        "model.add(Dense(64, input_dim=20, init='uniform', activation='relu'))\n",
        "model.add(Dropout(0.5))\n",
        "model.add(Dense(64, activation='relu'))\n",
        "model.add(Dropout(0.5))\n",
        "model.add(Dense(1, activation='sigmoid'))\n",
        "\n",
        "model.compile(loss='binary_crossentropy',\n",
        "              optimizer='rmsprop')"
        ]
    },
    {
      "cell_type": "code",
      "execution_count": null,
      "metadata": {
        "collapsed": true
      },
      "outputs": [],
      "source": []
    }
    ],
  "metadata": {
    "kernelspec": {
      "display_name": "Python 2",
      "language": "python",
      "name": "python2"
    },
    "language_info": {
      "codemirror_mode": {
        "name": "ipython",
        "version": 2
      },
      "file_extension": ".py",
      "mimetype": "text/x-python",
      "name": "python",
      "nbconvert_exporter": "python",
      "pygments_lexer": "ipython2",
      "version": "2.7.10"
    }
  },
  "nbformat": 4,
  "nbformat_minor": 0
}


####################################################################################


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


####################################################################################


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


####################################################################################


setwd("C:/Users/abhijit331/Desktop/289 Finals")
load("~/Desktop/Math 289/.RData")
#setwd("/home/abhijit331/Desktop/Math 289")
#install.packages("zoo")
#install.packages("doParallel")
#install.packages("xgboost")
install.packages("Matrix")
#library(doParallel)
library(Matrix)
library(zoo)
library(xgboost)
library(MASS)
library(randomForest)
library(e1071)
library(parallelSVM)
####To replace NAs with the nearest non NA neighbour
# replaceNA = function(s)
# {
#   ifelse(is.na(s),-999,s)
# }
train = read.csv("train.csv", header = TRUE)
train = train[sample(1:nrow(train)),]
#train = train[complete.cases(train),]
train1 = train
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

fac = which(lapply(train,class)=="factor")
factor = train[,fac]
factor = factor[,-c(2,8,19)]
train = train[,-fac]
train = train[,-1]
train = data.frame(lapply(train,fillNA))
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

# # SVM 
# dummy = train[1:10000,]
# begin = Sys.time()
# svm.radial = svm(as.factor(target) ~ . , data = dummy,type = "C-classification",kernel = "radial" ) 
# end = Sys.time()-begin
# end
# pred.svm = predict(svm.radial,train_test)
# length(which(pred.svm == labels))/dim(train_test)[1]

# ~ 3 hours execution time. 

# Parallel SVM
ntree = 100; numCore = 4
rep <- ntree/numCore # tree / numCore

z = c()
cl = makeCluster(4)
registerDoParallel(cl)
getDoParWorkers()
dummy = train[1:50000,]
begin = Sys.time()
model.svm = parallelSVM(as.factor(target) ~ .,data =dummy,numberCores = detectCores())
end = Sys.time() - begin
end
stopCluster(cl)
registerDoSEQ()
z =c(z,end*60)

tuned = tune(svm,as.factor(target) ~ . , data = dummy,ranges = list(epsilon = seq(0,.3,0.01),cost = 2(0:9)))
train_test = train[50001:60000,-1]
lab = train[50001:60000,1]
predict.svm = predict(model.svm,train_test)
length(which(predict.svm == lab))/10000




# Extreme Gradient Boosting
train = read.csv("train.csv", header = TRUE)
train = train[sample(1:nrow(train)),]
train = data.frame(lapply(train,replaceNA))
train = train[,-1]
dum = train[1:100000,-1]
la = train[1:100000,1]
dumtest = train[100001:nrow(train),-1]
spar2 = sparse.model.matrix(~.,data = dumtest)
latest = train[10001:20000,1]
spar = sparse.model.matrix(~.,data = dum)
xgb.model = xgboost(data = spar,label =as.factor(la),max_depth = 9,eta = 1 , nthread = 4,nround = 10,onjective = "binary:logistic")
important =xgb.importance(feature_names = spar@Dimnames[[2]],model = xgb.model)
testp = predict(xgb.model,newdata = spar2)
testp=ifelse(testp > 0.5,1,0)
correct = length(which(testp == latest))
length(which((testp == latest) & latest == 1))/correct
length(which((testp == latest) & latest == 0))/correct
head(testp)
head(latest)


# Extracting the top 20 features from the XGBoost
features = impo[1:20]$Feature
train = read.csv("train.csv", header = TRUE)
train = train[sample(1:nrow(train)),]
train = data.frame(lapply(train,replaceNA))
reduced.dataset = train[,c("target",features)]
head(reduced.dataset)
dim(reduced.dataset)

features.dum = reduced.dataset[1:20000,-1]
features.la = reduced.dataset[1:20000,1]
features.dumtest = reduced.dataset[20001:30000,-1]
features.spar2 = sparse.model.matrix(~.,data = features.dumtest)
features.latest = reduced.dataset[20001:30000,1]
features.spar = sparse.model.matrix(~.,data = features.dum)
features.xgb.model = xgboost(data = features.spar,label =as.factor(features.la),max_depth = 9,eta = 1 , nthread = 4,nround = 10,onjective = "binary:logistic")
features.impo =xgb.importance(feature_names = features.spar@Dimnames[[2]],model = features.xgb.model)
features.testp = predict(features.xgb.model,newdata = features.spar2)
features.testp=ifelse(features.testp > 0.5,1,0)
features.correct = length(which(features.testp == features.latest))
length(which((features.testp == features.latest) & features.latest == 1))/features.correct
length(which((features.testp == features.latest) & features.latest == 0))/features.correct


####################################################################################


#setwd("C:/Users/abhijit331/Desktop/289 Finals")
load("~/Desktop/Math 289/.RData")
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

# Parallel
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
registerDoParallel(cl)
split = sort(rank(1:nrow(train_test))%%4)
svm.prediction = foreach(i = unique(split),.combine = combine,.packages = c("e1071")) %dopar%
{as.numeric(predict(model.svm,newdata=train_test[split==i,]))}
stopCluster(cl)
registerDoSEQ()
length(which(svm.prediction == labels))/dim(train_test)[1]


####################################################################################