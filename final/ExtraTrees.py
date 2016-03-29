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