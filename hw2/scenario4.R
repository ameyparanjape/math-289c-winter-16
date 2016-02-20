library(Lock5Data)
data(CommuteAtlanta)
str(CommuteAtlanta)

require(tree)


#choose a subset of data which most features are present
#only choose people who likes videogams and people who hates playing
#by deleting people who never played and people who didn't answer the question
data.filtered = data[which(data$like!=99&data$like!=1),]

#very much and somewhat fits into group "like" like = 2 or 3
#not really and not at all fits in group "not at all" like = 4 or 5

data.filtered$like[data.filtered$like!=2&data.filtered$like!=3] <- 0
data.filtered$like[data.filtered$like==2|data.filtered$like==3] <- 1
data.filtered$like <- factor(data.filtered$like)

tree.overfit = tree(like ~  sex+home+grade+time, data=data.filtered,mindev=0.0001)
plot(tree.overfit)
text(tree.overfit, cex=0.55)


# prune the tree
p_tree = prune.tree(tree.overfit, best=6) # return best pruned tree with 5 leaves, USING traing data!
prune.tree.seq = prune.tree(tree.overfit)
plot(p_tree)
text(p_tree, cex=0.75)
plot(prune.tree.seq) # of course, always decrease

tree.cv.seq = cv.tree(tree.overfit) # 10-fold cv
plot(tree.cv.seq)
# get the best splitting number
opt.tree = which(tree.cv.seq$dev == min(tree.cv.seq$dev))
min(tree.cv.seq$size[opt.tree]) # size of smallest optimal tree

#----------------------------------------------------------------------
#exclude people who didn't answer educ, most of them not at all like video games'
data.filtered = data[which(data$like!=99&data$like!=1&data$educ!=99&busy!=99),]


#very much and somewhat fits into group "like" like = 2 or 3
#not really and not at all fits in group "not at all" like = 4 or 5

data.filtered$like[data.filtered$like!=2&data.filtered$like!=3] <- 0
data.filtered$like[data.filtered$like==2|data.filtered$like==3] <- 1
data.filtered$like <- factor(data.filtered$like)

tree.overfit = tree(like ~  busy+ sex+educ+time+home+grade, data=data.filtered,mindev=0.0001)
plot(tree.overfit)
text(tree.overfit, cex=0.75)


# prune the tree
p_tree = prune.tree(tree.overfit, best=5) # return best pruned tree with 5 leaves, USING traing data!
prune.tree.seq = prune.tree(tree.overfit)
plot(p_tree)
text(p_tree, cex=0.75)
plot(prune.tree.seq) # of course, always decrease

tree.cv.seq = cv.tree(tree.overfit) # 10-fold cv
plot(tree.cv.seq)
# get the best splitting number
opt.tree = which(tree.cv.seq$dev == min(tree.cv.seq$dev))
min(tree.cv.seq$size[opt.tree]) # size of smallest optimal tree
