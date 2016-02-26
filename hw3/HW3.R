palloc = read.table(file = "hcmv.txt", header = T)
diffs = (palloc[2:(nrow(palloc)),1] - palloc[1:(nrow(palloc)-1),1])

palrand = sort(sample(1:229354, 296))
diffsrand = (palrand[2:(length(palrand))] - palrand[1:(length(palrand)-1)])

plot((diffs), type="p")
lmdiff = lm(diffs~c(1:295))
abline(a = lmdiff$coefficients[1], b = lmdiff$coefficients[2])

plot((diffsrand), type="p")
lmdiffrand = lm(diffsrand~c(1:295))
abline(a = lmdiffrand$coefficients[1], b = lmdiffrand$coefficients[2])

totw = rep(0, 41)
for(i in 1:41){
  totw[i] = kmeans(palloc, centers = i, nstart = 10)$tot.withinss
}
plot(totw, type = "l")

kc = kmeans(palloc, centers = 41, nstart = 1000)
kc$tot.withinss
kc10 = kmeans(palloc, centers = 16, nstart = 1000)


withfix = c()
count = c()
for(i in 1:41){
  temp = palloc[which(palloc>=(1+(5594*(i-1))) & palloc<=(5594*i)),]
  m = mean(temp)
  count[i] = length(temp)
  withfix[i] = sum((temp - m)^2)
}
library(ggplot2)
palloc$clust=clust$cluster
ggplot(aes(y=rep(0, 296), x = location, col=as.factor(clust)), data=palloc) +
  geom_point() +
  geom_point(aes(x=centers, y=rep(0,3), col=as.factor(sort(cluster)), size=10), data=data.frame(centers=clust$centers[,1], cluster=clust$centers[,2]))

ggplot(aes(x=as.factor(clust)), data=palloc)+geom_bar()

palloc$clust=NULL
hcl = hclust(dist(palloc$location))
hcl2 = hclust(dist(palloc$location), method = "ward.D")
plot(hcl2)

# Histogram for 41 bins (intervals of 5594)
ggplot(aes(location), data=palloc) + geom_histogram(bins=41, alpha=0.5, col="blue", fill="blue")+ggtitle("Non-overlapping intervals of 5594 base pairs")
# Random
ggplot(aes(location), data=data.frame(location=sample(1:229354, size = 296))) + geom_histogram(bins=41, alpha=0.5, col="blue", fill="blue")

t = cut(palloc$location, seq(0, 229354, 5594))
t1 = table(as.matrix(table(t)))
t2 = t1
t2[8] = sum(t1[8:length(t1)])
t2 = t2[1:8]

l = 296/41
texp = rep(0, length(t1))
for(j in 1:length(t1)){
  texp[j] = 41*exp(-l)*((l^as.numeric(names(t1)[j]))/factorial(as.numeric(names(t1)[j])))
}
texp[1] = texp[1] + 41*exp(-l)*(1+l)
texp[8] = sum(texp[8:length(texp)])
texp = texp[1:8]
texp[8] = 41*(1 - sum(texp[1:7]/41))
###########################################################3
palloc[which(kc$cluster==27),]
t = palloc[which(kc$cluster==27),]
t[length(t)] - t[1]
###########################################################
#install.packages("NbClust")
library(NbClust)
library(ggplot2)

# 41 clusters
col.clust = as.numeric(kc$cluster==27)
ggplot(aes(x=location, y=rep(0,296), col=as.factor(clust)), data = data.frame(location=palloc$location, clust = col.clust))+
  geom_point()+annotate("text", x=90000, y=0.005, label="Cluster with largest", size=5)+
  scale_y_continuous(limits = c(-0.1, 0.1))+theme(legend.position='null')+labs(x="locations",y=NULL)+
  theme(axis.text.y = element_blank())+ggtitle("Plot of locations")+
  annotate("text", x=90000, y=-0.005, label="number of palindromes", size=5)

# 14 clusters
col.clust10 = as.numeric(kc10$cluster==kc10$cluster[113])
ggplot(aes(x=location, y=(rep(0,296)), col=as.factor(clust)), data = data.frame(location=palloc$location, clust = col.clust10))+
  geom_point(size=1)+annotate("text", x=90000, y=0.005, label="Cluster with largest", size=5)+
  scale_y_continuous(limits = c(-0.1, 0.1))+theme(legend.position='null')+labs(x="locations",y=NULL)+
  theme(axis.text.y = element_blank())+ggtitle("Plot of locations")+
  annotate("text", x=90000, y=-0.005, label="number of palindromes", size=5)

ggplot(aes(x=location, y=rep(0,296), col=as.factor(clust)), data = data.frame(location=palloc$location, clust = col.clust10+col.clust))+geom_point()
###########################################################
# group with count
subset(palloc, subset = 1:296%%37==0) - subset(palloc, subset = 1:296%%37==1)
subset(palloc, subset = 1:296%%8==0) - subset(palloc, subset = 1:296%%8==1)
plot(y =(subset(palloc, subset = 1:296%%8==0) - subset(palloc, subset = 1:296%%8==1))$location, x = 1:37, type='l')
listint = list()
sel = rep(T, 296)
for(i in 5:50){
  if(296%%i==0){
    listint = c(listint, subset(palloc, subset = (1:296)%%i==0) - subset(palloc, subset = (1:296)%%i==1))
  }else{
    sel = rep(T, 296)
    sel[(297 - 296%%i):296] = F
    temp2 = subset(palloc, subset = sel)
    listint = c(listint, subset(temp2, subset = (1:nrow(temp2))%%i==0) - subset(temp2, subset = (1:nrow(temp2))%%i==1))
  }
}

ggplot(aes(y=Interval, x=IntNumber), data = data.frame(Interval = listint[[10]], IntNumber=1:length(listint[[10]]))) +
  geom_line(col="blue") + annotate("text", y=min(listint[[10]])-190, x=9, label = paste("Least interval size = ",min(listint[[10]]), "base pairs"))+
  ggtitle("Non-overlapping intervals with 14 palindromes each")

######################################
# OVERLAPPING

wind.size = 5594
lwind = list()
palcounts = c()
rangemax = c()
rangemin = c()
actual.size = c()
for(i in palloc$location){
  temp3 = subset(palloc, subset = (location>=i & location<=(i+wind.size)))
  if(nrow(temp3)>0){
    palcounts = c(palcounts, nrow(temp3))
    rangemax = c(rangemax, max(temp3))
    rangemin = c(rangemin, min(temp3))
    actual.size = c(actual.size, max(temp3) - min(temp3))
    if(max(temp3)==palloc[nrow(palloc),]){
      break
    }
  }
}
windowout = cbind(palcounts, rangemin, rangemax, actual.size)
ggplot(aes(x=interval.number, y=palindrome.count), 
       data=data.frame(interval.number = 1:nrow(windowout), palindrome.count = windowout[,1]))+
  geom_bar(stat='identity', alpha=0.5, col='blue')+
  geom_bar(data=data.frame(interval.number=107, palindrome.count=20),stat='identity', col='red')+
  annotate("text", y=21,x=which(windowout[,1]==20),label="Interval of size 5371 ranging from 88803 to 94174")+
  ggtitle("Overlapping intervals of size 5594")
wind.count = 20
lwind = list()
rangemax2 = c()
rangemin2 = c()
actual.size2 = c()
for(i in 1:(nrow(palloc)-wind.count+1)){
  temp4 = palloc[i:(i+wind.count-1),]
  rangemax2 = c(rangemax2, max(temp4))
  rangemin2 = c(rangemin2, min(temp4))
  actual.size2 = c(actual.size2, max(temp4) - min(temp4))
  if(max(temp4)==palloc[nrow(palloc),])
    break
}
windowout2 = cbind(rangemin2, rangemax2, actual.size2)
ggplot(aes(x=interval.number, y=interval.size), 
       data=data.frame(interval.number=1:nrow(windowout2), interval.size=windowout2[,3]))+
  geom_line(col="blue")+geom_hline(yintercept = mean(windowout2[,3]))+
  geom_point(aes(x=x,y=y),
             data=data.frame(x=which(windowout2[,3]==min(windowout2[,3])), y=min(windowout2[,3])), size=4)+
  annotate("text", x=which(windowout2[,3]==min(windowout2[,3])), y=min(windowout2[,3])-800, label="Interval size of 5371 ranging from 88803 and 94174", size=5)+
  ggtitle("Overlapping intervals with fixed count of palindromes")+
  annotate("text", x=100, y=mean(windowout2[,3])+500, label="Average interval size", size=5)

listint[[]]