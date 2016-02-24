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
ggplot(aes(location), data=palloc) + geom_histogram(bins=41, alpha=0.5, col="blue", fill="blue")
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
fullDNA = rep(0, 229354)
fullDNA[palloc[,1]] = 1
