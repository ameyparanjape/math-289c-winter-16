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

totw = rep(0, 10)
for(i in 1:10){
  totw[i] = kmeans(palloc, centers = i, nstart = 10)$tot.withinss  
}
plot(totw, type = "l")

clust = kmeans(palloc, centers = 3, nstart = 10)

library(ggplot2)
ggplot(aes(location), data=palloc)+geom_histogram(bins = 15)