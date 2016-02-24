library(plyr)
setwd("/home/abhijit331/Dropbox/Math 289")
setwd("C:/Users/abhijit331/Dropbox/Math 289")
dat = read.csv("dmv.txt")
head(dat)
dat$bin = as.integer(dat[,1]/5594) + 1
freq.table = count(dat$bin)
barplot(count(dat$bin)$freq,names = count(dat$bin)$x * 5594,las = 2)


counts = table(as.matrix(freq.table$freq))
counts["4"] = sum(counts[1:3])
counts["9"] = sum(counts[9:length(counts)])
counts = counts[3:8]
c = counts
# bin size = 5594
lambda = 296/41
prob = c()
prob[1] = exp(-lambda)*(1+lambda + lambda^2/2+lambda^3/factorial(3)+lambda^4/factorial(4))
for(i in 5:8)
{
  prob[i-3] = exp(-lambda)*(lambda^i/factorial(i))
}
prob[6] = 1-sum(prob[1:5])
counts = cbind(counts,41*prob)
counts[,2] = round(counts[,2],1)
dcs = chisq.test(counts[,1],p = prob)
sums = 0
for (i in 1:dim(counts)[1])
{
  sums = sums + ((counts[i,1] - counts[i,2])^2)/counts[i,2]
}
sums
chisq.test(counts,prob)
