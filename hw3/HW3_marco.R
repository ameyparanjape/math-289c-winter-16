library(plyr)
setwd("/home/abhijit331/Dropbox/Math 289")
setwd("C:/Users/abhijit331/Dropbox/Math 289")
dat = read.csv("hcmv.txt")
head(dat)
dat$bin = as.integer(dat[,1]/4587) + 1 #4587
freq.table = count(dat$bin)
barplot(count(dat$bin)$freq,names = count(dat$bin)$x * 4587,las = 2)


counts = table(as.matrix(freq.table$freq))
counts["2"] = sum(counts[1:2])
counts["9"] = sum(counts[9:length(counts)])
counts = counts[2:9]
c = counts
# bin size = 4587
lambda = 296/41
prob = c()
prob[1] = exp(-lambda)*(1+lambda + lambda^2/2)
for(i in 3:8)
{
  prob[i-1] = exp(-lambda)*(lambda^i/factorial(i))
}
prob[8] = 1-sum(prob[1:7])
counts = cbind(counts,41*prob)
counts[,2] = round(counts[,2],1)
#dcs = chisq.test(counts[,1],p = prob)
sums = 0
for (i in 1:dim(counts)[1])
{
  sums = sums + ((counts[i,1] - counts[i,2])^2)/counts[i,2]
}
pvalPoisson = pchisq(sums,df = 6,lower.tail = FALSE)

## Exponential distribution 
prob2 = c()
lambda = 41/296
prob2[1] = 1-exp(-4*lambda)
for(i in 5:8)
{
  prob2[i-3] = exp(-lambda*(i-1)) - exp(-lambda*i)
}
prob2[6] = 1- (1-exp(-8*lambda))
counts = cbind(counts,41*prob2)
colnames(counts) = c("observed","poisson","exponential")
sumsExp = 0
for (i in 1:dim(counts)[1])
{
  sumsExp = sumsExp + ((counts[i,1] - counts[i,3])^2)/counts[i,3]
}

sum(prob2)



# Uniform dist 
counts = cbind(counts,rep(7.22,6))
colnames(counts) = c("Observed","Poisson","Exponential","Uniform")
# dat$bin2 = as.integer(dat[,1]/5594) + 1
# tab = count(dat$bin2)
# counts2 = table(as.matrix(tab$freq))
# #tab$exp = rep(29.6,97)
sums2 = 0
for (i in 1:dim(counts)[1])
{
  sums2 = sums2 + ((counts[i,1] - counts[i,4])^2)/counts[i,4]
}
sums2
# chisq.test(tab[,2])
