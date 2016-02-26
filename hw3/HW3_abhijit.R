library(plyr)
#setwd("/home/abhijit331/Dropbox/Math 289")
setwd("C:/Users/abhijit331/Dropbox/Math 289")
dat = read.csv("dmv.txt")
head(dat)
dat$bin = as.integer(dat[,1]/5594) + 1 #4587
freq.table = count(dat$bin)
barplot(count(dat$bin)$freq,names = count(dat$bin)$x * 5594,las = 2)


counts = table(as.matrix(freq.table$freq))
counts["4"] = sum(counts[1:3])
counts["9"] = sum(counts[9:length(counts)])
counts = counts[3:8]
c = counts
# bin size = 5594

#Poisson
lambda = 296/41
prob.poisson = c()
prob.poisson[1] = exp(-lambda)*(1+lambda + lambda^2/2+lambda^3/factorial(3)+lambda^4/factorial(4))
for(i in 5:8)
{
  prob.poisson[i-3] = exp(-lambda)*(lambda^i/factorial(i))
}
prob.poisson[6] = t = exp(-lambda)*(lambda^9/factorial(9)+lambda^10/factorial(10)+lambda^11/factorial(11)+lambda^14/factorial(14)+lambda^19/factorial(19))
#1-sum(prob[1:5])
counts = cbind(counts,41*prob.poisson)
counts[,2] = round(counts[,2],1)
#dcs = chisq.test(counts[,1],p = prob)
sum.poisson = 0
for (i in 1:dim(counts)[1])
{
  sum.poisson = sum.poisson + ((counts[i,1] - counts[i,2])^2)/counts[i,2]
}
1 - pchisq(sum.poisson, df = 4)
qqplot(counts[,1],counts[,2],main = " Observed vs Expected values from \n Poisson Distribution",xlab ="Observed",ylab="Expected")
abline(0,1)


## Exponential distribution 
prob.exp = c()
lambda = 41/296
prob.exp[1] = 1-exp(-4*lambda)
for(i in 5:8)
{
  prob.exp[i-3] = exp(-lambda*(i-1)) - exp(-lambda*i)
}
prob.exp[6] = 1- (1-exp(-8*lambda))
counts = cbind(counts,41*prob.exp)
sum.exp = 0
for (i in 1:dim(counts)[1])
{
  sum.exp = sum.exp + ((counts[i,1] - counts[i,3])^2)/counts[i,3]
}
1 -pchisq(sum.exp,df = 4)

qqplot(counts[,1],counts[,3],main = " Observed vs Expected values from \n Exponential Distribution",xlab ="Observed",ylab="Expected")
abline(0,1)


# Gamma distribution 

prob.gamma = c()
lambda = 296/41
prob.gamma[1] = pgamma(4,shape = 2, scale = lambda/2)
sumsgamma = 0
for(i in 5:8)
{
  prob.gamma[i-3] = pgamma(i,shape = 2, scale = lambda/2) - pgamma(i-1, shape = 2, scale = lambda/2)
}
prob.gamma[6] = 1 - pgamma(8,shape = 2, scale = lambda/2)
counts = cbind(counts,41*prob.gamma)
colnames(counts) = c("Observed","Poisson","Exponential","Gamma")
sum.gamma = 0
for(i in 1:dim(counts)[1])
{
  sum.gamma = sum.gamma+((counts[i,1] - counts[i,4])^2)/counts[i,4]
}
1 - pchisq(sum.gamma , df = 3)
qqplot(counts[,1],counts[,4],main = " Observed vs Expected values from \n Gamma Distribution",xlab ="Observed",ylab="Expected")
abline(0,1)


#Calculating the power of the chi square tests for different distribution

power.poisson = pwr.chisq.test(w = sqrt(sum.poisson),N = 6,df = 4)
power.exp = pwr.chisq.test(w = sqrt(sum.exp),N = 6,df = 4)
power.gamma = pwr.chisq.test(w = sqrt(sum.gamma),N = 6,df = 3)
