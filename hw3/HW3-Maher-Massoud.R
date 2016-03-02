# NOTICE:
# The following code is not R code
# This code is written in Python

import numpy as np
import scipy.stats
import matplotlib.pyplot as plt
plt.style.use('ggplot')
segments = 13

# n_row = 4
n_col = 1
lengh_dna = 229354

# intervals = [4587,5090,5594]
intervals = int(lengh_dna/segments)

# fig,ax = plt.subplots(len(intervals),n_col,sharex=False,sharey=False)
data = np.loadtxt("data.txt",skiprows=1)
# print(data.shape)
num_pal = len(data)
uniform = [296/segments for i in range(segments)]

bins = range(0,lengh_dna,intervals)

n,bins,patchs = plt.hist(data,bins=bins)
plt.hist(uniform,bins=bins)
print(scipy.stats.chisquare(f_obs=n,f_exp=uniform,ddof=0))
print (n)
print(uniform)
n = np.array(n)
uniform = np.array(uniform)

print((n-uniform)/np.sqrt(uniform))
plt.xlabel('Intervals')
plt.ylabel('Counts')
title = 'Interval='+str(intervals)
plt.title(title)

# plt.show()


####################################################################################


# Load the data
hcmv <- read.csv("~/Documents/Math289c/hcmv.txt", sep="")

# Load ggplot2
library(ggplot2)

# Get random data for the simulated hist
randDist <- sample(1:229354, 296, replace = FALSE)
randDist = data.frame(randDist)

# Merge the two data frames (real and sim)
histData = merge(hcmv,randDist)

# Create a title var for the hist
title <- "Actual Data vs Simulated Data of Locations of Palindromes in Consecutive Intervals of 5594 Base Pairs"

# Create two histograms and layer them on top of one another
hist <- ggplot(data = histData) + 
  geom_histogram(aes(x=location, fill = "red"), 
                 binwidth = 5594, 
                 color = "black", 
                 alpha = .35) + 
  geom_histogram(aes(x=randDist, fill = "blue"), 
                 binwidth = 5594, color = "black", 
                 alpha = .35) + 
  ggtitle(title) + 
  xlab("locations") + 
  scale_fill_manual(name = "Kind", 
                    values =c('blue'='blue','red'='red'), labels = c('Simulated','Actual'))
hist

#################### 1st Moment Func ##########################################
firstMomentFunc = function(set) {
  
  numE = length(set)            # Get the length of the vector
  total = 0                     # Set the total recorded to 0
  for (i in 1:numE) {           # For all elements in the vec
    total = total + set[i]      # Add curr element to the total
  }
  lambdaHat = (total / numE)    # Get x bar (simulated lambda hat)
  return(lambdaHat)
  
}

# Standardized Residual Graphing Code 
stdResMax = c(0, 0.0424, 0, 1.1797, 0)
stdResMin = c(-0.1234, 0, -0.3717, 0, -0.1006)
stdResX = c("0-4", "5", "6", "7", "8+")
stdRes = ggplot() + geom_errorbar( aes(x=stdResX, ymax=stdResMax, ymin=stdResMin))
stdRes + labs(x="Palindrome Count", y="Standardized Residual")


stdResMax = c(0, 0, 0.4411, 0.0735, 0.4411, 0.2573, 0, 0.4411, 0.8087, 0)
stdResMin = c(-0.1103, -1.5807, 0, 0, 0, 0, -0.2941, 0, 0, -0.4779)
stdResX = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
stdResX = as.factor(stdResX)
stdRes = ggplot() + geom_errorbar( aes(x=stdResX, ymax=stdResMax, ymin=stdResMin))
stdRes + labs(x="Palindrome Count", y="Standardized Residual")

# Creating barplots for kmeans
# K-Means++ clustering
library(NbClust)       #load package
set.seed(1234)         #set seed for consistent clustering

kmeans = kmeans(hcmv, 57, algorithm="Lloyd")  #use kmeans with Lloyd algorithm
aggregate(hcmv,by=list(kmeans$cluster),FUN=mean)
hcmv <- data.frame(hcmv, kmeans$cluster)

##HAVE TO RUN THE ABOVE CODE FOR EACH ONE OF THESE VALUES IN THE VECTOR BELOW

clusterSizes = c(2,3,10,18,23,57)
for (j in 1:6) {
  v = vector(mode = "logical", length = 0)
  lab = rep(1:clusterSizes[j], 1)
  for (i in 1:clusterSizes[j]) {
    v[i] = length(which(hcmv[,j+1] == i)) 
  }
  barplot(v, names.arg = lab)
}


####################################################################################


library(plyr)
dat = read.csv("hcmv.txt")
head(dat)

# Get count of palindromes in each interval of length 5594
dat$bin = as.integer(dat[,1]/5594) + 1
freq.table = count(dat$bin)
barplot(count(dat$bin)$freq,names = count(dat$bin)$x * 5594,las = 2)

# Make table of frequency counts
counts = table(as.matrix(freq.table$freq))

# combine 0-3, 9+
counts["4"] = sum(counts[1:3])
counts["9"] = sum(counts[9:length(counts)])
counts = counts[3:8]
c = counts
# counts bottom row is number of intervals that hold x palindromes
# counts top row is number of palindromes

# calculate expected values
# bin size = 5594
lambda = 296/41
prob = c()

# probability of 0, 1, 2, 3, 4 palindromes in an interval
prob[1] = exp(-lambda)*(1+lambda + lambda^2/2+lambda^3/factorial(3)+lambda^4/factorial(4))
# calculate remaaining probabilities
for(i in 5:8)
{
  prob[i-3] = exp(-lambda)*(lambda^i/factorial(i))
}
prob[6] = 1-sum(prob[1:5])

# Multiply by #intervals to get expected value
counts = cbind(counts,41*prob)
counts[,2] = round(counts[,2],1)

# Run chi square test
dcs = chisq.test(counts[,1],p = prob)
sums = 0

# Null hypothesis: actual equals expeced AKA distribution is poisson
# Run chi square test in different way
dcs2 = chisq.test(x=as.vector(counts[,1]),p=prob)


####################################################################################


library(plyr)
dat = read.csv("hcmv.txt")
head(dat)

# Get count of palindromes in each interval of length 4000
dat$bin = as.integer(dat[,1]/4000)
freq.table = count(dat$bin)
# get rid of last bin because it is incomplete
freq.table = freq.table[1:57,]

# Make table of frequency counts
counts = table(as.matrix(freq.table$freq))

# combine 0-2, 9+
counts["2"] = sum(counts[1:2])
counts["9"] = sum(counts[9:length(counts)])
counts = counts[2:9]
#c = counts
# counts bottom row is number of intervals that hold x palindromes
# counts top row is number of palindromes

# calculate expected values
# bin size = 4000
lambda = 294/57
prob = c()

# probability of 0, 1, 2, palindromes in an interval
prob[1] = exp(-lambda)*(1+lambda + (lambda^2)/2)
# calculate remaaining probabilities
for(i in 3:8)
{
  prob[i-1] = exp(-lambda)*(lambda^i/factorial(i))
}
prob[8] = 1-sum(prob[1:7])

# Multiply by #intervals to get expected value
counts = cbind(counts,57*prob)
counts[,2] = round(counts[,2],1)
#get rid of random last count

# Run chi square test
diff = 0

# Null hypothesis: actual equals expeced AKA distribution is poisson
# Run chi square test in different way
dcs2 = chisq.test(x=as.vector(counts[,1]),p=prob)

for (i in 1:dim(counts)[1])
{
  diff = diff + ((counts[i,1] - counts[i,2])^2)/counts[i,2]
}

pchisq(diff, df=6)


####################################################################################


# NOTICE:
# The following code is not R code
# This code is written in Python

import numpy as np
import matplotlib.pyplot as plt
import math
import scipy.stats

def poisson(lamb,k):
  return (lamb**k) * np.exp(-lamb)/math.factorial(k)


def prob_max_hits(lamb,k,m):
  return 1-sum([poisson(lamb,i) for i in range(k)])**m

# n_row = 4
n_col = 2
lengh_dna = 229354
Intervals = range(3000,8000,10)
P = []


data = np.loadtxt("data.txt",skiprows=1)
num_pal = len(data)

for i in range(1,42):
  print(i,prob_max_hits(7.22,19,i))
quit()
for intervals in Intervals:
  n_bins = lengh_dna/intervals
# fig,ax = plt.subplots(len(intervals),n_col,sharex=True,sharey=True)
lamb = 296/n_bins
# print(data.shape)	
n,bins,patches = plt.hist(data,bins=range(0,lengh_dna,intervals))
n,bins,patches = plt.hist(n,bins=range(0,20,1))
# print(n)
# quit()
random = np.sort(np.random.choice(lengh_dna,num_pal,replace=False))
# # print(random)
# ax.set_xlabel('Intervals')
# ax.set_ylabel('Counts')
# for i in range(20):
# 	print(i,prob_max_hits(lamb,i,3))
# data_counts = []
lower = 0
while sum(n[:lower])<4:
  lower += 1
upper = 19
while sum(n[upper:])<4:
  upper -= 1
# lower = 3
# upper = 9
data_counts = []
data_counts.append(sum(n[:lower]))
for i in range(lower,upper):
  data_counts.append(n[i])
data_counts.append(sum(n[upper:]))
# print(data_counts)
# quit()
# data_counts = [0,0,1,1,4,5,5,9,8,1,4,1,0,0,1,0,0,0,1]
expected_num = []
for i in range(20):
  expected_num.append(n_bins*poisson(lamb,i))
# d_counts = []
e_counts = []

# d_counts.append(sum(data_counts [:lower]))
e_counts.append(sum(expected_num[:lower]))

for i in range(lower,upper):
  # d_counts.append(data_counts[i])
  e_counts.append(expected_num[i])

# d_counts.append(sum((data_counts[upper:])))
e_counts.append(sum((expected_num[upper:])))

# d_counts = np.array(d_counts)
e_counts = np.array(e_counts)

ch,p = scipy.stats.chisquare(f_obs=data_counts,f_exp=e_counts,ddof=1)
P.append(p)
plt.clf()
plt.plot(Intervals,P)
plt.show()
# print(data_counts,e_counts)
# print(sum(np.divide((d_counts-e_counts)**2,e_counts)))
# chi_cal = []
# print(e_counts)

# a = np.array([7,8,10,9,8,5,4,6])
# b= np.array([6.4,7.5,9.7,10,8.6,6.3,4.1,4.5])
# # print(sum(np.divide((b-a)**2,b)))
# print(scipy.stats.chisquare(f_obs=a,f_exp=b,ddof=1))
'''
H0: The distribution is Poisson sactter
p-value = 0.000571


'''


####################################################################################


# Read in data and get counts, generate chi sq table
library(plyr)
dat = read.csv("hcmv.txt")
head(dat)

# Get count of palindromes in each interval of length 5594
dat$bin = as.integer(dat[,1]/5594) + 1
freq.table = count(dat$bin)

# Make table of frequency counts
counts = table(as.matrix(freq.table$freq))

# combine 0-3, 9+
counts["4"] = sum(counts[1:3])
counts["9"] = sum(counts[8:length(counts)])
counts = counts[3:8]

# Calculate shape k and scale theta for gamma dist
s = log( sum(counts)/length(counts) ) - sum(log(counts))/length(counts)
k = ( 3 - s + sqrt( (s-3)^2 + 24*s ) )/(12*s)
theta = sum(counts) / (k*length(counts))
# scale by number of intervals
gamma.dist = dgamma(1:6, shape=k, scale=theta)*41

# Get chi sq value

chisq = 0

for(i in 1:length(counts)) {
  chisq = chisq + ((counts[i] - gamma.dist[i])^2)/gamma.dist[i]
}

pchisq(chisq, df=3)


####################################################################################


install.packages("pwr")
library(pwr)

library(plyr)
dat = read.csv("hcmv.txt")
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

counts = cbind(counts,41*prob.poisson)
counts[,2] = round(counts[,2],1)

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


####################################################################################


library(plyr)
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

sums2 = 0
for (i in 1:dim(counts)[1])
{
  sums2 = sums2 + ((counts[i,1] - counts[i,4])^2)/counts[i,4]
}
sums2


####################################################################################


library(plyr)

dat = read.csv("data.txt")
head(dat)
dim(dat)
dat$bin = as.integer(dat[,1]/4587)
dat$bin = dat$bin + 1
x = count(dat$bin)

freqs = x$freq

barplot(freqs)


####################################################################################


# Read in distances and do a line plot
locs = read.table(file = "hcmv.txt", header = T)
dists = (locs[2:(nrow(locs)),1] - locs[1:(nrow(locs)-1),1])

# Generate random locations, get distances, and do a line plot
rand_locs = sort(sample(1:229354, 296))
rand_dists = (rand_locs[2:(length(rand_locs))] - rand_locs[1:(length(rand_locs)-1)])

dists = data.frame(dists)
rand_dists = data.frame(rand_dists)

# Plot histograms
library(ggplot2)
gg_dists <- ggplot(dists, aes(x=dists)) + ylim(0,150)
gg_rand_dists <- ggplot(rand_dists, aes(rand_dists)) + ylim(0,150)


gg_dists + geom_histogram(binwidth = 500) + ggtitle('Distance between consecutive palindromes')
gg_rand_dists + geom_histogram(binwidth = 500) + ggtitle('Distance between consecutive palindromes of randomly generated data')

#############################

# Repeat for triples
# Read in distances and do a line plot
dists_three = (locs[3:(nrow(locs)),1] - locs[1:(nrow(locs)-2),1])

# Generate random locations, get distances, and do a line plot
rand_dists_three = (rand_locs[3:(length(rand_locs))] - rand_locs[1:(length(rand_locs)-2)])

dists_three = data.frame(dists_three)
rand_dists_three = data.frame(rand_dists_three)

# Plot histograms
library(ggplot2)
gg_dists_three <- ggplot(dists_three, aes(x=dists_three)) + ylim(0,70)
gg_rand_dists_three <- ggplot(rand_dists_three, aes(rand_dists_three)) + ylim(0,70)

gg_dists_three + geom_histogram(binwidth = 500) + ggtitle('Distance between 3 consecutive palindromes')
gg_rand_dists_three + geom_histogram(binwidth = 500) + ggtitle('Distance between 3 consecutive palindromes of randomly generated data')


# line plots for two
plot(dists$dists, ylab = "Distance", main = "Distance between consecutive palindromes", type='c')
lines(dists$dists)

plot(rand_dists$rand_dists, ylab = "Distance", main = "Distance between consecutive palindromes\nSimulated random data", type='c')
lines(rand_dists$rand_dists)

# Line plots for 3
plot(dists_three$dists_three, ylab = "Distance", main = "Distance between 3 consecutive palindromes", type='c')
lines(dists_three$dists_three)

plot(rand_dists_three$rand_dists_three, ylab = "Distance", main = "Distance between 3 consecutive palindromes\nSimulated random data", type='c')
lines(rand_dists_three$rand_dists_three)

############ Run chisq test for gamma distribution #########################
# break dists into frequency table, bin size is 5000
dist.freqs = c()

# 0-500
#dist.freqs[1] = count( which(as.integer(dists[,1]/500) == 0) )

# create frequency list with bins of size 500
freq.data = hist(dists_three[,1], xlab = "Distance", main="Distance between 3 consecutive palindromes")
dist.freqs = freq.data$counts

# Combine last 3 bins
dist.freqs[10] = sum(dist.freqs[10], dist.freqs[11], dist.freqs[12])
dist.freqs = dist.freqs[1:10]
# dist.freqs now holds counts of distances that are 0-500, 500-1000, 1000-1500, 1500-2000, ... 5000+

# Create gamma distribution table
avg = mean(dists_three[,1])

gamma.dist.freqs = c()
for(i in 1:10) {
  gamma.dist.freqs[i] = pgamma(i*500,shape=2, scale=avg/2) - pgamma((i-1)*500,shape = 2,scale=avg/2)
}
gamma.dist.freqs[10] = 1 - sum(gamma.dist.freqs[1:9])
gamma.dist.freqs = 294*gamma.dist.freqs

# plot gamma dist on histograms
gamma.df = c()
gamma.df$y = gamma.dist.freqs[1:9]
gamma.df$x = freq.data$breaks[1:9]
gamma.df$x = gamma.df$x + 250

points(gamma.df$x, gamma.df$y, col='red')
lines(gamma.df$x, gamma.df$y, col='red')

# get chi-squared statistic
chisq = 0
for(i in 1:10) {
  chisq = chisq + ((gamma.dist.freqs[i] - dist.freqs[i])^2)/gamma.dist.freqs[i]  
}
pchisq(chisq, df=7)

######################
# Run chisq for exponential distribution
freq.data = hist(dists[,1], xlab = "Distance", main="Distance between 2 consecutive palindromes")
dist.freqs = freq.data$counts

# combine last 4 bins
dist.freqs[8] = sum(dist.freqs[8:11])
dist.freqs = dist.freqs[1:8]

avg = mean(dists[,1])

exp.dist.freqs = c()
for(i in 1:8) {
  exp.dist.freqs[i] = pexp(i*500,rate=1/avg) - pexp((i-1)*500,rate=1/avg)
}
exp.dist.freqs[8] = 1 - sum(exp.dist.freqs[1:7])
exp.dist.freqs = 295*exp.dist.freqs

# plot exp fit
exp.df = c()
exp.df$y = exp.dist.freqs[1:7]
exp.df$x = freq.data$breaks[1:7]
exp.df$x = exp.df$x + 250

points(exp.df$x, exp.df$y, col='red')
lines(exp.df$x, exp.df$y, col='red')

chisq = 0
for(i in 1:8) {
  chisq = chisq + ((exp.dist.freqs[i] - dist.freqs[i])^2)/exp.dist.freqs[i]  
}
pchisq(chisq, df=6)


#############################################python code#######################################

import numpy as np
import matplotlib.pyplot as plt
import math
import scipy.stats

def poisson(lamb,k):
  return (lamb**k) * np.exp(-lamb)/math.factorial(k)


def prob_max_hits(lamb,k,m):
  return 1-sum([poisson(lamb,i) for i in range(k)])**m

# n_row = 4
n_col = 2
lengh_dna = 229354
Intervals = range(3000,8000,10)
P = []


data = np.loadtxt("data.txt",skiprows=1)
num_pal = len(data)

for i in range(1,42):
  print(i,prob_max_hits(7.22,19,i))
quit()
for intervals in Intervals:
  n_bins = lengh_dna/intervals
  # fig,ax = plt.subplots(len(intervals),n_col,sharex=True,sharey=True)
  lamb = 296/n_bins
  # print(data.shape) 
  n,bins,patches = plt.hist(data,bins=range(0,lengh_dna,intervals))
  n,bins,patches = plt.hist(n,bins=range(0,20,1))
  # print(n)
  # quit()
  random = np.sort(np.random.choice(lengh_dna,num_pal,replace=False))
  # # print(random)
  # ax.set_xlabel('Intervals')
  # ax.set_ylabel('Counts')
  # for i in range(20):
  #   print(i,prob_max_hits(lamb,i,3))
  # data_counts = []
  lower = 0
  while sum(n[:lower])<4:
    lower += 1
  upper = 19
  while sum(n[upper:])<4:
    upper -= 1
  # lower = 3
  # upper = 9
  data_counts = []
  data_counts.append(sum(n[:lower]))
  for i in range(lower,upper):
    data_counts.append(n[i])
  data_counts.append(sum(n[upper:]))
  # print(data_counts)
  # quit()
  # data_counts = [0,0,1,1,4,5,5,9,8,1,4,1,0,0,1,0,0,0,1]
  expected_num = []
  for i in range(20):
    expected_num.append(n_bins*poisson(lamb,i))
  # d_counts = []
  e_counts = []

  # d_counts.append(sum(data_counts [:lower]))
  e_counts.append(sum(expected_num[:lower]))

  for i in range(lower,upper):
    # d_counts.append(data_counts[i])
    e_counts.append(expected_num[i])

  # d_counts.append(sum((data_counts[upper:])))
  e_counts.append(sum((expected_num[upper:])))

  # d_counts = np.array(d_counts)
  e_counts = np.array(e_counts)

  ch,p = scipy.stats.chisquare(f_obs=data_counts,f_exp=e_counts,ddof=1)
  P.append(p)
plt.clf()
plt.plot(Intervals,P)
plt.show()
  # print(data_counts,e_counts)
# print(sum(np.divide((d_counts-e_counts)**2,e_counts)))
# chi_cal = []
# print(e_counts)

# a = np.array([7,8,10,9,8,5,4,6])
# b= np.array([6.4,7.5,9.7,10,8.6,6.3,4.1,4.5])
# # print(sum(np.divide((b-a)**2,b)))
# print(scipy.stats.chisquare(f_obs=a,f_exp=b,ddof=1))
'''
H0: The distribution is Poisson sactter
p-value = 0.000571


'''
#####################
