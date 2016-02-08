# Math 189/289
# Lab 5
# 02/03/2016





# Part I:
# Writing bootstrap function.
# Pro: Whenever you want to apply bootstrap in the future, just call this function

# @param x is a sample, B is the number of bootstrap times, width is the width of histogram
boot.statistics = function(x, B, breaks=NULL, adjust.density=TRUE){
  par(mfrow=c(1,2))
  n = length(x)
  boot.samples = matrix(sample(x, size=B*n, replace=TRUE), B, n)
  boot.mean = apply(boot.samples, 1, mean)
  se = sd(boot.mean)
  if (is.null(breaks)){
    breaks = 30 # 30 is how many breaks you want
  }
  hist(boot.mean, breaks = breaks, freq=FALSE)
  if (adjust.density){
    lines(density(boot.mean, adjust=2), col="red")
  }else{
    lines(density(boot.mean), col="red")
  }
  qqnorm(boot.mean)
  qqline(boot.mean)
  CI = mean(x) + c(-1,1)*1.96*se
  print(CI)
  return (list(boot.mean = boot.mean, CI = CI, se = se))
}

# Let's use this function
videodata <- read.table("videodata.txt", header=TRUE) # read the first line as header
head(videodata)
names(videodata)

# by default
boot.video = with(videodata, boot.statistics(time, B=1000))
# want to change break numbers
boot.video = with(videodata, boot.statistics(time, B=1000, breaks=50))
# don't wanna adjust density smoothness
boot.video = with(videodata, boot.statistics(time, B=1000, adjust.density=FALSE))

# get CI
boot.video$CI
# get se
boot.video$se





# Part II:
# sample size matters (N, n, N/n)
# recall formulae on page 36 of lecture note chp 3

install.packages("Lock5Data",repos="http://cran.us.r-project.org")
library(Lock5Data)
data(CommuteAtlanta)
str(CommuteAtlanta)
time = CommuteAtlanta$Time
total = length(time)
total

# case 1: N=200, n=90
# calculate the estimator of SE from page 36
N = 200
n = 90
time.population1 = sample(time, size=N, replace=FALSE)
time.sample1 = sample(time.population1, size=n, replace=FALSE)
s1 = sd(time.population1)
se1.page36 = s1*sqrt((N-n)/(n*N))
se1.page36
# compare with bootstrap
boot1 = boot.statistics(time.sample1, B=1000)
boot1$se
sprintf("SE estimator from page 36 is %f, bootstrap SE is %f. Statistics lies??!!", se1.page36, boot1$se)
# see the difference?

# case2: N=500, n=200
N = 500
n = 200
time.population2 = sample(time, size=N, replace=FALSE)
time.sample2 = sample(time.population2, size=n, replace=FALSE)
s2 = sd(time.population2)
se2.page36 = s2*sqrt((N-n)/(n*N))
se2.page36
# compare with bootstrap
boot2 = boot.statistics(time.sample2, B=1000)
boot2$se
sprintf("SE estimator from page 36 is %f, bootstrap SE is %f. Hmm, I just need more samples.", se2.page36, boot2$se)
# now the difference is much smaller

# conclusion: In this case, bootstrap tends to over estimate the se.

# conclusion: With larger sample size, bootstrap estimator is closer to the theoretical one.





# Part III: boot package
library(boot)
data(CommuteAtlanta)
# you have to define this my.mean method, cauz boot needs an indices parameter
my.mean = function(x, indices) {
  return( mean( x[indices] ) )
}
time.boot = boot(CommuteAtlanta$Time, my.mean, 1000)
boot.ci(time.boot)
# Basic uses the estimated standard error. Percentile uses percentiles. BCa also uses percentiles, but adjusted to account for bias and skewness.

