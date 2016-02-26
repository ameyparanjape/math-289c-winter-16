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

#for (i in 1:dim(counts)[1])
#{
#  sums = sums + ((counts[i,1] - counts[i,2])^2)/counts[i,2]
#}
#sums
#chisq.test(counts,prob)

