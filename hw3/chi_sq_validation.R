library(plyr)
dat = read.csv("hcmv.txt")
head(dat)

# Get count of palindromes in each interval of length 4000
dat$bin = as.integer(dat[,1]/4000)
freq.table = count(dat$bin)
# get rid of last bin because it is incomplete
freq.table = freq.table[1:57,]

#barplot(count(dat$bin)$freq,names = count(dat$bin)$x * 5594,las = 2)

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
#counts = counts[1:8,]

# Run chi square test
#dcs = chisq.test(counts[,1],p = prob)
diff = 0

# Null hypothesis: actual equals expeced AKA distribution is poisson
# Run chi square test in different way
dcs2 = chisq.test(x=as.vector(counts[,1]),p=prob)

for (i in 1:dim(counts)[1])
{
  diff = diff + ((counts[i,1] - counts[i,2])^2)/counts[i,2]
}

pchisq(diff, df=6)
