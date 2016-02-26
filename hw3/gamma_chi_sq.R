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
