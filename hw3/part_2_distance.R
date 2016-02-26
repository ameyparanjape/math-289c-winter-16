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

# Plot box plots
#boxplot(dists$dists, rand_dists$rand_dists, names=c('Actual', 'Randomly generated'),main="Distance between consecutive palindromes")

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

# Plot box plots
#boxplot(dists_three$dists_three, rand_dists_three$rand_dists_three, names=c('Actual', 'Randomly generated'),main="Distance between 3 consecutive palindromes")

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
freq.data = hist(dists_three[,1])
dist.freqs = freq.data$counts

# Combine last 3 bins
dist.freqs[10] = sum(dist.freqs[10], dist.freqs[11], dist.freqs[12])
dist.freqs = dist.freqs[1:10]
# dist.freqs now holds counts of distances that are 0-500, 500-1000, 1000-1500, 1500-2000, ... 5000+

# Create gamma distribution table
avg = mean(dists_three[,1])

gamma.dist.freqs = c()
for(i in 1:10) {
  gamma.dist.freqs[i] = 295*( pgamma(i*500,shape=2, scale=avg/2) - pgamma((i-1)*500,shape = 2,scale=avg/2))
}
gamma.dist.freqs

# get chi-squared statistic
chisq = 0
for(i in 1:10) {
 chisq = chisq + ((gamma.dist.freqs[i] - dist.freqs[i])^2)/gamma.dist.freqs[i]  
}
pchisq(chisq, df=7)

# Run chisq for exponential distribution
freq.data = hist(dists[,1])
dist.freqs = freq.data$counts

# combine last 4 bins
dist.freqs[8] = sum(dist.freqs[8:11])
dist.freqs = dist.freqs[1:8]

avg = mean(dists[,1])

exp.dist.freqs = c()
for(i in 1:8) {
  exp.dist.freqs[i] = 295*( pexp(i*500,rate=avg) - pexp((i-1)*500,rate=avg))
}

chisq = 0
for(i in 1:8) {
  chisq = chisq + ((exp.dist.freqs[i] - dist.freqs[i])^2)/exp.dist.freqs[i]  
}
pchisq(chisq, df=6)

