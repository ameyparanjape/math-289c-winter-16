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

