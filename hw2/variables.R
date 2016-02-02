# Data is all data
data = read.table("videodata.txt", header=TRUE)

# Sample size = 91
sample.size = nrow(data)

# Sqrt of sample size 91
sqrt.n = sqrt(sample.size)

# Population size = 314
pop.size = 314

# How many hours played week before survey
time.played = data$time


