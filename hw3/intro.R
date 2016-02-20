library(plyr)

dat = read.csv("data.txt")
head(dat)
dim(dat)
dat$bin = as.integer(dat[,1]/4587)
dat$bin = dat$bin + 1
x = count(dat$bin)

freqs = x$freq

barplot(freqs)
