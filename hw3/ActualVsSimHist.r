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
title <- "Actual Data vs Simulated Data of Locations of Palindromes in Consecutive Intervals of 4587 Base Pairs"

# Create two histograms and layer them on top of one another
hist <- ggplot(data = histData) + 
     geom_histogram(aes(x=location, fill = "red"), 
                    binwidth = 4587, 
                    color = "black", 
                    alpha = .35) + 
     geom_histogram(aes(x=randDist, fill = "blue"), 
                    binwidth = 4587, color = "black", 
                    alpha = .35) + 
     ggtitle(title) + 
     xlab("locations") + 
     scale_fill_manual(name = "Kind", 
                       values =c('blue'='blue','red'='red'), labels = c('Simulated','Actual'))
