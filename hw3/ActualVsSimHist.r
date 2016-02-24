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

################### Interval Maker ############################################
intervalMakerFunc = function(binSize) {

  currInterval  = 1                           # Current Interval to add to
  maxSize       = binSize                     # Max size of current interval
  realData      <- hcmv[, "location"]          # Get real data in vector form
  intervals     = data.frame(Ints=integer())  # Create return data frame

  for (i in 1:length(hcmv)) {           # For all elements in the vector
    num = realData[i]                   # Get current palindrome location
    if (maxSize < num) {                # if its location is greater than max 
      currInterval = currInterval + 1   # Move to next interval in df
      maxSize = maxSize + binSize       # Increase the max for next interval
    }
  #intervals[,currInterval] = num        # Assign it to the interval

  }

  return(intervals)  # Return the df with all intervals

}

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
