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
hist
                       
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

# Standardized Residual Graphing Code 
stdResMax = c(0, 0.0424, 0, 1.1797, 0)
stdResMin = c(-0.1234, 0, -0.3717, 0, -0.1006)
stdResX = c("0-4", "5", "6", "7", "8+")
stdRes = ggplot() + geom_errorbar( aes(x=stdResX, ymax=stdResMax, ymin=stdResMin))
stdRes + labs(x="Palindrome Count", y="Standardized Residual")


stdResMax = c(0, 0, 0.4411, 0.0735, 0.4411, 0.2573, 0, 0.4411, 0.8087, 0)
stdResMin = c(-0.1103, -1.5807, 0, 0, 0, 0, -0.2941, 0, 0, -0.4779)
stdResX = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
stdResX = as.factor(stdResX)
stdRes = ggplot() + geom_errorbar( aes(x=stdResX, ymax=stdResMax, ymin=stdResMin))
stdRes + labs(x="Palindrome Count", y="Standardized Residual")

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
