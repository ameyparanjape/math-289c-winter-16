# Remove outlier from time.played
time.played.prun = time.played[-59]

# Bootstrap function from discussion 5, modified for this project
# @param x is a sample, B is the number of bootstrap times, width is the width of histogram
boot.statistics = function(x, B, breaks=NULL, adjust.density=TRUE){
  #par(1)
  n = length(x)
  boot.samples = matrix(sample(x, size=B*n, replace=TRUE), B, n)
  boot.mean = apply(boot.samples, 1, mean)
  se = sd(boot.mean)
  if (is.null(breaks)){
    breaks = 30 # 30 is how many breaks you want
  }
  plot.new()
  hist(boot.mean, breaks = breaks, freq=FALSE, 
       main = "Bootstrap Distribution of Time Played", xlab = "Time Played in Hours")
  
  if (adjust.density){
    lines(density(boot.mean, adjust=2), col="red")
  }else{
    lines(density(boot.mean), col="red")
  }
  
  qqnorm(boot.mean, main = "Q-Q Plot of Bootstrap Sample")
  qqline(boot.mean)
  CI = mean(x) + c(-1,1)*1.96*se
  print(CI)
  return (list(boot.mean = boot.mean, CI = CI, se = se))
}

boot.data = boot.statistics(time.played, 1000, adjust.density = TRUE)

boot.data.prun = boot.statistics(time.played.prun, 1000, adjust.density = TRUE)

# Check kurtosis of bootstrap dist
library(moments)
kurtosis(boot.data$boot.mean)

ci = boot.data$CI

