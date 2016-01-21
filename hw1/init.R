# Read in data
library(moments)
library(ggplot2)
babies23 = read.table("babies23.txt", header = TRUE)
babies_dot = read.table("babies..txt", header = TRUE)

head(babies23)

# See distribution of smokers
# 579 samples
babies23.smoke = babies23[which(babies23$smoke == 1 | babies23$smoke == 2), ]
s.wt = babies23.smoke$wt
#ggplot(data, aes(babies23.smoke$wt)) + geom_histogram() 
hist(s.wt)
qqnorm(s.wt)


# See distribution of non smokers
#544 samples
babies23.nosmoke = babies23[which(babies23$smoke == 0), ]
ns.wt = babies23.nosmoke$wt
hist(ns.wt)
qqnorm(ns.wt)


# See overlapped distribution
hist(s.wt, col=rgb(0.1,0.1,0.5,0.5))
hist(ns.wt, col=rgb(0.7,0.5,0.1,0.5), add=T)

# See overlapped distribution with boxplots
#boxplot(babies23.smoke$wt, col=rgb(0.1,0.1,0.5,0.5), horizontal=T)
#boxplot(babies23.nosmoke$wt, col=rgb(0.7,0.5,0.1,0.5), add=T, horizontal=T)

boxplot(s.wt, ns.wt)

