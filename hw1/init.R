# Read in data
library(moments)
library(ggplot2)
babies23 = read.table("babies23.txt", header = TRUE)
babies_dot = read.table("babies..txt", header = TRUE)

head(babies23)

# See distribution of smokers
# 692 samples
babies23.smoke = babies23[which(babies23$smoke != 0), ]
#ggplot(data, aes(babies23.smoke$wt)) + geom_histogram() 
hist(babies23.smoke$wt)
qqnorm(babies23.smoke$wt)


# See distribution of non smokers
#544 samples
babies23.nosmoke = babies23[which(babies23$smoke == 0), ]
hist(babies23.nosmoke$wt)
qqnorm(babies23.nosmoke$wt)


# See overlapped distribution
hist(babies23.smoke$wt, col=rgb(0.1,0.1,0.5,0.5))
hist(babies23.nosmoke$wt, col=rgb(0.7,0.5,0.1,0.5), add=T)

# See overlapped distribution with boxplots
#boxplot(babies23.smoke$wt, col=rgb(0.1,0.1,0.5,0.5), horizontal=T)
#boxplot(babies23.nosmoke$wt, col=rgb(0.7,0.5,0.1,0.5), add=T, horizontal=T)

boxplot(babies23.smoke$wt, babies23.nosmoke$wt)

