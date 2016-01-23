# Read in data
library(moments)
library(ggplot2)
babies23 = read.table("babies23.txt", header = TRUE)
babies_dot = read.table("babies..txt", header = TRUE)

head(babies23)

# See distribution of smokers
# 692 samples
babies23.smoke = babies23[which(babies23$smoke != 0), ]
s.wt = babies23.smoke&smoke
#ggplot(data, aes(babies23.smoke$wt)) + geom_histogram() 
hist(s.wt)
qqnorm(s.wt)


# See distribution of non smokers
# 544 samples
babies23.nosmoke = babies23[which(babies23$time == 0), ]
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

<<<<<<< HEAD
# See distribution of former smokers
# 103 samples
babies23.hadsmoke = babies23[which(babies23$smoke == 3), ]
hs.wt = babies23.hadsmoke$wt
hist(hs.wt)
qqnorm(hs.wt)

# See overlapped distribution of all three
hist(ns.wt, col=rgb(1, 0, 0,0.5))
hist(s.wt, col=rgb(0, 1, 0, 0.5), add=T)
hist(hs.wt, col=rgb(0, 0, 1, 0.5), add=T)

# See ovelapped distribution with boxplots
boxplot(ns.wt, s.wt, hs.wt)
=======
# See if education and smoking during pregnancy are correlated
s.college = babies23[which(babies23$smoke == 1),]
s.college = s.college[which(s.college$ed == 5),]
head(s.college)

college = babies23[which(babies23$ed == 5),]
s.college.prop = nrow(s.college) / nrow(college)

smokers = babies23[which(babies23$smoke == 1),]
s.prop = nrows(smokers) / nrows(babies23)
>>>>>>> b736161ae3bf6c617e37b32d585346d72c576fca
