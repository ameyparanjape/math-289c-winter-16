# Read in data
library(moments)
library(ggplot2)
babies23 = read.table("babies23.txt", header = TRUE)
babies_dot = read.table("babies..txt", header = TRUE)

head(babies23)

# See distribution of smokers
# 692 samples
babies23.smoke = babies23[which(babies23$smoke != 0), ]
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
s_lb_rate = vector("numeric",length = 10)
ns_lb_rate = vector("numeric",length = 10)
for(i in 1:10){
  #low birthweight rate for smoke
  s_low_birth = babies23.smoke[which(babies23.smoke$wt<i+82),]
  s_lb_rate[i] = nrow(s_low_birth)/length(s.wt)
  
  #low birthweight rate for non-smoke
  ns_low_birth = babies23.nosmoke[which(babies23.nosmoke$wt<i+82),]
  ns_lb_rate[i] = nrow(ns_low_birth)/length(ns.wt)
}
plot(83:93,s_lb_rate)

lb_rate_ratio = s_lb_rate/ns_lb_rate
summary(lb_rate_ratio)

#testing robustness of frequencies by deleting some entries and
#comparing new frequencies with old ones
#1000 simulations
sample_s_rate = vector("numeric", length = 1000)
sample_ns_rate = vector("numeric", length = 1000)

for (i in 1:1000){
  #randomly choose 95% data from the original data as samples from the original data
  n_ori_data = nrow(babies23)*0.95
  n_mod_data = nrow(babies23)-n_ori_data
  sample_data = babies23[sample(1:nrow(babies23),n_ori_data,replace=FALSE),]
  
  sample_s = sample_data[which(sample_data$smoke!=0),]
  sample_ns = sample_data[which(sample_data$smoke==0),] 
  
  sample_s_lw = sample_s[which(sample_s$wt<88),]
  sample_ns_lw = sample_ns[which(sample_ns$wt<88),]
  
  sample_s_rate[i] = nrow(sample_s_lw)/nrow(sample_s)
  sample_ns_rate[i] = nrow(sample_ns_lw)/nrow(sample_ns)
  
}
hist(sample_s_rate,breaks=8,main='Rate of low birthweight for smokers\n1000 simulations',xlab='Low birthweight rate')

hist(sample_ns_rate,breaks=8,main='Rate of low birthweight for non-smokers\n1000 simulations',xlab='Low birthweight rate')


