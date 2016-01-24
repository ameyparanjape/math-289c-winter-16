# Read in data
library(moments)
library(ggplot2)
babies23 = read.table("babies23.txt", header = TRUE)
babies_dot = read.table("babies..txt", header = TRUE)

babies23.smoke = babies23[which(babies23$smoke != 0), ]
s.wt = babies23.smoke$wt

babies23.nosmoke = babies23[which(babies23$smoke == 0), ]
ns.wt = babies23.nosmoke$wt

#Examing the frequencies using different definition of low birth weight
#from 83 to 93
s_lb_rate = vector("numeric",length = 11)
ns_lb_rate = vector("numeric",length = 11)
for(i in 1:11){
  #low birthweight rate for smoke
  s_low_birth = babies23.smoke[which(babies23.smoke$wt<i+82),]
  s_lb_rate[i] = nrow(s_low_birth)/length(s.wt)
  
  #low birthweight rate for non-smoke
  ns_low_birth = babies23.nosmoke[which(babies23.nosmoke$wt<i+82),]
  ns_lb_rate[i] = nrow(ns_low_birth)/length(ns.wt)
}

test_range = 83:93
plot(test_range,s_lb_rate,ylim=c(0.0,0.1),col='red',main="Low Birth Weight Threshold vs Low Birth Rate",xlab="Low Birth Weight Threshold(pounds)",ylab="Low Birth Weight Rate")
lines(test_range,s_lb_rate,col='red',ylim=c(0.0,0.1),xlab='',ylab='')
par(new=T)
plot(test_range,ns_lb_rate,col='blue',ylim=c(0.0,0.1),xlab='',ylab='')
lines(test_range,ns_lb_rate,col='blue',ylim=c(0.0,0.1),xlab='',ylab='')
abline(lm(s_lb_rate ~ test_range),col='red')
abline(lm(ns_lb_rate ~ test_range),col='blue')
legend("topleft",c('Smoking mothers','Non-smoking mothers'),col=c('red','blue'),pch=c(1,3))
par(new=F)

#Ratio of (low birth weight rate for smokers)/(low birth weight rate for non-smokers)
lb_rate_ratio = s_lb_rate/ns_lb_rate
plot(test_range,lb_rate_ratio,ylim=c(0.0,2.5),main="Smoking mothers vs Non-Smoking Mothers\nRatio of low birth rate",xlab="Low Birth Weight Threshold(pounds)",ylab="Ratio")
lines(test_range,lb_rate_ratio,ylim=c(0.0,2.5),main="Smoking mothers vs Non-Smoking Mothers\nRatio of low birth rate",xlab="Low Birth Weight Threshold(pounds)",ylab="Ratio")


summary(s_lb_rate)
summary(ns_lb_rate)
summary(lb_rate_ratio)

#testing robustness of frequencies by deleting some entries and
#comparing new frequencies with old ones
#1000 simulations
sample_s_rate = vector("numeric", length = 1000)
sample_ns_rate = vector("numeric", length = 1000)

for (i in 1:1000){
  #randomly choose 95% data from the original data as samples from the original data
  n_ori_data = nrow(babies23)*0.9
  n_mod_data = nrow(babies23)-n_ori_data
  sample_data = babies23[sample(1:nrow(babies23),n_ori_data,replace=FALSE),]
  
  sample_s = sample_data[which(sample_data$smoke!=0),]
  sample_ns = sample_data[which(sample_data$smoke==0),] 
  
  sample_s_lw = sample_s[which(sample_s$wt<88),]
  sample_ns_lw = sample_ns[which(sample_ns$wt<88),]
  
  sample_s_rate[i] = nrow(sample_s_lw)/nrow(sample_s)
  sample_ns_rate[i] = nrow(sample_ns_lw)/nrow(sample_ns)
  
}

hist(sample_s_rate,breaks=8,main='Rate of Low Birth Weight for Smoking Mothers\n1000 Simulations',xlab='Low Birth Weight rate')
hist(sample_ns_rate,breaks=8,main='Rate of Low Birth Weight for Non-smoking Mothers\n1000 Simulations',xlab='Low Birthweight Rate')
summary(sample_s_rate)
summary(sample_ns_rate)

#original low birth weight rate for smokers
s_lb_rate[6]

#original low birth weight rate for non-smokers
ns_lb_rate[6]



