setwd("C:/Users/abhijit331/Dropbox/Math 289")
gauge = read.table("gauge.txt",header = T)
gauge$gain = log(gauge$gain)

gauge.avg = aggregate(gauge[,2], list(gauge$density), mean)
colnames(gauge.avg) = c("density","gain")
fit = lm(gain ~ density, data = gauge.avg)
plot(gauge.avg$density,gauge.avg$gain)
abline(fit$coefficients[1],fit$coefficients[2])



# Q4
fit.quad =lm(gain ~ poly(density,2,raw = TRUE),data = gauge.avg)
seq.values = seq(0,0.7,0.01)
predict.val = predict(fit.quad,list(density = seq.values,dne))