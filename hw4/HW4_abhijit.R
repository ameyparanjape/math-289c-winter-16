#setwd("C:/Users/abhijit331/Dropbox/Math 289")
setwd("/home/abhijit331/Dropbox/Math 289")
gauge = read.table("gauge.txt",header = T)
gauge$log.gain = log(gauge$gain)

gauge.avg = aggregate(gauge[,2], list(gauge$density), mean)
colnames(gauge.avg) = c("density","gain")
fit = lm(gain ~ density, data = gauge.avg)
plot(gauge.avg$density,gauge.avg$gain)
abline(fit$coefficients[1],fit$coefficients[2])





#SVM
library(e1071)
original.gauge = data.frame(gauge$density,gauge$gain)
plot(original.gauge)
lines(original.gauge$gauge.density,original.gauge$gauge.gain,col = "blue")
gauge.density= seq(0,0.7,0.01)

svm.radial = svm(gauge.gain ~ gauge.density,data = original.gauge,type = "eps",kernel = "radial")
pred.radial <- predict(svm.radial, data.frame(X = gauge.density))
lines(gauge.density, pred.radial, col="green")


svm.poly = svm(gauge.gain ~ gauge.density,data = original.gauge,type = "eps",kernel = "polynomial")
pred.poly <- predict(svm.poly, data.frame(X = gauge.density))
lines(gauge.density, pred.poly, col="red")

legend("topright",c("True Model","SVM-Radial Kernel","SVM-Polynomial Kernel"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col = c("blue","green","red"))
#Tuning the model

tuneParameter <- tune(svm,gauge.gain ~ gauge.density,data = original.gauge,ranges = list(epsilon = seq(0,.2,0.01),cost = 2^(1:9)))
print(tuneParameter) # output: MSE
plot(tuneParam)

gauge.density= seq(0,0.7,0.01)
tunedSVM.rad <- tuneParameter$best.model
tunePredY <- predict(tunedSVM.rad, data.frame(X = original.gauge$gauge.denstiy))
plot(original.gauge, pch=16)
lines(gauge.density, tunePredY, col="red")
# compared with before
lines(gauge.density, pred.radial, col="green")
print(tuneParameter)
legend("topright",c("True Model","Radial SVM-Tuned"),lty=c(1,1),lwd = c(2.5,2.5),col = c("red","green"))
