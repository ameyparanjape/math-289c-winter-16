gauge = read.table("gauge.txt", header = T)
gauge$log.gain = log(gauge$gain)
lm1 = lm(log.gain~density, data=gauge)
plot(y=gauge$log.gain, x=gauge$density)
abline(lm1$coefficients[1], lm1$coefficients[2])

lm2 = lm(density~log.gain, data=gauge)
plot(y=gauge$density, x=gauge$log.gain)
abline(lm2$coefficients[1], lm2$coefficients[2])
new.gauge = data.frame(log.gain=c(log(38.6),log(426.7)))
predict(lm2, newdata = new.gauge)

avg.log.gain = tapply(gauge$log.gain, as.factor(gauge$density), FUN = mean)
avg.gain = tapply(gauge$gain, as.factor(gauge$density), FUN = mean)
avg.gauge = data.frame(density=as.numeric(names(avg.gain)), log.gain=avg.log.gain, gain=avg.gain)
plot(y=avg.gauge$density, x=avg.gauge$log.gain)
abline(lm2.avg$coefficients[1], lm2.avg$coefficients[2])

#Log of gain
lm2.avg = lm(density~log.gain, data=avg.gauge)
new.gain = data.frame(log.gain=c(log(38.6),log(426.7)))
predict(lm2.avg, newdata = new.gain)

#Gain
lm2.avg.2 = lm(density~gain, data=avg.gauge)
new.gain.2 = data.frame(gain=c(38.6,426.7))
predict(lm2.avg.2, newdata = new.gain.2)

# EXTRA
lm3.avg = lm(density~log.gain, data=avg.gauge)
new.density = data.frame(density=c(log(38.6),log(426.7)))
predict(lm3.avg, newdata = new.density)

# PUT BANDS AROUND IT
plot_conf_band = function(x, y, p, title, xlabel, ylabel){
  # Create Vandermonde matrix
  X = matrix(1, nrow=length(x), ncol=1)
  for(i in 1:p){
    X = cbind(X, x^i)
  }
  # Perform polynomial regression with degree p
  lm_poly = lm(y ~ poly(x, p, raw = TRUE))
  # Compute the confidence band
  b = lm_poly$coefficients
  XtXInv = solve(t(X)%*%X)
  sigma = summary(lm_poly)$sigma
  F_stat = qf(p=0.95, df1 = p+1, df2 = length(x)-p-1)
  upper = apply(X, 1, function(i) t(b)%*%i + F_stat*sigma*sqrt(t(i)%*%XtXInv%*%i))
  lower = apply(X, 1, function(i) t(b)%*%i - F_stat*sigma*sqrt(t(i)%*%XtXInv%*%i))
  ycap = predict(lm_poly)
  
  # Plot the curve and the band
  temp_table = data.frame(x=x, upper=upper, lower=lower)
  
  temp_tablesort = temp_table[with(temp_table, order(x)), ]
  
  library(ggplot2)
  ggplot() + geom_line(aes(y=upper, x=x)) + geom_line(aes(y=lower, x=x), col="blue") + 
    geom_line(aes(y=ycap, x=x), col="red") +
    ggtitle(label = title) + xlab(xlabel) + ylab(ylabel)+
    geom_point(aes(x=x, y=y), col="red")+
    geom_polygon(aes(x=c(temp_tablesort$x,rev(temp_tablesort$x)),
                     y=c(temp_tablesort$lower,rev(temp_tablesort$upper))), col="green",
                 fill="green", alpha=0.5)
}

plot_conf_band(gauge$gain, gauge$density, 1, title = "Density vs Log of Gain",
               xlabel = "Log of Gain", ylabel = "Density")


####################################################################################


gauge = read.table("gauge.txt", header = TRUE)

gauge['ln.gain'] = log(gauge['gain'])

# Average data into groups of 10
gauge.avg = aggregate( gauge[,2:3], list(gauge$density), mean)

library(ggplot2)

# Plot raw data
plot(gauge[,'density'], gauge[,'gain'])
raw.plot =  ggplot(gauge, aes(gauge[,'density'], gauge[,'gain'])) +
  geom_point(shape=1, colour="blue") +
  xlab("Gain") +
  ylab("Density")+
  ggtitle("Raw Gain vs. Density")
raw.plot

# Plot logged data
plot(gauge[,'density'], gauge[,'ln.gain'])
#ln.plot =   ggplot(gauge, aes(gauge[,'density'], gauge[,'ln.gain'])) +
#            geom_point(shape=1) +
#           geom_smooth(method='lm') +
#            xlab("ln(Gain)") +
#            ylab("Density")+
#            ggtitle("ln(Gain) vs. Density")
#ln.plot

# Plot data averaged into groups of 10
plot(gauge.avg[,1], gauge.avg[,'gain'])

fit = lm(gauge.avg[,'ln.gain'] ~ gauge.avg[,1], data = gauge.avg)
# Plot data with logged gains averaged into groups of 10
plot(gauge.avg[,1], gauge.avg[,'ln.gain'], main = "Density vs. ln(gain) with averaged data",
     xlab = "Density", ylab = "ln(gain)")
abline(fit$coefficients[1],fit$coefficients[2])
ln.avg.plot =   ggplot(gauge.avg, aes(gauge.avg[,1], gauge.avg[,'ln.gain'])) +
  geom_point(shape=1, colour="blue") +
  ylab("ln(Gain)") +
  xlab("Density")+
  ggtitle("Density vs. ln(gain) with averaged data") +
  geom_abline(slope=fit$coefficients[[2]],intercept=fit$coefficients[[1]],colour = "red")
ln.avg.plot


#plot(fit)

residuals = fit$residuals

# Demonstrate constaant variability by plotting residuals
plot(gauge.avg[,'ln.gain'], residuals, main = "ln(gain) vs. Residuals of ln(gain)",
     xlab = "ln(gain)", ylab = "Residual")
abline(0,0)

plot(gauge.avg[,1], residuals, main = "Density vs. Residuals of ln(gain)", xlab = "Density",
     ylab = "Residual")
abline(0,0)


#fit = lm(gauge.avg[,'ln.gain'] ~ gauge.avg[,1], data = gauge.avg)
ln.fit = lm(gauge[,'ln.gain'] ~ gauge[,1], data = gauge)
plot(gauge[,3], ln.fit$residuals)
abline(0,0)


#valid_gain= log(38.6)
#valid_point = 0.508
valid_gain= log(426.70)
valid_point = 0.001
train_gauge=gauge.avg[gauge.avg$Group.1!=valid_point,]

train_fit = lm(Group.1~ln.gain, data = train_gauge)
# Plot data with logged gains averaged into groups of 10

valid_df = data.frame(ln.gain=c(valid_gain))
pre = predict(train_fit,valid_df)
p_conf_int = predict(train_fit, valid_df, interval="confidence")
limit = aes(x=valid_gain,ymax = p_conf_int[,3], ymin=p_conf_int[,2],width=0.2,colours='green')

train_gauge.plot =   ggplot(train_gauge,aes(train_gauge[,'ln.gain'],train_gauge[,1])) +
  geom_point(shape=16, colour="blue",size = 2.5) +
  geom_point(aes(valid_gain,pre),colour='green',size = 2.5)+
  geom_point(aes(valid_gain,valid_point),colour='red',size = 2.5)+
  geom_errorbar(limit)+
  xlab("ln(Gain)") +
  ylab("Density")+
  ggtitle("Density vs. ln(gain) with averaged data") +
  geom_abline(slope=train_fit$coefficients[2],intercept=train_fit$coefficients[1],colour = "red")

train_gauge.plot


####################################################################################


gauge = read.table("gauge.txt", header = TRUE)

gauge['ln.gain'] = log(gauge['gain'])

# Average data into groups of 10
gauge.avg = aggregate( gauge[,2:3], list(gauge$density), mean)

library(ggplot2)

# Plot raw data
plot(gauge[,'density'], gauge[,'gain'])
raw.plot =  ggplot(gauge, aes(gauge[,'density'], gauge[,'gain'])) +
  geom_point(shape=1, colour="blue") +
  xlab("Gain") +
  ylab("Density")+
  ggtitle("Raw Gain vs. Density")
raw.plot

# Plot logged data
plot(gauge[,'density'], gauge[,'ln.gain'])
#ln.plot =   ggplot(gauge, aes(gauge[,'density'], gauge[,'ln.gain'])) +
#            geom_point(shape=1) +
#           geom_smooth(method='lm') +
#            xlab("ln(Gain)") +
#            ylab("Density")+
#            ggtitle("ln(Gain) vs. Density")
#ln.plot

# Plot data averaged into groups of 10
plot(gauge.avg[,1], gauge.avg[,'gain'])

fit = lm(gauge.avg[,'ln.gain'] ~ gauge.avg[,1], data = gauge.avg)
# Plot data with logged gains averaged into groups of 10
plot(gauge.avg[,1], gauge.avg[,'ln.gain'], main = "Density vs. ln(gain) with averaged data",
     xlab = "Density", ylab = "ln(gain)")
abline(fit$coefficients[1],fit$coefficients[2])
ln.avg.plot =   ggplot(gauge.avg, aes(gauge.avg[,1], gauge.avg[,'ln.gain'])) +
  geom_point(shape=1, colour="blue") +
  ylab("ln(Gain)") +
  xlab("Density")+
  ggtitle("Density vs. ln(gain) with averaged data") +
  geom_abline(slope=fit$coefficients[[2]],intercept=fit$coefficients[[1]],colour = "red")
ln.avg.plot


#plot(fit)

residuals = fit$residuals

# Demonstrate constaant variability by plotting residuals
plot(gauge.avg[,'ln.gain'], residuals, main = "ln(gain) vs. Residuals of ln(gain)",
     xlab = "ln(gain)",ylab = "Residual")
abline(0,0)

plot(gauge.avg[,1], residuals, main = "Density vs. Residuals of ln(gain)", xlab = "Density",
     ylab = "Residual")
abline(0,0)
resid.plot =  ggplot(gauge.avg, aes(gauge.avg[,1], residuals)) +
  geom_point(shape=1, colour="blue") +
  ylab("Residual") +
  xlab("Density") +
  ggtitle("Density vs. Residuals of ln(gain)") + 
  geom_hline(aes(0,0), yintercept = 0)
resid.plot

resid.plot.2 =  ggplot(gauge.avg, aes(gauge.avg[,"ln.gain"], residuals)) +
  geom_point(shape=1, colour="blue") +
  ylab("Residual") +
  xlab("ln(gain)") +
  ggtitle("ln(gain) vs. Residuals of ln(gain)") + 
  geom_hline(aes(0,0), yintercept = 0)
resid.plot.2


#residuals = as.vector(residuals)
resid.hist = ggplot(fit, aes(fit$residuals)) + geom_histogram(bins = 3) + xlab("Residual value")

resid.hist

#fit = lm(gauge.avg[,'ln.gain'] ~ gauge.avg[,1], data = gauge.avg)
ln.fit = lm(gauge[,'ln.gain'] ~ gauge[,1], data = gauge)
plot(gauge[,3], ln.fit$residuals)
abline(0,0)

ln.resid.hist =   ggplot(ln.fit, aes(ln.fit$residuals)) + geom_histogram(bins = 9)
ln.resid.hist


####################################################################################


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


svm.poly = svm(gauge.gain ~ gauge.density,data = original.gauge,type = "eps",
               kernel = "polynomial")
pred.poly <- predict(svm.poly, data.frame(X = gauge.density))
lines(gauge.density, pred.poly, col="red")

legend("topright",c("True Model","SVM-Radial Kernel","SVM-Polynomial Kernel"),lty=c(1,1,1),
       lwd=c(2.5,2.5,2.5),col = c("blue","green","red"))
#Tuning the model

tuneParameter <- tune(svm,gauge.gain ~ gauge.density,data = original.gauge,
                      ranges = list(epsilon = seq(0,.2,0.01),cost = 2^(1:9)))
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
legend("topright",c("True Model","Radial SVM-Tuned"),lty=c(1,1),lwd = c(2.5,2.5),
       col = c("red","green"))
