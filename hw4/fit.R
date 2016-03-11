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
plot(gauge.avg[,'ln.gain'], residuals, main = "ln(gain) vs. Residuals of ln(gain)", xlab = "ln(gain)",
     ylab = "Residual")
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
resid.hist =    ggplot(fit, aes(fit$residuals)) + geom_histogram(bins = 3) + xlab("Residual value")
                
resid.hist

#fit = lm(gauge.avg[,'ln.gain'] ~ gauge.avg[,1], data = gauge.avg)
ln.fit = lm(gauge[,'ln.gain'] ~ gauge[,1], data = gauge)
plot(gauge[,3], ln.fit$residuals)
abline(0,0)

ln.resid.hist =   ggplot(ln.fit, aes(ln.fit$residuals)) + geom_histogram(bins = 9)
ln.resid.hist
