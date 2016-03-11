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
