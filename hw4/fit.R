gauge = read.table("gauge.txt", header = TRUE)

gauge['ln.gain'] = log(gauge['gain'])

# Average data into groups of 10
gauge.avg = aggregate( gauge[,2:3], list(gauge$density), mean)

# Plot raw data
plot(gauge[,'density'], gauge[,'gain'])

# Plot logged data
plot(gauge[,'density'], gauge[,'ln.gain'])

# Plot data averaged into groups of 10
plot(gauge.avg[,1], gauge.avg[,'gain'])

# Plot data with logged gains averaged into groups of 10
plot(gauge.avg[,1], gauge.avg[,'ln.gain'])

fit = lm(gauge.avg[,'ln.gain'] ~ gauge.avg[,1], data = gauge.avg)
abline(fit$coefficients[1],fit$coefficients[2])
#plot(fit)

residuals = fit$residuals

# Demonstrate constaant variability
plot(gauge.avg[,'ln.gain'], residuals)

# Plots to do
# x vs. y
# ln.gain vs. residuals
# qqplot of residuals
# data with logged gains averaged into groups of 10