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

fit = lm(density ~ ln(gain), data = gauge)
