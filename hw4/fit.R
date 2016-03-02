gauge = read.table("gauge.txt", header = TRUE)

gauge['ln.gain'] = log(gauge['gain'])

# Average data into groups of 10
gauge.avg = aggregate( gauge[,2:3], list(gauge$density), mean)

# Plot raw data
plot(gauge[,'gain'], gauge[,'density'])

# Plot logged data
plot(gauge[,'ln.gain'], gauge[,'density'])

# Plot data averaged into groups of 10
plot(gauge.avg[,'gain'], gauge.avg[,1])

# Plot data with logged gains averaged into groups of 10
plot(gauge.avg[,'ln.gain'], gauge.avg[,1])

fit = lm(density ~ ln(gain), data = gauge)
