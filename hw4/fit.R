gauge = read.table("gauge.txt", header = TRUE)

# Average data into groups of 10
gauge.avg = aggregate( gauge['gain'], list(gauge$density), mean)

plot(gauge['gain'], gauge['density'])
plot(gauge.avg['gain'], gauge.avg['density'])