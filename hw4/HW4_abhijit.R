setwd("C:/Users/abhijit331/Dropbox/Math 289")
gauge = read.table("gauge.txt", header = T)
head(gauge)
gauge$gain = log(gauge$gain)
head(gauge)
plot(gauge)
fit = lm()

gauge$avg.gain  = 0
z = trunc(gauge$density * 10)
for(i in 0:6)
{
  temp = which(z == i)
  gauge$avg.gain[i+1] = sum(gauge$gain[temp])/length(which(z == i))
}
gauge.new = data.frame(density = (0:6)/10,avg.gain = gauge$avg.gain[1:7])
