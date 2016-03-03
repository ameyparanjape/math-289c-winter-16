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

avg.gain = tapply(gauge$log.gain, as.factor(gauge$density), FUN = mean)
avg.gauge = data.frame(density=as.numeric(names(avg.gain)), log.gain=avg.gain)
plot(y=avg.gauge$density, x=avg.gauge$log.gain)
abline(lm2.avg$coefficients[1], lm2.avg$coefficients[2])

lm2.avg = lm(density~log.gain, data=avg.gauge)
new.gain = data.frame(log.gain=c(log(38.6),log(426.7)))
predict(lm2.avg, newdata = new.gain)

# EXTRA
lm3.avg = lm(density~log.gain, data=avg.gauge)
new.density = data.frame(density=c(log(38.6),log(426.7)))
predict(lm3.avg, newdata = new.density)

# PUT BANDS AROUND IT
plot_conf_band = function(x, y, p){
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
    geom_polygon(aes(x=c(temp_tablesort$x,rev(temp_tablesort$x)),y=c(temp_tablesort$lower,rev(temp_tablesort$upper))), col="green", fill="green", alpha=0.5)
}

plot_conf_band()
