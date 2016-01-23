# Read in data
babies23 = read.table("babies23.txt", header = TRUE)
babies_dot = read.table("babies..txt", header = TRUE)

# Distribution of smokers
# 579 samples
babies23.smoke = babies23[which(babies23$smoke == 1 | babies23$smoke == 2), ]
s.wt = babies23.smoke$wt

# Distribution of nonsmokers
# 544 samples
babies23.nosmoke = babies23[which(babies23$smoke == 0), ]
ns.wt = babies23.nosmoke$wt

# Numerical summary for smokers
mean(s.wt)
var(s.wt)
quantile(s.wt)

# Difference in range and difference between third and first quartiles
diff(range(s.wt))
IQR(s.wt)

# Numerical summary for nonsmokers
mean(ns.wt)
var(ns.wt)
quantile(ns.wt)

# Difference in range and difference between third and first quartiles
diff(range(ns.wt))
IQR(ns.wt)

# Vectors to hold the mean and variance of smokers after robustness
smoke.mean = c()
smoke.var = c()

# Vectors to hold the mean and variance of nonsmokers after robustness
nosmoke.mean = c()
nosmoke.var = c()


# For loop which creates 1000 different samples and stores their mean and
# variance in the above vectors
for (i in 1:1000) {
	
	# Sample size created by excluding 10% of the data
	smoke.sample = sample(s.wt, length(s.wt) * 0.9, replace = FALSE)
	nosmoke.sample = sample(ns.wt, length(ns.wt) * 0.9, replace = FALSE)

	# Storing the numerical data of the samples into their vectors
	smoke.mean = c(smoke.mean, mean(smoke.sample))
	smoke.var = c(smoke.var, var(smoke.sample))
	nosmoke.mean = c(nosmoke.mean, mean(nosmoke.sample))
	nosmoke.var = c(nosmoke.var, var(nosmoke.sample))
	
}

# Determine the average mean and variance after robustness for smokers
mean(smoke.mean)
mean(smoke.var)

# Determine the average mean and variance after robustness for nonsmokers
mean(nosmoke.mean)
mean(nosmoke.var)