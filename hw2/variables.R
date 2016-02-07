# Data is all data
data = read.table("videodata.txt", header=TRUE)

# Sample size = 91
sample.size = nrow(data)

# Sqrt of sample size 91
sqrt.n = sqrt(sample.size)

# Population size = 314
pop.size = 314

# How many hours played week before survey
time.played = data$time

# Students who prefer arcade and gamed the week before
played.arcade = data[ which(data$time != 0 & data$where == 1), ]

# Students who have access to a PC with a CDROM and gamed the week before
played.pc = data[ which( data$time != 0 & (data$own == 1 | data$home == 1) & data$cdrom == 1), ]

# Students who said they play semesterly
semesterly = data[which(data$freq == 4),]

# Students who said they play monthly
monthly = data[which(data$freq == 3),]
