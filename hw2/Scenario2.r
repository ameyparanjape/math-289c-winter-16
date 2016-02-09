videodata <- read.csv("~/Documents/Math289c/Datasets/videodata.txt", sep="")
View(videodata)
data = videodata

# Students who said they play semsterly
semesterly = data[which(data$freq == 4),]

# Students who said they play monthly
monthly = data[which(data$freq == 3),]

# Students who said they play weekly
weekly = data[which(data$freq == 2),]

# Students who said they play daily
daily = data[which(data$freq == 1),]

# Students who answered a valid answer for freq
valid = data[which(data$freq != 99),]
valid = valid[which(valid$busy != 99),]

# Create groups for number of people who played games the week prior
semesterly.played <- semesterly[which(semesterly$time != 0),]
monthly.played <- monthly[which(monthly$time != 0),]
weekly.played <- weekly[which(weekly$time != 0),]
daily.played <- daily[which(daily$time != 0),]

# Get the number of people with each reported freq
sem.size <- nrow(semesterly)
mon.size <- nrow(monthly)
wee.size <- nrow(weekly)
dai.size <- nrow(daily)

# Get the number of people who played the week prior in each group
sem.play.size <- nrow(semesterly.played)
mon.play.size <- nrow(monthly.played)
wee.play.size <- nrow(weekly.played)
dai.play.size <- nrow(daily.played)

# Get the proportions for each reported freq
sem.prop.played = sem.play.size / sem.size
mon.prop.played = mon.play.size / mon.size
wee.prop.played = wee.play.size / wee.size
dai.prop.played = dai.play.size / dai.size

# Get the number of people with each freq that play when busy
sem.busy <- nrow(semesterly[which(semesterly$busy == 1),])
mon.busy <- nrow(monthly[which(monthly$busy == 1),])
wee.busy <- nrow(weekly[which(weekly$busy == 1),])
dai.busy <- nrow(daily[which(daily$busy == 1),])

# Get the proportions for each reported freq
sem.prop.busy = sem.busy / sem.size
mon.prop.busy = mon.busy / mon.size
wee.prop.busy = wee.busy / wee.size
dai.prop.busy = dai.busy / dai.size

# Get the subsets

vector <- c(daily$busy,weekly$busy,monthly$busy,semesterly$busy)
print(vector[])

# Stacked Bar Plot with Colors and Legend

counts <- table(valid$busy, valid$freq)
barplot(counts, main="Students that Play if Busy by Reported Frequency",
        xlab="Reported Freq of Play", col=c("darkblue","red"))


library(ggplot2)
p <- ggplot(valid, aes(x=freq, fill=factor(busy))) 
p + geom_bar() 


p <- ggplot(valid, aes(x=freq, fill=factor(time>0))) 
p + geom_bar() 

# Create the boxplot without outliers
labels <- c("Daily", "Weekly", "Monthly", "Semesterly")
boxplot(valid$time~valid$freq, outline = FALSE, 
        main="Time Spend in the Week Prior vs Reported Frequency of Play", 
        xlab="Reported Frequency of Play",
        ylab="Hours played",
        las=2 , xaxt="n")
axis(1, at=1:4, labels=labels)

# Create the boxplot with outliers
boxplot(valid$time~valid$freq, 
        main="Time Spend in the Week Prior vs Reported Frequency of Play", 
        xlab="Reported Frequency of Play",
        ylab="Hours played",
        las=2 , xaxt="n")
axis(1, at=1:4, labels=labels)

