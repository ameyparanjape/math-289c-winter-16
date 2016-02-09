# Data is all data
data = read.table("videodata.txt", header=TRUE)

# Sample size = 91
sample.size = nrow(data)

###############################################################################

# Vector to hold the info on which students own a computer
computer = vector()

# For loop goes through each row in our table
for(i in 1:sample.size)
{
  # If, else statements to indicate if a person owns a computer
  if(data[i, "home"] == 1 | data[i, "own"] == 1) {
    computer = c(computer, "yes")
  }
  
  else {
    computer = c(computer, "no")
  }
  
  # If, else statements to replace the "sex" representation to male/female
  if(data[i, "sex"] == 1) {
    data[i, "sex"] = "male"
  }
  
  else {
    data[i, "sex"] = "female"
  }
}

# Adds our new vector to the data
data$comp = computer

###############################################################################

# Data of people who like to play video games and those who don't
d.like = data[which(data$like == 2 | data$like == 3 | data$like == 4), ]
d.nolike = data[which(data$like == 1 | data$like == 5), ]

# People who like to play = 82, People who do not = 8
like.size = nrow(d.like)
nolike.size = nrow(d.nolike)

# Percentage of sample size who like to play
like.size / sample.size

###############################################################################

library(gmodels)

# Displays cross tables based on sex and posession of a computer
CrossTable(d.like$sex, d.like$comp)
CrossTable(d.nolike$sex, d.nolike$comp)
CrossTable(data$sex, data$comp)

###############################################################################

# Data set excludes those who did not answer the work question correctly or did not
# answer the question
d.like = d.like[d.like$work != 99, ]
d.nolike = d.nolike[d.nolike$work != 99, ]

library(ggplot2)

# Displays a dotplot based on sex and how many hours a person worked
ggplot(d.like, aes(sex, work)) + geom_dotplot(binaxis = "y", stackdir = "center") +
  labs(title = "Males/Females vs number of hours worked (like)")
ggplot(d.nolike, aes(sex, work)) + geom_dotplot(binaxis = "y", stackdir = "center") +
  labs(title = "Males/Females vs numbe of hours worked (did not like)")

###############################################################################

# For loop goes through each row in our table
for(i in 1:nrow(d.like))
{
  # If, else statements to indicate if a person worked before taking the survey
  if(d.like[i, "work"] == 0) {
    d.like[i, "work"] = "no"
  }
  
  else {
    d.like[i, "work"] = "yes"
  }
}

# For loop goes through each row in our table
for(i in 1:nrow(d.nolike))
{
  # If, else statements to indicate if a person worked before taking the survey
  if(d.nolike[i, "work"] == 0) {
    d.nolike[i, "work"] = "no"
  }
  
  else {
    d.nolike[i, "work"] = "yes"
  }
}

# Displays cross table on sex and whether they worked
CrossTable(d.like$sex, d.like$work)
CrossTable(d.nolike$sex, d.nolike$work)