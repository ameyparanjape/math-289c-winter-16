# Proportion of student who played video games the week before the survey
prop.played = length( which(time.played != 0) ) / sample.size

# Estimation for standard error, found on slide 36
prop.played.se = sqrt( prop.played*(1 - prop.played)/(sample.size - 1) )  * sqrt((pop.size - sample.size) / pop.size)

# Confidence interval for proportion found on slide 43
error = 1.96*prop.played.se
prop.cint = c(prop.played - error,prop.played + error)

# Proportion of many people who played in week before played at arcade
prop.arcade = nrow(played.arcade) / length( which(time.played != 0) )

# Proportion of people who played week before that have access to PC with CDROM
prop.have.PC = nrow(played.pc)  / length( which(time.played != 0) )

# Of people who said they play semesterly, proportion who played week before
prop.played.semesterly = nrow( semesterly[which(semesterly$time != 0),] ) / length(semesterly)
