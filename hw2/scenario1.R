# Proportion of student who played video games the week before the survey
prop.played = length( which(time.played != 0) ) / sample.size

# Estimation for standard error, found on slide 36
prop.played.se = sqrt( prop.played*(1 - prop.played)/(sample.size - 1) )  * sqrt((pop.size - sample.size) / pop.size)

# Confidence interval for proportion found on slide 39
error = 1.96*prop.played.se
prop.cint = c(prop.played - error,prop.played + error)