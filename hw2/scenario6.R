# Get proportion of expected grades in class
prop.exp.A = length( expected.grades[which(expected.grades == 4)] ) / length(expected.grades)
prop.exp.B = length( expected.grades[which(expected.grades == 3)] ) / length(expected.grades)
prop.exp.C = length( expected.grades[which(expected.grades == 2)] ) / length(expected.grades)
prop.exp.D = length( expected.grades[which(expected.grades == 1)] ) / length(expected.grades)
prop.exp.F = length( expected.grades[which(expected.grades == 0)] ) / length(expected.grades)

# Plot exp and std next to eachother
barplot(rbind(expected.grade.props, standard.props), 
        main = "Grade Proportions", names.arg = grade.names, beside=TRUE
        ,col=c("darkblue","red"),legend = c("Expected", "Standard"), ylim = c(0,.6))
