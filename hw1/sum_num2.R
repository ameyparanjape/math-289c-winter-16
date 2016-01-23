# Grouping data
# Group the non smokers
data.nonsmoke <- babies23[babies23$smoke == 0,]

# Group the current smokers
data.current <- babies23[babies23$smoke == 1,]

# Group the smokers that quit right before preg
data.quitters <- babies23[babies23$smoke == 2,]

# Group the smokers that quit awhile ago
data.oldsmoker <- babies23[babies23$smoke == 3,]

#########################################################################
### Numerical summary of groups-- puting into dataframe
#########################################################################
# initialize table headers/indeces
sum_headers = c("Min", "1st quartile", "Median", "Mean", "3rd quartile", "Max", "Var")

# Summary for non smokers baby weight
non_smokers = c( summary(data.nonsmoke$wt), var(data.nonsmoke$wt))

# initialize dataframe
sum_table = data.frame(non_smokers, row.names = sum_headers)

# Summary for current smokers baby weight
current_smokers = c( summary(data.current$wt), var(data.current$wt))
# append to dataframe
sum_table = cbind(sum_table, current_smokers)

# Summary for smokers that quit right before preg baby weight
quitters = c( summary(data.quitters$wt), var(data.quitters$wt))
# append to dataframe
sum_table = cbind(sum_table, quitters)

# Summary for smokers that quit a while ago baby weight
old_smoker = c( summary(data.oldsmoker$wt), var(data.oldsmoker$wt))
# append to dataframe
sum_table = cbind(sum_table, old_smoker)

# export summary table
write.table(sum_table, "summary_table.tab", sep="\t")