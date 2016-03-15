# Replace numerical columns with median
train_filled = train
# For each column
for(i in 1:ncol(train_filled)) {
  # If column is numeric
  if(is.numeric( train_filled[,i] )) {
    median = median(train_filled[,i], na.rm = TRUE)
    
    # Replace all values with medians
    for(j in 1:nrow(train_filled)) {
      if( is.na(train_filled[j,i]) ) {
        train_filled[j,i] <- median 
      }
    }
  }
}

train_filled_3 = read.csv("~/Desktop/train_filled.csv")
# Replace missing categorical variables with mode

# Turn missing values into NAs instead of ""s
replace_with_NA <- function(column){
  column[ column == ""] <- NA
  return(column)
}

train_filled_2 = train_filled
for(i in 1:ncol(train_filled_2)) {
  if ( is.factor(train_filled_2[,i]) ) {
    train_filled_2[,i] = replace_with_NA(train_filled_2[,i])
  }
}

# Define function to get mode of categorical column
replace.mode <- function(column){
  mode = names( sort(table(column),decreasing=TRUE)[1] )
  if(mode == "") {
    mode = names( sort(table(column),decreasing=TRUE)[2] )
  }
  
  column[ column == ""] = mode
  return(column)
}

# Replace ""s in each column with mode
#for(i in 1:ncol(train_filled_2)) {
for(i in 1:ncol(train_filled_3)) {
  if(is.factor(train_filled_3[,i])) {
    train_filled_3[,i] = replace.mode(train_filled_3[,i])
  }
}


# df$value[is.na(df$value)] <- median(df$value, na.rm=TRUE)