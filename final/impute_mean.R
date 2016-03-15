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

# Replace categorical variables with mode


# df$value[is.na(df$value)] <- median(df$value, na.rm=TRUE)