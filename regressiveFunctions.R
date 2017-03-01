prev_n <- function(atomic_table, day_lag){
  regress_data <- sapply(1:length(atomic_table), function(x){
    if(x <= day_lag){
      return(atomic_table[x])
    }
    else {
      return(atomic_table[x-day_lag])
    }
  })
  return(regress_data)
}

prev_mean_n <- function(atomic_table, day_lag){
  regress_data <- sapply(1:length(atomic_table), function(x){
    if(x <= day_lag){
      return(atomic_table[x])
    }
    else {
      return(mean(atomic_table[x-day_lag:x]))
    }
  })
  return(regress_data)
}

#df$newColumn <- prev_mean_n(df$vectorToRegress, 7)