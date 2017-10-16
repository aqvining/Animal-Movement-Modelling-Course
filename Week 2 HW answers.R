###Alexander Vining
###Animal Movement Modelling
###Week 2 Homework

#####Problem 1
removeNegative <-  function(values) {
  ###input: a dataframe of any size
  ###output: the same data frame with any negative values replaced by 0's
  if (!is.vector(values) & !is.list(values)) { #check for valid input
    print("input must be a vector or a list")
    return(values)
  }
  if(is.data.frame(values)){    #data.frames are still considered lists, so an extra check is necessary
    print("input must be a vector or non-data.frame list")
    return(values)
  }
  negativeIndex <- which(values < 0)  #get position of all negative values
  values[negativeIndex] <- 0 #replace all negative values with 0
  values
}

#####Problem 2
reduce  <- function(v) {
  #input: a numeric vector
  #output: a numeric vector
  #function description: If the range of values in the input
  ####           vector is over 100, divide all values by 2. 2) If the previous reduction occured and any values are still greater
  ####           than 100 or less than -100, make them 100 or -100 respectively. 3) If there are more negative values than positive 
  ####           values, reverse the sign of all values (do this regardless of whether the previous two steps occurred).
  if (! is.numeric(v)) {
    print("input must be numeric vector")
    return(v)
  }
  if(max(v) - min(v) > 100) {
    v <- v/2
    v[v < -100] <- -100
    v[v > 100] <- 100
    if (sum(v < 0) > sum(v > 0)) v <- -v
    return(v)
  }
  if (sum(v < 0) > sum(v > 0)) {v <- -v
  } else print("No changes occured")
  return(v)
}
