###Write a function that tells you if any values in a vector are more 
###than a given number of standard deviations from the mean###

outlierCheck <- function(values, outlierLevel = 3) { #Setting a default means the variable doesn't have to be entered when 
                                                     #calling function
  #input: values = a vector of numeric values
  #       outlierLevel = a single numeric value indicating how many standard deviations indicates an outlier
  #output: logical. TRUE if an outlier exists, FALSE if no outlier
  #explanation: this function is fairly simple, but more complex function may require more explanation here
  
  if(!is.numeric(values) | !class(outlierLevel) == "numeric"){ #input check
    print("all variables must be numeric")
    return(NA)
  }  #when this if statement is executed a value is returned and we exit the function, so no 'else' is needed
  
  deviations <- values - mean(values) #In R, if subraction (or any other opperation) a single value from a vector performs
                                      #the operation on all values in the vector.
  
  standardDeviation <-  sd(values)
  return(values[abs(deviations) >= outlierLevel * standardDeviation])
  #if (sum(abs(deviations) >= outlierLevel * standardDeviation) > 0) return(TRUE) #don't need brackets for a single line!
                                                                                 ##"abs()" takes the absolute value of all inputs
  #FALSE #if you give a value without 'return()' it will be provided as output, but the function won't stop. Any values given later will
        #overwrite the output!
}

###Practice! Write a function that takes two vectors and a threshold value as input. It should output the indices that have
###the same value in both vectors AND that value is greater than the threshold value.

thresholdCompare = function(v1, v2, threshold) {
  which(v1 > threshold & v1 == v2)
}

#####In Class Tests
result <- 10
testFnct <- function(a,b){
  result <- a + b
  return(result)
}
