####Problem 1: Sort
####Write a function, "sortVector", that will take any vector of numbers and sort them in ascending order.
####This function should also have an argument "Ascending" which defaults to TRUE, but if FALSE will sort
####The vector in descending order (largest to smallest)

sortVector <- function(toSort, sorted = c(), ascending = TRUE) {
  ###input: two numeric vectors, one which is sorted (default is empty) and another to be sorted into it.
  ###ascending should be a logical, if FALSE the sorted vector will be from largest to smallest
  ###output: a numeric in ascending or descending order, dependant on imput
  if(!is.numeric(toSort)) print("toSort must be a numeric vector") #input check
  if (length(toSort) == 0) return(sorted)   #Base Case
  if (ascending) position <-  which(sorted > toSort[1])  #get position of indices in sorted greater than new value
  if (!ascending) position <-  which(sorted < toSort[1]) #new position when descending
  if (length(position) == 0) {sorted <-  c(sorted, toSort[1])  #handles comparisons w/ empty vector
  } else {position <- position[1] #get position of new value (first index in sorted greater or less that new value)
    if (position == 1) { sorted <- c(toSort[1], sorted) #handles new value being sorted to front of sorted
    } else {sorted <-  c(sorted[1:position-1], toSort[1], sorted[position:length(sorted)]) #insert first index of toSort into sorted
    }
  }
  return(sortVector(toSort[-1], sorted, ascending)) #recursive case, removed the index that was just sorted from toSort
}

####Problem 2: Random Movement
####Write a recursive function that takes as input an initial vector, list, or data-frame containing two values ("x" and "y"),
####a number of steps to move, and the mean and sd of this movement. The function should change the x and y coordinates
####"steps" times. The angle of movement should be drawn uniformly form 360 degrees. The distance of the movement
####should be drawn from a normal distribution with mean and sd as input by user. Your function should return a
####dataframe will locations for each step. The functions runif() (draws from uniform distribution), 
####sin(), and cos() will be useful. I've started your function set-up below.
randomMove <- function(start = data.frame(x = 0, y = 0), steps = 10, mean = 1, sd = 0.25) {
  #input: start is a data frame with an "x" and "y" column with 1 row of values
  ##``````steps is the number of moves the agent should make
  ##``````mean and sd are the mean step length and the sd in actual step lengths
  #output: a data frame with new x,y coordinates, rows = steps + 1
  #description: in each step, the agent will move in a random direction with a distance drawn from a normal distribution w/ mean and sd given by input.
  if (!identical(names(start), c("x","y"))) print("location should be a data frame with two columns named 'x' and 'y'")
  dist <- rnorm(1, mean, sd) #randomly determine step length
  turn <- runif(1, 0, 2 * pi) #randomly determine turn angle (radians)
  newX <- start$x + dist * cos(turn) #add x component of move to x 
  newY <-  start$y + dist* sin(turn) #add y component
  newLocation <- data.frame(x = newX, y = newY) #dataframe with new locations
  if (steps == 0) {return(newLocation) #Base Case: function stops when there are no more steps to take
  } else {
    return(rbind(start, randomMove(newLocation, steps - 1, mean, sd))) #rbind takes two dataframes (or a df and a vector) with the same columns and combines them
  }
}
