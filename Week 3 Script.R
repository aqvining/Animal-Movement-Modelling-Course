####Write a recursive function that moves an agent a given number of steps in an x-y plane, where it can move 1 unit up, down, left, or right
####(or remain in the same place). The output should return all locations the agent has been in a data frame
move <- function(location, steps) {
  #input: location is a data frame with an "x" and "y" column with 1 row of values
  ##``````steps is the number of moves the agent should make
  #output: a data frame with 10 new x,y coordinates
  #description: in each step, the agent can move 1 unit in any cardinal direction, or remain still.
  if (!identical(names(location), c("x","y"))) print("location should be a data frame with two columns named 'x' and 'y'")
  newX <- location$x + sample(-1:1, size = 1) #sample takes a random draw from a vector of numbers
  newY <-  location$y + sample(-1:1, size = 1)
  newLocation <- data.frame(x = newX, y = newY)
  if (steps == 0) {return(newLocation) #Base Case: function stops when there are no more steps to take
  } else {
    return(rbind(location, move(newLocation, steps - 1))) #rbind takes two dataframes (or a df and a vector) with the same columns and combines them
  }
}