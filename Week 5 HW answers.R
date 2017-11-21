###Problem 1
###The following function should take each number in a vector and add the next number in the vector to it, but it's not working right! 
###There are two errors in the code. Find them and fix them.
loopAdd <- function(toAdd) {
  #input: a numeric vector
  #output: the input vector in ascending order.
  if (!is.numeric(toAdd)) {  #input check
    print("input must be numeric")
  } else {
    for(i in seq(length(toAdd)-1)) {
      toAdd[i] <- toAdd[i] + toAdd[i+1]
    }
    return(toAdd)
  }
}

###Problem 2
#Imagine you are a rabbit trying to forage, but there are scary foxes around. You should only go out to find food when there are no foxes
#right next to you. Write a function that will take two lists, one containing the locations of rabbits and one with the locations of foxes.
#The function should return a vector of Booleans indicating whether it is safe or not for each rabbit to go outside (it's not safe if there is
#a fox within 2 units of the rabbit).
#Hint: Try writing a separate function to calculate the distance between two points.

toForage <- function(rabbits, foxes) {
  #input: two lists of coordinate pairs
  #output: A vector of booleans
  #function: determines if any of the coordinate pairs in the second list are within two units
  #          of each of the coordinate pairs in the first list
  forage = rep(TRUE, times = length(rabbits)) #set a default of TRUE(rabbit should forage)
  for (i in 1:length(rabbits)) {
    for (j in 1:length(foxes)) {
      if (getDist(rabbits[[i]], foxes[[j]]) <= 2) forage[[i]] <- FALSE #changes to False if any foxes are nearby
    }
  }
  return(forage)
}

getDist <- function(position1, position2) {
  #input:two coordinate pairs (vectors)
  #output: distance between coordinate pairs (numeric)
  deltaX <- position1[1] - position2[1] #difference between x coordinates
  deltaY <- position1[2] - position2[2] #diff between y coordinates
  return(sqrt(deltaX**2 + deltaY**2)) #pythagorean theorum
}

#Test Set 1, output = c(F,F,T,T)
rabbits1 = list('A' = c(0,0), 'B' = c(1,3), 'C' = c(-3, 2), 'D' = c(-2,2))
foxes1 = list('A' = c(1,1))

#Test Set 2, output = c(F,F,F,T)
rabbits2 = list('A' = c(0,0), 'B' = c(1,3), 'C' = c(-3, 2), 'D' = c(-4,5))
foxes2 = list('A' = c(0,0), 'B' = c(2,3), 'C' = c(-2, 1), 'D' = c(-10,5))

###Problem 3
#We just wrote a function to determine when it is safe for rabbits to go outside, but now imagine the rabbit is outside and foraging, but
#must still be on the lookout for foxes. Write a function that takes as input the location of a single rabbit and any number of foxes.
#Every animal moves at the same speed (1) in a random direction, but if a fox is nearby, the rabbit will not move. If there is no fox, the rabbit will move
#and have a 50% chance of finding food. The function should continue until the rabbit has found 10 pieces of food and return all the locations
#of the rabbit. I've gotten you stared below:
rabbitForage <- function(rabbit, foxes, speed = 1, full = 10, probabilityFood = 0.5) {
  #input:
  ##rabbit: a numeric vector with two numbers, x and y coordinates
  ##A list of coordinate pairs
  ##speed, full: single numeric values
  #output: a data frame with coordinate pairs
  rabbit = list(rabbit)
  food <- 0
  while(food < full) { #run until rabbit is full
    while(foxNearby(rabbit[[length(rabbit)]], foxes)) { #foxNearby is another function, below, you will need to fill in
      foxes <- move(foxes, speed) #move is another function, below, you will need to fill in. When a fox is nearby, only foxes move
    }
    foxes <- move(foxes, speed) #when a fox isn't nearby three things need to happen. What are they? Write code to do those things
    rabbit <- c(rabbit, move(rabbit[length(rabbit)], speed))
    if (runif(1,0,1) <= probabilityFood) food <- food + 1
  }
  return(rabbit)
}

move <- function(locations, distance) { #fill this in! I would recomend a for loop, and remember the trigonometry we used to move agents before!
  #input: a list of x,y coordinate pairs - eg: list(c(0,1), c(2,3), c(-1,-2))
  #distance: a single numeric
  #output: a list of same length as input in which each coordinate pair has moved the value given in distance
  for(i in 1:length(locations)) {
    theta <- runif(1,0, 2*pi)
    locations[[i]][1] <- locations[[i]][1] + distance * cos(theta)
    locations[[i]][2] <- locations[[i]][2] + distance * sin(theta)
  }
  return(locations)
}

foxNearby <- function(rabbit, foxes) {# this function should check if any foxes are within 2 units of the rabbit. Again, use a for loop and trigonometry.
  ##Hint: this should be very similar to the function you wrote for problem two.
  #input: rabbit: a vector/df row with 2 numeric values
  ##foxes: a list of coordinate pairs
  #output: Boolean, TRUE if a fox is within 2 units of rabbit.
  for (i in 1:length(foxes)) {
    if (getDist(rabbit, foxes[[i]]) <= 2) return(TRUE)
  }
  return(FALSE)
}

###Problem 4
#Congratulations! You made an Agent Based Model! What are 2 questions we could ask using this model?
