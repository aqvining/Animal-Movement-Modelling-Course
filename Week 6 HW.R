###Week 6 HW###

###Problem 1
#First, write or find a function that will take as input a coordinate pair, move it one unit in a random direction, and output the
#resulting coordinate pair. Then, with a single line of code, use and apply family function to a dataframe with an x and y column,
#where each row is a coordinate pair

###Problem 2
#using the same data frame and move function as in Problem 1, write an apply family command that moves each coordinate pair the number of
#units of its row. In other words, the first coordinate pair should move one unit, the second coordinate pair should move 2, etc.

###Problem 3
#Write a line of code using an apply family function that takes the following lists ('list1', 'list2') and gives a vector with the length of
#each element in the list
list1 <- list(runif(sample(1:100, 1),-100, 100), c(1:100), rnorm(25, 0, 25), sample(runif(100, -20, 20), sample(1:100, 1))) 
list2 <- lapply(X = 1:50, FUN = runif, min = 0, max = 1)

