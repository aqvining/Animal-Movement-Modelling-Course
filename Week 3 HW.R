####Problem 1: Sort
####Write a function, "sortVector", that will take any vector of numbers and sort them in ascending order.
####This function should also have an argument "Ascending" which defaults to TRUE, but if FALSE will sort
####The vector in descending order (largest to smallest)


####Problem 2: Random Movement
####Write a recursive function that takes as input an initial vector, list, or data-frame containing two values ("x" and "y"),
####a number of steps to move, and the mean and sd of this movement. The function should change the x and y coordinates
####"steps" times. The angle of movement should be drawn uniformly form 360 degrees. The distance of the movement
####should be drawn from a normal distribution with mean and sd as input by user. Your function should return a
####dataframe will locations for each step. The functions runif() (draws from uniform distribution), 
####sin(), and cos() will be useful. I've started your function set-up below.
randomMove <- function(start = data.frame(x = 0, y = 0), steps = 10) {
  
}