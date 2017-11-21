#Let's create a list with sets of values drawn from normal distributions with increasing means

exampleList <- vector("list", length = 10)   #make an empty list
for (i in 1:length(exampleList)) {          #for loop using index
  exampleList[[i]] <- rnorm(10, i, 0.5)     #generate normal distrubution of data centered around index of list
}

#And lets calculate some summary statistics from each set of values:
means <- c()                                 #empty vector to store means
std <- c()
for (vec in exampleList) {                  #for loop using values
  means <- c(means, mean(vec))               #get the mean of each item in list
  std <- c(std, sd(vec))
}
plot(means)
ggplot(data.frame(group = LETTERS[1:10], mean = means, sd = std)) + geom_point(aes(x = group, y = mean)) +geom_errorbar(aes(x = group, ymin = mean - sd, ymax = mean + sd)) #don't worry about this line of code, just check out the plot 

#Now lets write some functions to move an agent. The recursive function and the function using "while" do the same thing!
move <- function(x, y, averageDist, sd) {     #function to move randomly in x-yplane
  dist <- rnorm(1, averageDist, sd)
  angle <- runif(1, 0, 2*pi)
  x <- x + dist*cos(angle)
  y <- y + dist*sin(angle)
  return(c(x,y))
}

moveDistanceRecursive <- function(location, averageDist, sd, endDist) {
  #input: location is a dataframe w. x and y columns (numeric vectors). averageDist and sd are single numeric values.
  ##      endDist is a single numeric value giving the distance from the start location the agent must move
  #output: a dataframe w/ all locations reached
  newLocation <- move(location[nrow(location),1], location[nrow(location),2], averageDist, sd)
  distFromStart <- sqrt(sum((location[1,] - newLocation)^2)) #pythogorean theorum
  if (distFromStart >= endDist) return(rbind(location, newLocation))
  return(moveDistanceRecursive(rbind(location, newLocation),averageDist, sd, endDist))
}

moveDistanceWhile <- function(location, averageDist, sd, endDist) {
  #input: location is a dataframe w. x and y columns (numeric vectors). averageDist and sd are single numeric values.
  ##      endDist is a single numeric value giving the distance from the start location the agent must move
  #output: a dataframe w/ all locations reached
  while (distFromStart < endDist) {
    newLocation <- move(location[nrow(location),1], location[nrow(location),2], averageDist, sd)
    distFromStart <- sqrt(sum((location[1,] - newLocation)^2)) #pythogorean theorum
    location = rbind(location, newLocation)                    #This line still runs on last loop!
  }
  return(location)
}

times <- 40
simulations <- vector("list", times)
lengths <- vector(length = times)

#Two methods for running movement simulations multiple times
for (i in 1:times) {
  simulations[[i]] <- moveDistanceRecursive(data.frame(x=0, y =0), 1, 0.2, 3)
  lengths[i] <- nrow(simulations[[i]])
}
plot(table(lengths))
##OR##

i = 1
while(i <= times) {
  i = i+1
  simulations[[i]] <- moveDistanceRecursive(data.frame(x=0, y =0), 1, 0.2, 3)
  lengths[i] <- nrow(simulations[[i]])
}
plot(table(lengths))
