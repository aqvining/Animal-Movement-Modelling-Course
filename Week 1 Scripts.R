###Welcome to the Animal Movement Modelling team!
###Please add your name and email below
###Don't forget to merge with the main branch on DropBox



#######################################################


x = 1:10
y = LETTERS[1:10]
LoL = list(x,y)
is.vector(LoL[1])
is.vector(LoL[[1]])
is.list(LoL[[1]])
LoL[1][1]
LoL[[1]][1]
LoL[1,2]
is.data.frame(df)
df = data.frame(x,y)
df[1]
df[[1]]
df[1,2]
alphaCube = array(LETTERS, (3,3,3)) #Find the error in this line
###Write a line here that returns the letter "Q" from alphaCube
random = runif(20, 5, 15)
summary(random)
summary(letters[5:15])

#########Homework###########
##Load the "Week 1 hw workspace"

###Problem 1:
##The variable 'coordinates' is a dataframe with a column of x coordinates and a corresponding column of y coordinates.
##Write a script that creates a new variable, coordList, which is a list of coordinate pairs.
##The line 'coordList[[2]]' should return a vector with names ('x', 'y') and values (8.788644, 4.571029).

###Problem 2:
##There are 3 vectors, 'v1', 'v2', and 'v3'. Write a script that for each index of these vectors returns
##a string with the number in v1, the letter in v2, and the word "TRUE" if the number matches the letters position in the
##alphabet or "FALSE" if it does not. You should use v3 to create this final logical.

###Problem 3:
##Using only the variable 'alphaCube' and the 'paste()' function, create a string containing the name of your favorite color