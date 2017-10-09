###Name: Alexander Vining
###Animal Movement Modelling: Week 1 HW
###10-9-17

###Problem 1
coordList = list(coordinates[1,], coordinates[2,], coordinates[3,], coordinates[4,]) #Makes a list where each element is a row vector from coordinates
coordList[[2]] #check

###Problem 2
v3 = c(1,1,0,1) #fix error in hw workspace
paste(v1[1], v2[1], as.logical(v3[1]))  #concatenate strings from first elements of v1, v2, and v3
paste(v1[2], v2[2], as.logical(v3[2]))
paste(v1[3], v2[3], as.logical(v3[3]))
paste(v1[4], v2[4], as.logical(v3[1]))

#Problem 3
alphaCube #visualize cube
paste(alphaCube[1,3,2], alphaCube[3,1,3], alphaCube[3,3,2], alphaCube[1,3,2], alphaCube[3,1,2], alphaCube[2,2,1], sep = "")
