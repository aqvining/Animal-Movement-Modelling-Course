#####Set-up######
Test_1.1 = list(1, 2, -10, -1.1, TRUE, "A") #Expected output: 
#[[1]]
#[1] 1

#[[2]]
#[1] 2

#[[3]]
#[1] 0

#[[4]]
# [1] 0
# 
# [[5]]
# [1] TRUE
# 
# [[6]]
# [1] "A"

Test_1.2 = c(-10:10) #Expected output:  [1]  0  0  0  0  0  0  0  0  0  0  0  1  2  3  4  5  6  7  8  9 10
Test_1.3 = data.frame(-10:10, letters[1:21]) #Expected output: The original data frame with a message indicating invalid input
Test_1.4 = list(-10:10, letters[1:21]) #Expected output: This will probably return an error. Why? Bonus: Modify your function to handle this input

Test_2.1 = -150:150 #expected output: -75:75
Test_2.2 = -152:150 #expected output: 76:-75
Test_2.3 = -150:152 #expected output: -75:76
Test_2.4 = 1:25     #expected output: same as input, with a message indicating so
Test_2.5 = c(-220, -10, -4, 3) #expected output: c(100, 5, 2, -1.5)
Test_2.6 = c(-220, -200, -150) #expected output: c(220, 200, 150)
Test_2.7 = letters[1:5]   #expected output: same as input, with a message indicating invalid input


####Problem 1: Write a function, removeNegative, that will take a vector or list as input and 
####           replaces all negative values with 0's
####Note: When dealing with strings, you might find NAs are introduced by coercion. For now, it is okay
####      if your function produces this warning, so long as it is still able to produce the right answer
####Hint: the which() function may be helpful

####Problem 2: Write a function, reduce, that takes a numeric vector and does 3 things: If the range of values in the
####           vector is over 100, divide all values by 2. 2) If the previous reduction occured and any values are still greater
####           than 100 or less than -100, make them 100 or -100 respectively. 3) If there are more negative values than positive 
####           values, reverse the sign of all values (do this regardless of whether the previous two steps occurred). Finally,
####           return the new vector. If the vector is unchanged, print a message that says so.
####Hint:      Check out the range(), min(), and max() functions. Remember you can search for functions using ?
####Hint:      You can put if statments inside other if statements!
