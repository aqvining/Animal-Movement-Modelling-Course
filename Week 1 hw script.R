rm(list <-  ls())
coordinates <-  data.frame(x = runif(4, 0, 10), y = runif(4, 0, 10))
v1 <-  c(1, 6, 3, 22)
v2 <-  c("A", "F", "B", "V")
v3 <-  c(1, 1, 0, 1)
alphaCube <-  array(LETTERS, c(3,3,3))

##Problem 4 test variables
P4test1 = "this is a string"
P4test2 = list('this', 'is', 'a', 'list', 'of', 'strings')
P4test3 = c(1,2,3,4,5,6,7,8)
P4test4 = c(1,2,3,'A',5,6,7,8)
P4test5 = list(1,2,3,'A',5,6,7,8)
P4test6 = list(words = P4test2, numbers = P4test3)
P4test7 = data.frame(numeric = P4test3, string = P4test4)
P4test8 = data.frame(numeric = P4test3, string = P4test4, stringsAsFactors = FALSE)
