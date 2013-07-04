---
language: R
contributors:
    - ["e99n09", "http://github.com/e99n09"]
filename: learnr.r
---

R is a statistical computing language. It has lots of good built-in functions for uploading and cleaning data sets, running common statistical tests, and making graphs. You can also easily compile it within a LaTeX document.

```python

# Comments start with hashtags.

# You can't make a multi-line comment per se,
# but you can stack multiple comments like so.

# Hit COMMAND-ENTER to execute a line

#########################
# The absolute basics
#########################

# NUMBERS

# We've got doubles! Behold the "numeric" class
5 # => [1] 5
class(5) # => [1] "numeric"
# We've also got integers! They look suspiciously similar,
# but indeed are different
5L # => [1] 5
class(5L) # => [1] "integer"
# Try ?class for more information on the class() function
# In fact, you can look up the documentation on just about anything with ?

# All the normal operations!
10 + 66 # => [1] 76
53.2 - 4 # => [1] 49.2
2 * 2.0 # => [1] 4
3L / 4 # => [1] 0.75
3 %% 2 # => [1] 1

# Finally, we've got not-a-numbers! They're numerics too
class(NaN) # => [1] "numeric"

# CHARACTERS

# We've (sort of) got strings! Behold the "character" class
"plugh" # => [1] "plugh"
class("plugh") # "character"
# There's no difference between strings and characters in R

# LOGICALS

# We've got booleans! Behold the "logical" class
class(TRUE) # => [1] "logical"
class(FALSE) # => [1] "logical"
# Behavior is normal
TRUE == TRUE # => [1] TRUE
TRUE == FALSE # => [1] FALSE
FALSE != FALSE # => [1] FALSE
FALSE != TRUE # => [1] TRUE
# Missing data (NA) is logical, too
class(NA) # => [1] "logical"

# FACTORS

# The factor class is for categorical data
# It has an attribute called levels that describes all the possible categories
factor("dog")
# =>
# [1] dog
# Levels: dog
# (This will make more sense once we start talking about vectors)

# VARIABLES

# Lots of way to assign stuff
x = 5 # this is possible
y <- "1" # this is preferred
TRUE -> z # this works but is weird

# We can use coerce variables to different classes
as.numeric(y) # => [1] 1
as.character(x) # => [1] "5"

# LOOPS

# We've got for loops
for (i in 1:4) {
  print(i)
}

# We've got while loops
a <- 10
while (a > 4) {
	cat(a, "...", sep = "")
	a <- a - 1
}

# Keep in mind that for and while loops run slowly in R
# Operations on entire vectors (i.e. a whole row, a whole column)
# or apply()-type functions (we'll discuss later) are preferred

# IF/ELSE

# Again, pretty standard
if (4 > 3) {
	print("Huzzah! It worked!")
} else {
	print("Noooo! This is blatantly illogical!")
}
# =>
# [1] "Huzzah! It worked!"

# FUNCTIONS

# Defined like so:
myFunc <- function(x) {
	x <- x * 4
	x <- x - 1
	return(x)
}

# Called like any other R function:
myFunc(5) # => [1] 19

#########################
# Fun with data: vectors, matrices, data frames, and arrays
#########################

# ONE-DIMENSIONAL

# You can vectorize anything, so long as all components have the same type
vec <- c(8, 9, 10, 11)
vec # => [1]  8  9 10 11
# The class of a vector is the class of its components
class(vec) # => [1] "numeric"
# If you vectorize items of different classes, weird coercions happen
c(TRUE, 4) # => [1] 1 4
c("dog", TRUE, 4) # => [1] "dog"  "TRUE" "4"

# We ask for specific components like so (R starts counting from 1)
vec[1] # => [1] 8
# We can also search for the indices of specific components,
which(vec %% 2 == 0) # => [1] 1 3
# or grab just the first or last entry in the vector
head(vec, 1) # => [1] 8
tail(vec, 1) # => [1] 11
# If an index "goes over" you'll get NA:
vec[6] # => [1] NA
# You can find the length of your vector with length()
length(vec) # => [1] 4

# You can perform operations on entire vectors or subsets of vectors
vec * 4 # => [1] 16 20 24 28
vec[2:3] * 5 # => [1] 25 30
# and there are many built-in functions to summarize vectors
mean(vec) # => [1] 9.5
var(vec) # => [1] 1.666667
sd(vec) # => [1] 1.290994
max(vec) # => [1] 11
min(vec) # => [1] 8
sum(vec) # => [1] 38

# TWO-DIMENSIONAL (ALL ONE CLASS)

# You can make a matrix out of entries all of the same type like so:
mat <- matrix(nrow = 3, ncol = 2, c(1,2,3,4,5,6))
mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# Unlike a vector, the class of a matrix is "matrix", no matter what's in it
class(mat) # => "matrix"
# Ask for the first row
mat[1,] # => [1] 1 4
# Perform operation on the first column
3 * mat[,1] # => [1] 3 6 9
# Ask for a specific cell
mat[3,2] # => [1] 6
# Transpose the whole matrix
t(mat)
# =>
#      [,1] [,2] [,3]
# [1,]    1    2    3
# [2,]    4    5    6

# cbind() sticks vectors together column-wise to make a matrix
mat2 <- cbind(1:4, c("dog", "cat", "bird", "dog"))
mat2
# =>
#      [,1] [,2]   
# [1,] "1"  "dog"  
# [2,] "2"  "cat"  
# [3,] "3"  "bird" 
# [4,] "4"  "dog"
class(mat2) # => [1] matrix
# Again, note what happened!
# Because matrices must contain entries all of the same class,
# everything got converted to the character class
c(class(mat2[,1]), class(mat2[,2]))

# rbind() sticks vectors together row-wise to make a matrix
mat3 <- rbind(c(1,2,4,5), c(6,7,0,4))
mat3
# =>
#      [,1] [,2] [,3] [,4]
# [1,]    1    2    4    5
# [2,]    6    7    0    4
# Aah, everything of the same class. No coercions. Much better.

# TWO-DIMENSIONAL (DIFFERENT CLASSES)

# For columns of different classes, use the data frame
dat <- data.frame(c(5,2,1,4), c("dog", "cat", "bird", "dog"))
names(dat) <- c("number", "species") # name the columns
class(dat) # => [1] "data.frame"
dat
# =>
#   number species
# 1      5     dog
# 2      2     cat
# 3      1    bird
# 4      4     dog
class(dat$number) # => [1] "numeric"
class(dat[,2]) # => [1] "factor"
# The data.frame() function converts character vectors to factor vectors

# There are many twisty ways to subset data frames, all subtly unalike
dat$number # => [1] 5 2 1 4
dat[,1] # => [1] 5 2 1 4
dat[,"number"] # => [1] 5 2 1 4

# MULTI-DIMENSIONAL (ALL OF ONE CLASS)

# Arrays creates n-dimensional tables
# You can make a two-dimensional table (sort of like a matrix)
array(c(c(1,2,4,5),c(8,9,3,6)), dim=c(2,4))
# =>
#      [,1] [,2] [,3] [,4]
# [1,]    1    4    8    3
# [2,]    2    5    9    6
# You can use array to make three-dimensional matrices too
array(c(c(c(2,300,4),c(8,9,0)),c(c(5,60,0),c(66,7,847))), dim=c(3,2,2))
# =>
# , , 1
#
#     [,1] [,2]
# [1,]    1    4
# [2,]    2    5
#
# , , 2
#
#      [,1] [,2]
# [1,]    8    1
# [2,]    9    2

# LISTS (MULTI-DIMENSIONAL, POSSIBLY RAGGED, OF DIFFERENT TYPES)

# Finally, R has lists (of vectors)
list1 <- list(time = 1:40)
list1$price = c(rnorm(40,.5*list1$time,4)) # random
list1

# You can get items in the list like so
list1$time
# You can subset list items like vectors
list1$price[4]

#########################
# The apply() family of functions
#########################

# Remember mat?
mat
# =>
#      [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# Use apply(X, MARGIN, FUN) to apply function FUN to a matrix X
# over rows (MAR = 1) or columns (MAR = 2)
# That is, R does FUN to each row (or column) of X, much faster than a
# for or while loop would do
apply(mat, MAR = 2, myFunc)
# =>
#      [,1] [,2]
# [1,]    3   15
# [2,]    7   19
# [3,]   11   23
# Other functions: ?lapply, ?sapply

# Don't feel too intimidated; everyone agrees they are rather confusing

# The plyr package aims to replace (and improve upon!) the *apply() family.

install.packages("plyr")
require(plyr)
?plyr

#########################
# Loading data
#########################

# "pets.csv" is a file on the internet
pets <- read.csv("http://learnxinyminutes.com/docs/pets.csv")
pets
head(pets, 2) # first two rows
tail(pets, 1) # last row

# To save a data frame or matrix as a .csv file
write.csv(pets, "pets2.csv") # to make a new .csv file
# set working directory with setwd(), look it up with getwd()

# Try ?read.csv and ?write.csv for more information

#########################
# Plots
#########################

# Scatterplots!
plot(list1$time, list1$price, main = "fake data")
# Regressions!
linearModel <- lm(price  ~ time, data = list1)
linearModel # outputs result of regression
# Plot regression line on existing plot
abline(linearModel, col = "red")
# Get a variety of nice diagnostics
plot(linearModel)

# Histograms!
hist(rpois(n = 10000, lambda = 5), col = "thistle")

# Barplots!
barplot(c(1,4,5,1,2), names.arg = c("red","blue","purple","green","yellow"))

# Try the ggplot2 package for more and better graphics

install.packages("ggplot2")
require(ggplot2)
?ggplot2

```

## How do I get R?

* Get R and the R GUI from [http://www.r-project.org/](http://www.r-project.org/)
* [RStudio](http://www.rstudio.com/ide/) is another GUI
