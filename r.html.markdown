---
language: R
contributors:
    - ["e99n09", "http://github.com/e99n09"]
    - ["isomorphismes", "http://twitter.com/isomorphisms"]
filename: learnr.r
---

R is a statistical computing language. It has lots of libraries for uploading and cleaning data sets, running statistical procedures, and making graphs. You can also run `R`commands within a LaTeX document.

```python

# Comments start with hashtags.

# You can't make a multi-line comment per se,
# but you can stack multiple comments like so.

# in Windows, hit COMMAND-ENTER to execute a line


###################################################################
# Stuff you can do without understanding anything about programming
###################################################################

data()	# Browse pre-loaded data sets
data(rivers)	# Lengths of Major North American Rivers
ls()	# Notice that "rivers" appears in the workspace
head(rivers)	# peek at the dataset
# 735 320 325 392 524 450
length(rivers)	# how many rivers were measured?
# 141
summary(rivers)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  135.0   310.0   425.0   591.2   680.0  3710.0 
stem(rivers)	#stem-and-leaf plot (like a histogram)
#
#  The decimal point is 2 digit(s) to the right of the |
#
#   0 | 4
#   2 | 011223334555566667778888899900001111223333344455555666688888999
#   4 | 111222333445566779001233344567
#   6 | 000112233578012234468
#   8 | 045790018
#  10 | 04507
#  12 | 1471
#  14 | 56
#  16 | 7
#  18 | 9
#  20 | 
#  22 | 25
#  24 | 3
#  26 | 
#  28 | 
#  30 | 
#  32 | 
#  34 | 
#  36 | 1


stem(log(rivers))	#Notice that the data are neither normal nor log-normal! Take that, Bell Curve fundamentalists.

#  The decimal point is 1 digit(s) to the left of the |
#
#  48 | 1
#  50 | 
#  52 | 15578
#  54 | 44571222466689
#  56 | 023334677000124455789
#  58 | 00122366666999933445777
#  60 | 122445567800133459
#  62 | 112666799035
#  64 | 00011334581257889
#  66 | 003683579
#  68 | 0019156
#  70 | 079357
#  72 | 89
#  74 | 84
#  76 | 56
#  78 | 4
#  80 | 
#  82 | 2


hist(rivers, col="#333333", border="white", breaks=25)	#play around with these parameters
hist(log(rivers), col="#333333", border="white", breaks=25)	#you'll do more plotting later

#Here's another neat data set that comes pre-loaded. R has tons of these. data()
data(discoveries)
plot(discoveries, col="#333333", lwd=3, xlab="Year", main="Number of important discoveries per year")
plot(discoveries, col="#333333", lwd=3, type = "h", xlab="Year", main="Number of important discoveries per year")


#rather than leaving the default ordering (by year) we could also sort to see what's typical
sort(discoveries)
#  [1]  0  0  0  0  0  0  0  0  0  1  1  1  1  1  1  1  1  1  1  1  1  2  2  2  2
# [26]  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  3  3  3
# [51]  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  4  4  4  4  4  4  4  4
# [76]  4  4  4  4  5  5  5  5  5  5  5  6  6  6  6  6  6  7  7  7  7  8  9 10 12

stem(discoveries, scale=2)
# 
#  The decimal point is at the |
#
#   0 | 000000000
#   1 | 000000000000
#   2 | 00000000000000000000000000
#   3 | 00000000000000000000
#   4 | 000000000000
#   5 | 0000000
#   6 | 000000
#   7 | 0000
#   8 | 0
#   9 | 0
#  10 | 0
#  11 | 
#  12 | 0

max(discoveries)
# 12

summary(discoveries)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0.0     2.0     3.0     3.1     4.0    12.0 




#Basic statistical operations don't require any programming knowledge either

#roll a die a few times
round(runif(7, min=.5, max=6.5))
# 1 4 6 1 4 6 4

#your numbers will differ from mine unless we set the same random.seed(31337)


#draw from a standard Gaussian 9 times
rnorm(9)
# [1]  0.07528471  1.03499859  1.34809556 -0.82356087  0.61638975 -1.88757271
# [7] -0.59975593  0.57629164  1.08455362









#########################
# Basic programming stuff
#########################

# NUMBERS

# "numeric" means double-precision floating-point numbers
5	# 5
class(5)	# "numeric"
5e4	# 50000				#handy when dealing with large,small,or variable orders of magnitude
6.02e23	# Avogadro's number
1.6e-35	# Planck length

# long-storage integers are written with L
5L	# 5
class(5L)	# "integer"

# Try ?class for more information on the class() function
# In fact, you can look up the documentation on `xyz` with ?xyz
# or see the source for `xyz` by evaluating xyz

# Arithmetic
10 + 66	# 76
53.2 - 4	# 49.2
2 * 2.0	# 4
3L / 4	# 0.75
3 %% 2	# 1

# Weird number types
class(NaN)	# "numeric"
class(Inf)	# "numeric"
class(-Inf)	# "numeric"		#used in for example integrate( dnorm(x), 3, Inf ) -- which obviates Z-score tables

# but beware, NaN isn't the only weird type...
class(NA)	# see below
class(NULL)	# NULL


# SIMPLE LISTS
c(6, 8, 7, 5, 3, 0, 9)	# 6 8 7 5 3 0 9
c('alef', 'bet', 'gimmel', 'dalet', 'he')	# "alef"   "bet"    "gimmel" "dalet"  "he"
c('Z', 'o', 'r', 'o') == "Zoro"	# FALSE FALSE FALSE FALSE

#some more nice built-ins
5:15	# 5  6  7  8  9 10 11 12 13 14 15

seq(from=0, to=31337, by=1337)
#  [1]     0  1337  2674  4011  5348  6685  8022  9359 10696 12033 13370 14707
# [13] 16044 17381 18718 20055 21392 22729 24066 25403 26740 28077 29414 30751

letters
#  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
# [20] "t" "u" "v" "w" "x" "y" "z"

month.abb	# "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"


# Access the n'th element of a list with list.name[n] or sometimes list.name[[n]]
letters[18]	# "r"
LETTERS[13]	# "M"
month.name[9]	# "September"
c(6, 8, 7, 5, 3, 0, 9)[3]	# 7



# CHARACTERS

# There's no difference between strings and characters in R

"Horatio"	# "Horatio"
class("Horatio") # "character"
substr("Fortuna multis dat nimis, nulli satis.", 9, 15)	# "multis "
gsub('u', 'ø', "Fortuna multis dat nimis, nulli satis.")	# "Fortøna møltis dat nimis, nølli satis."



# LOGICALS

# booleans
class(TRUE)	# "logical"
class(FALSE)	# "logical"
# Behavior is normal
TRUE == TRUE	# TRUE
TRUE == FALSE	# FALSE
FALSE != FALSE	# FALSE
FALSE != TRUE	# TRUE
# Missing data (NA) is logical, too
class(NA)	# "logical"



# FACTORS

# The factor class is for categorical data
# which can be ordered (like childrens' grade levels)
# or unordered (like gender)
levels(factor(c("female", "male", "male", "female", "NA", "female")))	# "female" "male"   "NA" 

factor(c("female", "female", "male", "NA", "female"))
#  female female male   NA     female
# Levels: female male NA

data(infert)	#Infertility after Spontaneous and Induced Abortion
levels(infert$education)	# "0-5yrs"  "6-11yrs" "12+ yrs"



# VARIABLES

# Lots of way to assign stuff
x = 5 # this is possible
y <- "1" # this is preferred
TRUE -> z # this works but is weird

# We can use coerce variables to different classes
as.numeric(y)	# 1
as.character(x)	# "5"

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
jiggle <- function(x) {
	x+ rnorm(x, sd=.1)	#add in a bit of (controlled) noise
	return(x)
}

# Called like any other R function:
jiggle(5)	# 5±ε. After set.seed(2716057), jiggle(5)==5.005043

#########################
# Fun with data: vectors, matrices, data frames, and arrays
#########################

# ONE-DIMENSIONAL

# You can vectorize anything, so long as all components have the same type
vec <- c(8, 9, 10, 11)
vec	#  8  9 10 11
# The class of a vector is the class of its components
class(vec)	# "numeric"
# If you vectorize items of different classes, weird coercions happen
c(TRUE, 4)	# 1 4
c("dog", TRUE, 4)	# "dog"  "TRUE" "4"

# We ask for specific components like so (R starts counting from 1)
vec[1]	# 8
# We can also search for the indices of specific components,
which(vec %% 2 == 0)	# 1 3
# or grab just the first or last entry in the vector
head(vec, 1)	# 8
tail(vec, 1)	# 11
# If an index "goes over" you'll get NA:
vec[6]	# NA
# You can find the length of your vector with length()
length(vec)	# 4

# You can perform operations on entire vectors or subsets of vectors
vec * 4	# 16 20 24 28
vec[2:3] * 5	# 25 30
# and there are many built-in functions to summarize vectors
mean(vec)	# 9.5
var(vec)	# 1.666667
sd(vec)	# 1.290994
max(vec)	# 11
min(vec)	# 8
sum(vec)	# 38

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
mat[1,]	# 1 4
# Perform operation on the first column
3 * mat[,1]	# 3 6 9
# Ask for a specific cell
mat[3,2]	# 6
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
class(mat2)	# matrix
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
class(dat)	# "data.frame"
dat
# =>
#   number species
# 1      5     dog
# 2      2     cat
# 3      1    bird
# 4      4     dog
class(dat$number)	# "numeric"
class(dat[,2])	# "factor"
# The data.frame() function converts character vectors to factor vectors

# There are many twisty ways to subset data frames, all subtly unalike
dat$number	# 5 2 1 4
dat[,1]	# 5 2 1 4
dat[,"number"]	# 5 2 1 4

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
#      [,1] [,2]
# [1,]    2    8
# [2,]  300    9
# [3,]    4    0
#
# , , 2
#
#      [,1] [,2]
# [1,]    5   66
# [2,]   60    7
# [3,]    0  847

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
