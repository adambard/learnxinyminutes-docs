---
language: GolfScript
filename: golfscript.gs
contributors:
    - ["Nicholas S Georgescu", "http://github.com/ngeorgescu"]
---

GolfScript is an esoteric language that was developed in 2007 by Darren
Smith. It is a scripting language with an interpreter written in Ruby. It lets
you write very dense code in very few characters. The main goal of the language
is, as the name suggests, to solve problems in as few keystrokes as possible.
The examples page on the GolfScript website even has an entire Sudoku solver
written in just 77 characters.

If you get really good at GolfScript you can easily find yourself using it as a
go-to language for solving some (even somewhat hard) coding problems. It's never
going to be faster than Ruby, but it can be very fast to write, since a single
character of GolfScript can replace an entire line of code in some other languages.

GolfScript is based on the use of the stack. This tutorial therefore will
read as a sequence of stack operations on an actual stack, as opposed to some
standalone code and individual results. The stack starts as an empty list, and
everything either adds to the stack, or it pops some items off, transforms them,
and puts them back onto the stack.

To get started running GolfScript, you can get the golfscript.rb file from the
[GitHub repo](https://github.com/darrenks/golfscript).  Copy it into your `$PATH`,
(dropping the .rb and chmodding as necessary). You can run GolfScript from either
the interactive interpreter (which mirrors the tutorial below). Once you get the hang
of GolfScript, you can start running from "stdin". If you see a script starting with `~`,
it was probably designed to be dropped in a file and run with `golfscript file.gs`.  You
can pipe in or enter in your input at runtime.

```
> anything undefined technically evaluates to nothing and so is also a comment
# but commenting it out explicitly anyway is probably a good idea because if
# you use a reserved keyword or any punctuation you'll run into trouble.
[]
> ######################################################################
#       datatypes
########################################################################
> 1 # Here we add 1 to the stack.  Any object entry adds things to the stack
[1]
> 'abc' # here we are adding a string. The only difference between single and
# double quotes is that double lets you escape more things other than \' and \n
# it won't matter for the sake of this tutorial.
[1 "abc"]
> {+} # the third type of object you can put on the stack is a block
[1 "abc" {+}]
> ] # this takes everything prior and puts it into an array, the fourth type
# of object. (besides bug exploits like [2-1?] those are the only four types)
[[1 "abc" {+}]]
> ; # let's clear the stack by executing the discard function on this array.
# if you type the characters  ]; it always clears the stack.
[]
> 1"abc"{+}]; # newlines are whitespaces. Everything we did up to this point
# can be put into one line and it all works the exact same.
########################################################################
#       operators and math
########################################################################
[]
> 1 1 # we add two 1s to the stack.  We could also duplicate the first with .
[1 1]
> + # math is done by executing an operation on the top of the stack. This
# can be a standalone character. The way to read this is that we put a 1 on
# the stack, another one one the stack, and then executed a + operation which
# takes the top two elements off of the stack, sums them up, and returns them
# to the stack.  This is typically referred to as postfix notation.  It can be
# a bit jarring, but this is the way to think about things.  You're adding to
# the stack with objects and modifying the top of the stack with operators.
[2]
> 8 1- # minus works the same way. N.B. that we still have that 2 on the stack
# from earlier
[2 7]
> 10 2* # multiplication works the same way. The product is added to the stack
[2 7 20]
> 35 4/ # all division is integer division
[2 7 20 8]
> 35 4%  # modulo operation
[2 7 20 8 3]
> 2 3? # exponentiation
[2 7 20 8 3 8]
> 8~ # bitwise "not" function on signed integers
[2 7 20 8 3 8 -9]
> -1~ # this yields 0, which is useful to know for the ? operator
[2 7 20 8 3 8 -9 0]
> 5 3| # or: yields 7, since [1 0 1] | [0 1 1] => [1 1 1]
[2 7 20 8 3 8 -9 0 7]
> 5 3^ # xor: yields 6, since the parity differs at [1 1 0]
[2 7 20 8 3 8 -9 0 7 6]
> 5 3& # and: yields 1, since it's the only bit active in both: [0 0 1]
[2 7 20 8 3 8 -9 0 7 6 1]
> ]; ###################################################################
#       booleans
########################################################################
[]
> 5 3
[5 3]
> < #add two numbers to the stack, and then perform a lessthan operation
# booleans are False if 0, [], {}, '', and true if anything else.
[0]
> 5 3> # greater than operation.
[0 1]
> 5 3= #single equal is the operator. Again, before the equals is executed,
# the stack reads [0 1 5 3], and then the equals operator checks the top 2
# values and yields:
[0 1 0]
> ! #not, returns 1 if 0 else 0.
[0 1 1]
> ) #increments the last number
[0 1 2]
> ( #decrements the last number
[0 1 1]
> ]; ###################################################################
#       stack control
########################################################################
[]
> 1 # put a number on the stack
[1]
> . # duplicate the number
[1 1]
> ) # increment
[1 2]
> \ # flip the top two items
[2 1]
> 1$ # $ copies the nth-to-last item on the stack at the index preceding.
# Here we get the 1-indexed item.
[2 1 2]
> 0$ # to copy the 0-indexed item we use the appropriate index.
# This is identical to . operation
[2 1 2 2]
> ) # increment
[2 1 2 3]
> @ # pulls the third item up to the top
[2 2 3 1]
> [@] # use this trick to flip the top 3 items and put them into an array
# if you wrap any operation in brackets it flips the results into an array.
# even math operations like, [+] and [-]
[2 [3 1 2]]
> ]; # also, using at most two strokes you can orient the top three items
# in any permutation. Below are shown the results on 3,~
  #      => 0 1 2    (i.e. doing nothing)
  #   \  => 0 2 1
  #   @\ => 1 0 2
  #   @  => 1 2 0
  #   @@ => 2 0 1
  #   \@ => 2 1 0
[]
> ######################################################################
#       using arrays
########################################################################
[]
> 2, # comma is the range() function
[[0 1]]
> , # and also the length() function
[2]
> ;4, # let's get an array of four items together
[[0 1 2 3]]
> ) # we can pop off the last value
[[0 1 2] 3]
> + # and put it back
[[0 1 2 3]]
> ( # we can pop off the first value
[[1 2 3] 0]
> \+ # and put it back
[[0 1 2 3]]
> 2- # we can subtract a particular value
[[0 1 3]]
> [1 3] # or a list of values
[[0 1 3] [1 3]]
> -
[[0]]
> ! # boolean operations also work on lists, strings, and blocks. If it's
# empty it's a 1, otherwise 0. Here, the list has a zero, but it's not zero-
# length, so the array as a whole is still True... and hence "not" is False
[0]
> ;4,(+ # let's make a range, pop the first value, and tack it on the end
[[1 2 3 0]]
> $ # we can also restore order by sorting the array
[[0 1 2 3]]
> 1 >  # we can also use < > and = to get the indeces that match. Note this
# is not a filter! This is an index match. Filtering items greater than one
# is done with {1>},
[[1 2 3]]
> 2 < # remember it's zero-indexed, so everything in this array is at an index
# less than 2, the indeces are 0 and 1.
[[1 2]]
> 1= # < and > return an array, even if it's one item.  Equals always drops
# it out of the array
[2]
> ;6,2% # the modulo operator works on lists as the step.
[[0 2 4]]
> ;4,2,-:a 3,2+:b # booleans also work on lists. lets define two lists
[[2 3] [0 1 2 2]]
> | # "or" - returns set of items that appear in either list i.e. "union set"
[[2 3 0 1]]
> ;a b& # returns set of items that appear in 1 AND 2, e.g. "intersection set"
[[2]]
> ;a b^ # returns the symmetric difference set between two lists,
[[3 0 1]]
> ~ # tilde unpacks the items from a list
[3 0 1]
> ]; a
[2 3]
> 2? # finds the index of an item
[0]
> ;3a?
[1]
> 4a? # returns -1 if the item doesn't exist. Note: Order of element and array
# doesn't matter for searching. it can be [item list?] or [list item?].
[1 -1]
> ]; # clear
[]
> 3,[4]* # join or intersperse: puts items in between the items
[[0 4 1 4 2]]
> ; 3,4* # multiplication of lists
[[0 1 2 0 1 2 0 1 2 0 1 2]]
> ;[1 2 3 2 3 5][2 3]/ # "split at"
[[[1] [] [5]]]
> ;[1 2 3 2 3 5][2 3]% # modulo is "split at... and drop empty"
[[[1] [5]]]
> ];####################################################################
#       strings
########################################################################
# strings work just like arrays
[]
> "use arch, am vegan, drive a stick" ', '/ # split
[["use arch" "am vegan" "drive a stick"]]
> {'I '\+', BTW.'+}% # map
[["I use arch, BTW." "I am vegan, BTW." "I drive a stick, BTW."]]
> n* # join.  Note the variable n is defined as a newline char by default
["I use arch, BTW.\nI am vegan, BTW.\nI drive a stick, BTW."]
> n/ # to replace, use split, and join with the replacement string.
[n "Also, not sure if I mentioned this, but" n]{+}* # fold sum 3-item array
* # and use join to get the result
n+ print # and then pop/print the results prettily
I use arch, BTW.
Also, not sure if I mentioned this, but
I am vegan, BTW.
Also, not sure if I mentioned this, but
I drive a stick, BTW.
[]
> '22222'{+}* # note that if you fold-sum a string not in an array, you'll
# get the sum of the ascii values. '2' is 50, so five times that is:
[250]
> ]; # this actually is a clever trick to get ascii values into an array.
[]
> "aabc" [{""+~}*] # if you fold over addition and drop it into a string: 
[[97 97 98 99]]
> {[.]""^}%""+ # which can be returned to a string as such using a ""^ map.
# and an empty string join.
["aabc"]
> {32-}% # note that most mapping operations work on the ascii values as
# you would expect, for instance with the difference between A and a being
# 32, you can just subtract that from the ascii value to get:
["AABC"] 
> ]; ###################################################################
#       blocks
########################################################################
[]
> 3,~ # start with an unpacked array
[0 1 2]
> {+-} # brackets define a block which can comprise multiple functions
[0 1 2 {+-}]
> ~ # blocks are functions waiting for execution. tilde does a single
# execution of the block in this case, we added the top two values, 1 and 2,
# and subtracted from 0
[-3]
> ;10,~{+}5* # multiplication works on executing blocks multiple times
# in this case we added the last 6 values together by running "add" 5 times
[0 1 2 3 39]
> ];10,4> # we can achieve the same result by just grabbing the last 6 items
[[4 5 6 7 8 9]]
> {+}* # and using the "fold" function for addition.
[39]
> # "fold" sequentially applies the operation pairwise from the left
# and then dumps the results.  Watch what happens when we use the duplicate
# operator to fold. it's clear what happens when we duplicate and then negate
# the duplicated item:
> ;4,{.-1*}*
[0 1 -1 2 -2 3 -3]
> ]{3%}, # we can filter a list based on applying the block to each element
# in this case we get the numbers that do NOT give 0 mod 3
[[1 -1 2 -2]]
> ;10,{3%0}, # note that only the last element matters for retaining in the
# array.  Here we take 0..9, calculate x mod 3, and then return a 0. The
# intermediate generated values are dumped out sequentially.
[0 1 2 0 1 2 0 1 2 0 []]
> ]; # clear
[]
> 5,{5*}% # map performs each operation on the array and returns the result
# to an array
[[0 5 10 15 20]]
> {.}% # watch what happens when you map duplicate on each item
[[0 0 5 5 10 10 15 15 20 20]]
> ]; ###################################################################
#       Control Flow!
########################################################################
# This is the most important part of scripting. Most languages have
# two main types of loops, for loops and while loops. Even though golfscript
# has many possible loops, only a few are generally useful and terse. For loops
# are implemented using mapping, filtering, folding, and sorting over lists.
# For instance, we can take the factorial of 6 by:
6, # get 0..5
{)}% # increment the list, i.e. "i++ for i in list" to get 1..6
{*}* # fold by multiplication , 9 characters for the operator itself.
[720]
> 6),(;{*}* # but can we get shorter? We can save some space by incrementing
# the 6, dropping the zero, and folding. 8 characters.
> # we can also use fold to do the same thing with unfold
1 6        # accumlator and multiplicand, we'll call A and M
{}{        # while M
  .        # copy M, so now the stack is A M M
    @      # bring A to the top, so now M M A
     *     # apply M to the accumulator, so M A
       \(  # flip the order, so it's A M, and M--
}/;        # "end", drop the list of multiplicands
# this is effectively a while-loop factorial
[720 720]
> 1.{6>!}{.@*\)}/; # we can also do the same thing with M++ while M not > 6
> 1 6{.@*\(.}do; # works the same way as the decrementing fold.
[720 720 720]
> ]; #obviously a for loop is ideal for factorials, since it naturally lends
# itself to running over a finite set of items.
########################################################################
#       Writing code
########################################################################
# Let's go through the process for writing a script. There are some tricks and
# ways to think about things. Let's take a simple example: a prime sieve.
# There are a few strategies for sieving. First, there's a strategy that
# uses two lists, candidates and primes. We pop a value from candidates,
# remove all the candidates divisible by it, and then add it to the primes.
# Second, there's just a filtering operation on numbers. I think it's
# probably shorter to write a program that just checks if a number has no
# numbers mod zero besides 0, 1, and itself. Slower, but shorter is king.
# Let's try this second strategy first.
[]
> 10 # we're probably going to filter a list using this strategy. It's easiest
# to start working with one element of the list. So let's take some example
# where we know the answer that we want to get.
[10]
> .,2> # let's duplicate it and take a list of values, and drop the first two
[10 [2 3 4 5 6 7 8 9]]
> {1$\%!}, # duplicate the ten, and scoot it behind the element, and then run
# 10 element %, and then ! the answer, so we are left with even multiples
[10 [2 5]]
> \; # we want to get rid of the intermediate so it doesn't show up in our
# solution.
[[2 5]]
> 10.,2,-{1$\%!},\; # Okay, let's put our little function together on one line
[[2 5] [2 5]]
> ;; # now we just filter the list using this strategy. We need to negate the
# result with ! so when we get a number with a factor, ! evaluates to 0, and
# the number is filtered out.
[]
> 10,{.,2,-{1$\%!},\;!}, # let's try filtering on the first 10 numbers
[[0 1 2 3 5 7]]
> 2> # now we can just drop 0 and 1.
[[2 3 5 7]]
> 4.?,{.,2,-{1$\%!},\;!},2> # trick: an easy way to generate large numbers in
# a few bytes is duplicate and exponentiate. 4.? is 256, and 9.? is 387420489
[[2 3 5 7] [2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89
97 101 103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193
197 199 211 223 227 229 233 239 241 251]]
> ];'4.?,{.,2,-{1$\%!},\;!},2>', # how long is our code for p<256 ?
[25]
> ; # this is 25 characters. Can we do better?!
[]
> []99,2> # let's go with the first strategy.  We'll start with an empty list
# of primes and a list of candidates
[[] [2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54
55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98]]
> (\ # pop left and leave left, we're going to copy this value with the filter
[[] 2 [3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54
55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80
81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98]]
> {1$%}, # filter out anything that is 0 mod by the popped item one back on the
# stack
[[] 2 [3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51
53 55 57 59 61 63 65 67 69 71 73 75 77 79 81 83 85 87 89 91 93 95 97]]
> @@+ # great, all the 2-divisible values are off the list! now we need to add
# it to the running list of primes
[[3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53 55
57 59 61 63 65 67 69 71 73 75 77 79 81 83 85 87 89 91 93 95 97] [2]]
> \ # swap back. Now it seems pretty clear when our candidates list is empty
# we're done. So let's try it with a do loop. Remember we need to duplicate
# the final value for the pop check. So we add a dot
[[2] [3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53
55 57 59 61 63 65 67 69 71 73 75 77 79 81 83 85 87 89 91 93 95 97]]
> {(\{1$%},@@+\.}do;
[[2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97]]
> ; # ok that worked. So let's start with our initialization as well.
[]4.?,2>{(\{1$%},@@+\.}do; # and let's check our work
[[2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101
103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199
211 223 227 229 233 239 241 251]]
> ,'[]99,2>{(\{1$%},@@+\.}do;', # how long is this?
[26]
> ]; # wow this solution is only 26 long, and much more effective. I don't see
# a way to get any smaller here. I wonder if with unfold we can do better?  The
# strategy here is to use unfold and then at the end grab the first value from
# each table.
[]
> 99,2> # start with the candidates list
[[2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55
56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81
82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98]]
> (\{1$%}, # pop left and filter
[2 [3 5 7 9 11 13 15 17 19 21 23 25 27 29 31 33 35 37 39 41 43 45 47 49 51 53
55 57 59 61 63 65 67 69 71 73 75 77 79 81 83 85 87 89 91 93 95 97]]
> (\{1$%}, # again
[2 3 [5 7 11 13 17 19 23 25 29 31 35 37 41 43 47 49 53 55 59 61 65 67 71 73 77
79 83 85 89 91 95 97]]
89 91 95 97]]
> {}{(\{1$%},}/ # ok I think it'll work. let's try to put it into an unfold.
[2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 [[5 7
11 13 17 19 23 25 29 31 35 37 41 43 47 49 53 55 59 61 65 67 71 73 77 79 83 85
89 91 95 97] [7 11 13 17 19 23 29 31 37 41 43 47 49 53 59 61 67 71 73 77 79 83
89 91 97] [11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97] [13
17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97] [17 19 23 29 31 37 41
43 47 53 59 61 67 71 73 79 83 89 97] [19 23 29 31 37 41 43 47 53 59 61 67 71 73
79 83 89 97] [23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97] [29 31 37 41
43 47 53 59 61 67 71 73 79 83 89 97] [31 37 41 43 47 53 59 61 67 71 73 79 83 89
97] [37 41 43 47 53 59 61 67 71 73 79 83 89 97] [41 43 47 53 59 61 67 71 73 79
83 89 97] [43 47 53 59 61 67 71 73 79 83 89 97] [47 53 59 61 67 71 73 79 83 89
97] [53 59 61 67 71 73 79 83 89 97] [59 61 67 71 73 79 83 89 97] [61 67 71 73
79 83 89 97] [67 71 73 79 83 89 97] [71 73 79 83 89 97] [73 79 83 89 97] [79 83
89 97] [83 89 97] [89 97] [97]]]
> ;] # drop that list of candidates generated at each step and put the items
# left behind by the unfold at each step (which is the primes) into a list
[[2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97]]
> ]; # clear and let's try with larger numbers
[]
> 4.?,2>{}{(\{1$%},}/;]
[[2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101
103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199
211 223 227 229 233 239 241 251]]
>;'4.?,2>{}{(\{1$%},}/;]', # find the length of our solution.
[21]
> ]; # only 21 characters for the primes! Let's see if we actually can use this
# strategy of leaving items behind, now using the do loop to get even shorter!
> 3.?,2> # candidates
[[2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26]]
> (\{1$%}, # pop and filter
[2 [3 5 7 9 11 13 15 17 19 21 23 25]]
> (\{1$%}, # again!
[2 3 [5 7 11 13 17 19 23 25]]
> {(\{1$%},.}do;] # try in a do loop and drop the empty list of candidates at
# the end of the do loop.  Don't forget the dot before the closing brace!
[[2 3 5 7 11 13 17 19 23]]
> ;4.?,2>{(\{1$%},.}do;] # check our work
[[2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101
103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199
211 223 227 229 233 239 241 251]]
> ;'4.?,2>{(\{1$%},.}do;]',
[21]
>]; # Still 21 characters. there's one other thing to try, which is the prime
# test known as Wilson's theorem. We can try filtering the items down using
# this test.
[]
> '4.?,2>{.,(;{*}*.*\%},'.~\, # let's run it and take the length
[[2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101
103 107 109 113 127 131 137 139 149 151 157 163 167 173 179 181 191 193 197 199
211 223 227 229 233 239 241 251] 21]
> ; # Still 21 characters! I think this number is quite good and it's not
# obvious how to beat it. The problem with GolfScript is there's always someone
# out there who thinks of some trick you didn't. For instance, you might think
# you're doing well with a Collatz seq generator of {(}{.2%{3*)}{2/}if}/ until
# you find that someone figured out {(}{3*).2%6\?/}/ which is so much shorter
# and cleaner - the unfold operation is nearly half the length!
########################################################################
#       How to read GolfScript
########################################################################
# let's take the gcd from the GolfScript banner. It starts with:
[]
> '2706 410'~ # so that's pretty straightforward, that it just evals the list
# and dumps the results on the stack. It's common to read from stdin which
# necessitates unpacking with ~
[2706 410]
> . # we want to know what that do loop does. the best way to do that is to
# drop the braces and run the loop one command at a time. We duplicate
[2706 410 410]
> @\ # We rearrange
[410 2706 410]
> % # we take the modulo
[410 246]
> .@\% # repeat. Note we don't need to run the final dot before the closing
# brace since this is just a value that is popped to check the loop condition
# you can also replicate the loop end with a semicolon to pop it yourself.
[246 164]
> .@\% # again!
[164 82]
> .@\% # and finally we hit zero. The loop would exit and ; would pop the zero,
# leaving you with the gcd of 82.
[82 0]
> ;; 2706 410{1$1$%.}do # Clearly this involves knowing about Euclid's method.
# you can also try a more obvious method like this one here which shows the
# numbers in sequence.
[2706 410 246 164 82 0]
>]; # so sometimes it pays dividends to know the math and you can write short
# algorithms that rely on easy tricks that aren't immediately obvious.
[]
> # let's try looking at the sudoku solver that is on the examples page. I'll
# skip the unpack step.
[2 8 4 3 7 5 1 6 9 0 0 9 2 0 0 0 0 7 0 0 1 0 0 4 0 0 2 0 5 0 0 0 0 8 0 0 0 0 8
0 0 0 9 0 0 0 0 6 0 0 0 0 4 0 9 0 0 1 0 0 5 0 0 8 0 0 0 0 7 6 0 4 4 2 5 6 8 9 7
3 1]:a 0?:b # again the grid is put into an array. Now, the next step
# is to define the "@" symbol as the working grid. This is because "@9" is
# interpreted as two symbols, whereas if you used something like "a" as the
# variable "a9" is interpreted as a single symbol, and this is not defined,
# so it will not get run at execution time. You would need a space which is an
# additional char. On the other hand, redefining built-ins is confusing so I
# will use "a" and "b" for the "@" and "^" definitions respectively. So the
# grid is "a" and the zero-index location of the first zero is "b", at index 9.
[9]
> ~! # this makes sure that the value is not -1 for find, i.e. -1~ evaluates to
# 0 so a ! makes it nonzero.  ?~! is a great trick for "isn't in the list"
[0]
> {@p}* # this prints out the grid the number of times as the previous value,
# which is how this thing "finishes". So if 0 isn't in the grid, it prints.
> 10, # let's get the digits 0-9. Zero will be eliminated because our original
# value is zero so when we look in any row or column, zero is guaranteed to be
# there.
[[0 1 2 3 4 5 6 7 8 9]]
> a 9/  # split the original grid row-wise
b 9/    # get the row of our checked value, in this case the second row
=       # and we get that row and
-       # take those numbers off the candidates
[[1 3 4 5 6 8]]
> a     # put the grid on the stack
b 9%    # get the column of the zero
>       # drop the first x values of the grid
9%      # take every ninth digit. We now have the column the zero is in
> -     # pull those items off the candidates list
[[1 3 5 6]]
> a 3/  # split the grid into three-long arrays
b 9%    # get the column of the zero
3/      # is the column in the left (0), middle (1), or right (2) triad?
 >      # pull that many three-groups off
3%      # get every third. Now we have 9 groups - the left side of the grid
3/      # divide those 9 groups it into thirds
b 27/   # was the zero on top (0), middle (1), or bottom (2) third of the grid?
=       # since it's the top, grab the top group of triads. You now have the
        # 1/9th of The sudoku grid where the zero sits
[[1 3 5 6] [[2 8 4] [0 0 9] [0 0 1]]]
> {+}*- # flatten those lists and remove those items from the candidates
# We now have the possible values for the position in question that work given
# the current state of the grid! if this list is empty then we've hit a
# contradiction given our previous values.
[[3 5 6]]
> 0= # {a b<\+a 1 b+>+}/ # now we've hit this unfold operation. If you run it
# you'll find we get the grids back. How does that work?! Let's take the first
# value in the "each" []{}/ operation. This is the best way to figure out what
# is happening in a mapping situation.
[3]
> a     # get the grid
b<      # get the grid up to the zero
\+      # and tack on our value of 3.
[[2 8 4 3 7 5 1 6 9 3]]
> a 1b+>+ # and we add on the rest of the grid. Note: we could do 1 char better
# because 1b+ is equivalent to but, longer than, just b)
[[2 8 4 3 7 5 1 6 9 3 0 9 2 0 0 0 0 7 0 0 1 0 0 4 0 0 2 0 5 0 0 0 0 8 0 0 0 0 8
0 0 0 9 0 0 0 0 6 0 0 0 0 4 0 9 0 0 1 0 0 5 0 0 8 0 0 0 0 7 6 0 4 4 2 5 6 8 9 7
3 1]]
> 1;; # and the do block runs again no matter what. So it's now clear why this
# thing exists with an error: if you solve the last digit, then this loop just
# keeps on rolling! You could add some bytes for some control flow but if it
# works it works and short is king.
[]

# Closing Tips for getting to the next level:
# 0. using lookback might be more effective than swapping around the values.
#    for instance, 1$1$ and \.@.@.\ do the same thing: duplicate last two items
#    but the former is more obvious and shorter.
# 1. golfscript can be fun to use for messing around with integer sequences or
#    do other cool math. So, don't be afraid to define your own functions to
#    make your life easier, like
> {$0=}:min; {$-1=}:max; {.,(;{*}*.*\%}:isprime; {.|}:set; # etc.
# 2. write pseudocode in another language or port a script over to figure out
#    what's going on. Especially useful when you combine this strategy with
#    algebra engines. For instance, you can port the examples-page 1000 digits
#    of pi to python and get:
#        import sympy as sp
#        a, k = sp.var('a'), list(range(20))[1::2]
#        for _ in range(len(k)-1):
#            m = k.pop()
#            l = k.pop()
#            k.append(((l+1)//2*m)//(l+2)+2*a)
#        print(str(k[0]))
#    which gives "2*a + floor(2*a/3 + floor(4*a/5 + 2*floor(6*a/7 + 3*floor(
#    8*a/9 + 4*floor(10*a/11 + 5*floor(12*a/13 + 6*floor(14*a/15 + 7*floor(16*
#    a/17 + 72/17)/15)/13)/11)/9)/7)/5)/3)"... which makes it much more obvious
#    what's going on than 10.3??:a;20,-2%{2+.2/@*\/a 2*+}* especially when
#    you're new to the language
# 3. a little math goes a long way. The above prime test uses Wilson's theorem
#    a comparable program testing for factors {:i,(;{i\%!},(;!}:isprime is
#    longer and slower. Also, as discussed above, Collatz is much shorter if
#    you recognize that you can do (3x+1) and then divide by 6 to the power
#    ((3x+1) mod 2).  (If x was even, (3x+1) is now odd, so 3x+1 div 6 is x/2.)
#    avoiding conditionals and redundancy can sometimes require such insight.
#    And of course, unless you know this continued fraction of pi it's hard to
#    calculate it in a terse block of code.
# 4. don't be afraid to define variables and use arrays! particularly if you
#    have 4 or more items to shuffle.
# 5. don't be afraid to use [some_long_script] to pack a bunch of items in an
#    array after the fact, rather than gathering or adding them later or
#    forcing yourself to use a datastructure that keeps the items in an array
# 6. sometimes you might get in a jam with - followed by an int that can be
#    solved with ^ to do a symmetric set difference without adding a space
# 7. "#{require 'net/http';Net::HTTP.get_response(URI.parse(address)).body}"
#    can get any page source from the internet, substituting 'address' for your
#    URL. Try it with an OEIS b-file or wordlists, etc. You can also use the
#    shorter "#{File.open('filename.txt').read}" to read in a file. GolfScript
#    can run "#{any_ruby_code_here}" and add the results to the stack.
# 8. you can set anything to mean anything, which can be useful for golf:
#       3:^;^2?  => 9 because this set ^ to 3, and 3 2 ? => 9
#       3:a;a2?  => Warning: pop on empty stack - because a2 doesn't exist
#       3:a;a 2? => 9 - it works again, but takes an extra character over ^2
#    usually you will only want to do this once you're trying to squeeze the
#    last few chars out of your code because it ruins your environment.
```

* [Run GolfScript online](https://tio.run/#golfscript)
* [GolfScript's documentation](http://www.golfscript.com/golfscript/builtin.html)
* [Useful StackExchange thread](https://codegolf.stackexchange.com/questions/5264/tips-for-golfing-in-golfscript)
* [GolfScript on GitHub](https://github.com/darrenks/golfscript)
