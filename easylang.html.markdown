---
language: Easylang
filename: easylang.txt
contributors:
    - ["chkas", "https://github.com/chkas"]
---

*Easylang* is a simple programming language with built-in graphical functions and an easy-to-use and offline usable browser IDE. Its simple syntax and semantics make it well suited as a teaching and learning programming language. You can also use it to write graphical applications that you can embed in a web page. 

*Easylang* is statically typed and has as data types only strings and numbers (floating point), resizeable arrays of strings and numbers and arrays of arrays.

[The browser IDE](https://easylang.dev/ide/) includes various tutorials, including one for beginners.

```
# number variable (64 bit floating point)
f = 3.14
print f
# 
# string variable
# 
str$ = "monkey"
# 
# strings can grow
# 
str$ &= " circus"
print str$
# 
# blocks end with 'end' or a dot, a newline has no
# other meaning than a space
# 
for i = 1 to 5
   sum += i * i
.
print sum
# 
# functions have value and reference
# parameters, no return values
# 
func gcd a b . res .
   # a and b are value parameters
   # res is a reference parameter
   while b <> 0
      # h is a local variable, because 
      # it is first used in the function
      h = b
      b = a mod b
      a = h
   .
   res = a
.
call gcd 120 35 r
print "gcd 120 35 -> " & r
# 
# strings can be concatenated and numbers are
# automatically converted to strings
# 
# 
# array of numbers
a[] = [ 2.1 3.14 7 ]
# 
# arrays are 1-based
print a[3]
# 
# arrays can grow
a[] &= 4
print a[]
# 
# arrays, strings and numbers are copied by value
# 
b[] = a[]
a[] &= 4
print a[] ; print b[]
# 
# array swapping ist fast
# 
swap a[] b[]
print a[] ; print b[]
print ""
# 
# array of strings
# 
fruits$[] = [ "apple" "banana" "orange" ]
# 
# for-in iterates over the elements of an array
# 
for fruit$ in fruits$[]
   print fruit$
.
# 
# strings are also used for single characters
# 
letters$[] = strchars "ping"
print letters$[]
letters$[2] = "o"
print strjoin letters$[]
print ""
# 
# 2-dimensional arrays are arrays of arrays
# this defines 3 arrays with length 4
# 
len a[][] 3
for i = 1 to len a[][]
   len a[i][] 4
.
a[1][2] = 99
print a[][]
print ""
# 
# builtin functions
if sin 90 = 1
   print "angles are in degree"
.
print pow 2 32
# seconds since 1970
print floor systime
# 
# random numbers
# 
# [0 1[
print randomf
# 
# [1, 2 .. 6] 
print random 6
print ""
# 
# hour and minutes
print "Time: " & substr timestr systime 12 5
# 
print strcode "A"
print strchar 65
print ""
# 
# set number format
numfmt 4 0
print sqrt 2
print pi
print logn 10
print ""
# 
a$[] = strsplit "10,15,22" ","
print a$[]
print 2 * number a$[1]
print len a$[]
print len "Hello"
print ""
# 
# With 'break n' you can leave nested loops and a function
# 
names$[] = [ ]
func name2id name$ . id .
   for id to len names$[]
      if names$[id] = name$
         # leave loop and function
         break 2
      .
   .
   names$[] &= name$
.
call name2id "alice" id ; print id
call name2id "bob" id ; print id
call name2id "alice" id ; print id
print ""
# 
#
# with 'repeat' you can make loops, which you can leave
# in the loop body using 'until'
# 
sum = 0
repeat
   s$ = input
   until s$ = ""
   sum += number s$
.
print "sum: " & sum
# 
# 'input' reads a string from the 'input_data' section,
# if it exists, otherwise via a prompt. The 'input_data'
# is at the end of the code
#
#
# Graphic commands are built in
# 
ang = 45
on animate
   clear
   move 50 50
   circle 1
   x = 50 + 40 * sin ang
   y = 50 + 40 * cos ang
   line x y
   circle 6
   v += sin ang / 5
   ang += v
.
# 
#
input_data
10
-21
67
```

* [More about Easylang](https://easylang.dev/)

* [Source code](https://github.com/chkas/easylang)
