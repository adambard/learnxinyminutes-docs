---
language: easylang
contributors:
    - ["chkas", "https://github.com/chkas"]
---

Easylang is a simple programming language with built-in graphical functions and an easy-to-use and offline usable browser IDE. Its simple syntax and semantics make it well suited as a teaching and learning programming language. You can also use it to write graphical applications that you can embed in a web page. 

Easylang is statically typed and has as data types only strings and numbers (floating point), resizeable arrays of strings and numbers and arrays of arrays.

[The browser IDE](https://easylang.online/ide/) includes various tutorials, including one for absolute beginners.

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
print "1 + 2 = " & 1 + 2
# 
# array of numbers
a[] = [ 2.1 3.14 3 ]
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
letters$[1] = "o"
print strjoin letters$[]
print ""
# 
# 2-dimensional arrays are arrays of arrays
# this defines 3 arrays with length 4
# 
len a[][] 3
for i range len a[][]
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
print pow 2 8
# seconds since 1970
print floor systime
# 
# random numbers
print randomf
print random 6 + 1
print ""
# 
# hour and minutes
print substr timestr systime 11 5
# 
print strcode "A"
print strchar 65
print ""
# 
# set number format
numfmt 0 4
print sqrt 2
print pi
print logn 10
print ""
# 
a$[] = strsplit "10,15,22" ","
print a$[]
print 2 * number a$[0]
print len a$[]
print len "Hello"
print ""
# 
# With 'break n' you can leave nested loops and a function
# 
names$[] = [ ]
func name2id name$ . id .
  for id range len names$[]
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
# if it exists, otherwise via a prompt.
# 
input_data
10
-2
6
```

* [More about easylang](https://easylang.online/)

* [Source code](https://github.com/chkas/easylang)

