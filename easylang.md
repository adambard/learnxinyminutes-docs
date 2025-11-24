---
name: Easylang
contributors:
    - ["chkas", "https://github.com/chkas"]
filename: easylang.el
---

**Easylang** is a simple programming language with built-in graphical functions and an easy-to-use and browser IDE. Its simplified syntax and semantics make it well suited as a teaching and learning programming language. You can also use it to write graphical applications that you can embed in a web page.

[The browser IDE](https://easylang.online/ide/) includes various tutorials, including one for beginners.

```
print "Hello Easylang"
#
f = 2 * pi
print "Numbers are floating point: " & f
print "and are implicitly converted to strings"
#
str$ = "Strings can"
str$ &= " grow"
print str$
#
# Blocks end with 'end' or a dot, newline is just a token seperator
#
for i = 1 to 3
   print i * i
.
func gcd a b .
   # a and b are value parameters
   while b <> 0
      # h is local, since it is first used in the function
      h = b
      b = a mod b
      a = h
   .
   return a
.
print "The greatest common divisor of 182 and 28: " & gcd 182 28
#
func gcdr a b .
   if b = 0 : return a
   return gcdr b (a mod b)
.
print "Recursively calculated: " & gcd 182 28
#
# Arrays are 1-based and can grow
#
a[] = [ 13 3.14 3 ]
a[] &= 4
for i = 1 to len a[] : write a[i] & " "
print ""
#
proc sort &data[] .
   # data is a reference parameter
   for i = 1 to len data[] - 1
      for j = i + 1 to len data[]
         if data[j] < data[i] : swap data[j] data[i]
      .
   .
.
sort a[]
print "Sorted: " & a[]
#
# arrays, strings and numbers are copied by value
#
b[] = a[]
a[1] = 111
a[4] = 777
print "a[]:" & a[] & "  b[]:" & b[]
#
# array swapping ist fast
#
swap a[] b[]
print "a[]:" & a[] & "  b[]:" & b[]
#
# for-in iterates over the elements of an array
#
fruits$[] = [ "apple" "banana" "orange" ]
for fruit$ in fruits$[] : print fruit$
#
# strings are also used for single characters
#
letters$[] = strchars "ping"
print letters$[]
letters$[2] = "o"
print strjoin letters$[] ""
#
# a 2-dimensional array is an array of arrays
#
len a[][] 3
for i = 1 to len a[][]
   len a[i][] 3
   a[i][i] = 1
.
print a[][]
#
# some builtin functions
#
if sin 90 = 1
   print "Angles are in degree"
.
print "A kibibyte: " & pow 2 10
print "Seconds since 1970: " & floor systime
print "Random between 0 and 1: " & randomf
print "A dice roll: " & random 6
print "Current time: " & substr timestr systime 12 5
print strcode "A" & " is " & strchar 65
#
# set number output format
#
numfmt 0 4
print "Square root of 2: " & sqrt 2
print "Pi: " & pi
print "10's logarithm of 999: " & log10 999
#
a$[] = strsplit "10,15,22" ","
print a$[]
print 2 * number a$[1]
print len a$[]
print len a$[1]
#
# "input" reads a string from the "input_data" section, if it exists,
# otherwise via a prompt.
#
repeat
   s$ = input
   until s$ = ""
   sum += number s$
.
print "Sum of input data: " & sum
#
input_data
567
1023
69
```

Built-in graphic primitives and event-driven programming

```
# simple drawing with the mouse
#
glinewidth 4
gcolor 900
# the colors are coded from 0 to 999, where the
# decimal digits specify the RGB components
#
on mouse_down
   down = 1
   mx = mouse_x
   my = mouse_y
   gcircle mx my 2
.
on mouse_up
   down = 0
.
on mouse_move
   if down = 1
      gline mx my mouse_x mouse_y
      mx = mouse_x
      my = mouse_y
   .
.
```

```
# an animated pendulum
#
ang = 45
on animate
   # The animate event occurs after each screen refresh.
   gclear
   gcircle 50 50 1
   x = 50 + 40 * sin ang
   y = 50 + 40 * cos ang
   gline 50 50 x y
   gcircle x y 6
   vel += sin ang / 5
   ang += vel
.
```

* [More about Easylang](https://easylang.online/apps/)

* [Source code](https://github.com/chkas/easylang/)

