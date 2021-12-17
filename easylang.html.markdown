---
language: Easylang
contributors:
    - ["chkas", "https://github.com/chkas"]
filename: easylang.el
---

**Easylang** is a simple programming language with built-in graphical functions and an easy-to-use and offline usable browser IDE. Its simple syntax and semantics make it well suited as a teaching and learning programming language. You can also use it to write graphical applications that you can embed in a web page. 

*Easylang* is statically typed and has as data types only strings and numbers (floating point), resizeable arrays of strings and numbers and arrays of arrays.

[The browser IDE](https://easylang.online/ide/) includes various tutorials, including one for beginners.

```
print "Hello world"
#
# number variable (64 bit floating point)
#
h = 3.14
print h
#
# string variable
#
str$ = "monkey"
# strings can grow
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
print r
#
# strings can be concatenated and numbers are
# automatically converted to strings
#
print "1 + 2 = " & 1 + 2
#
# array of numbers
#
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
letters$[] = str_chars "ping"
print letters$[]
letters$[1] = "o"
print str_join letters$[]
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
#
# builtin functions
if sin 90 = 1
  print "angles are in degree"
.
print pow 2 8
# seconds since 1970
print floor sys_time
# random numbers
print randomf
print random 6 + 1
# 
# hour and minutes
print substr time_str sys_time 11 5
# 
print str_ord "A"
print str_chr 65
# 
# set number format
numfmt 0 4
print sqrt 2
print pi
print logn 10
# 
a$[] = str_split "10,15,22" ","
print a$[]
print 2 * number a$[0]
print len a$[]
print len "Hello"
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
call name2id "alice" id ; print i
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
# "input" reads a string from the "input_data" section, 
# if it exists, otherwise via a prompt.
#
input_data
10
-2
6
```

Built-in graphic primitives and event-driven programming

```
# simple drawing with the mouse
# 
set_linewidth 4
set_color 900
# the colors are coded from 0 to 999, with 
# the left digit specifying the red component,
# the middle digit the green component and
# the right digit the blue component. 
# 
on mouse_down
  down = 1
  move_pen mouse_x mouse_y
  # moves the drawing pen to the actual mouse position
  draw_circle 2
.
on mouse_up
  down = 0
.
on mouse_move
  if down = 1
    draw_line mouse_x mouse_y
  .
.
```

```
# an animated pendulum
#
on animate
  # The animate event occurs after each screen refresh.
  #
  clear_screen
  move_pen 50 50
  draw_circle 1
  x = 50 + 40 * sin ang
  y = 50 - 40 * cos ang
  draw_line x y
  draw_circle 5
  vel += sin ang / 5
  ang += vel
.
ang = 10
```

* [More about Easylang](https://easylang.online/)

* [Source code](https://github.com/chkas/easylang)

