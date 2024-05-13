---
language: "montilang"
filename: montilang.ml
contributors:
  - ["Leo Whitehead", "https://github.com/lduck11007"]
---

MontiLang is a Stack-Oriented concatenative imperative programming language. Its syntax
is roughly based off of forth with similar style for doing arithmetic in [reverse polish notation.](https://en.wikipedia.org/wiki/Reverse_Polish_notation)

A good way to start with MontiLang is to read the documentation and examples at [montilang.ml](http://montilang.ml),
then download MontiLang or build from source code with the instructions provided.

```
/# Monti Reference sheet #/
/#
Comments are multiline
Nested comments are not supported 
#/
/# Whitespace is all arbitrary, indentation is optional #/
/# All programming in Monti is done by manipulating the parameter stack 
arithmetic and stack operations in MontiLang are similar to FORTH
https://en.wikipedia.org/wiki/Forth_(programming_language)
#/

/# in Monti, everything is either a string or a number. Operations treat all numbers
similarly to floats, but anything without a remainder is treated as type int #/

/# numbers and strings are added to the stack from left to right #/

/# Arithmetic works by manipulating data on the stack #/

5 3 + PRINT . /# 8 #/

/#  5 and 3 are pushed onto the stack
    '+' replaces top 2 items on stack with sum of top 2 items
    'PRINT' prints out the top item on the stack
    '.' pops the top item from the stack. 
    #/

6 7 * PRINT . /# 42 #/
1360 23 - PRINT . /# 1337 #/
12 12 / PRINT . /# 1 #/
13 2 % PRINT . /# 1 #/

37 NEG PRINT . /# -37 #/
-12 ABS PRINT . /# 12 #/
52 23 MAX PRINT . /# 52 #/
52 23 MIN PRINT . /# 23 #/

/# 'PSTACK' command prints the entire stack, 'CLEAR' clears the entire stack #/

3 6 8 PSTACK CLEAR /# [3, 6, 8] #/

/# Monti comes with some tools for stack manipulation #/

2 DUP PSTACK CLEAR /# [2, 2] - Duplicate the top item on the stack#/
2 6 SWAP PSTACK CLEAR /# [6, 2] - Swap top 2 items on stack #/
1 2 3 ROT PSTACK CLEAR /# [2, 3, 1] - Rotate top 3 items on stack #/
2 3 NIP PSTACK CLEAR /# [3] - delete second item from the top of the stack #/
4 5 6 TRIM PSTACK CLEAR /# [5, 6] - Deletes first item on stack #/
/# variables are assigned with the syntax 'VAR [name]'#/
/# When assigned, the variable will take the value of the top item of the stack #/

6 VAR six . /# assigns var 'six' to be equal to 6 #/
3 6 + VAR a . /# assigns var 'a' to be equal to 9 #/

/# the length of the stack can be calculated with the statement 'STKLEN' #/
1 2 3 4 STKLEN PRINT CLEAR /# 4 #/

/# strings are defined with | | #/

|Hello World!| VAR world . /# sets variable 'world' equal to string 'Hello world! #/ 

/# variables can be called by typing its name. when called, the value of the variable is pushed
to the top of the stack #/
world PRINT .

/# with the OUT statement, the top item on the stack can be printed without a newline #/

|world!| |Hello, | OUT SWAP PRINT CLEAR

/# Data types can be converted between strings and integers with the commands 'TOINT' and 'TOSTR'#/
|5| TOINT PSTACK . /# [5] #/
45 TOSTR PSTACK . /# ['45'] #/

/# User input is taken with INPUT and pushed to the stack. If the top item of the stack is a string, 
the string is used as an input prompt #/

|What is your name? | INPUT NIP 
|Hello, | OUT SWAP PRINT CLEAR


/# FOR loops have the syntax 'FOR [condition] [commands] ENDFOR' At the moment, [condition] can
only have the value of an integer. Either by using an integer, or a variable call to an integer.
[commands] will be interpereted the amount of time specified in [condition] #/
/# E.G: this prints out 1 to 10 #/

1 VAR a .
FOR 10
    a PRINT 1 + VAR a
ENDFOR

/# the syntax for while loops are similar. A number is evaluated as true if it is larger than
0. a string is true if its length > 0. Infinite loops can be used by using literals.
#/
10 var loop .
WHILE loop
    loop print 
    1 - var loop
ENDWHILE
/#
this loop would count down from 10.

IF statements are pretty much the same, but only are executed once.
#/
IF loop
 loop PRINT .
ENDIF

/# This would only print 'loop' if it is larger than 0 #/

/# If you would want to use the top item on the stack as loop parameters, this can be done with the ':' character #/

/# eg, if you wanted to print 'hello' 7 times, instead of using #/

FOR 7
    |hello| PRINT .
ENDFOR

/# this could be used #/
7
FOR : 
    |hello| PRINT .
ENDFOR

/# Equality and inequality statements use the top 2 items on the stack as parameters, and replace the top two items with the output #/
/# If it is true, the top 2 items are replaced with '1'. If false, with '0'. #/

7 3 > PRINT . /# 1 #/
2 10 > PRINT . /# 0 #/
5 9 <= PRINT . /# 1 #/
5 5 == PRINT . /# 1 #/
5 7 == PRINT . /# 0 #/
3 8 != PRINT . /# 1 #/

/# User defined commands have the syntax of 'DEF [name] [commands] ENDDEF'. #/
/# eg, if you wanted to define a function with the name of 'printseven' to print '7' 10 times, this could be used #/

DEF printseven
    FOR 10
       7 PRINT .
    ENDFOR
ENDDEF

/# to run the defined statement, simply type it and it will be run by the interpereter #/

printseven

/# Montilang supports AND, OR and NOT statements #/

1 0 AND PRINT . /# 0 #/
1 1 AND PRINT . /# 1 #/
1 0 OR PRINT . /# 1 #/
0 0 OR PRINT . /# 0 #/
1 NOT PRINT . /# 0 #/
0 NOT PRINT . /# 1 #/

/# Preprocessor statements are made inbetween '&' characters #/
/# currently, preprocessor statements can be used to make c++-style constants #/

&DEFINE LOOPSTR 20&
/# must have & on either side with no spaces, 'DEFINE' is case sensitive. #/
/# All statements are scanned and replaced before the program is run, regardless of where the statements are placed #/

FOR LOOPSTR 7 PRINT . ENDFOR /# Prints '7' 20 times. At run, 'LOOPSTR' in source code is replaced with '20' #/ 

/# Multiple files can be used with the &INCLUDE <filename>& Command that operates similar to c++, where the file specified is tokenized,
   and the &INCLUDE statement is replaced with the file #/
   
/# E.G, you can have a program be run through several files. If you had the file 'name.mt' with the following data:

[name.mt]
|Hello, | OUT . name PRINT .

a program that asks for your name and then prints it out can be defined as such: #/

|What is your name? | INPUT VAR name . &INCLUDE name.mt&

/# ARRAYS: #/

/# arrays are defined with the statement 'ARR'
When called, everything currently in the stack is put into one
array and all items on the stack are replaced with the new array. #/

2 3 4 ARR PSTACK . /# [[2, 3, 4]] #/

/# the statement 'LEN' adds the length of the last item on the stack to the stack.
This can be used on arrays, as well as strings. #/

3 4 5 ARR LEN PRINT . /# 3 #/

/# values can be appended to an array with the statement 'APPEND' #/

1 2 3 ARR 5 APPEND . PRINT . /# [1, 2, 3, 5] #/

/# an array at the top of the stack can be wiped with the statement 'WIPE' #/
3 4 5 ARR WIPE PRINT . /# [] #/

/# The last item of an array can be removed with the statement 'DROP' #/

3 4 5 ARR DROP PRINT . /# [3, 4]
/# arrays, like other datatypes can be stored in variables #/
5 6 7 ARR VAR list .
list PRINT . /# [5, 6, 7] #/

/# Values at specific indexes can be changed with the statement 'INSERT <index>' #/
4 5 6 ARR 
97 INSERT 1 . PRINT /# 4, 97, 6 #/

/# Values at specific indexes can be deleted with the statement 'DEL <index>' #/
1 2 3 ARR
DEL 1 PRINT . /# [1, 3] #/

/# items at certain indexes of an array can be gotten with the statement 'GET <index>' #/

1 2 3 ARR GET 2 PSTACK /# [[1, 2, 3], 3] #/
```

## Extra information

- [MontiLang.ml](http://montilang.ml/)
- [GitHub Page](https://github.com/lduck11007/MontiLang)
