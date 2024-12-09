---
language: LDPL
filename: learnLDPL.ldpl
contributors:
    - ["Martín del Río", "https://github.com/lartu"]
    - ["John Paul Wohlscheid", "https://github.com/JohnBlood"]
---

**LDPL** is a powerful, C++ transpiled, open-source programming language designed
from the ground up to be excessively expressive, readable, fast and easy to learn.
It mimics plain English, in the likeness of older programming languages like COBOL,
with the desire that it can be understood by anybody. It's very portable and runs on a
plethora of different architectures and operating systems and it even supports UTF-8
out of the box.

[Read more here.](https://github.com/lartu/ldpl)

```coffeescript
# This is a single line comment in LDPL.
# LDPL doesn't have multi-line comments.

# LDPL is a case-insensitive language: dIsPlaY and DISPLAY are the same
# statement, and foo and FOO name the same variable.

# An LDPL source file is divided in two sections, the DATA section and
# the PROCEDURE section.

DATA:
# Within the DATA section, variables are declared.

myNumber is number          # Defines a real number.
myString is text            # Defines a string.
myList is number list       # Defines a list of numbers.
myMap  is number map        # Defines a map of numbers.

# LDPL understands four data types: two scalar types (NUMBER, TEXT)
# and two container types (LISTs and MAPs).
# LISTs can be TEXT LISTs or NUMBER LISTs, while MAPs can be
# TEXT MAPs and NUMBER MAPs. You can also chain many containers
# to create larger data types:
textListList is text list list
myMulticontainer is number list list map 
# Defines a map of lists of lists of numbers.

PROCEDURE:
# Within the PROCEDURE section, your code is written.

store -19.2 in myNumber         # Use the STORE statement to assign values
store "Hi there" in myString    # to variables.
push 890 to myList # Use PUSH - TO to append values to lists.
push 100 to myList
push 500 to myList
store 45 in myMap:"someIndex" # Use the : operator to index containers.

push list to textListList # Push an empty list into a list of lists.
push "LDPL is nice!" to textListList:0 #Push text to the pushed list.

display "Hello World!" # Use the DISPLAY statement to print values.
# The display statement can receive multiple values separated by spaces.
display crlf "How are you today?" myNumber myString crlf
# CRLF is the standard line break value in LDPL.
display textListList:0:0 " Isn't it?" crlf

# IF statements in LDPL are extremely verbose:
if myNumber is equal to -19.2 and myList:0 is less than 900 then
    display "Yes!" crlf
else if myMap:"someIndex" is not equal to 45 then
    display "This is an else if!" crlf
else
    display "Else!" crlf
end if
# Valid LDPL comparison operators are
# - IS EQUAL TO
# - IS NOT EQUAL TO
# - IS LESS THAN
# - IS GREATER THAN
# - IS LESS THAN OR EQUAL TO
# - IS GREATER THAN OR EQUAL TO
if "Hi there!" is not equal to "Bye bye!" then
    display "Yep, those weren't equal." crlf
end if
# LDPL normally doesn't understand inline expressions, so you
# cannot do stuff like:
# if myNumber - 9 * 2 is equal to 10 then
# LDPL will set your computer on fire and burst your screen if you do so.

# WHILE loops follow the same rules
store 0 in myNumber
while myNumber is less than 10 do
    display "Loop number " myNumber "..." crlf
    in myNumber solve myNumber + 1 # You can do math like this.
repeat
# You can use 'break' and 'continue' inside loops just like any other language.

# LDPL also has FOR loops and FOR EACH loops
for myNumber from 0 to 100 step 2 do
    display myNumber crlf
repeat

for each myNumber in myList do
    display myNumber
repeat

display "Enter your name: "
accept myString # Use ACCEPT to let the user input values.
display "Hi there, " myString crlf
display "How old are you?: "
accept myNumber
if myNumber is greater than 200 then
    display "Woah, you are so old!" crlf
end if

wait 1000 milliseconds # Pause the program for a whole second.

# Let's do some math
store 1.2 in myNumber
in myNumber solve myNumber * (10 / 7.2) # Operators are separated by spaces.
floor myNumber
display myNumber crlf
get random in myNumber # get a random number between 0 and 1 
                       # and store it in myNumber

# Functions in LDPL are called sub-procedures. Sub-procedures, like source
# files, are divided in sections. The sections found in sub-procedures are
# the PARAMETERS section, the LOCAL DATA section and the PROCEDURE section.
# All sections except the PROCEDURE section can be skipped if they aren't
# used. If no PARAMETERS nor LOCAL DATA sections are used, the PROCEDURE
# keyword may be omitted.
sub myFunction
    parameters:
        a is number # LDPL is pass by reference
        b is number
        result is number # Thus you can return values through a parameter.
    local data:
        c is number
    procedure:
        get random in c
        in result solve a + b * c
end sub

sub sayHello
    display "Hi there!" crlf
    return
    display "This won't be displayed :("
end sub

call myFunction with 1 2 myNumber
display myNumber crlf
call sayHello
call sayBye # sub-procedures may be called before they are declared

sub sayBye
    display "Bye!"
end sub

# One of the greatest features of LDPL is the ability to create your
# own statements.

create statement "say hi" executing sayHello
say hi

create statement "random add $ and $ in $" executing myFunction
random add 1 and 2 in myNumber
display myNumber crlf

exit
```

## Further Reading

 * [LDPL Docs](https://docs.ldpl-lang.org)
