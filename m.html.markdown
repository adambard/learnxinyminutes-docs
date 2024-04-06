--- 
language: M (MUMPS)
contributors: 
    - ["Fred Turkington", "http://z3ugma.github.io"] 
filename: LEARNM.m 
---

M, or MUMPS (Massachusetts General Hospital Utility Multi-Programming System) is
a procedural language with a built-in NoSQL database. Or, it’s a database with
an integrated language optimized for accessing and manipulating that database.
A key feature of M is that accessing local variables in memory and persistent
storage use the same basic syntax, so there's no separate query
language to remember. This makes it fast to program with, especially for
beginners. M's syntax was designed to be concise in an era where
computer memory was expensive and limited. This concise style means that a lot
more fits on one screen without scrolling.

The M database is a hierarchical key-value store designed for high-throughput
transaction processing. The database is organized into tree structures called
"globals", (for global variables) which are sparse data structures with parallels
to modern formats like JSON.

Originally designed in 1966 for the healthcare applications, M continues to be
used widely by healthcare systems and financial institutions for high-throughput
real-time applications. 

### Example

Here's an example M program using expanded syntax to calculate the Fibonacci series:

```
fib ; compute the first few Fibonacci terms
    new i,a,b,sum
    set (a,b)=1 ; Initial conditions
    for i=1:1 do  quit:sum>1000
    . set sum=a+b
    . write !,sum
    . set a=b,b=sum
```

### Comments
Comments in M need at least one space before the comment marker of semicolon.
Comments that start with at least two semicolons (;;) are guaranteed to be accessible
within a running program.

```
 ;   Comments start with a semicolon (;)
```

### Data Types

M has one data type (String) and three interpretations of that string.:

```
;   Strings - Characters enclosed in double quotes.
;       "" is the null string. Use "" within a string for "
;       Examples: "hello", "Scrooge said, ""Bah, Humbug!"""
;
;   Numbers - no commas, leading and trailing 0 removed.
;       Scientific notation with 'E'.  (not 'e')
;       Numbers with at least with IEEE 754 double-precision values (guaranteed 15 digits of precision)
;       Examples: 20 (stored as 20) , 1e3 (stored as 1000), 0500.20 (stored as 500.2),
;                 the US National Debt AT sometime on 12-OCT-2020 retrieved from http://www.usdebt.org is 27041423576201.15)
;                     (required to be stored as at least 27041422576201.10 but most implementations store as 27041432576201.15)
;
;   Truthvalues - String interpreted as 0  is used for false and any string interpreted as non-zero (such as 1) for true.
```

### Commands

Commands are case insensitive, and have full form, and a shortened abbreviation, often the first letter. Commands have zero or more arguments,depending on the command. This page includes programs written in this terse syntax. M is whitespace-aware. Spaces are treated as a delimiter between commands and arguments. Each command is separated from its arguments by 1 space. Commands with zero arguments are followed by 2 spaces. (technically these are called argumentless commands)

#### Common Commands from all National and International Standards of M

#### Write (abbreviated as W) 

Print data to the current device.

``` 
WRITE !,"hello world" 
```

Output Formatting characters:

 The ! character is syntax for a new line.
 The # character is syntax for a new page.
 The sequence of the ? character and then a numeric expression is syntax for output of spaces until the number'th colum is printed.

Multiple statements can be provided as additional arguments before the space separators to the next command:

```
w !,"foo bar"," ","baz" 
```

#### Read (abbreviated as R)

Retrieve input from the user

```
READ var
r !,"Wherefore art thou Romeo? ",why
```

As with all M commands, multiple arguments can be passed to a read command. Constants like quoted strings, numbers, and formatting characters are output directly. Values for both global variables and local variables are retrieved from the user. The terminal waits for the user to enter the first variable before displaying the second prompt.

```
r !,"Better one, or two? ",lorem," Better two, or three? ",ipsum
```

#### Set (abbreviated as S)

Assign a value to a variable

```
SET name="Benjamin Franklin"
s centi=0.01,micro=10E-6
w !,centi,!,micro

;.01
;.00001
```

#### Kill (abbreviated as K)

Remove a variable from memory or remove a database entry from disk.
A database node (global variable) is killed depending on the variable name being prefixed by the caret character (^).
If it is not, then the local variable is removed from memory. 
If KILLed, automatic garbage collection occurs.

```
KILL centi
k micro
```

### Globals and Arrays

In addition to local variables, M has persistent, shared variables that are the built-in database of M.  They are stored to disk and called _globals_. Global names must start with a __caret__ (__^__). 

Any variable (local or global) can be an array with the assignment of a _subscript_. Arrays are sparse and do not have a predefined size. Only if data is stored will a value use memory. Arrays should be visualized like trees, where subscripts are branches and assigned values are leaves. Not all nodes in an array need to have a value. 

```
s ^cars=20
s ^cars("Tesla",1,"Name")="Model 3"
s ^cars("Tesla",2,"Name")="Model X"
s ^cars("Tesla",2,"Doors")=5

w !,^cars 
; 20
w !,^cars("Tesla")
; null value - there's no value assigned to this node but it has children
w !,^cars("Tesla",1,"Name")
; Model 3
```

The index values of Arrays are automatically sorted in order. There is a catchphrase of "MUMPS means never having to say you are sorting". Take advantage of the built-in sorting by setting your value of interest as the last child subscript of an array rather than its value, and then storing an empty string for that node.

```
; A log of temperatures by date and time
s ^TEMPS("11/12","0600",32)=""
s ^TEMPS("11/12","1030",48)=""
s ^TEMPS("11/12","1400",49)=""
s ^TEMPS("11/12","1700",43)=""
```

### Operators

```jinja
; Assignment:       =
; Unary:            +   Convert a string value into a numeric value.
; Arthmetic:
;                   +   addition
­;                   -   subtraction
;                   *   multiplication
;                   /   floating-point division
;                   \   integer division
;                   #   modulo
;                   **  exponentiation
; Logical:  
;                   &   and
;                   !   or
;                   '   not
; Comparison:
;                   =   equal 
;                   '=  not equal
;                   >   greater than
;                   <   less than
;                   '>  not greater / less than or equal to
;                   '<  not less / greater than or equal to
; String operators:
;                   _   concatenate
;                   [   contains ­          a contains b 
;                   ]]  sorts after  ­      a comes after b
;                   '[  does not contain
;                   ']] does not sort after
```

#### Order of operations

Operations in M are _strictly_ evaluated left to right. No operator has precedence over any other.
For example, there is NO order of operations where multiply is evaluated before addition.
To change this order, just use parentheses to group expressions to be evaluated first.

```
w 5+3*20
;160
;You probably wanted 65
write 5+(3*20) 
```

### Flow Control, Blocks, & Code Structure

A single M file is called a _routine_. Within a given routine, you can break your code up into smaller chunks with _tags_. The tag starts in column 1 and the commands pertaining to that tag are indented.

A tag can accept parameters and return a value, this is a function. A function is called with '$$':

```
; Execute the 'tag' function, which has two parameters, and write the result.
w !,$$tag^routine(a,b) 
```

M has an execution stack. When all levels of the stack have returned, the program ends. Levels are added to the stack with _do_ commands and removed with _quit_ commands.

#### Do (abbreviated as D)

With an argument: execute a block of code & add a level to the stack. 

```
d ^routine    ;run a routine from the beginning. 
;             ;routines are identified by a caret.
d tag         ;run a tag in the current routine
d tag^routine ;run a tag in different routine
```

Argumentless do: used to create blocks of code. The block is indented with a period for each level of the block:

```
set a=1
if a=1 do  
. write !,a
. read b
. if b > 10 d
. . w !, b 
w "hello"
```

#### Quit (abbreviated as Q)
Stop executing this block and return to the previous stack level.
Quit can return a value, following the comamnd with a single space.
Quit can stop a loop. remember to follow with two spaces.
Quit outside a loop will return from the current subroutine followed by two spaces or a linefeed

#### New (abbreviated as N)
Hide with a cleared value a given variable's value _for just this stack level_. Useful for preventing side effects.

Putting all this together, we can create a full example of an M routine:

```
; RECTANGLE - a routine to deal with rectangle math
    q ; quit if a specific tag is not called

main 
    n length,width ; New length and width so any previous value doesn't persist
    w !,"Welcome to RECTANGLE. Enter the dimensions of your rectangle."
    r !,"Length? ",length,!,"Width? ",width
    d area(length,width)            ;Do/Call subroutine using a tag
    s per=$$perimeter(length,width)      ;Get the value of a function
    w !,"Perimeter: ",per
    quit

area(length,width)  ; This is a tag that accepts parameters. 
                    ; It's not a function since it quits with no value.
    w !, "Area: ",length*width
    q  ; Quit: return to the previous level of the stack.

perimeter(length,width)
    q 2*(length+width) ; Returns a value using Quit ; this is a function
```



### Conditionals, Looping and $Order()

F(or) loops can follow a few different patterns:

```jinja
;Finite loop with counter
;f var=start:increment:stop

f i=0:5:25 w i," "
;0 5 10 15 20 25 

; Infinite loop with counter
; The counter will keep incrementing forever. Use a conditional with Quit to get out of the loop.
;f var=start:increment 

f j=1:1 w j," " i j>1E3 q
; Print 1-1000 separated by a space

;Argumentless for - infinite loop. Use a conditional with Quit.
;   Also read as "forever" - f or for followed by two spaces.
s var=""
f  s var=var_"%" w !,var i var="%%%%%%%%%%" q  
; %
; %%
; %%%
; %%%%
; %%%%%
; %%%%%%
; %%%%%%%
; %%%%%%%%
; %%%%%%%%%
; %%%%%%%%%%
```

#### I(f), E(lse), Postconditionals

M has an if/else construct for conditional evaluation, but any command can be conditionally executed without an extra if statement using a _postconditional_. This is a condition that occurs immediately after the command, separated with a colon (:).

```jinja
; Conditional using traditional if/else
r "Enter a number: ",num
i num>100 w !,"huge"
e i num>10 w !,"big"
e w !,"small"

; Postconditionals are especially useful in a for loop.
; This is the dominant for loop construct:
;   a 'for' statement
;   that tests for a 'quit' condition with a postconditional
;   then 'do'es an indented block for each iteration

s var=""
f  s var=var_"%" q:var="%%%%%%%%%%" d  ;Read as "Quit if var equals "%%%%%%%%%%"
. w !,var

;Bonus points - the $L(ength) built-in function makes this even terser

s var=""
f  s var=var_"%" q:$L(var)>10  d  ;
. w !,var
```

#### Array Looping - $Order
As we saw in the previous example, M has built-in functions called with a single $, compared to user-defined functions called with $$. These functions have shortened abbreviations, like commands.
One of the most useful is __$Order()__ / $O(). When given an array subscript, $O returns the next subscript in that array. When it reaches the last subscript, it returns "".

```jinja
;Let's call back to our ^TEMPS global from earlier:
; A log of temperatures by date and time
s ^TEMPS("11/12","0600",32)=""
s ^TEMPS("11/12","0600",48)=""
s ^TEMPS("11/12","1400",49)=""
s ^TEMPS("11/12","1700",43)=""
; Some more
s ^TEMPS("11/16","0300",27)=""
s ^TEMPS("11/16","1130",32)=""
s ^TEMPS("11/16","1300",47)=""

;Here's a loop to print out all the dates we have temperatures for:
n date,time ; Initialize these variables with ""

; This line reads: forever; set date as the next date in ^TEMPS.
; If date was set to "", it means we're at the end, so quit.
; Do the block below
f  s date=$ORDER(^TEMPS(date)) q:date="" d
. w !,date

; Add in times too:
f  s date=$ORDER(^TEMPS(date)) q:date=""  d
. w !,"Date: ",date
. f  s time=$O(^TEMPS(date,time)) q:time=""  d
. . w !,"Time: ",time

; Build an index that sorts first by temperature - 
; what dates and times had a given temperature?
n date,time,temp
f  s date=$ORDER(^TEMPS(date)) q:date=""  d
. f  s time=$O(^TEMPS(date,time)) q:time=""  d
. . f  s temp=$O(^TEMPS(date,time,temp)) q:temp=""  d
. . . s ^TEMPINDEX(temp,date,time)=""

;This will produce a global like
^TEMPINDEX(27,"11/16","0300")
^TEMPINDEX(32,"11/12","0600")
^TEMPINDEX(32,"11/16","1130")
```

## Further Reading

There's lots more to learn about M. A great short tutorial comes from the University of Northern Iowa and  Professor Kevin O'Kane's [Introduction to the MUMPS Language][1] presentation.  More about M using VistA is at 

Intersystems has some products which are a super-set of the M programming language.
* [Iris Description Page][5]
* [Cache Description Page][6]

To install an M interpreter / database on your computer, try a [YottaDB Docker image][2]. 

YottaDB and its precursor, GT.M, have thorough documentation on all the language features including database transactions, locking, and replication:

* [YottaDB Programmer's Guide][3]
* [GT.M Programmer's Guide][4]

[1]: https://www.cs.uni.edu/~okane/source/MUMPS-MDH/MumpsTutorial.pdf 
[2]: https://yottadb.com/product/get-started/
[3]: https://docs.yottadb.com/ProgrammersGuide/langfeat.html
[4]: http://tinco.pair.com/bhaskar/gtm/doc/books/pg/UNIX_manual/index.html
[5]: https://www.intersystems.com/products/intersystems-iris/
[6]: https://en.wikipedia.org/wiki/InterSystems_Caché
