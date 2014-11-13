---
language: forth
contributors:
    - ["Horse M.D.", "http://github.com/HorseMD/"]
filename: learnforth.fs
---

Forth was created by Charles H. Moore in the 70s.

Note: This article focuses predominantly on the Gforth implementation of
Forth, but most of what is written here should work elsewhere.

> If Lisp is the ultimate high level lang, Forth is the ultimate low level lang.

```forth

\ Forth is an interactive programming language which is comprised of
\ *words*. These are Forth subroutines which are executed once you press
<Cr>, from left to right.

\ ------------------------------ Precursor ------------------------------

\ It's important to know how forth processes instructions. All
\ programming in Forth is done by manipulating what's known as the parameter
\ stack (more commonly just referred to as "the stack"). Typing:
5 2 3 56 76 23 65

\ Makes those numbers get added to the stack, from left to right.
.s    \ <7> 5 2 3 56 76 23 65 ok

\ Forth's interpreter interprets what you type in one of two ways: as *words*
\ (i.e. the name of subroutines) or as *numbers*.

\ ------------------------------ Basic Arithmetic ------------------------------

\ Arithmetic (in fact most words requiring data) works by manipulating data on
\ the stack.
5 4 +    \ ok

\ This adds 5 and 4 to the stack and then `+` is called, which removes them and
\ adds the result to the stack. We can see it with `.`:
.    \ 9 ok

\ A few more examples of arithmetic
6 7 * .     \ 42 ok
1360 23 - . \ 1337 ok
12 12 / .   \ 1 ok

\ And so on.

\ ----------------------------- Stack Manipulation -----------------------------

\ Naturally, as we work with the stack, we'll want some useful methods:

3 dup -          \ duplicate the top item (1st now equals 2nd): 3 - 3
2 5 swap /       \ swap the top with the second element:        5 / 2
6 4 5 rot .s     \ rotate the top 3 elements:                   4 5 6 ok
4 0 drop 2 /     \ remove the top item (dont print to screen):  4 / 2

\ ---------------------- More Advanced Stack Manipulation ----------------------

1 2 3 4 tuck   \ duplicate the top item into the second slot:      1 2 4 3 4 ok
1 2 3 4 over   \ duplicate the second item to the top:             1 2 3 4 3 ok
1 2 3 4 2 roll \ *move* the item at that position to the top:      1 3 4 2 ok
1 2 3 4 2 pick \ *duplicate* the item at that position to the top: 1 2 3 4 2 ok

\ When referring to stack indexes, they are zero-based.

\ ------------------------------ Creating Words --------------------------------

\ Quite often one will want to write their own words.
: square ( n -- n ) dup * ;    \ ok

\ The `:` word sets forth into compile mode. `(` and `)` are both words which
\ tell forth to ignore between them. Up until the `;` word is what our word
\ does.

\ We can check the definition of a word with the `see` word:
see square     \ dup * ; ok

\ -------------------------------- Conditionals --------------------------------

\ In forth, -1 is used to represent truth, and 0 is used to represent false.
\ The idea is that -1 is 11111111 in binary, whereas 0 is obviously 0 in binary.
\ However, any non-zero value is usually treated as being true:

42 42 =    / -1 ok
12 53 =    / 0 ok

\ `if` is a *compile-only word*. This means that it can only be used when we're
\ compiling a word. The format is `if` <stuff to do> `then` <rest of program>.

: ?>64 ( n -- n ) DUP 64 > if ." Greater than 64!" then ; \ ok
100 ?>64                                                  \ Greater than 64! ok

\ Else:

: ?>64 ( n -- n ) DUP 64 > if ." Greater than 64!" else ." Less than 64!" then ;
100 ?>64    \ Greater than 64! ok
20 ?>64     \ Less than 64! ok

\ ------------------------------------ Loops -----------------------------------

\ `do` is like `if` in that it is also a compile-only word, though it uses
\ `loop` as its terminator:
: myloop ( -- ) 5 0 do cr ." Hello!" loop ; \ ok
test
\ Hello!
\ Hello!
\ Hello!
\ Hello!
\ Hello! ok

\ `do` expects two numbers on the stack: the end number and the index number:

\ Get the value of the index as we loop with `i`:
: one-to-12 ( -- ) 12 0 do i . loop ;     \ ok
one-to-12                                 \ 0 1 2 3 4 5 6 7 8 9 10 11 12 ok
: squares ( -- ) 10 0 do i DUP * . loop ; \ ok
squares                                   \ 0 1 4 9 16 25 36 49 64 81 ok

\ Change the "step" with `+loop`:
: threes ( -- ) 15 0 do i . 3 +loop ; \ ok
threes                                \ 0 3 6 9 12 ok

\ Finally, while loops with `begin` <stuff to do> <flag> `unil`:
: death ( -- ) begin ." Are we there yet?" 0 until ;

\ ---------------------------- Variables and Memory ----------------------------

\ Sometimes we'll be in a situation where we want more permanent variables:
\ First, we use `variable` to declare `age` to be a variable.
variable age

\ Then we write 21 to age with the word `!`.
21 age !

\ Finally we can print our variable using the "read" word '@', which adds the
\ value to the stack, or use `?` that reads and prints it in one go.
age @ . \ 12 ok
age ?   \ 12 ok

\ What's happening here is that `age` stores the memory address, and we use `!`
\ and `@` to manipulate it.

\ Constants are quite simiar, except we don't bother with memory addresses:
100 constant WATER-BOILING-POINT \ ok
WATER-BOILING-POINT .            \ 100 ok

\ Arrays!

\ Set up an array of length 3:
variable mynumbers 2 cells allot

\ Initialize all the values to 0
mynumbers 3 cells erase
\ (alternatively we could do `0 fill` instead of `erase`, but as we're setting
\ them to 0 we just use `erase`).

\ or we can just skip all the above and initialize with specific values:
create mynumbers 64 , 9001 , 1337 , \ the last `,` is important!

\ ...which is equivalent to:

\ [64, 9001, 1337]
64 mynumbers 0 cells + !
9001 mynumbers 1 cells + !
1337 mynumbers 2 cells + !

\ Reading values at certain array indexes:
0 cells mynumbers + ? \ 64 ok
1 cells mynumbers + ? \ 9001 ok
2 cells mynumbers + ? \ 1337 ok

\ Of course, you'll probably want to define your own words to manipulate arrays:
: ?mynumbers ( n -- n ) cells mynumbers + ; \ ok
64 mynumbers 2 cells + !                    \ ok
2 ?mynumbers ?                              \ 64 ok

\ ------------------------------ The Return Stack ------------------------------

\ TODO

\ --------------------------------- Final Notes --------------------------------

\ Floats
\ Commenting (types)
\ bye

```

##Ready For More?

* [Starting Forth](http://www.forth.com/starting-forth/)
* [Thinking Forth](http://thinking-forth.sourceforge.net/)
