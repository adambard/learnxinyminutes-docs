---
language: forth
contributors:
    - ["Horse M.D.", "http://github.com/HorseMD/"]
filename: learnforth.fs
---

Forth was created by Charles H. Moore in the 70s.

Note: This article focuses predominantly on the Gforth implementation of Forth, but most
of what is written here should work elsewhere. 

> If Lisp is the ultimate high level language, Forth is the ultimate low level language.

```forth

\ Forth is an interactive programming language which is comprised of *words*. These are
\ Forth subroutines which are executed once you press <Cr>, from left to right.

\ ------------------------------ Precursor ------------------------------

\ It's important to know how forth processes instructions. All programming in Forth is
\ done by manipulating what's known as the parameter stack (more commonly just referred
\ to as "the stack"). The stack is a typical last-in-first-out (LIFO) stack. Typing:

5 2 3 56 76 23 65

\ Means 5 gets put on the stack first, then 2, then 3, etc all the way to 65, which
\ is now at the top of the stack. We can see the length and contents of the stack by
\ passing forth the word `.s`:

.s <7> 5 2 3 56 76 23 65    \ ok

\ Forth's interpreter interprets what you type in one of two ways: as *words* (i.e. the
\ name of subroutines) or as *numbers*. Words are essentially "symbols that do things".

\ Finally, as the stack is LIFO, we obviously must use postfix notation to manipulate
\ the stack. This should become clear shortly.

\ ------------------------------ Basic Arithmetic ------------------------------

\ Lets do a simple equation: adding 5 and 4. In infix notation this would be 5 + 4,
\ but as forth works in postfix (see above about stack manipulation) we input it like so:

5 4 +    \ ok

\ However, this alone yields "ok", yet no answer. Why? The way forth interprets what
\ we typed is as such: 5 gets added to the top of the stack, and then 4. Finally,
\ it runs word `+` on the stack (which pops the top and second value, and adds them),
\ and inserts the result at the top of the stack. Typing the word `.` will yield
\ the result.

.    \ 9 ok

\ This should illustrate the fundamentals of forth. Lets do a few more arithmetic
\ tests:

6 7 * .     \ 42 ok
1360 23 - . \ 1337 ok
12 12 / .   \ 1 ok

\ And so on.

\ ------------------------------ More Advanced Stack Maniulation ------------------------------

\ Naturally, as we do so much work with the stack, we'll want some useful methods.

drop \ drop (remove) the item at the top of the stack (note the difference between this and `.`)
dup  \ duplicate the item on top the stack
rot  \ rotate the top three items (third -> first, first -> second, second -> third)
swap \ swaps the top item with the second item

\ Examples:

dup *            \ square the top item
2 5 dup * swap / \ half the top item squared
6 4 5 rot * -    \ sometimes we just want to reorganize
4 0 drop 2 /     \ add 4 and 0, remove 0 and divide the top by 2 

\ ------------------------------ Extra Stack Manipulation ------------------------------

tuck   \ acts like dup, except it duplicates the top item into the 3rd* position in the stack
over   \ duplicate the second item to the top of the stack
n roll \ where n is a number, *move* the stack item at that position to the top of the stack
n pick \ where n is a number, *duplicate* the item at that position to the top of the stack

\ 3rd*: when referring to stack indexes, they are zero-based - i.e. the first element is at
\ position 0, the second element is at position 1, etc... Just like indexing arrays in
\ most other languages.

\ ------------------------------ Creating Words ------------------------------

\ Quite often one will want to write their own words.

: square ( n -- n ) dup * ;    \ ok

\ Lets break this down. The `:` word says to Forth to enter "compile" mode. After that,
\ we tell Forth what our word is called - "square". Between the parentheses we have a
\ comment depicting what this word does to the stack - it takes a number and adds a
\ number. Finally, we have what the word does, until we reach the `;` word which
\ says that you've finished your definition, Forth will add this to the dictionary and
\ switch back into interpret mode.

\ We can check the definition of a word with the `see` word:

see square     \ dup * ; ok

\ ------------------------------ Conditionals ------------------------------

\ TODO

\ ------------------------------ Loops ------------------------------

\ TODO

\ ------------------------------ The Return Stack ------------------------------

\ TODO

\ ------------------------------ Variables and Memory ------------------------------

\ TODO

\ ------------------------------ Final Notes ------------------------------

\ Booleans
\ Floats
\ Commenting (types)
\ bye

```

##Ready For More?

* [Starting Forth](http://www.forth.com/starting-forth/)
* [Thinking Forth](http://thinking-forth.sourceforge.net/)
