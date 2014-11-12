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

\ ------------------------------ Stack Maniulation ------------------------------

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

\ ------------------------------ More Advanced Stack Manipulation ------------------------------

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

\ Booleans:
\ In forth, -1 is used to represent truth, and 0 is used to represent false.
\ The idea behind this is that -1 is 11111111 in binary, whereas 0 is obviously 0 in binary.
\ However, any non-zero value is usually treated as being true:

42 42 =    / -1 ok
12 53 =    / 0 ok

\ `if` is a *compile-only word*. This means that it can *only* be used when we're compiling a word.
\ when creating conditionals, the format is <boolean> `if` <stuff to do> `then` <rest of program>.

: ?>64 ( n -- n ) DUP 64 > if ." Greater than 64!" then ; \ ok
100 ?>64                                                  \ Greater than 64! ok

\ This unimaginative example displays "Greater than 64!" when the number on the stack is greater
\ than 64. However, it does nothing when the test is false. Let's fix that with the `else` word!

: ?>64 ( n -- n ) DUP 64 > if ." Greater than 64!" else ." Less than 64!" then ; \ ok
100 ?>64                                                                         \ Greater than 64! ok
20 ?>64                                                                          \ Less than 64! ok

\ As you can see, conditionals behave more or less like they do in most programming languages.

\ ------------------------------ Loops ------------------------------

\ `do` is like `if` in that it is also a compile-only word, though it uses `loop` as its
\ terminator.

: myloop ( -- ) 5 0 do cr ." Hello!" loop ; \ ok
test
\ Hello!
\ Hello!
\ Hello!
\ Hello!
\ Hello! ok

\ `do` expects two numbers before it: the end number and the index number, respectively.
\ (cr means carraige-return, essentially it a newline). This is equivalent to a for-loop
\ in other languages, with a definite number of times to loop.

\ So what if we want to get the value of the index as we loop? We use `i`.

: one-to-15 ( -- ) 15 0 do i . loop ;     \ ok
one-to-15                                 \ 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 ok
: squares ( -- ) 10 0 do i DUP * . loop ; \ ok
squares                                   \ 0 1 4 9 16 25 36 49 64 81 ok

\ Thidly, we can also change how large the step is between each loop iteration with `+loop`.
\ `+loop` reads the number on the top of the stack for how far to move each iteration.

: threes ( -- ) 15 0 do i . 3 +loop ; \ ok
threes                                \ 0 3 6 9 12 ok

\ Finally, while loops:

: death ( -- ) begin ." Are we there yet?" 0 until ;

\ Will print "Are we there yet?" forever. While loops are constructed in the format
\ of `begin` <stuff to do> <flag> `until`. The loop will run until flag is a
\ truthy value (not 0).

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
