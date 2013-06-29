---
language: haskell
author: Adit Bhargava
author_url: http://adit.io
---

Haskell was designed as a practical, purely functional programming language. It's famous for
it's monads and it's type system, but I keep coming back to it because of it's elegance. Haskell
makes coding a real joy for me.

```haskell
-- Single line comments start with two dashes.
{- Multiline comments can be enclosed
in a block like this.
-}

----------------------------------------------------
-- 1. Primitive Datatypes and Operators
----------------------------------------------------

-- You have numbers
3 -- 3

-- Math is what you would expect
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20
35 / 5 -- 7.0

-- Division is not integer division by default
35 / 4 -- 8.75

-- integer division
35 `div` 4 -- 8

-- Boolean values are primitives
True
False

-- Boolean operations
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- Strings and characters
"This is a string."
'a' -- character
'You cant use single quotes for strings.' -- error!

-- Strings can be added too!
"Hello " ++ "world!" #=> "Hello world!"

-- A string can be treated like a list of characters
"This is a string" !! 0 #=> 'T'


----------------------------------------------------
-- Lists and Tuples
----------------------------------------------------

-- Every element in a list must have the same type.
-- Two lists that are the same
[1, 2, 3, 4, 5]
[1..5]

-- You can also have infinite lists in Haskell!
[1..] -- a list of all the natural numbers

-- joining two lists
[1..5] ++ [6..10]

-- adding to the head of a list
0:[1..5] -- [0, 1, 2, 3, 4, 5]

-- indexing into a list

[0..] !! 5 -- 4

-- more list operations

head [1..5] -- 1
tail [1..5] -- [2, 3, 4, 5]
init [1..5] -- [1, 2, 3, 4]
last [1..5] -- 5

-- list comprehensions
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

-- with a conditional
[x*2 | x <- [1..5], x*2 > 4] # [6, 8, 10]

-- Every element in a tuple can be a different type, but a tuple has a fixed length.
-- A tuple:
("haskell", 1)

-- accessing elements of a tuple
fst ("haskell", 1) -- "haskell"
snd ("haskell", 1) -- 1

----------------------------------------------------
-- 3. Functions
----------------------------------------------------
-- A simple function that takes two variables
add a b = a + b

-- Using the function
add 1 2 -- 3

-- You can also put the function name between the two arguments with backticks:
1 `add` 2 -- 3

-- You can also define functions that have no characters! This lets you define
-- your own operators:

-- Here's an operator that does integer division
(//) a b = a `div` b
35 // 4 -- 8

-- Guards: an easy way to do branching in functions
fib x
  | x < 2 = x
  | otherwise = fib (x - 1) + fib (x - 2)

-- You can do the same thing with pattern matching.
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

-- So we have given three different definitions for fib.
-- Haskell will automatically call the first function that matches
-- the pattern of the value. 

-- Pattern matching on tuples:
foo (x, y) = (x + 1, y + 2)

-- Pattern matching on arrays. Here `x` is the first element
-- in the array, and `xs` is the rest of the array:
map func [x] = [func x]
map func (x:xs) = func x:(map func xs)

-- Anonymous functions are created with a backslash followed by all the arguments.
map (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]

-- using fold (called `inject` in some languages) with an anonymous function.
-- foldl1 means fold left, and use the first value in the array as the initial
-- value for the accumulator.
foldl1 (\acc x -> acc + x) [1..5] -- 15

----------------------------------------------------
-- 4. Some fancy things you can do with functions
----------------------------------------------------

-- currying: if you don't pass in all the arguments to a function,
it gets "curried". That means it returns a function that takes the 
rest of the arguments.

add a b = a + b
foo = add 10 -- foo is now a function that takes a number and adds 10 to it
foo 5 -- 15

-- Another way to write the same thing
foo = (+10)
foo 5 -- 15

-- function composition
the (.) function chains functions together.
For example, here foo is a function that takes a value. It adds 10 to it,
multiplies the result of that by 5, and then returns the final value.

foo = (*5) . (+10)

-- (5 + 10) * 5 = 75
foo 5 -- 75

-- fixing precedence
-- Haskell has another function called `$`. This changes the precedence
so that everything to the left of it gets computed first and then applied
to everything on the right. You can use `.` and `$` to get rid of a lot
of parentheses:

-- before
(even (double 7)) -- true

-- after
even . double $ 7 -- true

----------------------------------------------------
-- 5. Type signatures
----------------------------------------------------

Haskell has a very strong type system, and everything has a type signature.

Some basic types:
5 :: Integer
"hello" :: String
True :: Bool

Functions have types too.
Not takes a boolean and returns a boolean:
not :: Bool -> Bool

Here's a function that takes two arguments:
add :: Integer -> Integer -> Integer

----------------------------------------------------
-- 6. Control Flow
----------------------------------------------------

-- if statements
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- if statements can be on multiple lines too, indentation is important
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- case statements

-- Here's how you could parse command line arguments in Haskell

case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"


-- loops: recursion
-- Haskell doesn't have loops because it uses recursion instead.

-- map a function over every element in an array

map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- you can make a for function using map
for array func = map func array

-- and then use it

for [0..5] $ \i -> print i

----------------------------------------------------
-- 7. Data Types
----------------------------------------------------

-- Here's how you make your own data type in Haskell

data Color = Red | Blue | Green

Now you can use it in a function:

say :: Color -> IO String
say Red = putStrLn "You are Red!"
say Blue = putStrLn "You are Blue!"
say Green = putStrLn "You are Green!"

-- Your data types can have parameters too:

data Maybe a = Nothing | Just a

-- These are all of type Maybe
Nothing
Just "hello"
Just 1

----------------------------------------------------
-- 8. The Haskell REPL
----------------------------------------------------

-- Start the repl by typing `ghci`.
-- Now you can type in Haskell code. Any new values
-- need to be created with `let`:

let foo = 5

-- You can see the type of any value with `:t`:

>:t foo
foo :: Integer
```

There's a lot more to Haskell, including typeclasses and monads. These are the big ideas that make Haskell such fun to code in. I'll leave you with one final Haskell example: an implementation of quicksort in Haskell:

```haskell
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs
```
