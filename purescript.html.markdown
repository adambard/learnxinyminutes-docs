---
language: PureScript
filename: purescript.purs
contributors:
    - ["Fredrik Dyrkell", "http://www.lexicallyscoped.com"]
    - ["Thimoteus", "https://github.com/Thimoteus"]
---

PureScript is a small strongly, statically typed language compiling to JavaScript.

* Learn more at [https://www.purescript.org/](https://www.purescript.org/)
* Documentation: [https://pursuit.purescript.org/](https://pursuit.purescript.org/)
* Book: Purescript by Example, [https://book.purescript.org/](https://book.purescript.org/)

All the noncommented lines of code can be run in the PSCi REPL, though some
will require "paste" mode (`:paste` followed by multiple lines, terminated by
^D).

```haskell
--
-- 1. Primitive datatypes that corresponds to their JavaScript
-- equivalents at runtime.

import Prelude
-- Numbers
1.0 + 7.2*5.5 :: Number -- 40.6
-- Ints
1 + 2*5 :: Int -- 11
-- Types are inferred, so the following works fine
9.0/2.5 + 4.4 -- 8.0
-- But Ints and Numbers don't mix, so the following won't
5/2 + 2.5 -- Expression 2.5 does not have type Int
-- Hexadecimal literals
0xff + 1 -- 256
-- Unary negation
6 * -3 -- -18
6 * negate 3 -- -18
-- Modulus, from purescript-math (Math)
3.0 % 2.0 -- 1.0
4.0 % 2.0 -- 0.0
-- Inspect the type of an expression in psci
:t 9.5/2.5 + 4.4 -- Number

-- Booleans
true :: Boolean -- true
false :: Boolean -- false
-- Negation
not true -- false
23 == 23 -- true
1 /= 4 -- true
1 >= 4 -- false
-- Comparisons < <= > >=
-- are defined in terms of compare
compare 1 2 -- LT
compare 2 2 -- EQ
compare 3 2 -- GT
-- Conjunction and Disjunction
true && (9 >= 19 || 1 < 2) -- true

-- Strings
"Hello" :: String -- "Hello"
-- Multiline string without newlines, to run in PSCi use "paste" mode.
"Hello\
\orld" -- "Helloworld"
-- Multiline string with newlines
"""Hello
world""" -- "Hello\nworld"
-- Concatenate
"such " <> "amaze" -- "such amaze"

--
-- 2. Arrays are JavaScript arrays, but must be homogeneous

[1,1,2,3,5,8] :: Array Int -- [1,1,2,3,5,8]
[1.2,2.0,3.14] :: Array Number -- [1.2,2.0,3.14]
[true, true, false] :: Array Boolean -- [true,true,false]
-- [1,2, true, "false"] won't work
-- `Cannot unify Int with Boolean`

-- Requires purescript-arrays (Data.Array)
-- Cons (prepend)
1 : [2,4,3] -- [1,2,4,3]

-- and purescript-maybe (Data.Maybe)
-- Safe access return Maybe a
head [1,2,3] -- (Just 1)
tail [3,2,1] -- (Just [2,1])
init [1,2,3] -- (Just [1,2])
last [3,2,1] -- (Just 1)
-- Array access - indexing
[3,4,5,6,7] !! 2 -- (Just 5)
-- Range
1..5 -- [1,2,3,4,5]
length [2,2,2] -- 3
drop 3 [5,4,3,2,1] -- [2,1]
take 3 [5,4,3,2,1] -- [5,4,3]
append [1,2,3] [4,5,6] -- [1,2,3,4,5,6]

--
-- 3. Records are JavaScript objects, with zero or more fields, which
-- can have different types.
book = {title: "Foucault's pendulum", author: "Umberto Eco"}
-- Access properties
book.title -- "Foucault's pendulum"

getTitle b = b.title
-- Works on all records with a title (but doesn't require any other field)
getTitle book -- "Foucault's pendulum"
getTitle {title: "Weekend in Monaco", artist: "The Rippingtons"} -- "Weekend in Monaco"
-- Can use underscores as shorthand
_.title book -- "Foucault's pendulum"
-- Update a record
changeTitle b t = b {title = t}
getTitle (changeTitle book "Ill nome della rosa") -- "Ill nome della rosa"

--
-- 4. Functions
-- In PSCi's paste mode
sumOfSquares :: Int -> Int -> Int
sumOfSquares x y = x*x + y*y
sumOfSquares 3 4 -- 25

myMod x y = x % y
myMod 3.0 2.0 -- 1.0
-- Infix application of function
3 `mod` 2 -- 1

-- function application has higher precedence than all other
-- operators
sumOfSquares 3 4 * sumOfSquares 4 5 -- 1025

-- Conditional
abs' n = if n>=0 then n else -n
abs' (-3) -- 3

-- Guarded equations
-- In PSCi's paste mode
abs'' n | n >= 0    = n
        | otherwise = -n

-- Pattern matching

-- Note the type signature, input is a list of numbers. The pattern matching
-- destructures and binds the list into parts.
-- Requires purescript-lists (Data.List) and purescript-maybe (Data.Maybe)
first :: forall a. List a -> Maybe a
first (x : _) = Just x
first Nil = Nothing
first (fromFoldable [3,4,5]) -- (Just 3)

second :: forall a. List a -> Maybe a
second Nil = Nothing
second (_ : Nil) = Nothing
second (_ : (y : _)) = Just y
second (fromFoldable [3,4,5]) -- (Just 4)

-- Complementing patterns to match
-- Good ol' Fibonacci
fib 1 = 1
fib 2 = 2
fib x = fib (x-1) + fib (x-2)
fib 10 -- 89

-- Use underscore to match any, where you don't care about the binding name
isZero 0 = true
isZero _ = false
isZero 9 -- false

-- Pattern matching on records
ecoTitle {author: "Umberto Eco", title: t} = Just t
ecoTitle _ = Nothing

ecoTitle {title: "Foucault's pendulum", author: "Umberto Eco"} -- (Just "Foucault's pendulum")
ecoTitle {title: "The Quantum Thief", author: "Hannu Rajaniemi"} -- Nothing
-- ecoTitle requires both field to type check:
ecoTitle {title: "The Quantum Thief"} -- Object lacks required property "author"

-- Lambda expressions
(\x -> x*x) 3 -- 9
(\x y -> x*x + y*y) 4 5 -- 41
sqr = \x -> x*x

-- Currying
myAdd x y = x + y -- is equivalent with
myAdd' = \x -> \y -> x + y
add3 = myAdd 3
:t add3 -- Int -> Int

-- Forward and backward function composition
-- drop 3 followed by taking 5
(drop 3 >>> take 5) (1..20) -- [4,5,6,7,8]
-- take 5 followed by dropping 3
(drop 3 <<< take 5) (1..20) -- [4,5]

-- Operations using higher order functions
even x = x `mod` 2 == 0
filter even (1..10) -- [2,4,6,8,10]
map (\x -> x + 11) (1..5) -- [12,13,14,15,16]

-- Requires purescript-foldable-traversable (Data.Foldable)

foldr (+) 0 (1..10) -- 55
sum (1..10) -- 55
product (1..10) -- 3628800

-- Testing with predicate
any even [1,2,3] -- true
all even [1,2,3] -- false
```
