---
language: purescript
contributors:
    - ["Fredrik Dyrkell", "http://www.lexicallyscoped.com"]
---

PureScript is a small strongly, statically typed language compiling to Javascript.

* Learn more at [http://www.purescript.org/](http://www.purescript.org/)
* Documentation: [http://docs.purescript.org/en/latest/](http://docs.purescript.org/en/latest/)
* Book: Purescript by Example, [https://leanpub.com/purescript/](https://leanpub.com/purescript/)

```haskell

--
-- 1. Primitive datatypes that corresponds to their Javascript
-- equivalents at runtime.

-- Numbers
1 + 7*5 :: Number -- 36
-- Types are inferred, so the following works fine
9 / 2.5 + 4.4 -- 8
-- Hexadecimal literals
0xff + 1 -- 256
-- Unary negation
6 * -3 -- -18
6 * negate 3 -- -18
-- Modulus
3 % 2 -- 1
4 % 2 -- 0
-- Inspect the type of an expression in psci
:t 9 / 2.5 + 4.4 -- Prim.Number

-- Booleans
true :: Boolean -- true
false :: Boolean -- false
-- Negation
not true --false
23 == 23 -- true
1 /= 4 -- true
1 >= 4 -- false
-- Comparisions < <= > >=
-- are defined in terms of compare
compare 1 2 -- LT
compare 2 2 -- EQ
compare 3 2 -- GT
-- Conjunction and Disjunction
true && (9 >= 19 || 1 < 2) -- true

-- Strings
"Hellow" :: String -- "Hellow"
-- Multiline string
"Hellow\
\orld" -- "Helloworld"
-- Concatenate
"such " ++ "amaze" -- "such amaze"

--
-- 2. Arrays are Javascript arrays, but must be homogeneous

[1,1,2,3,5,8] :: [Number] -- [1,1,2,3,5,8]
[true, true, false] :: [Boolean] -- [true,true,false]
-- [1,2, true, "false"] won't work
-- `Cannot unify Prim.Number with Prim.Boolean`
-- Cons (prepend)
1 : [2,4,3] -- [1,2,4,3]

-- Requires purescript-arrays (Data.Array)
-- and purescript-maybe (Data.Maybe)

-- Safe access return Maybe a
head [1,2,3] -- Just (1)
tail [3,2,1] -- Just ([2,1]) 
init [1,2,3] -- Just ([1,2])
last [3,2,1] -- Just (1)
-- Random access - indexing
[3,4,5,6,7] !! 2 -- Just (5)
-- Range 
1..5 -- [1,2,3,4,5]
length [2,2,2] -- 3
drop 3 [5,4,3,2,1] -- [2,1]
take 3 [5,4,3,2,1] -- [5,4,3]
append [1,2,3] [4,5,6] -- [1,2,3,4,5,6]

--
-- 3. Records are Javascript objects, with zero or more fields, which
-- can have different types
let book = {title: "Foucault's pendulum", author: "Umberto Eco"}
-- Access properties
book.title -- "Foucault's pendulum"

getTitle b = b.title
-- Works on all records with a title (but doesn't require any other field)
getTitle book -- "Foucault's pendulum"
getTitle {title: "Weekend in Monaco", artist: "The Rippingtons"} -- "Weekend in Monaco"
-- Update a record
changeTitle b t = b {title = t}
changeTitle book "Ill nome della rosa" -- {title: "Ill nome della
  -- rosa", author: "Umberto Eco"}

--
-- 4. Functions
sumOfSquares x y = x*x+y*y
sumOfSquares 3 4 -- 25
-- In psci you have to write `let` in front of the function to get a
-- top level binding
mod x y = x % y
mod 3 2 -- 1
-- Infix application of function
3 `mod` 2 -- 1

-- function application have higher precedence than all other
-- operators
sumOfSquares 3 4 * sumOfSquares 4 5 -- 1025

-- Conditional
abs' n = if n>=0 then n else -n
abs' (-3) -- 3

-- Guarded equations
abs n | n >= 0    = n
      | otherwise = -n

-- Pattern matching

-- Note the type signature, input is an array of numbers The pattern
-- matching destructures and binds the array into parts
first :: [Number] -> Number
first (x:_) = x
first [3,4,5] -- 3
second :: [Number] -> Number
second (_:y:_) = y 
second [3,4,5] -- 4
sumTwo :: [Number] -> [Number]
sumTwo (x:y:rest) = (x+y) : rest 
sumTwo [2,3,4,5,6] -- [5,4,5,6]

-- sumTwo doesn't handle when the array is empty or just have one
-- element in which case you get an error
sumTwo [1] -- Failed pattern match

-- Complementing patterns to match
-- Good ol' Fibonacci
fib 1 = 1
fib 2 = 2
fib x = fib (x-1) + fib (x-2)
fib 10 -- 89

-- Use underscore to match any, where you don't care about the binding name
isZero 0 = true
isZero _ = false

-- Pattern matching on records
ecoTitle {author = "Umberto Eco", title = t} = Just t
ecoTitle _ = Nothing

ecoTitle book -- Just ("Foucault's pendulum")
ecoTitle {title: "The Quantum Thief", author: "Hannu Rajaniemi"} -- Nothing
-- ecoTitle requires both field to type check:
ecoTitle {title: "The Quantum Thief"} -- Object does not have property author

-- Lambda expressions
(\x -> x*x) 3 -- 9
(\x y -> x*x + y*y) 4 5 -- 41 
sqr = \x -> x*x

-- Currying
add x y = x + y -- is equivalent with
add = \x -> (\y -> x+y)
add3 = add 3
:t add3 -- Prim.Number -> Prim.Number

-- Forward and backward function composition
-- drop 3 followed by taking 5
(drop 3 >>> take 5) (1..20) -- [4,5,6,7,8]
-- take 5 followed by dropping 3
(drop 3 <<< take 5) (1..20) -- [4,5]

-- Operations using higher order functions
even x = x % 2 == 0
filter even (1..10) -- [2,4,6,8,10]
map (\x -> x+11) (1..5) -- [12,13,14,15,16]

-- Requires purescript-foldable-traversabe (Data.Foldable)

foldr (+) 0 (1..10) -- 55
sum (1..10) -- 55
product (1..10) -- 3628800

-- Testing with predicate 
any even [1,2,3] -- true
all even [1,2,3] -- false

```

