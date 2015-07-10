---
language: elm
contributors:
    - ["Glen De Cauwsemaecker", "http://glendc.com"]
filename: learnelm.elm
---

[Elm](http://elm-lang.org) is a pure functional programming language for writing web-apps in a safe and intuitive way. Elm uses reactivity on top of the functional style, to provide a bridge between the app and the outside world, without giving up the advantages it has from being pure and functional.

You can experiment with the language via [http://elm-lang.org/try](http://elm-lang.org/try). The [syntax guide](http://elm-lang.org/docs/syntax) is a good start point to learn the language, and a lot of this guide is based on that one. If you're already familiar with Javascript, which is the language elm compiles to, you might want to read [http://elm-lang.org/docs/from-javascript](http://elm-lang.org/docs/from-javascript).

We only cover the basics here. After this you might want to learn more about Signals, which allow you to react on user input and other changes from the outside world. You can learn more about that [here](http://elm-lang.org/guide/reactivity#signals).

```elm
-- Single line comments start with two dashes.

{-- Multiple line comments are possible as well.
    Removing and re-adding the curly opening brace
    is a nice trick to quickly enable and disable
    a block of code.

    {- They also can be nested -}
--}

-- To use the elm repl use the `elm repl` command.
-- Compile your modules with the `elm-make` command.

-- Both should be in your path if you installed elm correctly.

{-----------------------------
----- Literals
-----------------------------}

-- Boolean
True  : Bool
False : Bool

42    : number  -- Int or Float depending on usage
3.14  : Float

'a'   : Char
"abc" : String

-- multi-line String
"""
This is useful for holding JSON or other
content that has "quotation marks".
"""

-- Typical manipulation of literals:

True && not (True || False)
(2 + 4) * (4^2 - 9)
"abc" ++ "def"

-- There are no symbols/atoms, as you have in
-- other languages.

-- Tuples that are stored contiguously in memory.
(1, 2, 3) -- tuple

-- There is a special function for creating tuples:
(,) 1 2              == (1,2)
(,,,) 1 True 'a' []  == (1,True,'a',[])

{-----------------------------
----- Maybe
-----------------------------}

-- Nil/Null is not possible in Elm, but has a special type instead.
-- A Maybe can help you with optional arguments
-- error handling, and records with optional fields.

-- Definition:
type Maybe a
 = Just a
 | Nothing

-- Examples:
age : Maybe Int
age = Just 42 -- she's 42

-- Elm is a pure language so mutation isn't possible
age = 43 -- not allowed due to ambiguity and immutability

-- Types of variables and functions can be inferred if not given explicitly.
santasAge = Nothing -- we don't know the age of Santa Claus

{-----------------------------
----- List
-----------------------------}

-- Elm has Lists as well, which is included as a core module.
[1,2,3] -- List Int

-- We can access the head and tail of a list as follows:
List.head [1,2,3] == Just 1
List.tail [1,2,3] == Just [2,3]

-- Here are four things that are equivalent:
[1..4]
[1,2,3,4]
1 :: [2,3,4]
1 :: 2 :: 3 :: 4 :: []

{-----------------------------
----- Conditionals
-----------------------------}

-- Simple if else statement, nothing you haven't seen before
if powerLevel > 9000 then "OVER 9000!!!" else "meh"

-- Elm comes however also with multi-way if-expressions:
if | key == 40 -> n+1
   | key == 38 -> n-1
   | otherwise -> n -- `otherwise` is optional but may give
                    -- runtime exceptions if omitted,
                    -- so really it shouldn't be optional

-- You can also have conditional behaviour based
-- on the structure of algebraic data types and literals
case maybe of
  Just xs -> xs
  Nothing -> []

case xs of
  hd::tl -> Just (hd,tl)
  []     -> Nothing

case n of
  0 -> 1
  1 -> 1
  _ -> fib (n-1) + fib (n-2)

-- Each pattern is indentation sensitive,
-- meaning that you have to align all of your patterns.

{-----------------------------
----- Union Types
-----------------------------}

-- Where you would use enumerations in other languages
-- you can use union types in Elm.

type List = Empty | Node Int List

-- The values have to be unique on a per-scope basis
-- Values can also contain subtypes as you can see above.
-- Union Types are quite useful for pattern matching
-- so you'll be using them quite a lot.

{-----------------------------
----- Records
-----------------------------}

-- Records allows you to define data type
-- Creating a record is easy
point = { x = 4, y = 2 }

-- Accessing members of a record is straightforward
point.x

-- Adding and removing fields is possible as well
{ point - x } == { y = 2 }
{ point | z = 12 } == { x = 4, y = 2, z = 12 }

-- Updating a field is the same as removing and re-adding the field
{ point - x | x = 6 } == { x = 6, y = 2 }

-- There is however a nicer way, using some syntactic sugar
{ point | x <- 6 }

-- Updating multiple fields at once is possible too
{ point | x <- point.x + 1
        , y <- point.y + 1 }

-- You might want to alias a record type,
-- in case you're using it in multiple locations
type alias Point = { x:Int, y:Int }

{-----------------------------
----- Type Aliases
-----------------------------}

-- When using a type in multiple places you might want to alias it as a different type.
-- This makes it easier to modify that type in the future.
-- You already saw this kind of aliasing for defining a record type.
-- Any type can be aliased however.
type alias Name = String
type alias Age = Int

-- From then on you can use it in type annotations of functions and variables
info : (Name,Age)
info = ("Steve", 28)


{-----------------------------
----- Functions
-----------------------------}

-- Defining a function is very similar to defining a variable.
-- Note that the function parameter `n` comes before the assignment operator.
square n = n ^ 2

-- Anonymous functions look very similar to normal functions.
square = \n -> n ^ 2

-- This can be handy in case you need a specialised function
-- to be applied to for example a List of elements
List.map (\n -> n ^ 2) [2, 4, 6] == [4, 16, 36]

-- In order to reduce parentheses usage you can use `|>` and `<|`.
-- These `Infix Operators` are alias for function application
-- Imagine for example that we want to create & transform a circle:

dot =
  scale 2 (move (20,20) (filled blue (circle 10)))

-- As you can see, parentheses add up quickly.
-- Therefore you can and should use the above mentioned `Infix Operators`
dot' =
  circle 10 -- creating a circle with a radius of 10
    |> filled blue -- filling that circle with the color blue
    |> move (20,20) -- moving the blue circle
    |> scale 2 -- making the circle twice as big

-- Each function result will always be used as the last argument of the next function.
-- This allows for powerful function composition in a readable way.
-- If you know Unix pipes, you'll feel right at home with this.

-- `let` can be used to create local variables within a (function) block
let n = 42
    (a,b) = (3,4)
    square n = n * n
in
    square a + square b

-- As you saw earlier, applying function is done as follows
sum a b = a + b
answerToLife = sum 40 2

{-----------------------------
----- Modules
-----------------------------}

-- Modules allow you to separate your code base into separate files and folders.
-- Module names must match their file name, so module Parser.Utils needs to be in file Parser/Utils.elm.

-- Defining a module is the first thing you'll do in a file
module MyModule where

-- Optionally, you can also choose to expose, instead of exposing everything
module MyModule (a, b) where

-- Importing modules can be done in a qualified way, as preferred.
import List -- List.map, List.foldl
import List as L -- L.map, L.foldl

-- However, open imports are possible too
import List exposing (..) -- map, foldl, concat, ...
import List exposing ( map, foldl ) -- map, foldl
```

## References

* [Syntax guide](http://elm-lang.org/docs/syntax) from [Elm webpage](http://elm-lang.org)
* [Elm Core Documentation](http://package.elm-lang.org/packages/elm-lang/core/2.1.0/)