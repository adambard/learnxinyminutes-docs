---
language: elm
contributors:
    - ["Glen De Cauwsemaecker", "http://glendc.com"]
filename: learnelm.elm
---

[Elm](http://elm-lang.org) is a pure functional programming language for writing web-apps in a safe and intuitive way. Elm uses reactivity on top of the functional style, to provide a bridge between the app and the outside world, without giving up the advantages it has from being pure and functional.

You can experiment with the language via [http://elm-lang.org/try](http://elm-lang.org/try). The [syntax guide](http://elm-lang.org/docs/syntax) is a good start point to learn the language, and a lot of this guide is based on that one. If you're already familiar with Javascript, which is the language elm compiles to, you might want to read [http://elm-lang.org/docs/from-javascript](http://elm-lang.org/docs/from-javascript).

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

-- Types of variables and functions can be infered if not given explictly.
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

-- Simple if else statement, nothing you havent seen before
if powerLevel > 9000 then "OVER 9000!!!" else "meh"

-- Elm comes however also with multi-way if-expressions:
if | key == 40 -> n+1
   | key == 38 -> n-1
   | otherwise -> n -- `otherwise` is optional but may give
                    -- runtime exceptions if ommited,
                    -- so really it shouldn't be optional

-- You can also have conditional behavior based
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
-- Union Types are quite usefull for pattern matching
-- so you'll be using them quite a lot.
```

## References

* [Syntax guide](http://elm-lang.org/docs/syntax) from [Elm webpage](http://elm-lang.org)
* [Elm Core Documentation](http://package.elm-lang.org/packages/elm-lang/core/2.1.0/)