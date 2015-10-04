---
language: Haskell
contributors:
    - ["Adit Bhargava", "http://adit.io"]
translators:
    - ["Jorge Antonio Atempa", "http://www.twitter.com/atempa09"]
lang: es-es
---

Haskell fue diseñado como lenguaje de programación funcional práctico y puro. Es famoso por sus mónadas y su sistema de tipos, pero regreso a él debido a su elegancia. Haskell hace la codificación una verdadera alegría para mí.

```haskell
-- Para comentar una sola línea utiliza dos guiones.
{- Para comentar múltiples líneas puedes encerrarlas
en un bloque como este.
-}

----------------------------------------------------
-- 1. Tipos de datos primitivos y Operadores
----------------------------------------------------

-- Cuentas con números
3 -- 3

-- Matématicas, es lo que esperas
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20
35 / 5 -- 7.0

-- Por defecto la división no devuelve un entero
35 / 4 -- 8.75

-- Para la división entera utiliza
35 `div` 4 -- 8

-- Valores booleanos
True
False

-- Operaciones booleanas
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- En los ejemplos superiores, `not` es una función que toma un valor.
-- Haskell no necesita paréntisis para las llamadas a funciones...todos los argumentos
-- son enlistados después de la función. Entonces el patrón general es:
-- func arg1 arg2 arg3...
-- Observa la sección de funciones para obtener información de como escribir tu propia función.

-- Cadenas y caracteres
"Esto es una cadena."
'a' -- caracter
'No puedes utilizar comillas simples para cadenas.' -- ¡error!

-- Concatenación de cadenas
"¡Hola " ++ "mundo!" -- "¡Hola mundo!"

-- Una cadena es una lista de caracteres
['H', 'o', 'l', 'a'] -- "Hola"
"Esto es una cadena" !! 0 -- 'E'


----------------------------------------------------
-- Listas y Tuplas
----------------------------------------------------

-- Cada elemento en una lista debe ser del mismo tipo.
-- Estas dos listas son iguales:
[1, 2, 3, 4, 5]
[1..5]

-- Los rangos son versátiles.
['A'..'F'] -- "ABCDEF"

-- Puedes crear un paso en un rango.
[0,2..10] -- [0, 2, 4, 6, 8, 10]
[5..1] -- Esto no funciona debido a que Haskell incrementa por defecto.
[5,4..1] -- [5, 4, 3, 2, 1]

-- indexación en una lista
[0..] !! 5 -- 5

-- También tienes listas infinitas en Haskell!
[1..] -- una lista de todos los números naturales

-- Las listas infinitas funcionan porque Haskell tiene "lazy evaluation". Esto significa
-- que Haskell solo evalúa las cosas cuando lo necesita. Así que puedes pedir
-- el elemento 1000 de tú lista y Haskell te devolverá:

[1..] !! 999 -- 1000

-- Y ahora Haskell ha evaluado elementos 1 - 1000 de esta lista...pero el
-- resto de los elementos de esta lista "infinita" !no existen todavía! Haskell no lo hará
-- en realidad los evalúa hasta que los necesita.

-- uniendo dos listas
[1..5] ++ [6..10]

-- añadiendo a la cabeza de la lista
0:[1..5] -- [0, 1, 2, 3, 4, 5]

-- más operaciones con listas
head [1..5] -- 1
tail [1..5] -- [2, 3, 4, 5]
init [1..5] -- [1, 2, 3, 4]
last [1..5] -- 5

-- listas intencionales
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

-- con condiciones
[x*2 | x <- [1..5], x*2 > 4] -- [6, 8, 10]

-- Cada elemento en una tupla puede ser de diferente tipo, pero una tupla tiene
-- longitud fija.
-- Ejemplo de una tupla:
("haskell", 1)

-- acceder a los elementos (por ejemplo una tupla de longitud 2)
fst ("haskell", 1) -- "haskell"
snd ("haskell", 1) -- 1

----------------------------------------------------
-- 3. Funciones
----------------------------------------------------
-- Una función simple que recibe dos variables
add a b = a + b

-- Nota: Si estas utilizando ghci (el interprete de Haskell)
-- Necesitas utilizar `let`, por ejemplo
-- let add a b = a + b

-- Utilizando la función
add 1 2 -- 3

-- También puedes llamar a la función enmedio de dos argumentos
-- con acentos abiertos:
1 `add` 2 -- 3

-- !También puedes definir las funciones que no tienen letras! De este modo
-- !Tú defines tus propios operadores! Aquí esta un operador que realiza
-- una división entera
(//) a b = a `div` b
35 // 4 -- 8

-- Guardas: son una manera fácil para ramificar funciones
fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)

-- Coincidencia de patrones es similar. Aquí hemos dado tres diferentes
-- definiciones para fib. Haskell llamará automáticamente la primer
-- función que coincide con el patrón del valor.
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

-- Coincidencia de patrones en tuplas:
foo (x, y) = (x + 1, y + 2)

-- Coincidencia de patrones en listas. Aquí `x` es el primer elemento
-- en una lista, y `xs` es el resto de la lista. Podemos escribir
-- nuestra propia función map:
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)

-- Funciones anónimas son creadas con una diagonal invertida seguido de
-- todos los argumentos.
myMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]

-- utilizando pliegues (llamado `inject` en algunos lenguajes) con una función
-- anónima. foldl1 significa pliegue por la izquierda, y usa el primer valor 
-- en la lista como el valor inicial para el acumulador.
foldl1 (\acc x -> acc + x) [1..5] -- 15

----------------------------------------------------
-- 4. Más funciones
----------------------------------------------------

-- aplicación parcial: si no quieres pasar todos los argumentos a una función,
-- esta es "parcialmente aplicada". Es decir esta retorna una función que toma
-- el resto de los argumentos.

add a b = a + b
foo = add 10 -- foo es actualmente una función que toma un número y suma 10 a esta
foo 5 -- 15

-- Otra manera de escribir los mismo
foo = (+10)
foo 5 -- 15

-- composición de funciones
-- el (.) encadena funciones.
-- Por ejemplo, aquí foo es una función que toma un valor. Y se le suma 10,
-- posteriormente multiplica el resultado por 5, y devuelve el resultado final.
foo = (*5) . (+10)

-- (5 + 10) * 5 = 75
foo 5 -- 75

-- fijación de precedencia 
-- Haskell tiene otro operador llamado `$`. Este operador aplica a una función 
-- para un parámetro dado. En contraste a la aplicación de función estándar,  
-- la cúal tiene prioridad más alta posible de 10 y es asociativa por la izquierda, 
-- el operador `$` tiene prioridad de 0 y es asociativa por la derecha. Tal que
-- una baja prioridad significa que la expresión a su derecha es aplicada como parámetro a la función a su izquierda.

-- antes
even (fib 7) -- false

-- equivalentemente
even $ fib 7 -- false

-- composición de funciones
even . fib $ 7 -- false


----------------------------------------------------
-- 5. Type signatures
----------------------------------------------------

-- Haskell has a very strong type system, and everything has a type signature.

-- Some basic types:
5 :: Integer
"hello" :: String
True :: Bool

-- Functions have types too.
-- `not` takes a boolean and returns a boolean:
-- not :: Bool -> Bool

-- Here's a function that takes two arguments:
-- add :: Integer -> Integer -> Integer

-- When you define a value, it's good practice to write its type above it:
double :: Integer -> Integer
double x = x * 2

----------------------------------------------------
-- 6. Control Flow and If Expressions
----------------------------------------------------

-- if expressions
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- if expressions can be on multiple lines too, indentation is important
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- case expressions: Here's how you could parse command line arguments
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"

-- Haskell doesn't have loops; it uses recursion instead.
-- map applies a function over every element in an array

map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- you can make a for function using map
for array func = map func array

-- and then use it
for [0..5] $ \i -> show i

-- we could've written that like this too:
for [0..5] show

-- You can use foldl or foldr to reduce a list
-- foldl <fn> <initial value> <list>
foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43

-- This is the same as
(2 * (2 * (2 * 4 + 1) + 2) + 3)

-- foldl is left-handed, foldr is right-
foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16

-- This is now the same as
(2 * 1 + (2 * 2 + (2 * 3 + 4)))

----------------------------------------------------
-- 7. Data Types
----------------------------------------------------

-- Here's how you make your own data type in Haskell

data Color = Red | Blue | Green

-- Now you can use it in a function:


say :: Color -> String
say Red = "You are Red!"
say Blue = "You are Blue!"
say Green =  "You are Green!"

-- Your data types can have parameters too:

data Maybe a = Nothing | Just a

-- These are all of type Maybe
Just "hello"    -- of type `Maybe String`
Just 1          -- of type `Maybe Int`
Nothing         -- of type `Maybe a` for any `a`

----------------------------------------------------
-- 8. Haskell IO
----------------------------------------------------

-- While IO can't be explained fully without explaining monads,
-- it is not hard to explain enough to get going.

-- When a Haskell program is executed, `main` is
-- called. It must return a value of type `IO ()`. For example:

main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue)
-- putStrLn has type String -> IO ()

-- It is easiest to do IO if you can implement your program as
-- a function from String to String. The function
--    interact :: (String -> String) -> IO ()
-- inputs some text, runs a function on it, and prints out the
-- output.

countLines :: String -> String
countLines = show . length . lines

main' = interact countLines

-- You can think of a value of type `IO ()` as representing a
-- sequence of actions for the computer to do, much like a
-- computer program written in an imperative language. We can use
-- the `do` notation to chain actions together. For example:

sayHello :: IO ()
sayHello = do
   putStrLn "What is your name?"
   name <- getLine -- this gets a line and gives it the name "name"
   putStrLn $ "Hello, " ++ name

-- Exercise: write your own version of `interact` that only reads
--           one line of input.

-- The code in `sayHello` will never be executed, however. The only
-- action that ever gets executed is the value of `main`.
-- To run `sayHello` comment out the above definition of `main`
-- and replace it with:
--   main = sayHello

-- Let's understand better how the function `getLine` we just
-- used works. Its type is:
--    getLine :: IO String
-- You can think of a value of type `IO a` as representing a
-- computer program that will generate a value of type `a`
-- when executed (in addition to anything else it does). We can
-- store and reuse this value using `<-`. We can also
-- make our own action of type `IO String`:

action :: IO String
action = do
   putStrLn "This is a line. Duh"
   input1 <- getLine
   input2 <- getLine
   -- The type of the `do` statement is that of its last line.
   -- `return` is not a keyword, but merely a function
   return (input1 ++ "\n" ++ input2) -- return :: String -> IO String

-- We can use this just like we used `getLine`:

main'' = do
    putStrLn "I will echo two lines!"
    result <- action
    putStrLn result
    putStrLn "This was all, folks!"

-- The type `IO` is an example of a "monad". The way Haskell uses a monad to
-- do IO allows it to be a purely functional language. Any function that
-- interacts with the outside world (i.e. does IO) gets marked as `IO` in its
-- type signature. This lets us reason about what functions are "pure" (don't
-- interact with the outside world or modify state) and what functions aren't.

-- This is a powerful feature, because it's easy to run pure functions
-- concurrently; so, concurrency in Haskell is very easy.


----------------------------------------------------
-- 9. The Haskell REPL
----------------------------------------------------

-- Start the repl by typing `ghci`.
-- Now you can type in Haskell code. Any new values
-- need to be created with `let`:

let foo = 5

-- You can see the type of any value with `:t`:

>:t foo
foo :: Integer

-- You can also run any action of type `IO ()`

> sayHello
What is your name?
Friend!
Hello, Friend!

```

There's a lot more to Haskell, including typeclasses and monads. These are the
big ideas that make Haskell such fun to code in. I'll leave you with one final
Haskell example: an implementation of quicksort in Haskell:

```haskell
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs
```

Haskell is easy to install. Get it [here](http://www.haskell.org/platform/).

You can find a much gentler introduction from the excellent
[Learn you a Haskell](http://learnyouahaskell.com/) or
[Real World Haskell](http://book.realworldhaskell.org/).
