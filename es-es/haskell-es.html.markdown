---
language: Haskell
contributors:
    - ["Adit Bhargava", "http://adit.io"]
translators:
    - ["Jorge Antonio Atempa", "http://www.twitter.com/atempa09"]
filename: haskell-es.md
lang: es-es
---

Haskell fue diseñado como lenguaje de programación funcional práctico y puro. Es famoso por sus mónadas y su sistema de tipos, pero siempre regreso a él debido a su elegancia. Haskell hace la codificación una verdadera alegría para mí.

```haskell
-- Para comentar una sola línea utiliza dos guiones.
{- Para comentar múltiples líneas puedes encerrarlas
en un bloque como este.
-}

----------------------------------------------------
-- 1. Tipos de datos primitivos y Operadores
----------------------------------------------------

-- Tienes números a tu disposición
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
-- 2. Listas y Tuplas
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
-- resto de los elementos de esta lista "infinita" ¡no existen todavía! Haskell no lo hará
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

-- Listas por comprensión
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

-- Listas por comprensión utilizando condicionales
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

-- ¡También puedes definir funciones sin tener que utilizar letras! De este modo
-- ¡Tú defines tus propios operadores! Aquí esta un operador que realiza
-- una división entera
(//) a b = a `div` b
35 // 4 -- 8

-- Guardas: son una manera fácil para ramificar funciones
fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)

-- La coincidencia de patrones es similar. Aquí hemos dado tres diferentes
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
-- esta es "parcialmente aplicada". Esto significa que retorna una función que toma
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
-- 5. Firma de tipos
----------------------------------------------------

-- Haskell tiene un fuerte sistema de tipado, y cada cosa tiene una firma de tipo.

-- Algunos tipos básicos:
5 :: Integer
"hola" :: String
True :: Bool

-- Las funciones tienen muchos tipos.
-- `not` toma un booleano y devuelve un booleano:
-- not :: Bool -> Bool

-- Aquí, esta función toma dos argumentos:
-- add :: Integer -> Integer -> Integer

-- Cuando defines un valor, es una buena práctica escribir su tipo en una línea superior:
double :: Integer -> Integer
double x = x * 2

----------------------------------------------------
-- 6. Control de flujo y Expresiones If
----------------------------------------------------

-- expressiones if en una sola línea
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- expressiones if en múltiples líneas, la identación es importante
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- expressiones case: Aquí se muestra como analizar los argumentos 
-- desde línea de comandos
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"

-- Haskell no tiene ciclos; en lugar de esto utiliza recursión.
-- map aplica una función sobre cada elemento en un arreglo

map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- tú puedes crear una función utilizando map
for array func = map func array

-- y entonces utilizarla
for [0..5] $ \i -> show i

-- también podríamos haberlo escrito de esta manera:
for [0..5] show

-- Puedes utilizar foldl o foldr para reducir una lista
-- foldl <fn> <valor inicial> <lista>
foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43

-- Esto es lo mismo que
(2 * (2 * (2 * 4 + 1) + 2) + 3)

-- foldl es izquierda, foldr es derecha
foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16

-- Esto es los mismo que
(2 * 1 + (2 * 2 + (2 * 3 + 4)))

----------------------------------------------------
-- 7. Tipos de datos
----------------------------------------------------

-- Por ejemplo, para crear tu propio tipo de dato en Haskell

data Color = Rojo | Azul | Verde

-- Ahora puedes utilizarlo en una función:


say :: Color -> String
say Rojo = "¡Es Rojo!"
say Azul = "¡Es Azul!"
say Verde =  "¡Es Verde!"

-- Tus tipos de datos pueden tener parámetros también:

data Maybe a = Nothing | Just a

-- Estos son todos de tipo Maybe
Just "hello"    -- de tipo `Maybe String`
Just 1          -- de tipo `Maybe Int`
Nothing         -- de tipo `Maybe a` para cualquier `a`

----------------------------------------------------
-- 8. Haskell IO
----------------------------------------------------

-- Mientras que IO no puede ser explicado plenamente sin explicar las mónadas,
-- no es difícil explicar lo suficiente para ponerse en marcha.

-- Cuando un programa en Haskell se ejecuta, `main` es
-- llamado. Este debe devolver un valor de tipo `IO ()`. Por ejemplo:

main :: IO ()
main = putStrLn $ "¡Hola, cielo! " ++ (say Blue)
-- putStrLn tiene tipo String -> IO ()

-- Es más fácil de hacer IO si puedes implementar tu programa como
-- una función de String a String. La función
--    interact :: (String -> String) -> IO ()
-- recibe como entrada un texto,  ejecuta una función e imprime
-- una salida.

countLines :: String -> String
countLines = show . length . lines

main' = interact countLines

-- Puedes pensar en el valor de tipo `IO ()` como la representación 
-- de una secuencia de acciones que la computadora hace, al igual que
-- un programa escrito en un lenguaje imperativo. Podemos utilizar
-- la notación `do` para encadenar acciones. Por ejemplo:

sayHello :: IO ()
sayHello = do
   putStrLn "¿Cual es tu nombre?"
   name <- getLine -- obtenemos un valor y lo proporcionamos a "name"
   putStrLn $ "Hola, " ++ name

-- Ejercicio: escribe tu propia version de `interact` que solo lea 
--           una linea como entrada.

-- Nunca se ejecuta el código en `sayHello`, sin embargo. La única
-- acción que siempre se ejecuta es el valor de `main`.
-- Para ejecutar `sayHello` comenta la definición anterior de `main`
-- y sustituyela por:
--   main = sayHello

-- Vamos a entender mejor como funciona la función `getLine` cuando
-- la utilizamos. Su tipo es:
--    getLine :: IO String
-- Puedes pensar en el valor de tipo `IO a` como la representación
-- programa que generará un valor de tipo `a`
-- cuando es ejecutado (además de cualquier otra cosa que haga). Podemos
-- almacenar y reutilizar el valor usando `<-`. También podemos
-- crear nuestra propia acción de tipo `IO String`:

action :: IO String
action = do
   putStrLn "Esta es una linea."
   input1 <- getLine
   input2 <- getLine
   -- El tipo de la sentencia `do` es la de su última línea.
   -- `return` no es una palabra clave, sino simplemente una función
   return (input1 ++ "\n" ++ input2) -- return :: String -> IO String

-- Podemos usar esto sólo como usabamos `getLine`:

main'' = do
    putStrLn "¡Volveré a repetir dos líneas!"
    result <- action
    putStrLn result
    putStrLn "Esto es todo, ¡amigos!"

-- El tipo `IO` es un ejemplo de una "mónada". La forma en que Haskell utiliza una monada
-- permite que sea un lenguaje puramente funcional. Cualquier función que
-- interactue con el mundo exterior (por ejemplo usar IO) obtiene una marca `IO`
-- como su firma de tipo. Esto nos permite pensar qué funciones son "puras"
-- (que no interactuan con el mundo exterior o modifican el estado) y que funciones no lo son.

-- Esta es una poderosa característica, porque es una manera fácil de ejecutar funciones puras
-- concurrentemente; entonces, la concurrencia en Haskell es muy fácil.


----------------------------------------------------
-- 9. El interprete de comandos de Haskell
----------------------------------------------------

-- Para comenzar escribe desde la terminal `ghci`.
-- Ahora puede escribir código en Haskell. Para cualquier valor nuevo
-- que necesites crear utiliza `let`:

let foo = 5

-- Puedes inspeccionar el tipo de cualquier valor con `:t`:

>:t foo
foo :: Integer

-- Puedes ejecutar acciones de tipo `IO ()`

> sayHello
¿Cual es tu nombre?
Amigo
Hola, Amigo

```

Existe mucho más de Haskell, incluyendo clases de tipos y mónadas. Estas son
las grandes ideas que hacen a Haskell divertido. Te dejamos un ejemplo final
de Haskell: una implementación del algoritmo QuickSort:

```haskell
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs
```

Haskell es fácil de instalar. Obtenlo [aquí](http://www.haskell.org/platform/).

Usted puede encontrar más información en:
[Learn you a Haskell](http://learnyouahaskell.com/) o
[Real World Haskell](http://book.realworldhaskell.org/) o
[Aprende Haskell por el bien de todos](http://aprendehaskell.es/)
