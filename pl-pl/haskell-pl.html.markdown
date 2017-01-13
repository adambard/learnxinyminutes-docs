---
language: Haskell
lang: pl-pl
contributors:
    - ["Remigiusz Suwalski", "https://github.com/remigiusz-suwalski"]
---

Haskell został zaprojektowany jako praktyczy, czysto funkcyjny język 
programowania. Jest znany przede wszystkim ze względu na jego monady oraz system
typów, ale ja lubię do niego wracać przez jego elegancję. Sprawił on, że 
programowanie jest prawdziwą przyjemnością.

```haskell
-- Komentarze jednolinijkowe zaczynają się od dwóch myślników
{- Komentarze wielolinijkowe należy 
zamykać w bloki klamrami.
-}

----------------------------------------------------
-- 1. Podstawowe typy danych oraz operatory
----------------------------------------------------

-- Mamy liczby
3 -- 3

-- Podstawowe działania działają tak, jak powinny
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20
35 / 5 -- 7.0

-- dzielenie domyślnie zwraca ,,dokładny'' wynik
35 / 4 -- 8.75

-- dzielenie całkowitoliczbowe
35 `div` 4 -- 8

-- wartości logiczne także są podstawowym typem danych:
True
False

-- operacje logiczne: negacja oraz porównania
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- W powyższych przykładach, `not` jest funkcją przyjmującą jeden argument.
-- Haskell nie potrzebuje nawiasów, by wywołać funkcję: argumenty są po prostu
-- wypisywane jeden za drugim. Ogólnie wygląda to tak:
-- funkcja arg1 arg2 arg3...
-- Sekcja poświęcona funkcjom zawiera informacje, jak stworzyć własne.

-- Łańcuchy znaków (stringi) i pojedyncze znaki:
"To jest lancuch."
'a' -- znak
'Nie mozna laczyc apostrofow z lancuchami.' -- błąd!

-- Łańcuchy można sklejać
"Hello " ++ "world!" -- "Hello world!"

-- Łańcuch jest listą własnych znaków
['H', 'e', 'l', 'l', 'o'] -- "Hello"
"To jest lancuch" !! 0 -- 'T'

----------------------------------------------------
-- Listy oraz krotki
----------------------------------------------------

-- Wszystkie elementy listy muszą być tego samego typu.
-- Poniższe dwie listy są identyczne:
[1, 2, 3, 4, 5]
[1..5]

-- Zakresy są uniwersalne.
['A'..'F'] -- "ABCDEF"

-- Przy tworzeniu zakresów można określić krok.
[0,2..10] -- [0, 2, 4, 6, 8, 10]
[5..1] -- To nie zadziała, gdyż w Haskellu zakresy tworzone są domyślnie rosnąco
[5,4..1] -- [5, 4, 3, 2, 1]

-- indeksowanie listy od zera
[1..10] !! 3 -- 4

-- Można nawet tworzyć listy nieskończone!
[1..] -- lista wszystkich liczb naturalnych

-- Nieskończone listy mają prawo działać, ponieważ Haskell cechuje się leniwym 
-- wartościowaniem. To oznacza, że obliczane są jedynie te elementy listy, 
-- których istotnie potrzebujemy. Możemy poprosić o tysiączny element i 
-- dostaniemy go:

[1..] !! 999 -- 1000

-- Haskell wyznaczył pierwsze tysiąc elementów listy, ale cała jej reszta 
-- jeszcze nie istnieje! Nie zostanie obliczona ich wartość, póki nie zajdzie
-- taka potrzeba.

-- łączenie dwóch list
[1..5] ++ [6..10]

-- dodawanie pojedynczego elementu na początek listy
0:[1..5] -- [0, 1, 2, 3, 4, 5]

-- więcej operacji na listach
head [1..5] -- 1
tail [1..5] -- [2, 3, 4, 5]
init [1..5] -- [1, 2, 3, 4]
last [1..5] -- 5

-- list comprehensions
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

-- z dodatkowym warunkiem
[x*2 | x <- [1..5], x*2 > 4] -- [6, 8, 10]

-- każdy element krotki może być innego typu, jednak sama krotka musi być stałej 
-- długości. Przykładowo:
("haskell", 1)

-- dostęp do elementów pary (krotki długości 2):
fst ("haskell", 1) -- "haskell"
snd ("haskell", 1) -- 1

----------------------------------------------------
-- 3. Functions
----------------------------------------------------
-- A simple function that takes two variables
add a b = a + b

-- Note that if you are using ghci (the Haskell interpreter)
-- You'll need to use `let`, i.e.
-- let add a b = a + b

-- Using the function
add 1 2 -- 3

-- You can also put the function name between the two arguments
-- with backticks:
1 `add` 2 -- 3

-- You can also define functions that have no letters! This lets
-- you define your own operators! Here's an operator that does
-- integer division
(//) a b = a `div` b
35 // 4 -- 8

-- Guards: an easy way to do branching in functions
fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)

-- Pattern matching is similar. Here we have given three different
-- definitions for fib. Haskell will automatically call the first
-- function that matches the pattern of the value.
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

-- Pattern matching on tuples:
foo (x, y) = (x + 1, y + 2)

-- Pattern matching on lists. Here `x` is the first element
-- in the list, and `xs` is the rest of the list. We can write
-- our own map function:
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)

-- Anonymous functions are created with a backslash followed by
-- all the arguments.
myMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]

-- using fold (called `inject` in some languages) with an anonymous
-- function. foldl1 means fold left, and use the first value in the
-- list as the initial value for the accumulator.
foldl1 (\acc x -> acc + x) [1..5] -- 15

----------------------------------------------------
-- 4. Więcej funkcji
----------------------------------------------------

-- częściowe nakładanie: jeśli funkcja nie otrzyma wszystkich swoich argumentów, 
-- zostaje cześciowo nałożona - zwraca funkcję, która przyjmuje pozostałe,
-- brakujące argumenty.

add a b = a + b
foo = add 10 -- foo jest teraz funkcją, która przyjmuje liczbę, zwiększa ją o 10
foo 5 -- 15

-- Inny sposób na zapisanie tego samego:
foo = (10+)
foo 5 -- 15

-- składanie funkcji:
-- operator `.` składa wiele funkcji w jedną.
-- Dla przykładu, foo jest funkcją, która powiększa swój argument o 10, mnoży 
-- tak uzyskaną liczbę przez 4 i zwraca wynik:
foo = (4*) . (10+)

-- 4*(10 + 5) = 60
foo 5 -- 60

-- ustalanie kolejności
-- Haskell posiada inny operator, `$`, który nakłada funkcję do podanego
-- parametru. W przeciwieństwie do zwykłego lewostronnie łącznego nakładania 
-- funkcji, którego priorytet jest najwyższy (10), operator `$` posiada
-- priorytet 0 i jest prawostronnie łączny. Tak niski priorytet oznacza, że
-- wyrażenie po prawej traktowane jest jako parametr funkcji po lewej

-- wcześniej
even (fib 7) -- fałsz

-- równoważnie
even $ fib 7 -- fałsz

-- składanie funkcji
even . fib $ 7 -- fałsz


----------------------------------------------------
-- 5. Sygnatury typów
----------------------------------------------------

-- Haskell posiada wyjątkowo silny system typów, w którym każde poprawne 
-- wyrażenie ma swój typ.

-- Kilka podstawowych typów:
5 :: Integer
"hello" :: String
True :: Bool

-- Funkcje też są określonego typu.
-- `not` przyjmuje wartość logiczną i taką też zwraca:
-- not :: Bool -> Bool

-- Przykład funkcji przyjmującej dwa argumenty
-- add :: Integer -> Integer -> Integer

-- Dobrą praktyką podczas definiowania wartości jest napisanie nad nią
-- także jej typu:
double :: Integer -> Integer
double x = x * 2

----------------------------------------------------
-- 6. Wyrażenia warunkowe
----------------------------------------------------

-- wyrażenie warunkowe
haskell = if 1 == 1 then "wspaniale" else "paskudnie" -- haskell = "wspaniale"

-- wyrażenie warunkowe można rozbić na wiele linii, 
-- ale trzeba uważać na wcięcia w kodzie
haskell = if 1 == 1
            then "wspaniale"
            else "paskudnie"

-- rozpatrywanie przypadków: oto jak można parsować argumenty z linii poleceń:
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"

-- Haskell zastępuje pętle (których nie ma) rekurencyjnymi wywołaniami funkcji.
-- map aplikuje funkcję do każdego elementu listy:

map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- możesz zdefiniować funkcję for przy użyciu map:
for array func = map func array

-- a następnie użyć jej:
for [0..5] $ \i -> show i

-- mogliśmy użyć krótszego zapisu bez zmiany działania funkcji for:
for [0..5] show

-- Do redukcji listy służy polecenie foldl (foldr):
-- foldl <fn> <initial value> <list>
foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43

-- Jest to równoważne z:
(2 * (2 * (2 * 4 + 1) + 2) + 3)

-- foldl składa od od lewej strony, foldr od prawej
foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16

-- To zaś równoważne jest:
(2 * 1 + (2 * 2 + (2 * 3 + 4)))

----------------------------------------------------
-- 7. Typy danych
----------------------------------------------------

-- Oto jak tworzy się nowe typy danych w Haskellu:

data Color = Red | Blue | Green

-- Teraz można używać ich we własnych funkcjach:

say :: Color -> String
say Red = "You are Red!"
say Blue = "You are Blue!"
say Green =  "You are Green!"

-- Twoje typy danych mogą posiadać nawet parametry:

data Maybe a = Nothing | Just a

-- Wszystkie poniższe są typu Maybe
Just "hello"    -- typu `Maybe String`
Just 1          -- typu `Maybe Int`
Nothing         -- typu `Maybe a` for any `a`

----------------------------------------------------
-- 8. Haskell IO
----------------------------------------------------

-- Chociaż obsługa wejścia i wyjścia nie może zostać wyjaśniona przez poznaniem
-- monad, spróbujemy zrobić to częściowo

-- Wykonanie programu napisanego w Haskellu wywołuje funkcję `main` 
-- Musi zwrócić wartość typu `IO a` dla pewnego `a`. Przykład:

main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue)
-- putStrLn has type String -> IO ()

-- Najłatwiej obsłużyć wejście i wyjście, kiedy program zostanie 
-- zaimplementowany jako funkcja String -> String. Funkcja
--    interact :: (String -> String) -> IO ()
-- pobiera pewien tekst, wykonuje na nim operacje, po czym wypisuje wynik.

countLines :: String -> String
countLines = show . length . lines

main' = interact countLines

-- Możesz myśleć o wartości typu `IO ()` jako reprezentującej ciąg czynności,
-- które komputer ma wykonać, zupełnie niczym program komputerowy w imperatywnym
-- języku programowania. Akcje można łączyć przy użyciu notacji `do`:

sayHello :: IO ()
sayHello = do
   putStrLn "What is your name?"
   name <- getLine -- this gets a line and gives it the name "name"
   putStrLn $ "Hello, " ++ name

-- Ćwiczenie: napisz własną wersję `interact`, 
-- która czyta tylko jedną linię wejścia.

-- Kod w `sayHello` nigdy się nie wykona. Jedyną akcją, która zostanie 
-- uruchomiona, jest wartość `main`.
-- Aby uruchomić `sayHello`, należy zastąpić poprzednią definicję `main` przez
--   main = sayHello

-- Spróbujmy lepiej zrozumieć, jak działa funkcja `getLine`, której właśnie
-- użyliśmy. Jej typem jest
--    getLine :: IO String
-- Możesz myśleć o wartości typu `IO a` jako reprezentującej program, który
-- wygeneruje wartość typu `a`, poza wszystkim innym, co jeszcze zrobi.
-- Możemy także tworzyć własne akcje typu `IO String`:

action :: IO String
action = do
   putStrLn "This is a line. Duh"
   input1 <- getLine
   input2 <- getLine
   -- The type of the `do` statement is that of its last line.
   -- `return` is not a keyword, but merely a function
   return (input1 ++ "\n" ++ input2) -- return :: String -> IO String

-- Możemy użyć tego tak jak używaliśmy `getLine`:

main'' = do
    putStrLn "I will echo two lines!"
    result <- action
    putStrLn result
    putStrLn "This was all, folks!"

-- Typ `IO` jest przykładem monady. Sposób w jakim Haskell używa monad do 
-- obsługi wejścia i wyjścia pozwala mu być czysto funkcyjnym językiem.
-- Każda funkcja, która wchodzi w interakcje ze światem zewnętrznym, oznaczana
-- jest jako `IO` w jej sygnaturze typu, co umożliwia odróżnianie funkcji
-- czystych od zależnych od świata lub modyfikujących stan.

-- To naprawdę użyteczna własność, dzięki której jesteśmy w stanie uruchamiać
-- czyste funkcje jednocześnie.

----------------------------------------------------
-- 9. Interaktywne środowisko programowania
----------------------------------------------------

-- Aby uruchomić repl (read-eval-print loop, interaktywne środowisko), należy
-- wpisać `ghci`. Można już programować. Do definiowania nowych wartości służy
-- słowo kluczowe `let`:

let foo = 5

-- Do sprawdzania typów dowolnej wartości (wyrażenia) wykorzystuje się `:t`:

> :t foo
foo :: Integer

-- Działania takie jak `+`, `:` czy `$`, są funkcjami.
-- Przed sprawdzeniem ich typu należy otoczyć je nawiasami:

> :t (:)
(:) :: a -> [a] -> [a]

-- Dodatkowych informacji dostarcza `:i`:

> :i (+)
class Num a where
  (+) :: a -> a -> a
  ...
    -- Defined in ‘GHC.Num’
infixl 6 +

-- Można nawet wykonywać akcje typu `IO ()`!

> sayHello
What is your name?
Friend!
Hello, Friend!

```

Pominęliśmy wiele aspektów Haskella, wliczając w to monady. To właśnie one 
sprawiają, że programowanie w Haskellu sprawia tyle frajdy. Na zakończenie
pokażę Tobie implementację algorytmu quicksort w Haskellu:

```haskell
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs
```

Haskell może zostać zainstalowany na co najmniej dwa sposoby:
 - tradycyjnie [przy użyciu Cabala](http://www.haskell.org/platform/),
 - nowocześnie [z pomocą Stack](https://www.stackage.org/install).

Godnymi poleceniami wprowadzeniami są wspaniałe
[Learn you a Haskell](http://learnyouahaskell.com/) albo
[Real World Haskell](http://book.realworldhaskell.org/).
