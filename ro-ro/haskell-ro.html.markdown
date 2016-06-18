---
language: Haskell
contributors:
    - ["Adit Bhargava", "http://adit.io"]
translators:
    - ["Petru Dimitriu", "http://petru-dimitriu.github.io"]
lang: ro-ro
filename: haskell-ro.html
---

Haskell este un limbaj de programare practic, pur funcțional.

```haskell
-- Comentariile pe o singura linie incep cu 2 cratime.
{- Comentariile multilinie
  se scriu astfel.
-}

----------------------------------------------------
-- 1. Tipuri de date primitive si operatori
----------------------------------------------------

-- Exista numere
3 -- 3

-- Matematica functioneaza ca de obicei
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20
35 / 5 -- 7.0

-- Impartirea este cu virgula
35 / 4 -- 8.75

-- Impartirea cu rest
35 `div` 4 -- 8

-- Valorile booleene sunt primitive
True
False

-- Operatii logice
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- In exemplele de mai sus, `not` este o functie ce primeste o valoare.
-- In Haskell nu se pun paranteze pentru apelurile de functie. Toate
-- argumentele sunt insirate dupa numele functiei. Sablonul general este:
-- func arg1 arg2 arg3
-- Vedeti sectiunea despre functii pentru a afla cum sa scrieti propria functie.

-- Caractere si siruri de caractere
"Acesta este un sir de caractere"
'a' -- un caracter
'Nu se pot folosi apostroafe pentru siruri.' -- eroare!

-- Sirurile pot fi concatenate
"Hello " ++ "world!" -- "Hello world!"

-- Un string e de fapt o lista de caractere
['H', 'e', 'l', 'l', 'o'] -- "Hello"
"Acesta este un string" !! 0 -- 'A'


----------------------------------------------------
-- Liste si tupli
----------------------------------------------------

-- Fiecare element dintr-o lista trebuie sa aiba acelasi tip.
-- Urmatoarele liste sunt identice.
[1, 2, 3, 4, 5]
[1..5]

-- Intervalele sunt versatile.
['A'..'F'] -- "ABCDEF"

-- Se poate specifica un pas pentru intervale.
[0,2..10] -- [0, 2, 4, 6, 8, 10]
[5..1] -- Aceasta nu functioneaza deoarece pasul implicit este incrementarea.
[5,4..1] -- [5, 4, 3, 2, 1]

-- indexarea intr-o lista este de la zero
[1..10] !! 3 -- se obtine 4

-- Se pot crea liste infinite
[1..] -- lista tuturor numerelor naturale

-- Listele infinite functioneaza pentru ca Haskell foloseste "evaluare lenesa"
-- adica evalueaza lucrurile doar cand este nevoie de ele. Deci se poate
-- cere al 1000-lea element din lista infinita a numerelor naturale astfel:

[1..] !! 999 -- rezulta 1000

-- Haskell a evaluat elementele 1 - 1000 din lista... dar restul elementelor
-- acestei liste "infinite" nu exista inca! Haskell nu le va evalua pana
-- nu va fi nevoie de ele.

-- concatenarea a doua liste
[1..5] ++ [6..10]

-- alipirea la capul listei
0:[1..5] -- [0, 1, 2, 3, 4, 5]

-- operatii cu liste
head [1..5] -- 1
tail [1..5] -- [2, 3, 4, 5]
init [1..5] -- [1, 2, 3, 4]
last [1..5] -- 5

-- intelegerea listelor
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

-- folosind o conditie
[x*2 | x <- [1..5], x*2 > 4] -- [6, 8, 10]

-- Fiecare element dintr-un tuplu poate fi de un tip diferit
-- dar un tuplu are lungime fixa
-- Un tuplu:
("haskell", 1)

-- accesarea elementelor unui tuplu pereche
fst ("haskell", 1) -- "haskell" (first)
snd ("haskell", 1) -- 1 (second)

----------------------------------------------------
-- 3. Functii
----------------------------------------------------
-- O functie simpla ce sumeaza doua variabile
add a b = a + b

-- Aveti in vedere ca daca folositi ghci (interpretorul Haskell)
-- trebuie sa scrieti in fata si `let`, adica
-- let add a b = a + b

-- Apelarea functiei
add 1 2 -- rezulta 3

-- Numele functiei se poate pune si intre argumente
-- folosind apostrof intors:
1 `add` 2 -- 3

-- Se pot defini functii fara litere in denumire! Astfel se pot
-- defini noi operatori! De exemplu, iata un operator care realizeaza
-- impartirea intreaga
(//) a b = a `div` b
35 // 4 -- rezulta 8

-- Guards: o metoda usoara de a crea ramuri de executie
fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)

-- Potrivirea sirurilor se face similar. Aici am definit 3 definitii
-- pentru fib. Haskell o va alege automat pe prima care se potriveste
-- cu sablonul valorii.
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

-- Potrivirea in tupli:
foo (x, y) = (x + 1, y + 2)

-- Potrivirea in liste. Aici `x` este primul element al listei,
-- iar `xs` este restul litei. Putem scrie propria functie
-- de mapare
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)

-- Functiile anonime sunt create folosind un backslash urmat
-- de toate argumentele.
myMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]

-- utilizarea fold (denumit `inject` in alte limbaje) cu o functie
-- anonima. foldl1 inseamna pliere la stanga, folosind prima valoare
-- din lista drept valoarea initiala pentru acumulator
foldl1 (\acc x -> acc + x) [1..5] -- 15

----------------------------------------------------
-- 4. Mai multe functii
----------------------------------------------------

-- aplicare partiala; daca nu se introduc toate argumentele unei functii,
-- este "aplicata partial", adica returneaza o noua functie ce primeste
-- restul argumentelor, avand deja setate argumentele introduse

add a b = a + b
foo = add 10 -- foo este o functie ce primeste un numar si ii aduna 10
foo 5 -- 15

-- alta maniera de a scrie acelasi lucru
foo = (10+)
foo 5 -- 15

-- compunerea functiilor
-- operatorul `.` inlantuieste functiile.
-- De exeplu, aici foo este o functie care aduna 10 unui numar, il inmul
-- teste cu 4 si returneaza rezultatul calcului
foo = (4*) . (10+)

-- 4*(10 + 5) = 60
foo 5 -- 60

-- alterarea precedentei
-- Haskell detine un operator numit `$`. Acest operator aplica o functie
-- unui parametru dat. Fata de aplicarea standard a functiilor, care
-- foloseste prioritatea maxim posibila 10 si este asociativa la stanga,
-- operatorul `$` are prioritatea 0 si este asociativ la dreapta.
-- Aceasta inseamna ca expresia de la dreapta este aplicata ca parametru
-- functiei din stanga

-- inainte
even (fib 7) -- false

-- echivalent
even $ fib 7 -- false

-- compunerea functiilor
even . fib $ 7 -- false


----------------------------------------------------
-- 5. Type signatures
----------------------------------------------------

-- Haskell are un sistem de tipuri de date foarte puternic; fiecare expresie
-- valida are un tip.

-- Cateva tipuri de baza:
5 :: Integer
"hello" :: String
True :: Bool

-- Functiile au tipuri de asemenea.
-- `not` primeste un boolean si returneaza un boolean.
-- not :: Bool -> Bool

-- Iata o functie ce primeste doi intregi
-- add :: Integer -> Integer -> Integer

-- Cand se defineste o valoare, este bine sa se precizeze tipul ei deasupra.
double :: Integer -> Integer
double x = x * 2

---------------------------------------------------------
-- 6. Controlul executiei si instructiunile conditionale
---------------------------------------------------------

-- expresia conditionala if
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- cand expresiile sunt pe mai multe linii, este importanta indentarea
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- expresiile de tip case; iata cum se verifica argumentele programului
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"


-- Haskell nu foloseste cicluri, ci recursie
-- map aplica o functie fiecarui element dintr-o lista

map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- se poate face o functie for folosind map
for array func = map func array

-- si apoi se poate folosi astfel:
for [0..5] $ \i -> show i

-- se poate scrie si asa:
for [0..5] show

-- Se poate folosi foldl sau foldr pentru a reduce o lista
-- foldl <fn> <valoare initiala> <lista>
foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43

-- Acelasi lucru ca a scrie
(2 * (2 * (2 * 4 + 1) + 2) + 3)

-- foldl functioneaza spre stanga, foldr spre dreapta
foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16

-- Acealsi lucru ca:
(2 * 1 + (2 * 2 + (2 * 3 + 4)))

----------------------------------------------------
-- 7. Tipuri de date
----------------------------------------------------

-- Iata cum se creeaza un tip de date in Haskell

data Culoare = Rosu | Albastru | Verde

-- Acum poate fi folosit in functii


spune :: Culoare -> String
spune Rosu = "Esti Rosu!"
spune Albastru = "Esti Albastru!"
spune Verde =  "Esti Verde!"

-- Tipul de date poate avea si parametri.

data Maybe a = Nothing | Just a

-- Toate acestea sunt de tipul Maybe
Just "hello"    -- de tipul `Maybe String`
Just 1          -- de tipul `Maybe Int`
Nothing         -- de tipul `Maybe a` pentru oricare `a`

----------------------------------------------------
-- 8. IO in Haskell
----------------------------------------------------

-- Desi IO nu se poate explica intru totul fara a explica monadele,
-- nu este atat de greu de explicat pentru o idee de baza.

-- Cand se executa un program Haskell, se apeleaza `main`.
-- Trebuie sa returneze o valoare de tio `IO a` pentru un anumit tip `a`.
-- De exemplu:

main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue)
-- putStrLn are tipul String -> IO ()

-- Cel mai usor se lucreaza cu IO daca se implementeaza programul
-- ca o functie de la String la String. Functia
--    interact :: (String -> String) -> IO ()
-- citeste un text, executa o functie asupra ei, apoi afiseaza
-- iesirea.

countLines :: String -> String
countLines = show . length . lines

main' = interact countLines

-- O valoare de tipul `IO ()` poate fi privita ca reprezentand
-- o secventa de actiuni pe care care computerul sa le execute,
-- similar cu felul in care un program este scris intr-un limbaj
-- imperativ. Putem folosi notatia `do` pentru a inlantui actiunile.
-- De exemplu:

sayHello :: IO ()
sayHello = do
   putStrLn "What is your name?"
   name <- getLine -- citeste o linie
   putStrLn $ "Hello, " ++ name

-- Exercise: Scrieti propria functie `interact` care citeste
--           o singura linie de la intrare.


-- Codul din `sayHello` nu va fi niciodata executat. Singura actiunile
-- care este executata este valoarea lui `main`.
-- Pentru a rula `sayHello.`, eliminati definitia de mai sus a `main`.
-- si inlocuiti-o cu
--   main = sayHello

-- Sa intelegem mai bine cum functioneaza functia `getLine`.
-- Tipul ei este:
--    getLine :: IO String
-- Pueti privi o valoare de tipul `IO a` ca fiind un program
-- de computer care va genera o valoare de tipul `a` cand
-- este executata (pe langa orice altceva face). O putem denumi
-- si refolosi utilizand `<-`. De asemenea putem face propriile
-- actiuni te tipul `IO String`:

action :: IO String
action = do
   putStrLn "Aceasta e o linie."
   input1 <- getLine
   input2 <- getLine
   --Tipul instructiunii `do` este cel de pe ultima sa linie.
   -- `return` nu este un cuvant cheie, ci o functie
   return (input1 ++ "\n" ++ input2) -- return :: String -> IO String

-- Putem folosi aceasta exact cum am folosit `getLine`:

main'' = do
    putStrLn "I will echo two lines!"
    result <- action
    putStrLn result
    putStrLn "This was all, folks!"

-- Tipul `IO` este un exemplu de "monada". Felul in care Haskell foloseste
-- o monada pentru a realiza opeartii de intrare si iesire il face un limbaj
-- pur functional. Orice functie care interactioneaza cu exteriorul (adica
-- realieaza IO) este marcata ca `IO` in semnatura ei. Aceasta ne permite
-- sa spunem ce functii sunt "pure", adica nu interactioneaza cu exteriorul.

-- Aceasta este o facilitate foarte puternica, deoarece este usor sa
-- se ruleze functii pure concurent; asadar, concurenta in Haskell se face usor

----------------------------------------------------
-- 9. REPL in Haskell
----------------------------------------------------

-- Se porneste introducand `ghci`.
-- Dupa aceasta, se poate introduce cod Haskell.
-- Toate valorile noi trebuie precedate de `let`.

let foo = 5

-- Puteti vedea tipul oricarei valori sau expresii cu `:t`.

> :t foo
foo :: Integer

-- Operatorii, precum `+`, `:` si `$` sunt functii.
-- Tipul lor poate fi observat punand operatorii intre paranteze.

> :t (:)
(:) :: a -> [a] -> [a]

-- Se pot obtine informatii despre fiecare nume folosind `:i`

> :i (+)
class Num a where
  (+) :: a -> a -> a
  ...
    -- Defined in ‘GHC.Num’
infixl 6 +

--De asemenea se poate executa orice actiune de tipul `IO ()`

> sayHello
What is your name?
Friend!
Hello, Friend!

```
Mai sunt multe de spus despre Haskell, printre care typclasses și monade.
Acestea sunt marile idei care fac programarea în Haskell atât de interesantă.
Vă las un exemplu final în Haskell: o variantă de implementare a sortării rapide
(quicksort) în Haskell:

```haskell
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs
```

Există două maniere populare de a instala Haskell: prin [instalarea bazată pe Cabal](http://www.haskell.org/platform/), și prin mai noul [proces bazat pe Stack](https://www.stackage.org/install).

Se poate găsi o introducere în Haskell mult mai blândă la adresele
[Learn you a Haskell](http://learnyouahaskell.com/) sau
[Real World Haskell](http://book.realworldhaskell.org/).
