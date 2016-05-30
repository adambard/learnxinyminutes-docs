---
language: Haskell
lang: de-de
contributors:
    - ["Adit Bhargava", "http://adit.io"]
translators:
    - ["Henrik Jürges", "https://github.com/santifa"]
    - ["Nikolai Weh", "http://weh.hamburg"]
filename: haskell-de.hs

---

Haskell wurde als praktische und funktionale Sprache entworfen.
Es ist berühmt für das Schema der Monaden und des Typsystems, aber
es sticht vor allem die Einfachheit und Eleganz hervor.

```haskell
-- Einfache Kommentare beginnen mit 2 Bindestriche.
{- So wird ein Kommentar
über mehrere Zeilen angelegt.
-}

----------------------------------------------------
-- 1. Primitive Datentypen und Operatoren
----------------------------------------------------

-- Normale Zahlen.
3 -- 3

-- Einfache Rechenoperationen.
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20
35 / 5 -- 7.0

-- Die Division ist per se auf Fließkommazahlen.
35 / 4 -- 8.75

-- Ganzzahlige Division
35 `div` 4 -- 8

-- Boolesche Werte sind Primitiven.
True
False

-- Logik Operationen
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- `not` ist eine Funktion die ein Argument entgegenimmt.
-- Haskell benötigt keine Klammern um Argumente.
-- Sie werden einfach aufgelistet: func arg1 arg2 arg3...
-- Wie man Funktionen definiert kommt weiter unten.


-- Strings und Zeichen
"Das ist ein String."
'a' -- Zeichen
'Einfache Anführungszeichen gehen nicht.' -- error!

-- Strings können konkateniert werden.
"Hello " ++ "world!" -- "Hello world!"

-- Ein String ist eine Liste von Zeichen.
['H', 'a', 'l', 'l', 'o', '!'] -- "Hallo!"
"Das ist eine String" !! 0 -- 'D'


----------------------------------------------------
-- Listen und Tupel
----------------------------------------------------

-- Jedes Element einer Liste muss vom gleichen Typ sein.
-- Zwei gleiche Listen
[1, 2, 3, 4, 5]
[1..5]

-- Die zweite Variante nennt sich die "range"-Syntax.
-- Ranges sind recht flexibel:
['A'..'F'] -- "ABCDEF"

-- Es ist möglich eine Schrittweite anzugeben:
[0,2..10] -- [0,2,4,6,8,10]
[5..1] -- [], da Haskell standardmässig inkrementiert.
[5,4..1] -- [5,4,3,2,1]

-- Der "!!"-Operator extrahiert das Element an einem bestimmten Index:
[1..10] !! 3 -- 4

-- Haskell unterstützt unendliche Listen!
[1..] -- Die Liste aller natürlichen Zahlen

-- Unendliche Listen funktionieren in Haskell, da es "lazy evaluation"
-- unterstützt. Haskell evaluiert erst etwas, wenn es benötigt wird.
-- Somit kannst du nach dem 1000. Element fragen und Haskell gibt es dir:

[1..] !! 999 -- 1000

-- Haskell evaluiert nun die ersten 1 - 1000 Elemente, aber der Rest der Liste
-- bleibt unangetastet. Haskell wird sie solange nicht weiterevaluieren
-- bis es muss.

-- Zwei Listen konkatenieren
[1..5] ++ [6..10]

-- Ein Element als Head hinzufügen
0:[1..5] -- [0, 1, 2, 3, 4, 5]

-- Weitere Listenoperationen
head [1..5] -- 1
tail [1..5] -- [2, 3, 4, 5]
init [1..5] -- [1, 2, 3, 4]
last [1..5] -- 5

-- Listen erschaffen ("list comprehensions")
[x*2 | x <- [1..5]] -- [2, 4, 6, 8, 10]

-- Mit Bedingungen
[x*2 | x <- [1..5], x*2 > 4] -- [6, 8, 10]

-- Tupel haben eine feste Länge, jedes Element darf aber ein anderen Typ haben.
-- Ein Tupel:
("haskell", 1)

-- Ein Paar (Pair) ist ein Tupel mit 2 Elementen, auf die man wie folgt
-- zugreifen kann:
fst ("haskell", 1) -- "haskell"
snd ("haskell", 1) -- 1

----------------------------------------------------
-- 3. Funktionen
----------------------------------------------------
-- Eine einfache Funktion die zwei Argumente hat.
add a b = a + b

-- Wenn man ghci (den Haskell Interpreter) benutzt, muss ein `let` davor.
-- let add a b = a + b

-- Eine Funktion aufrufen
add 1 2 -- 3

-- Man kann eine Funktion auch Infix verwenden,
-- wenn man sie mit backticks umgibt
1 `add` 2 -- 3

-- So sieht die Definition eines eigenen Operators aus.
-- Also einer Funktion deren Name aus Symbolen besteht.
-- Die Integer Division:
(//) a b = a `div` b
35 // 4 -- 8

-- Guards sind eine einfache Möglichkeit für Fallunterscheidungen.
fib x
  | x < 2 = 1
  | otherwise = fib (x - 1) + fib (x - 2)

-- Pattern Matching funktioniert ähnlich.
-- Hier sind drei Definitionen von fib. Haskell wird automatisch
-- die erste Funktionen nehmen die dem Pattern der Eingabe entspricht.
fib 1 = 1
fib 2 = 2
fib x = fib (x - 1) + fib (x - 2)

-- Pattern matching auf Tupeln:
foo (x, y) = (x + 1, y + 2)

-- Pattern matching auf Listen.
-- `x` ist das erste Element der Liste und `xs` der Rest der Liste.
-- Damit können wir unsere eigene map Funktion bauen:
myMap func [] = []
myMap func (x:xs) = func x:(myMap func xs)

-- Anonyme Funktionen (Lambda-Funktionen) werden mit einem
-- Backslash eingeleitet, gefolgt von allen Argumenten.
myMap (\x -> x + 2) [1..5] -- [3, 4, 5, 6, 7]

-- Fold (`inject` in einigen Sprachen)
-- Foldl1 bedeutet: fold von links nach rechts und nehme den ersten
-- Wert der Liste als Basiswert für den Akkumulator.
foldl1 (\acc x -> acc + x) [1..5] -- 15

----------------------------------------------------
-- 4. Mehr Funktionen
----------------------------------------------------

-- currying: Wenn man nicht alle Argumente an eine Funktion übergibt,
-- so wird sie eine neue Funktion gebildet ("curried").
-- Es findet eine partielle Applikation statt und die neue Funktion
-- nimmt die fehlenden Argumente auf.

add a b = a + b
foo = add 10 -- foo ist nun Funktion die ein Argument nimmt und 10 addiert
foo 5 -- 15

-- Ein alternativer Weg
foo = (+10)
foo 5 -- 15

-- Funktionskomposition
-- Die (.) Funktion verkettet Funktionen.
-- Zum Beispiel, die Funktion Foo nimmt ein Argument, addiert 10 dazu und
-- multipliziert dieses Ergebnis mit 4.
foo = (*4) . (+10)

-- (5 + 10) * 4 = 60
foo 5 -- 60


-- Haskell hat einen Operator `$`, welcher Funktionsapplikation durchführt.
-- Im Gegenzug zu der Standard-Funktionsapplikation, welche linksassoziativ ist
-- und die höchstmögliche Priorität von "10" hat, ist der `$`-Operator
-- rechtsassoziativ und hat die Priorität 0. Dieses hat (i.d.R.) den Effekt,
-- dass der `komplette` Ausdruck auf der rechten Seite als Parameter für die
-- Funktion auf der linken Seite verwendet wird.
-- Mit `.` und `$` kann man sich so viele Klammern ersparen.

(even (fib 7)) -- false

-- Äquivalent:
even $ fib 7 -- false

-- Funktionskomposition:
even . fib $ 7 -- false

----------------------------------------------------
-- 5. Typensystem
----------------------------------------------------

-- Haskell hat ein sehr starkes Typsystem.
-- Alles hat einen Typ und eine Typsignatur.

-- Einige grundlegende Typen:
5 :: Integer
"hello" :: String
True :: Bool

-- Funktionen haben genauso Typen.
-- `not` ist Funktion die ein Bool annimmt und ein Bool zurückgibt:
-- not :: Bool -> Bool

-- Eine Funktion die zwei Integer Argumente annimmt:
-- add :: Integer -> Integer -> Integer

-- Es ist guter Stil zu jeder Funktionsdefinition eine
-- Typdefinition darüber zu schreiben:
double :: Integer -> Integer
double x = x * 2

----------------------------------------------------
-- 6. If-Ausdrücke und Kontrollstrukturen
----------------------------------------------------

-- If-Ausdruck:
haskell = if 1 == 1 then "awesome" else "awful" -- haskell = "awesome"

-- If-Ausdrücke können auch über mehrere Zeilen verteilt sein.
-- Die Einrückung ist dabei wichtig.
haskell = if 1 == 1
            then "awesome"
            else "awful"

-- Case-Ausdruck: Am Beispiel vom Parsen von "commandline"-Argumenten.
case args of
  "help" -> printHelp
  "start" -> startProgram
  _ -> putStrLn "bad args"

-- Haskell nutzt Rekursion anstatt Schleifen.
-- map wendet eine Funktion auf jedes Element einer Liste an.

map (*2) [1..5] -- [2, 4, 6, 8, 10]

-- So kann man auch eine for-Funktion kreieren.
for array func = map func array

-- und so benutzt man sie:
for [0..5] $ \i -> show i

-- wir hätten sie auch so benutzen können:
for [0..5] show

-- foldl oder foldr reduziren Listen auf einen Wert.
-- foldl <Funktion> <initialer Wert> <Liste>
foldl (\x y -> 2*x + y) 4 [1,2,3] -- 43

-- die Abarbeitung sieht so aus:
(2 * (2 * (2 * 4 + 1) + 2) + 3)

-- foldl ist linksseitig und foldr rechtsseitig.
foldr (\x y -> 2*x + y) 4 [1,2,3] -- 16

-- die Abarbeitung sieht so aus:
(2 * 1 + (2 * 2 + (2 * 3 + 4)))

----------------------------------------------------
-- 7. Datentypen
----------------------------------------------------

-- So kann man seine eigenen Datentypen in Haskell anlegen:

data Color = Red | Blue | Green

-- Nun können wir sie in einer Funktion benutzen.

say :: Color -> String
say Red = "You are Red!"
say Blue = "You are Blue!"
say Green =  "You are Green!"

-- Datentypen können auch Parameter aufnehmen:

data Maybe a = Nothing | Just a

-- Diese sind alle vom Typ Maybe:
Just "hello"    -- vom Typ `Maybe String`
Just 1          -- vom Typ `Maybe Int`
Nothing         -- vom Typ `Maybe a` für jedes `a`

----------------------------------------------------
-- 8. Haskell IO
----------------------------------------------------

-- IO kann nicht völlig erklärt werden ohne Monaden zu erklären,
-- aber man kann die grundlegenden Dinge erklären.

-- Wenn eine Haskell Programm ausgeführt wird, so wird `main` aufgerufen.
-- Diese muss etwas vom Typ `IO ()` zurückgeben. Zum Beispiel:

main :: IO ()
main = putStrLn $ "Hello, sky! " ++ (say Blue)
-- putStrLn hat den Typ String -> IO ()

-- Es ist am einfachsten, wenn man sein Programm als Funktion von
-- String nach String implementiert.
-- Zum Beispiel die Funktion interact :: (String -> String) -> IO ()
-- nimmt einen Text, tut etwas damit und gibt diesen wieder aus.

countLines :: String -> String
countLines = show . length . lines

main' = interact countLines

-- Man kann den Typ `IO ()` als Repräsentation einer Sequenz von
-- Aktionen sehen, die der Computer abarbeiten muss.
-- Wie bei einem Programm das in einer Imperativen Sprache geschreiben wurde.
-- Mit der `do` Notation können Aktionen verbunden werden.

sayHello :: IO ()
sayHello = do
   putStrLn "What is your name?"
   name <- getLine -- eine Zeile wird geholt und
                   -- an die Variable "name" gebunden
   putStrLn $ "Hello, " ++ name

-- Übung: Schreibe deine eigene Version von `interact`,
-- die nur eine Zeile einliest.

-- `sayHello` wird niemals ausgeführt, nur `main` wird ausgeführt.
-- Um `sayHello` laufen zulassen kommentiere die Definition von `main`
-- aus und ersetze sie mit:
--     main = sayHello

-- Lass uns untersuchen wie `getLine` arbeitet.
-- Der Typ ist: getLine :: IO String
-- Man kann sich vorstellen das der Wert vom Typ `IO a` ein
-- Programm repräsentiert das etwas vom Typ `a` generiert.
-- Der Wert wird mit `<-` gespeichert und kann wieder benutzt werden.
-- Wir könne auch eigene Funktionen vom Typ `IO String` definieren:

action :: IO String
action = do
   putStrLn "This is a line. Duh"
   input1 <- getLine
   input2 <- getLine
   -- Der Typ von `do` ergibt sich aus der letzten Zeile.
   -- `return` ist eine Funktion und keine Schlüsselwort
   return (input1 ++ "\n" ++ input2) -- return :: String -> IO String

-- Nun können wir `action` wie `getLine` benutzen:

main'' = do
    putStrLn "I will echo two lines!"
    result <- action
    putStrLn result
    putStrLn "This was all, folks!"

-- Der Typ `IO` ist ein Beispiel für eine Monade.
-- Haskell benutzt Monaden Seiteneffekte zu kapseln und somit
-- eine rein funktional Sprache zu sein.
-- Jede Funktion die mit der Außenwelt interagiert (z.B. IO)
-- hat den Typ `IO` in seiner Signatur.
-- Damit kann man zwischen "reinen" Funktionen (interagieren nicht
-- mit der Außenwelt oder ändern ihren Zustand) und Anderen unterscheiden.

-- Nebenläufigkeit ist in Haskell sehr einfach, da reine Funktionen
-- leicht nebenläufig arbeiten können.

----------------------------------------------------
-- 9. Die Haskell REPL
----------------------------------------------------

-- Starte die REPL mit dem Befehl `ghci`
-- Nun kann man Haskell Code eingeben.
-- Alle neuen Werte müssen mit `let` gebunden werden:

let foo = 5

-- `:t` zeigt den Typen von jedem Wert an:

>:t foo
foo :: Integer

-- Auch jede `IO ()` Funktion kann ausgeführt werden.

> sayHello
What is your name?
Friend!
Hello, Friend!

```

Es gibt noch viel mehr in Haskell, wie zum Beispiel Typklassen und Monaden.
Dies sind die Ideen durch die Haskell Programmierung zum Spaß wird.
Mit dem folgenden kleinen Beispiel werde ich euch verlassen:
Quicksort in Haskell:

```haskell
qsort [] = []
qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater
    where lesser  = filter (< p) xs
          greater = filter (>= p) xs
```

Haskell ist sehr einfach zu installieren.
Hol es dir von [hier](http://www.haskell.org/platform/).

Eine sehr viele langsamere Einführung findest du unter:
[Learn you a Haskell](http://learnyouahaskell.com/) oder
[Real World Haskell](http://book.realworldhaskell.org/).
