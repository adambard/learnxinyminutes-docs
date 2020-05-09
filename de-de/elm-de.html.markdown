---
language: Elm
filename: learnelm.elm
contributors:
  - ["Max Goldstein", "http://maxgoldste.in/"]
translators:
  - ["waynee95", "https://waynee95.me"]
lang: de-de
---

Elm ist eine pure funktionale Programmiersprache. Mit Elm werden GUIs
(grafische Benutzeroberfläche) für Webanwendungen erstellt. Durch die statische
Typisierung kann Elm viele Fehler schon bei der Kompilierung abfangen. Ein
Hauptmerkmal von Elm sind die ausführlichen und gut erklärten Fehlermeldungen.

```haskell
-- Einzeilige Kommentare beginnen mit 2 Bindestrichen.
{- So wird ein mehrzeiliger Kommentar angelegt.
{- Diese können auch verschachtelt werden. -}
-}

{-- Die Grundlagen --}

-- Arithmetik
1 + 1 -- 2
8 - 1 -- 7
10 * 2 -- 20

-- Zahlen ohne Punkt sind entweder vom Typ Int oder Float.
33 / 2 -- 16.5 mit Division von Gleitkommazahlen
33 // 2 -- 16  mit ganzzahliger Division

-- Exponenten
5 ^ 2 -- 25

-- Boolsche Werte
not True -- False
not False -- True
1 == 1 -- True
1 /= 1 -- False
1 < 10 -- True

-- Strings (Zeichenketten) und Zeichen
"Das hier ist ein String."
'a' -- Zeichen

-- Strings können konkateniert werden.
"Hello " ++ "world!" -- "Hello world!"

{-- Listen und Tupel --}

-- Jedes Element einer Liste muss vom gleichen Typ sein. Listen sind homogen.
["the", "quick", "brown", "fox"]
[1, 2, 3, 4, 5]
-- Das zweite Beispiel kann man auch mit Hilfe der "range" Funktion schreiben.
List.range 1 5

-- Listen werden genauso wie Strings konkateniert.
List.range 1 5 ++ List.range 6 10 == List.range 1 10 -- True

-- Mit dem "cons" Operator lässt sich ein Element an den Anfang einer Liste anfügen.
0 :: List.range 1 5 -- [0, 1, 2, 3, 4, 5]

-- Die Funktionen "head" und "tail" haben als Rückgabewert den "Maybe" Typ.
-- Dadurch wird die Fehlerbehandlung von fehlenden Elementen explizit, weil
-- man immer mit jedem möglichen Fall umgehen muss.
List.head (List.range 1 5) -- Just 1
List.tail (List.range 1 5) -- Just [2, 3, 4, 5]
List.head [] -- Nothing
-- List.funktionsName bedeutet, dass diese Funktion aus dem "List"-Modul stammt.

-- Tupel sind heterogen, jedes Element kann von einem anderen Typ sein.
-- Jedoch haben Tupel eine feste Länge.
("elm", 42)

-- Das Zugreifen auf Elemente eines Tupels geschieht mittels den Funktionen
-- "first" und "second".
Tuple.first ("elm", 42) -- "elm"
Tuple.second ("elm", 42) -- 42

-- Das leere Tupel, genannt "Unit",  wird manchmal als Platzhalter verwendet.
-- Es ist das einzige Element vom Typ "Unit".
()

{-- Kontrollfluss --}

-- Eine If-Bedingung hat immer einen Else-Zweig und beide Zweige müssen den
-- gleichen Typ haben.
if powerLevel > 9000 then
  "WHOA!"
else
  "meh"

-- If-Bedingungen können verkettet werden.
if n < 0 then
  "n is negative"
else if n > 0 then
  "n is positive"
else
  "n is zero"

-- Mit dem Mustervergleich (pattern matching) kann man bestimmte Fälle direkt
-- behandeln.
case aList of
  [] -> "matches the empty list"
  [x]-> "matches a list of exactly one item, " ++ toString x
  x::xs -> "matches a list of at least one item whose head is " ++ toString x
-- Mustervergleich geht immer von oben nach unten. Würde man [x] als letztes
-- platzieren, dann würde dieser Fall niemals getroffen werden, weil x:xs diesen
-- Fall schon mit einschließt (xs ist in dem Fall die leere Liste).

-- Mustervergleich an einem Maybe Typ.
case List.head aList of
  Just x -> "The head is " ++ toString x
  Nothing -> "The list was empty."

{-- Funktionen --}

-- Die Syntax für Funktionen in Elm ist minimal. Hier werden Leerzeichen anstelle
-- von runden oder geschweiften Klammern verwendet. Außerdem gibt es kein "return"
-- Keyword.

-- Eine Funktion wird durch ihren Namen, einer Liste von Parametern gefolgt von
-- einem Gleichheitszeichen und dem Funktionskörper angegeben.
multiply a b =
  a * b

-- Beim Aufruf der Funktion (auch Applikation genannt) werden die Argumente ohne
-- Komma übergeben.
multiply 7 6 -- 42

-- Partielle Applikation einer Funktion (Aufrufen einer Funktion mit fehlenden
-- Argumenten). Hierbei entsteht eine neue Funktion, der wir einen Namen geben.
double =
  multiply 2

-- Konstanten sind Funktionen ohne Parameter.
answer =
  42

-- Funktionen, die Funktionen als Parameter haben, nennt man Funktionen höherer
-- Ordnung. In funktionalen Programmiersprachen werden Funktionen als "first-class"
-- behandelt. Man kann sie als Argument übergeben, als Rückgabewert einer Funktion
-- zurückgeben oder einer Variable zuweisen.
List.map double (List.range 1 4) -- [2, 4, 6, 8]

-- Funktionen können auch als anonyme Funktion (Lambda-Funktionen) übergeben werden.
-- Diese werden mit einem Blackslash eingeleitet, gefolgt von allen Argumenten.
-- Die Funktion "\a -> a * 2" beschreibt die Funktion f(x) = x * 2.
List.map (\a -> a * 2) (List.range 1 4) -- [2, 4, 6, 8]

-- Mustervergleich kann auch in der Funktionsdefinition verwendet werden.
-- In diesem Fall hat die Funktion ein Tupel als Parameter. (Beachte: Hier
-- werden die Werte des Tupels direkt ausgepackt. Dadurch kann man auf die
-- Verwendung von "first" und "second" verzichten.)
area (width, height) =
  width * height

area (6, 7) -- 42

-- Mustervergleich auf Records macht man mit geschweiften Klammern.
-- Bezeichner (lokale Variablen) werden mittels dem "let" Keyword angelegt.
-- (Mehr zu Records weiter unten!)
volume {width, height, depth} =
  let
    area = width * height
  in
    area * depth

volume { width = 3, height = 2, depth = 7 } -- 42

-- Rekursive Funktion
fib n =
  if n < 2 then
    1
  else
    fib (n - 1) + fib (n - 2)

List.map fib (List.range 0 8) -- [1, 1, 2, 3, 5, 8, 13, 21, 34]

-- Noch eine rekursive Funktion (Nur ein Beispiel, verwende stattdessen immer
-- List.length!)
listLength aList =
  case aList of
    [] -> 0
    x::xs -> 1 + listLength xs

-- Funktionsapplikation hat die höchste Präzedenz, sie binden stärker als Operatoren.
-- Klammern bietet die Möglichkeit der Bevorrangung.
cos (degrees 30) ^ 2 + sin (degrees 30) ^ 2 -- 1
-- Als erstes wird die Funktion "degrees" mit dem Wert 30 aufgerufen.
-- Danach wird das Ergenis davon den Funktionen "cos", bzw. "sin" übergeben.
-- Dann wird das Ergebnis davon mit 2 quadriert und als letztes werden diese
-- beiden Werte dann addiert.

{-- Typen und Typ Annotationen --}

-- Durch Typinferenz kann der Compiler jeden Typ genau bestimmen. Man kann diese
-- aber auch manuell selber angeben (guter Stil!).
-- Typen beginnen immer mit eine Großbuchstaben. Dabei liest man "x : Typ" als
-- "x" ist vom Typ "Typ".
-- Hier ein paar übliche Typen:
5 : Int
6.7 : Float
"hello" : String
True : Bool

-- Funktionen haben ebenfalls einen Typ. Dabei ist der ganz rechte Typ der
-- Rückgabetyp der Funktion und alle anderen sind die Typen der Parameter.
not : Bool -> Bool
round : Float -> Int

-- Es ist guter Stil immer den Typ anzugeben, da diese eine Form von Dokumentation
-- sind. Außerdem kann so der Compiler genauere Fehlermeldungen geben.
double : Int -> Int
double x = x * 2

-- Funktionen als Parameter werden durch Klammern angegeben. Die folgende Funktion
-- ist nicht auf einen Typ festgelegt, sondern enthält Typvariablen (beginnend
-- mit Kleinbuchstaben). Die konkreten Typen werden erst bei Anwendung der
-- Funktion festgelegt. "List a" bedeutet, dass es sich um eine Liste mit
-- Elementen vom Typ "a" handelt.
List.map : (a -> b) -> List a -> List b

-- Es gibt drei spezielle kleingeschriebene Typen: "number", "comparable" und
-- "appendable".
add : number -> number -> number
add x y = x + y -- funktioniert mit Ints und Floats.

max :: comparable -> comparable -> comparable
max a b = if a > b then a else b -- funktioniert mit Typen, die vergleichbar sind.

append :: appendable -> appendable -> appendable
append xs ys = xs ++ ys -- funktioniert mit Typen, die konkatenierbar sind.

append "hello" "world"  -- "helloworld"
append [1,1,2] [3,5,8] -- [1,1,2,3,5,8]

{-- Eigene Datentypen erstellen --}

-- Ein "Record" ist ähnlich wie ein Tupel, nur das jedes Feld einen Namne hat.
-- Dabei spielt die Reihenfolge keine Rolle.
{ x = 3, y = 7 }

-- Um auf Werte eines Records zuzugreifen, benutzt man einen Punkt gefolgt
-- von dem Namen des Feldes.
{ x = 3, y = 7 }.x -- 3

-- Oder mit einer Zugriffsfunktion, welche aus einem Punkt und dem Feldnamen besteht.
.y { x = 3, y = 7 } -- 7

-- Wert eines Feldes ändern. (Achtung: Das Feld muss aber vorher schon vorhanden sein!)
{ person |
  name = "George" }

-- Mehrere Felder aufeinmal ändern unter Verwendung des alten Wertes.
{ particle |
  position = particle.position + particle.velocity,
  velocity = particle.velocity + particle.acceleration }

-- Du kannst ein Record auch als Typ Annotation verwenden.
-- (Beachte: Ein Record Typ benutzt einen Doppelpunkt und ein Record Wert benutzt
-- ein Gleichheitszeichen!)
origin : { x : Float, y : Float, z : Float }
origin =
  { x = 0, y = 0, z = 0 }

-- Durch das "type" Keyword kann man einem existierenden Typen einen Namen geben.
type alias Point3D =
  { x : Float, y : Float, z : Float }

-- Der Name kann dann als Konstruktor verwendet werden.
otherOrigin : Point3D
otherOrigin =
  Point3D 0 0 0

-- Aber es ist immernoch der selbe Typ, da es nur ein Alias ist!
origin == otherOrigin -- True

-- Neben den Records gibt es auch noch so genannte Summentypen.
-- Ein Summentyp hat mehrere Konstruktoren.
type Direction =
  North | South | East | West

-- Ein Konstruktor kann außerdem noch andere Typen enthalten. Rekursion ist
-- auch möglich.
type IntTree =
  Leaf | Node Int IntTree IntTree

-- Diese können auch als Typ Annotation verwendet werden.
root : IntTree
root =
  Node 7 Leaf Leaf

-- Außerdem können auch Typvariablen verwendet werden in einem Konstruktor.
type Tree a =
  Leaf | Node a (Tree a) (Tree a)

-- Beim Mustervergleich kann man auf die verschiedenen Konstruktoren matchen.
leftmostElement : Tree a -> Maybe a
leftmostElement tree =
  case tree of
    Leaf -> Nothing
    Node x Leaf _ -> Just x
    Node _ subtree _ -> leftmostElement subtree

{-- Module und Imports --}

-- Die Kernbibliotheken und andere Bibliotheken sind in Module aufgeteilt.
-- Für große Projekte können auch eigene Module erstellt werden.

-- Eine Modul beginnt mit ganz oben. Ohne diese Angabe befindet man sich
-- automatisch im Modul "Main".
module Name where

-- Ohne genaue Angabe von Exports wird alles exportiert. Es können aber alle
-- Exporte explizit angegeben werden.
module Name (MyType, myValue) where

-- Importiert das Modul "Dict". Jetzt kann man Funktionen mittels "Dict.insert"
-- aufrufen.
import Dict

-- Importiert das "Dict" Modul und den "Dict" Typ. Dadurch muss man nicht "Dict.Dict"
-- verwenden. Man kann trotzdem noch Funktionen des Moduls aufrufen, wie "Dict.insert".
import Dict exposing (Dict)

-- Abkürzung für den Modulnamen. Aufrufen der Funktionen mittels "C.funktionsName".
import Graphics.Collage as C

{-- Kommandozeilen Programme --}

-- Eine Elm-Datei kompilieren.
$ elm make MyFile.elm

-- Beim ersten Aufruf wird Elm die "core" Bibliotheken installieren und eine
-- "elm-package.json"-Datei anlegen, die alle Informationen des Projektes
-- speichert.

-- Der Reactor ist ein Server, welche alle Dateinen kompiliert und ausführt.
$ elm reactor

-- Starte das REPL (read-eval-print-loop).
$ elm repl

-- Bibliotheken werden durch den Github-Nutzernamen und ein Repository identifiziert.
-- Installieren einer neuen Bibliothek.
$ elm package install elm-lang/html
-- Diese wird der elm-package.json Datei hinzugefügt.

-- Zeigt alle Veränderungen zwischen zwei bestimmten Versionen an.
$ elm package diff elm-lang/html 1.1.0 2.0.0
-- Der Paketmanager von Elm erzwingt "semantic versioning"!
```

Elm ist eine besonders kleine Programmiersprache. Jetzt hast du genug Wissen an
deiner Seite, um dich in fast jedem Elm Code zurecht zu finden.

Noch ein paar weitere hilfreiche Ressourcen (in Englisch):

- Die [Elm Homepage](http://elm-lang.org/). Dort findest du:

  - [Anleitung zur Installierung von Elm](http://elm-lang.org/install)
  - [Dokumentation](http://elm-lang.org/docs), sowie eine [Referenz zur Syntax](http://elm-lang.org/docs/syntax)
  - Viele hilfreiche [Beispiele](http://elm-lang.org/examples)

- Dokumentation der [Elm Kernbibliotheken](http://package.elm-lang.org/packages/elm-lang/core/latest/). Insbesondere:

  - [Basics](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics) (standardmäßig importiert)
  - [Maybe](http://package.elm-lang.org/packages/elm-lang/core/latest/Maybe) sowie [Result](http://package.elm-lang.org/packages/elm-lang/core/latest/Result) (benutzt für Fehlerbehandlung)
  - Datenstrukturen, wie [List](http://package.elm-lang.org/packages/elm-lang/core/latest/List), [Array](http://package.elm-lang.org/packages/elm-lang/core/latest/Array), [Dict](http://package.elm-lang.org/packages/elm-lang/core/latest/Dict), und [Set](http://package.elm-lang.org/packages/elm-lang/core/latest/Set)
  - JSON [encoding](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Encode) und [decoding](http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode)

- [Die Elm Architektur](https://github.com/evancz/elm-architecture-tutorial#the-elm-architecture).

- Die [Elm mailing list](https://groups.google.com/forum/#!forum/elm-discuss).
