---

language: Paren
filename: learnparen-de.paren
contributors:
  - ["KIM Taegyoon", "https://github.com/kimtg"]
  - ["Claudson Martins", "https://github.com/claudsonm"]
translators:
    - ["Dennis Keller", "https://github.com/denniskeller"]
lang: de-de
---

[Paren](https://bitbucket.org/ktg/paren) ist ein Dialekt von Lisp.
Es ist als eingebettete Sprache konzipiert.

Manche Beispiele sind von <http://learnxinyminutes.com/docs/racket/>.

```scheme
;;; Kommentare
# Kommentare

;; Einzeilige Kommentare starten mit einem Semikolon oder einem Hashtag

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Primitive Datentypen und Operatoren
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Zahlen
123 ; int
3.14 ; double
6.02e+23 ; double
(int 3.14) ; => 3 : int
(double 123) ; => 123 : double

;; Funktionsapplikationen werden so geschrieben: (f x y z ...)
;; Dabei ist f eine Funktion und x, y, z sind die Operatoren.
;; Wenn du eine Literalliste von Daten erstelllen möchtest,
;; verwende (quote) um zu verhindern, dass sie ausgewertet zu werden.
(quote (+ 1 2)) ; => (+ 1 2)
;; Nun einige arithmetische Operationen
(+ 1 1)  ; => 2
(- 8 1)  ; => 7
(* 10 2) ; => 20
(^ 2 3) ; => 8
(/ 5 2) ; => 2
(% 5 2) ; => 1
(/ 5.0 2) ; => 2.5

;;; Wahrheitswerte
true ; for Wahr
false ; for Falsch
(! true) ; => Falsch
(&& true false (prn "doesn't get here")) ; => Falsch
(|| false true (prn "doesn't get here")) ; => Wahr

;;; Zeichen sind Ints.
(char-at "A" 0) ; => 65
(chr 65) ; => "A"

;;; Zeichenketten sind ein Array von Zahlen mit fester Länge.
"Hello, world!"
"Benjamin \"Bugsy\" Siegel"   ; Backslash ist ein Escape-Zeichen
"Foo\tbar\r\n" ; beinhaltet C Escapes: \t \r \n

;; Zeichenketten können auch verbunden werden!
(strcat "Hello " "world!") ; => "Hello world!"

;; Eine Zeichenketten kann als Liste von Zeichen behandelt werden
(char-at "Apple" 0) ; => 65

;; Drucken ist ziemlich einfach
(pr "Ich bin" "Paren. ") (prn "Schön dich zu treffen!")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Variablen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Du kannst Variablen setzen indem du (set) verwedest
;; eine Variable kann alle Zeichen besitzen außer: ();#"
(set some-var 5) ; => 5
some-var ; => 5

;; Zugriff auf eine zuvor nicht zugewiesene Variable erzeugt eine Ausnahme
; x ; => Unknown variable: x : nil

;; Lokale Bindung: Verwende das Lambda Calculus! 'a' und 'b' 
;; sind nur zu '1' und '2' innerhalb von (fn ...) gebunden.
((fn (a b) (+ a b)) 1 2) ; => 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Sammlungen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Listen

;; Listen sind Vektrorartige Datenstrukturen. (Zufälliger Zugriff ist O(1).
(cons 1 (cons 2 (cons 3 (list)))) ; => (1 2 3)
;; 'list' ist ein komfortabler variadischer Konstruktor für Listen
(list 1 2 3) ; => (1 2 3)
;; und ein quote kann als literaler Listwert verwendet werden
(quote (+ 1 2)) ; => (+ 1 2)

;; Du kannst 'cons' verwenden um ein Element an den Anfang einer Liste hinzuzufügen.
(cons 0 (list 1 2 3)) ; => (0 1 2 3)

;; Listen sind ein sehr einfacher Typ, daher gibt es eine Vielzahl an Funktionen
;; für Sie. Ein paar Beispiele:
(map inc (list 1 2 3))          ; => (2 3 4)
(filter (fn (x) (== 0 (% x 2))) (list 1 2 3 4))    ; => (2 4)
(length (list 1 2 3 4))     ; => 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Funktionen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Verwende 'fn' um Funktionen zu erstellen.
;; eine Funktion gibt immer den Wert ihres letzten Ausdrucks zurück
(fn () "Hello World") ; => (fn () Hello World) : fn

;; Verwende Klammern um alle Funktionen aufzurufen, inklusive Lambda Ausdrücke
((fn () "Hello World")) ; => "Hello World"

;; Zuweisung einer Funktion zu einer Variablen
(set hello-world (fn () "Hello World"))
(hello-world) ; => "Hello World"

;; Du kannst dies mit syntaktischen Zucker für die Funktionsdefinition verkürzen:
(defn hello-world2 () "Hello World")

;; Die () von oben ist eine Liste von Argumente für die Funktion.
(set hello
  (fn (name)
    (strcat "Hello " name)))
(hello "Steve") ; => "Hello Steve"

;; ... oder gleichwertig, unter Verwendung mit syntaktischen Zucker:
(defn hello2 (name)
  (strcat "Hello " name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Gleichheit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Für Zahlen verwende '=='
(== 3 3.0) ; => wahr
(== 2 1) ; => falsch

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Kontrollfluss
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Bedingungen

(if true               ; test Ausdruck
    "this is true"   ; then Ausdruck
    "this is false") ; else Ausdruck
; => "this is true"

;;; Schleifen

;; for Schleifen ist für Zahlen 
;; (for SYMBOL START ENDE SCHRITT AUSDRUCK ..)
(for i 0 10 2 (pr i "")) ; => schreibt 0 2 4 6 8 10
(for i 0.0 10 2.5 (pr i "")) ; => schreibt 0 2.5 5 7.5 10

;; while Schleife
((fn (i)
  (while (< i 10)
    (pr i)
    (++ i))) 0) ; => schreibt 0123456789

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 6. Mutation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Verwende 'set' um einer Variablen oder einer Stelle einen neuen Wert zuzuweisen.
(set n 5) ; => 5
(set n (inc n)) ; => 6
n ; => 6
(set a (list 1 2)) ; => (1 2)
(set (nth 0 a) 3) ; => 3
a ; => (3 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 7. Makros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Makros erlauben es dir die Syntax der Sprache zu erweitern.
;; Parens Makros sind einfach.
;; Tatsächlich ist (defn) ein Makro.
(defmacro setfn (name ...) (set name (fn ...)))
(defmacro defn (name ...) (def name (fn ...)))

;; Lass uns eine Infix Notation hinzufügen
;; Let's add an infix notation
(defmacro infix (a op ...) (op a ...))
(infix 1 + 2 (infix 3 * 4)) ; => 15

;; Makros sind nicht hygenisch, Du kannst bestehende Variablen überschreiben!
;; Sie sind Codetransformationenen.
```
