---
language: brainfuck
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
---

Brainfuck ist eine extrem minimalistische Turing-vollständige Programmiersprache
mit lediglich 8 Befehlen.

Mit dem [brainfuck-visualizer](http://fatiherikli.github.io/brainfuck-visualizer/) kann
Brainfuck im Browser ausprobiert werden.

```
Alle Zeichen außer "><+-.,[]" (ohne die Klammern) werden ignoriert.

Brainfuck besteht aus einem Array mit unendlich vielen Elementen, die alle mit Null initalisiert
sind und einem Datenzeiger auf das aktuelle Element.

Es gibt acht Befehle:
+ : Erhöht den Wert an der aktuellen Stelle um Eins.
- : Verringert den Wert an der aktuellen Stelle um Eins.
> : Bewegt den Zeiger um eine Stelle weiter.
< : Bewegt den Zeiger um eine Stelle zurück.
. : Gibt den Wert der aktuellen Zelle als ASCII Wert aus (z.B. 65 = 'A').
, : Liest ein einzelnes Zeichen von der Standardeingabe und speichert dessen ASCII Wert in der aktuellen Zelle.
[ : Wenn der Wert des aktuellen Elements Null ist, bewege des Zeiger hinter den
    zugehörigen ]-Befehl.
    Ansonsten, bewege den Zeiger ein Element weiter.
] : Wenn der Wert des aktuellen Elements Null ist, bewege des Zeiger um eine Stelle
    weiter.
    Ansonsten, bewege den Zeiger hinter den zugehörigen [-Befehl.

[ und ] bilden eine while-Schleife. Offensichtlich müssen sie paarweise vorkommen.

Schauen wir uns einige grundlegende Programme an.

++++++ [ > ++++++++++ < - ] > +++++ .

Dieses Programm gibt den Buchstaben 'A' aus. Zunächst erhöht es den Wert der 1. Zelle auf 6.
Diese erste Zelle wird für die Schleife verwendet. Danach beginnt das Programm
die Schleife ([) und geht vor zu Zelle #2. Es erhöht den Zellwert inkrementell 10 Mal, geht dann zurück
zu Zelle #1, und verringert Zelle #1. Diese Schleife wird 6 Mal durchlaufen (nach 6
Durchläufen ist der Wert der Zelle #1 auf 0 reduziert, dadurch wird die Schleife abgebrochen
und das Programm hinter dem korrespondierenden ] fortgesetzt).

An dieser Stelle befinden wir uns an Zelle #1, die jetzt den Wert 0 hat, während Zelle #2
den Wert 60 hat. Wir gehen vor zu Zelle #2, inkrementieren 5 Mal, bis zum Wert 65,
und geben dann den Wert der Zelle #2 aus. 65 ist ein 'A' im ASCII Zeichensatz,
daher wird 'A' am Terminal ausgegeben..


, [ > + < - ] > .

Dieses Programm liest ein Zeichen von der Benutzereingabe und schreibt dessen Wert
in Zelle #1. Danach beginnt eine Schleife. Rücke vor auf Zelle #2, erhöhe den Wert der Zelle #2,
gehe zurück auf Zelle #1, verringere den Wert der Zelle #1. Dies geht solange bis
Zelle #1 den Wert 0 und Zelle #2 den alten Wert aus #1 hat. Da wir am Ende der Schleife
bie Zelle #1 sind, gehe vor zu Zelle #2 und gibt denb Wert als ASCII Zeichen aus.

Beachte biite, dass die Leerzeichen nur aus Gründen der Lesbarkeit geschrieben werden.
Man könnte genauso schreiben:

,[>+<-]>.

Versuche herauszufinden, was dieses Programm macht:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Dieses Programm nimmt zwei Zahlen als Eingabe und multipliziert sie.

Im Wesentlichen liest es zunächst zwei Werte ein. Dann beginnt die äußere Schleife
mit Zelle #1 als Zähler. Danach geht das Programm zu Zelle #2 vor und startet die innere Schleife
mit Zelle #2 als Zähler. Diese zählt Zelle #3 hoch. Es gibt jedoch ein Problem:
Am Ende der inneren Schleife hat Zelle #2 den Wert Null. Daher würde die innere
Schleife beim nächsten Durchgang nicht mehr funktionieren. Daher wird auch Zelle #4
erhöht und anschließend in Zelle #2 zurückkopiert.
Am Ende steht in Zelle #3 das Ergebnis.
```

Das ist Brainfuck. Nicht so schwierig, oder? Zum Spaß kannst du dein eigenes Brainfuchs
Programm schreiben oder du schreibst einen Brainfuck Interpreter in einer anderen
Programmiersprache. Der Interpreter lässt sich ziemlich einfach implementieren.
Falls du Masochist bist, kannst du auch versuchen, einen Brainfuck Interpreter in Brainfuck zu implementieren.
