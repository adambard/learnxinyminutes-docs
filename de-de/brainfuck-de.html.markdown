---
language: brainfuck
contributors:
    - ["Prajit Ramachandran", "http://prajitr.github.io/"]
    - ["Mathias Bynens", "http://mathiasbynens.be/"]
translators:
    - ["Kevin Peters", "https://github.com/igeligel"]
lang: de-de
---

Brainfuck (nicht groß geschrieben, außer am Anfang eines Satzes) ist eine 
extrem minimalistische Turing-komplette Programmiersprache mit nur acht 
Befehlen.

Man kann brainfuck direkt im Browser ausprobieren mit [brainfuck-visualizer](http://fatiherikli.github.io/brainfuck-visualizer/).

```
Jegliches Zeichen außer "><+-.,[]" wird ignoriert.

Brainfuck wird mit einem Array mit 30.000 Zellen dargestellt. Dabei ist jede
Zelle mit 0 initialisiert. Der Datenzeiger zeigt auf die aktuelle Zelle.

Die acht Befehle sind:
+ : Erhöht den Wert der aktuellen Zelle um 1.
- : Zieht vom aktuellen Wert der Zelle 1 ab.
> : Bewegt den Datenzeiger an die nächste Zelle (rechte Zelle).
< : Bewegt den Datenzeiger an die vorherige Zelle (linke Zelle).
. : Gibt den aktuellen ASCII-Wert der aktuellen Zelle aus (Bsp.: 65 = 'A').
, : Liest ein einstelliges Eingabezeichen in die aktuelle Zelle.
[ : Wenn der aktuelle Wert der Zelle null ist, änder den Datenzeiger zu dem
	entsprechendem ]. Andernfalls fahre mit der nächsten Instruktion fort.
]:  Wenn der aktuelle Wert der Zelle null ist, fahre fort mit der nächsten
	Instruktion. Andernfalls bewege den Datenzeiger zu dem entsprechendem
	[ .

[ und ] bildet eine While-Schleife. Sie muss balanciert sein.

Nun gucken wir uns ein paar grundlegende brainfuck Programme an:

++++++ [ > ++++++++++ < - ] > +++++ .

Diese Programm gibt den Buchstaben 'A' aus. Zuerst inkrementiert es den Wert 
der ersten Zelle zu 6. Die erste Zelle wird für die Schleife benutzt. Danach 
wird zur Schleife ([) gelangt und der Zeiger bewegt sich zur zweiten Zelle. 
Die zweite Zelle wird zehnmal inkrementiert, der Zeiger bewegt sich zurück zur
ersten Zelle und zieht von dem Wert der ersten Zelle ab. Diese Schleife
passiert sechsmal (Es benötigt sechs Dekrementierungen, damit die erste Zelle 
null erreicht. Danach bewegt sich der Zeiger zu dem dazugehörigem ] und führt 
dort seine Arbeit weiter).

Zu diesem Zeitpunkt beträgt der Wert der ersten Zelle 0, während die zweite 
Zelle den Wert 60 besitzt. Wir bewegen den Zeiger zur zweiten Zelle und 
inkrementieren fünfmal, damit der Wert 65 beträgt und dann geben den Wert der 
zweiten Zelle aus. 65 ist ein 'A' im ASCII-Code. Dieses 'A' wird im Terminal 
ausgegeben.


, [ > + < - ] > .

Dieses Programm liest ein Zeichen des Benutzers ein und kopiert dieses Zeichen
in die erste Zelle. Danach starten wir die Schleife. Bewegen den Zeiger zur
zweiten Zelle, inkrementieren den Wert der zweiten Zelle, bewegen den Zeiger
zurück zur ersten Zelle und dekrementieren den Wert der ersten Zelle. Dies 
führt sich so weiter bis der Wert der ersten Zelle 0 beträgt. Die zweite Zelle 
hält den alten Wert der ersten Zelle. Weil unser Zeiger sich auf der ersten
Zelle am Ende der Schleife aufhält, bewegen wir ihn zur zweiten Zelle und dann
geben wir den Wert der Zelle als ASCII aus.

Beachte außerdem: Die Leerzeichen sind nur für die Lesbarkeit in diesem 
Beispiel. Man könnte auch schreiben:

,[>+<-]>.

Versuche herauszufinden, was dieses Programm macht:

,>,< [ > [ >+ >+ << -] >> [- << + >>] <<< -] >>

Dieses Programm erhält zwei Zahlen als Eingabe und multipliziert diese.

Der Hauptinhalt des Programms ist zuerst einmal, dass beide Zahlen eingelesen 
werden. Dann startet die äußere Schleife, bedingt auf der ersten Zelle. Danach
bewegt sich der Zeiger auf die zweite Zelle und startet die innere Schleife, 
bedingt auf der zweiten Zelle, welche den Wert der dritten Zelle inkrementiert.
Wir kommen zu einem Problem: Am Ende der inneren Schleife ist der Wert der
zweiten Zelle null. In diesem Fall kann die innere Schleife nicht mehr weiter
funktionieren. Um dieses Problem zu beheben inkrementieren wir den Wert der
vierten Zellen und kopieren diesen dann später zurück in die zweite Zelle.
Dann ist der Wert der dritten Zelle das Ergebnis.
```

Und das ist brainfuck. Nicht so hart, oder? Wenn du Spaß daran hattest, dann
schreibe doch ein paar brainfuck Programme oder schreibe einen Interpreter in
der Programmiersprache deiner Wahl. Der Interpreter ist relativ einfach zu
implementieren. Doch wenn du ein richtiger Profi sein willst, dann schreibe
einen brainfuck interpreter… in brainfuck.