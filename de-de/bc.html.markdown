---
language: bc
contributors:
    - ["caminsha", "https://github.com/caminsha"]
filename: learnbc-de.bc
lang: de-de
---
```c
/* Das is ein mehr-
zeiliger Kommentar */
# Das ist ein (einzeiliger) Kommentar (in GNU bc).

    /*1. Variablen und Kontrollstrukturen*/
num = 45 /* Alle Variablen speichern nur Doubles und es ist
    nicht möglich String-Konstanten direkt zu speichern */
num = 45; /* Es kann nach jedem Statement ein optionales Semikolon
	hinzugefügt werden */
/* Blöcke werden mit den Operatoren {} (ähnlich wie in C) bezeichnet */
while(num < 50) {
	num += 1 /* äquivalent zu num=num+1.
	a = a Op b ist äquivalent zu a Op= b*/
}
/* Ausserdem gibt es ++ (Inkrement) und -- (Dekrement) Operatoren */
/* Es gibt 3 spezielle Variablen:
scale: definiert die Anzahl Nachkommastellen
ibase: definiert die Basis der Eingabe
obase: definiert die Basis der Ausgabe*/
/*Wenn-Bedingungen:*/
hour = read() /*Eingabe einer Zahl*/

if(hour < 12) { /*Operatoren sind genau wie in C*/
    print "Guten Morgen\n" /*"print" Gibt Strings oder Variablen
	mit einem Komma separiert aus.*/
} else if(hour == 12) {
    print "Hallo\n"
	/* Escape-Sequenzen starten mite einem \ in einem String.
	Um Escape-Sequenzen klarer zu machen, ist hier eine vereinfachte
	Liste, welche in bc funktioneren.:
    \b: Backspace
    \c: carriage return
    \n: Zeilenumbruch
    \t: Tab
    \\: Backslash*/
} else {
	/* Standardmässig sind Variablen global. */
    thisIsGlobal = 5
	/*Variablen können lokal gemacht werden. Benutze das Schlüsselwort "auto"
      in einer Funktion.*/
}

/* Jede Variable hat als Voreinstellung den Wert 0. */
num = blankVariable /*num wurde auf 0 gesetzt.*/

/*Wie in C ist nur 0 falsch.*/
if(!num) {print "false\n"}

/*Im Gegensatz zu C hat bc den Ternäroperator ?: nicht. Zum Beispiel
führt dieser Codeblok zu einem Fehler:
a = (num) ? 1 : 0
Jedoch kann dies simuliert werden:*/
a = (num) && (1) || (0) /*&& ist das UND, || ist das ODER*/

/*For-Schleifen*/
num = 0
for(i = 1; i <= 100; i++) {/*Gleich wie die For-Schleife in C*/
    num += i
}

    /*2.Funktionen und Arrays*/
define fac(n) { /*Definiere eine Funktion mit define*/
    if(n == 1 || n == 0) {
        return 1 /*Gebe einen Wert zurück*/
    }
    return n * fac(n - 1) /*Rekursion ist möglich*/
}

/*Closures und anonyme Funktionen sind nicht möglich */

num = fac(4) /*24*/

/*Dies ist ein Beispiel von lokalen Variabeln.*/
define x(n) {
    auto x
    x = 1
    return n + x
}
x(3) /*4*/
print x /*Es stellt sich heraus, dass x ausserhalb der Funktion nicht
          zugänglich ist.*/
/*Arrays sind äquivalent zu C Arrays.*/
for(i = 0; i <= 3; i++) {
    a[i] = 1
}
/*Greife wie folgt darauf zu:*/
print a[0], " ", a[1], " ", a[2], " ", a[3], "\n"
quit /* Füge diese Codezeile hinzu, um sicherzustellen, dass
das Programm beendet. Diese Codezeile ist optional.*/
```
Viel Spass mit diesem einfachen Rechner! (Oder dieser Programmiersprache, um exakt zu sein.)

Das ganze Programm wurde in GNU bc geschrieben. Um es auszuführen, benutze ```bc learnbc.bc```.

