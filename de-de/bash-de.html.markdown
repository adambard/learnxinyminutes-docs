---
category: tool
tool: bash
lang: de-de
contributors:
    - ["Max Yankov", "https://github.com/golergka"]
    - ["Darren Lin", "https://github.com/CogBear"]
translators:
    - ["kultprok", "http://www.kulturproktologie.de"]
filename: LearnBash-de.sh
---

Bash ist der Name der Unix-Shell, die als Shell des GNU-Betriebssystems und auch als Standard-Shell von Linux und Mac OS X ausgeliefert wurde.
Beinahe alle der folgenden Beispiele können als Teile eines Shell-Skripts oder direkt in der Shell ausgeführt werden.

[Weitere Informationen \(Englisch\)](http://www.gnu.org/software/bash/manual/bashref.html)

```bash
#!/bin/bash
# Die erste Zeile des Scripts nennt sich Shebang, dies gibt dem System an, 
# wie das Script ausgeführt werden soll: http://de.wikipedia.org/wiki/Shebang
# Du hast es bestimmt schon mitgekriegt, Kommentare fangen mit # an. Das Shebang ist auch ein Kommentar

# Ein einfaches Beispiel mit hello world:
echo Hello, world!

# Jeder Befehl fängt auf einer neuen Zeile oder nach einem Semikolon an:
echo 'Dies ist die erste Zeile'; echo 'Dies die zweite Zeile'

# Variablen deklariert man so:
Variable="irgendein String"

# Aber nicht so:
Variable = "irgendein String"
# Bash wird 'Variable' für einen Befehl halten, den es ausführen soll. Es wird einen Fehler ausgeben, 
# weil es den Befehl nicht findet. 

# Und so auch nicht:
Variable= 'Some string'
# Bash wird 'Variable' wieder für einen Befehl halten, den es ausführen soll. Es wird einen Fehler ausgeben, 
# Hier wird der Teil 'Variable=' als nur für diesen einen Befehl gültige Zuweisung an die Variable gesehen.

# Eine Variable wird so benutzt:
echo $Variable
echo "$Variable"
echo ${Variable}
# aber
echo '$Variable'
# Wenn du eine Variable selbst benutzt – ihr Werte zuweist, sie exportierst oder irgendetwas anderes –, 
# dann über ihren Namen ohne $. Aber wenn du ihren zugewiesenen Wert willst, dann musst du $ voranstellen.
# Beachte: ' (Hochkomma) verhindert das Interpretieren der Variablen

# Ersetzen von Zeichenketten in Variablen
echo ${Variable/irgendein/neuer}
# Ersetzt das erste Vorkommen von "irgendein" durch "neuer"

# Teil einer Zeichenkette
Laenge=7
echo ${Variable:0:Laenge}
# Gibt nur die ersten 7 Zeichen zurück

# Standardwert verwenden
echo ${Foo:-"ErsatzWennLeerOderUngesetzt"}
# Das funktioniert mit nicht gesetzten Variablen (Foo=) und leeren Zeichenketten (Foo="")
# Die Zahl 0 (Foo=0) liefert 0.
# Beachte: der wert der Variablen wird nicht geändert

# Eingebaute Variable (BUILTINS):
# Einige nützliche Beispiele
echo "Rückgabewert des letzten Befehls: $?"
echo "Die PID des skripts: $$"
echo "Anzahl der Argumente beim Aufruf: $#"
echo "Alle Argumente beim Aufruf: $@"
echo "Die Argumente in einzelnen Variablen: $1 $2..."

# Einen Wert aus der Eingabe lesen:
echo "Wie heisst du?"
read NAME # Wir mussten nicht mal eine neue Variable deklarieren
echo Hello, $NAME!

# Wir haben die übliche if-Struktur:
# 'man test' liefert weitere Informationen zu Bedingungen
if [ "$NAME" -ne $USER ]
then
    echo "Dein Name ist nicht dein Login-Name"
else
    echo "Dein Name ist dein Login-Name"
fi

# Es gibt auch bedingte Ausführung
echo "immer ausgeführt" || echo "Nur ausgeführt wenn der erste Befehl fehlschlägt"
echo "immer ausgeführt" && echo "Nur ausgeführt wenn der erste Befehl Erfolg hat"

# Um && und || mit if statements zu verwenden, braucht man mehrfache Paare eckiger Klammern:
if [ "$NAME" == "Steve" ] && [ "$Alter" -eq 15 ]
then
    echo "Wird ausgeführt wenn $NAME gleich 'Steve' UND $Alter gleich 15."
fi

if [ "$Name" == "Daniya" ] || [ "$Name" == "Zach" ]
then
    echo "Wird ausgeführt wenn $NAME gleich 'Daniya' ODER $NAME gleich 'Zach'."
fi

# Ausdrücke haben folgendes Format:
echo $(( 10 + 5 ))

# Anders als andere Programmiersprachen ist Bash eine Shell – es arbeitet also im Kontext von Verzeichnissen.
# Du kannst alle Dateien und Verzeichnisse im aktiven Verzeichnis mit ls auflisten:
ls

# Diese Befehle haben Optionen, die ihre Ausführung beeinflussen:
ls -l # Liste alle Dateien und Unterverzeichnisse auf einer eigenen Zeile auf

# Ergebnisse eines vorangegangenen Befehls können an den nächsten Befehl als Input übergeben werden.
# Der grep-Befehl filtert den Input nach dem vorgegebenen Muster. So können wir alle
# txt-Dateien im aktuellen Verzeichnis auflisten:
ls -l | grep "\.txt"

# Ein- und Ausgabe können umgeleitet werden (stdin, stdout, and stderr).
# Von stdin lesen bis "EOF" allein in einer Zeile auftaucht
# und die Datei hello.py mit den Zeilen zwischen den beiden "EOF"
# überschreiben:
cat > hello.py << EOF
#!/usr/bin/env python
from __future__ import print_function
import sys
print("#stdout", file=sys.stdout)
print("#stderr", file=sys.stderr)
for line in sys.stdin:
    print(line, file=sys.stdout)
EOF

# Führe hello.py mit verschiedenen Umleitungen von
# stdin, stdout und stderr aus:
python hello.py < "input.in"
python hello.py > "output.out"
python hello.py 2> "error.err"
python hello.py > "output-and-error.log" 2>&1
python hello.py > /dev/null 2>&1
# Die Fehlerausgabe würde die Datei "error.err" überschreiben (falls sie existiert) 
# verwende ">>" um stattdessen anzuhängen:
python hello.py >> "output.out" 2>> "error.err"

# Überschreibe output.out, hänge an error.err an und zähle die Zeilen beider Dateien:
info bash 'Basic Shell Features' 'Redirections' > output.out 2>> error.err
wc -l output.out error.err

# Führe einen Befehl aus und gib dessen "file descriptor" (zB /dev/fd/123) aus
# siehe: man fd
echo <(echo "#helloworld")

# Mehrere Arten, um output.out mit "#helloworld" zu überschreiben:
cat > output.out <(echo "#helloworld")
echo "#helloworld" > output.out
echo "#helloworld" | cat > output.out
echo "#helloworld" | tee output.out >/dev/null

# Löschen der Hilfsdateien von oberhalb, mit Anzeige der Dateinamen
# (mit '-i' für "interactive" erfolgt für jede Date eine Rückfrage)
rm -v output.out error.err output-and-error.log

# Die Ausgabe von Befehlen kann mit Hilfe von $( ) in anderen Befehlen verwendet weden:
# Der folgende Befehl zeigt die Anzahl aller Dateien und Unterverzeichnisse
# im aktuellen Verzeichnis an.
echo "Dieser Ordner beinhaltet $(ls | wc -l) Dateien und Verzeichnisse."

# Dasselbe kann man mit "backticks" `` erreichen, aber diese können
# nicht verschachtelt werden. $() ist die empfohlene Methode.
echo "Dieser Ordner beinhaltet `ls | wc -l` Dateien und Verzeichnisse."

# Bash nutzt einen case-Ausdruck, der sich ähnlich wie switch in Java oder C++ verhält.
case "$Variable"
in
    # Liste der Fälle, die unterschieden werden sollen
    0) echo "Hier ist eine Null."
    1) echo "Hier ist eine Eins."
    *) echo "Das ist etwas anderes."
esac

# 'for' Schleifen iterieren über die angegebene Zahl von Argumenten:
# Der Inhalt von $Variable wird dreimal ausgedruckt.
for $Variable in {1..3}
do
    echo "$Variable"
done

# Oder verwende die "traditionelle 'for'-Schleife":
for ((a=1; a <= 3; a++))
do
    echo $a
done

# Schleifen können auch mit Dateien arbeiten:
# 'cat' zeigt zuerst file1 an und dann file2
for Variable in file1 file2
do
    cat "$Variable"
done

# .. oder mit der Ausgabe eines Befehls:
# Ausgabe des Inhalts jeder Datei, die von 'ls' aufgezählt wird
for Output in $(ls)
do
    cat "$Output"
done

# while Schleife:
while [ true ]
do
    echo "Schleifenkörper..."
    break
done

# Funktionen definieren
# Definition:
function foo ()
{
    echo "Argumente funktionieren wie bei skripts: $@"
    echo Und: $1 $2..."
    echo "Dies ist eine Funktion"
    return 0
}

# oder einfacher
bar ()
{
    echo "Auch so kann man Funktionen deklarieren!"
    return 0
}

# Aufruf der Funktion:
foo "My name is" $Name

# Was du noch lernen könntest:
# Ausgabe der letzten 10 Zeilen von file.txt
tail -n 10 file.txt
# Ausgabe der ersten 10 Zeilen von file.txt
head -n 10 file.txt
# sortierte Ausgabe von file.txt
sort file.txt
# Mehrfachzeilen in sortierten Dateien unterdrücken
# oder (mit -d) nur diese ausgeben
uniq -d file.txt
# Ausgabe nur der ersten Spalte (vor dem ersten ',')
cut -d ',' -f 1 file.txt
# ersetze in file.txt jedes vorkommende 'gut' durch 'super' (versteht regex)
sed -i 's/gut/super/g' file.txt
# Ausgabe nach stdout aller Zeilen von file.txt, die auf eine regex passen
# Im Beispiel: Zeilen, die mit "foo" beginnen und mit "bar" enden
grep "^foo.*bar$" file.txt
# Mit der Option "-c" wird stattdessen die Anzahl der gefundenen Zeilen ausgegeben
grep -c "^foo.*bar$" file.txt
# verwende 'fgrep' oder 'grep -F' wenn du buchstäblich nach den Zeichen
# suchen willst, ohne sie als regex zu interpretieren
fgrep "^foo.*bar$" file.txt

# Dokumentation über die in bash eingebauten Befehle
# bekommst du mit dem eingebauten Befehl 'help'
help
help help
help for
help return
help source
help .

# Das bash-Handbuch liest du mit 'man'
apropos bash
man 1 bash
man bash

# Dann gibt es noch das 'info' System (drücke ? um Hilfe angezeigt zu bekommen)
apropos info | grep '^info.*('
man info
info info
info 5 info

# info Dokumentation über bash:
info bash
info bash 'Bash Features'
info bash 6
info --apropos bash
```
