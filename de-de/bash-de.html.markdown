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
# Die erste Zeile des Scripts nennt sich Shebang in gibt dem System an, wie
# wie das Script ausgeführt werden soll: http://de.wikipedia.org/wiki/Shebang
# Du hast es bestimmt schon mitgekriegt, Kommentare fangen mit # an. Das Shebang ist auch ein Kommentar

# Ein einfaches Beispiel mit hello world:
echo Hello, world!

# Jeder Befehl fängt auf einer neuen Zeile oder nach einem Semikolon an:
echo 'Dies ist die erste Zeile'; echo 'Dies die zweite Zeile'

# Variablen deklariert man so:
VARIABLE="irgendein String"

# Aber nicht so:
VARIABLE = "irgendein String"
# Bash wird VARIABLE für einen Befehl halten, den es ausführen soll. Es wird einen Fehler ausgeben, 
# weil es den Befehl nicht findet. 

# Eine Variable wird so benutzt:
echo $VARIABLE
echo "$VARIABLE"
# Wenn du eine Variable selbst benutzt – ihr Werte zuweist, sie exportierst oder irgendetwas anders –, 
# dann über ihren Namen ohne $. Aber wenn du ihren zugewiesenen Wert willst, dann musst du $ voranstellen.

# Einen Wert aus der Eingabe lesen:
echo "Wie heisst du?"
read NAME # Wir mussten nicht mal eine neue Variable deklarieren
echo Hello, $NAME!

# Wir haben die übliche if-Struktur:
if true
then
    echo "Wie erwartet"
else
    echo "Und dies nicht"
fi

# Ausdrücke werden im folgenden Format festgehalten:
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

# Befehle können innerhalb anderer Befehle mit $( ) erstetzt werden:
# Der folgende Befehl zeigt die Anzahl aller Dateien und Unterverzeichnisse
# im aktuellen Verzeichnis an.
echo "Dieser Ordner beinhaltet $(ls | wc -l) Dateien und Verzeichnisse."

# Bash nutzt einen case-Ausdruck, der sich ähnlich wie switch in Java oder C++ verhält.
case "$VARIABLE"
in
    # Liste der Fälle, die unterschieden werden sollen
    0) echo "Hier ist eine Null."
    1) echo "Hier ist eine Eins."
    *) echo "Das ist nicht Null."
esac

# loops iterieren über die angegebene Zahl von Argumenten:
# Der Inhalt von $VARIABLE wird dreimal ausgedruckt.
for $VARIABLE in x y z
do
    echo "$VARIABLE"
done
```
