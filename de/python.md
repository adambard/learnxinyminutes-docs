---
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
translators:
    - ["kultprok", "http:/www.kulturproktologie.de"]
    - ["matthiaskern", "https://github.com/matthiaskern"]
    - ["Michael Wagner", "https://about.michi.onl"]
---

Python wurde Anfang der 90er Jahre von Guido van Rossum entwickelt. Es ist heute eine der 
beliebtesten Programmiersprachen. Ich (Louie Dinh) habe mich in Python wegen seiner syntaktischen 
Klarheit verliebt. Es ist im Grunde ausführbarer Pseudocode.

```python
# Einzeilige Kommentare beginnen mit einem Hashtag-Symbol.

""" Mehrzeilige Strings können mit
    drei " geschrieben werden und werden häufig
    als Dokumentation verwendet.
"""

####################################################
## 1. Primitive Datentypen und Operatoren
####################################################

# Zahlen sind vorhanden
3  # => 3

# Mathematik funktioniert wie erwartet
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20
35 / 5  # => 7.0

# Ganzzahldivision rundet in Richtung negativer Unendlichkeit
5 // 3       # => 1
-5 // 3      # => -2
5.0 // 3.0   # => 1.0  # funktioniert auch mit Gleitkommazahlen
-5.0 // 3.0  # => -2.0

# Das Ergebnis einer Division ist immer eine Gleitkommazahl
10.0 / 3  # => 3.3333333333333335

# Modulo-Operation
7 % 3   # => 1
# i % j hat das gleiche Vorzeichen wie j, im Gegensatz zu C
-7 % 3  # => 2

# Potenzierung (x**y, x hoch y)
2**3  # => 8

# Vorrang mit Klammern erzwingen
1 + 3 * 2    # => 7
(1 + 3) * 2  # => 8

# Boolesche Werte sind Primitive (Achtung: Großschreibung)
True   # => True
False  # => False

# Negation mit not
not True   # => False
not False  # => True

# Boolesche Operatoren
# Beachte: "and" und "or" unterscheiden Groß-/Kleinschreibung
True and False  # => False
False or True   # => True

# True und False sind tatsächlich 1 und 0, aber mit anderen Schlüsselwörtern
True + True  # => 2
True * 8     # => 8
False - 5    # => -5

# Vergleichsoperatoren betrachten den numerischen Wert von True und False
0 == False   # => True
2 > True     # => True
2 == True    # => False
-5 != False  # => True

# None, 0 und leere Strings/Listen/Dicts/Tupel/Sets werden alle als False ausgewertet.
# Alle anderen Werte sind True
bool(0)      # => False
bool("")     # => False
bool([])     # => False
bool({})     # => False
bool(())     # => False
bool(set())  # => False
bool(4)      # => True
bool(-6)     # => True

# Die Verwendung boolescher logischer Operatoren auf Ints wandelt sie zur Auswertung in Booleans um,
# aber ihr nicht-umgewandelter Wert wird zurückgegeben. Nicht verwechseln mit bool(ints) und bitweisen
# and/or (&,|)
bool(0)   # => False
bool(2)   # => True
0 and 2   # => 0
bool(-5)  # => True
bool(2)   # => True
-5 or 0   # => -5

# Gleichheit ist ==
1 == 1  # => True
2 == 1  # => False

# Ungleichheit ist !=
1 != 1  # => False
2 != 1  # => True

# Weitere Vergleiche
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# Prüfen, ob ein Wert in einem Bereich liegt
1 < 2 and 2 < 3  # => True
2 < 3 and 3 < 2  # => False
# Verkettung macht dies schöner
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# (is vs. ==) is überprüft, ob zwei Variablen auf dasselbe Objekt verweisen, aber == überprüft,
# ob die Objekte, auf die verwiesen wird, die gleichen Werte haben.
a = [1, 2, 3, 4]  # a zeigt auf eine neue Liste, [1, 2, 3, 4]
b = a             # b zeigt auf das, worauf a zeigt
b is a            # => True, a und b verweisen auf dasselbe Objekt
b == a            # => True, die Objekte von a und b sind gleich
b = [1, 2, 3, 4]  # b zeigt auf eine neue Liste, [1, 2, 3, 4]
b is a            # => False, a und b verweisen nicht auf dasselbe Objekt
b == a            # => True, die Objekte von a und b sind gleich

# Strings werden mit " oder ' erstellt
"Das ist ein String."
'Das ist auch ein String.'

# Strings können auch addiert werden
"Hallo " + "Welt!"  # => "Hallo Welt!"
# String-Literale (aber keine Variablen) können ohne '+' verkettet werden
"Hallo " "Welt!"    # => "Hallo Welt!"

# Ein String kann wie eine Liste von Zeichen behandelt werden
"Hallo Welt!"[0]  # => 'H'

# Die Länge eines Strings lässt sich finden
len("Das ist ein String")  # => 19

# Seit Python 3.6 können f-Strings oder formatierte String-Literale verwendet werden.
name = "Reiko"
f"Sie sagte, ihr Name ist {name}."  # => "Sie sagte, ihr Name ist Reiko."
# Jeder gültige Python-Ausdruck innerhalb dieser geschweiften Klammern wird in den String zurückgegeben.
f"{name} ist {len(name)} Zeichen lang."  # => "Reiko ist 5 Zeichen lang."

# None ist ein Objekt
None  # => None

# Verwende nicht das Gleichheitssymbol "==", um Objekte mit None zu vergleichen
# Verwende stattdessen "is". Dies prüft auf Gleichheit der Objekt-Identität.
"etc" is None  # => False
None is None   # => True

####################################################
## 2. Variablen und Collections
####################################################

# Python hat eine print-Funktion
print("Ich bin Python. Schön, dich kennenzulernen!")  # => Ich bin Python. Schön, dich kennenzulernen!

# Standardmäßig gibt die print-Funktion auch einen Zeilenumbruch am Ende aus.
# Verwende das optionale Argument end, um den End-String zu ändern.
print("Hallo, Welt", end="!")  # => Hallo, Welt!

# Einfache Möglichkeit, Eingabedaten von der Konsole zu erhalten
input_string_var = input("Gib etwas ein: ")  # Gibt die Daten als String zurück

# Es gibt keine Deklarationen, nur Zuweisungen.
# Konvention bei der Benennung von Variablen ist der snake_case-Stil
some_var = 5
some_var  # => 5

# Der Zugriff auf eine zuvor nicht zugewiesene Variable ist eine Exception.
# Siehe Kontrollfluss, um mehr über Exception-Handling zu erfahren.
some_unknown_var  # Löst einen NameError aus

# if kann als Ausdruck verwendet werden
# Entspricht dem '?:' ternären Operator von C
"yay!" if 0 > 1 else "nay!"  # => "nay!"

# Listen speichern Sequenzen
li = []
# Man kann mit einer vorgefüllten Liste beginnen
other_li = [4, 5, 6]

# Mit append kann man Elemente an das Ende einer Liste hinzufügen
li.append(1)    # li ist jetzt [1]
li.append(2)    # li ist jetzt [1, 2]
li.append(4)    # li ist jetzt [1, 2, 4]
li.append(3)    # li ist jetzt [1, 2, 4, 3]
# Mit pop vom Ende entfernen
li.pop()        # => 3 und li ist jetzt [1, 2, 4]
# Fügen wir es wieder hinzu
li.append(3)    # li ist jetzt wieder [1, 2, 4, 3].

# Zugriff auf eine Liste wie bei jedem Array
li[0]   # => 1
# Das letzte Element anschauen
li[-1]  # => 3

# Zugriff außerhalb der Grenzen ist ein IndexError
li[4]  # Löst einen IndexError aus

# Mit der Slice-Syntax kann man sich Bereiche anschauen.
# Der Startindex ist eingeschlossen, der Endindex nicht
# (Es ist ein geschlossenes/offenes Intervall für mathematisch Interessierte.)
li[1:3]   # Liste von Index 1 bis 2 zurückgeben => [2, 4]
li[2:]    # Liste ab Index 2 zurückgeben => [4, 3]
li[:3]    # Liste vom Anfang bis Index 3 zurückgeben  => [1, 2, 4]
li[::2]   # Liste mit Schrittgröße 2 zurückgeben => [1, 4]
li[::-1]  # Liste in umgekehrter Reihenfolge zurückgeben => [3, 4, 2, 1]
# Beliebige Kombinationen dieser Elemente verwenden, um erweiterte Slices zu erstellen
# li[start:end:step]

# Eine oberflächliche Kopie mit Slices erstellen
li2 = li[:]  # => li2 = [1, 2, 4, 3] aber (li2 is li) ergibt false.

# Beliebige Elemente aus einer Liste mit "del" entfernen
del li[2]  # li ist jetzt [1, 2, 3]

# Erstes Vorkommen eines Werts entfernen
li.remove(2)  # li ist jetzt [1, 3]
li.remove(2)  # Löst einen ValueError aus, da 2 nicht in der Liste ist

# Ein Element an einem bestimmten Index einfügen
li.insert(1, 2)  # li ist jetzt wieder [1, 2, 3]

# Den Index des ersten gefundenen Elements erhalten, das dem Argument entspricht
li.index(2)  # => 1
li.index(4)  # Löst einen ValueError aus, da 4 nicht in der Liste ist

# Listen können addiert werden
# Hinweis: Werte für li und für other_li werden nicht geändert.
li + other_li  # => [1, 2, 3, 4, 5, 6]

# Listen mit extend verketten
li.extend(other_li)  # Jetzt ist li [1, 2, 3, 4, 5, 6]

# Prüfen, ob ein Element in einer Liste vorhanden ist, mit in
1 in li  # => True

# Die Länge mit len untersuchen
len(li)  # => 6


# Tupel sind wie Listen, aber unveränderlich.
tup = (1, 2, 3)
tup[0]      # => 1
tup[0] = 3  # Löst einen TypeError aus

# Beachte, dass ein Tupel der Länge 1 ein Komma nach dem letzten Element benötigt
# aber Tupel anderer Längen, auch null, benötigen dies nicht.
type((1))   # => <class 'int'>
type((1,))  # => <class 'tuple'>
type(())    # => <class 'tuple'>

# Die meisten Listenoperationen funktionieren auch bei Tupeln
len(tup)         # => 3
tup + (4, 5, 6)  # => (1, 2, 3, 4, 5, 6)
tup[:2]          # => (1, 2)
2 in tup         # => True

# Tupel (oder Listen) können in Variablen entpackt werden
a, b, c = (1, 2, 3)  # a ist jetzt 1, b ist jetzt 2 und c ist jetzt 3
# Du kannst auch erweiterte Entpackung verwenden
a, *b, c = (1, 2, 3, 4)  # a ist jetzt 1, b ist jetzt [2, 3] und c ist jetzt 4
# Tupel werden standardmäßig erstellt, wenn Klammern weggelassen werden
d, e, f = 4, 5, 6  # Tupel 4, 5, 6 wird in d, e und f entpackt
# sodass zwei Werte zu tauschen so einfach ist
e, d = d, e  # d ist jetzt 5 und e ist jetzt 4

# Dictionaries speichern Zuordnungen von Schlüsseln zu Werten
empty_dict = {}
# Hier ist ein vorgefülltes Dictionary
filled_dict = {"eins": 1, "zwei": 2, "drei": 3}

# Beachte, dass Schlüssel für Dictionaries unveränderliche Typen sein müssen. Dies soll
# sicherstellen, dass der Schlüssel in einen konstanten Hash-Wert umgewandelt werden kann
# für schnelle Suche.
# Unveränderliche Typen sind z.B. Ints, Floats, Strings, Tupel.
invalid_dict = {[1,2,3]: "123"}  # => Löst einen TypeError aus: unhashable type: 'list'
valid_dict = {(1,2,3):[1,2,3]}   # Werte können von jedem Typ sein.

# Mit [] nach einem Wert suchen
filled_dict["eins"]  # => 1

# Alle Schlüssel als iterierbares Objekt mit "keys()" abrufen. Wir müssen es in eine Liste
# einwickeln, um es zu erhalten. Dazu kommen wir später. Hinweis - Für Python-Versionen <3.7
# ist die Reihenfolge der Dictionary-Schlüssel nicht garantiert. Deine Ergebnisse
# stimmen möglicherweise nicht genau mit diesem Beispiel überein. Ab Python 3.7 haben
# Dictionary-Objekte jedoch die Einfügereihenfolge der Schlüssel.
list(filled_dict.keys())  # => ["drei", "zwei", "eins"] in Python <3.7
list(filled_dict.keys())  # => ["eins", "zwei", "drei"] in Python 3.7+

# Alle Werte als iterierbares Objekt mit "values()" abrufen. Auch hier müssen wir es
# in eine Liste einwickeln, um es aus dem iterierbaren Objekt zu erhalten.
# Hinweis - Wie oben bezüglich der Reihenfolge der Schlüssel.
list(filled_dict.values())  # => [3, 2, 1]  in Python <3.7
list(filled_dict.values())  # => [1, 2, 3] in Python 3.7+

# Mit "in" prüfen, ob ein Schlüssel in einem Dictionary existiert
"eins" in filled_dict  # => True
1 in filled_dict       # => False

# Das Nachschlagen eines nicht existierenden Schlüssels ist ein KeyError
filled_dict["vier"]  # KeyError

# Verwende die "get()"-Methode, um zu vermeiden, dass der KeyError ausgelöst wird
filled_dict.get("eins")      # => 1
filled_dict.get("vier")      # => None
# Die get-Methode unterstützt ein Standardargument, wenn der Wert fehlt
filled_dict.get("eins", 4)   # => 1
filled_dict.get("vier", 4)   # => 4

# "setdefault()" fügt nur dann zu Dictionary hinzu, wenn der angegebene Schlüssel nicht vorhanden ist
filled_dict.setdefault("fünf", 5)  # filled_dict["fünf"] ist auf 5 gesetzt
filled_dict.setdefault("fünf", 6)  # filled_dict["fünf"] ist immer noch 5

# Einem Dictionary einen Eintrag hinzufügen
filled_dict.update({"vier":4})  # => {"eins": 1, "zwei": 2, "drei": 3, "vier": 4}
filled_dict["vier"] = 4         # ein weiterer Weg, um zu Dictionary hinzuzufügen

# Schlüssel aus einem Dictionary mit del entfernen
del filled_dict["eins"]  # Entfernt den Schlüssel "eins" aus filled_dict

# Ab Python 3.5 kannst du auch zusätzliche Entpackungsoptionen verwenden
{'a': 1, **{'b': 2}}  # => {'a': 1, 'b': 2}
{'a': 1, **{'a': 2}}  # => {'a': 2}


# Sets speichern ... nun ja ... Sets
empty_set = set()
# Ein Set mit vielen Werten initialisieren. Ja, es sieht ein bisschen wie ein Dictionary aus. Tut mir leid.
some_set = {1, 1, 2, 2, 3, 4}  # some_set ist jetzt {1, 2, 3, 4}

# Ähnlich wie Schlüssel eines Dictionary müssen Elemente eines Sets unveränderlich sein.
invalid_set = {[1], 1}  # => Löst einen TypeError aus: unhashable type: 'list'
valid_set = {(1,), 1}

# Einem Set ein weiteres Element hinzufügen
filled_set = some_set
filled_set.add(5)  # filled_set ist jetzt {1, 2, 3, 4, 5}
# Sets haben keine doppelten Elemente
filled_set.add(5)  # es ist immer noch {1, 2, 3, 4, 5}

# Die Schnittmenge zweier Sets mit &
other_set = {3, 4, 5, 6}
filled_set & other_set  # => {3, 4, 5}

# Die Vereinigung mit |
filled_set | other_set  # => {1, 2, 3, 4, 5, 6}

# Die Differenz eines Sets mit -
{1, 2, 3, 4} - {2, 3, 5}  # => {1, 4}

# Die symmetrische Differenz mit ^
{1, 2, 3, 4} ^ {2, 3, 5}  # => {1, 4, 5}

# Prüfen, ob das Set auf der linken Seite eine Obermenge des Sets auf der rechten Seite ist
{1, 2} >= {1, 2, 3}  # => False

# Prüfen, ob das Set auf der linken Seite eine Teilmenge des Sets auf der rechten Seite ist
{1, 2} <= {1, 2, 3}  # => True

# Prüfen, ob ein Element in einem Set vorhanden ist, mit in
2 in filled_set   # => True
10 in filled_set  # => False

# Eine oberflächliche Kopie erstellen
filled_set = some_set.copy()  # filled_set ist {1, 2, 3, 4, 5}
filled_set is some_set        # => False


####################################################
## 3. Kontrollfluss und Iterierbare Objekte
####################################################

# Erstellen wir einfach eine Variable
some_var = 5

# Hier ist eine if-Anweisung. Einrückung ist in Python wichtig!
# Konvention ist die Verwendung von vier Leerzeichen, keine Tabs.
# Dies gibt "some_var ist kleiner als 10" aus.
if some_var > 10:
    print("some_var ist absolut größer als 10.")
elif some_var < 10:    # Diese elif-Klausel ist optional.
    print("some_var ist kleiner als 10.")
else:                  # Dies ist auch optional.
    print("some_var ist tatsächlich 10.")


"""
For-Schleifen iterieren über Listen
gibt aus:
    hund ist ein Säugetier
    katze ist ein Säugetier
    maus ist ein Säugetier
"""
for tier in ["hund", "katze", "maus"]:
    # Du kannst mit format() Strings formatieren
    print("{} ist ein Säugetier".format(tier))

"""
"range(Zahl)" gibt ein iterierbares Objekt von Zahlen von null bis zur angegebenen Zahl zurück
gibt aus:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
"range(untere, obere)" gibt ein iterierbares Objekt von Zahlen
von der unteren Zahl bis zur oberen Zahl zurück
gibt aus:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
"range(untere, obere, schritt)" gibt ein iterierbares Objekt von Zahlen
von der unteren Zahl bis zur oberen Zahl zurück, während um schritt erhöht wird.
Wenn schritt nicht angegeben wird, ist der Standardwert eins.
gibt aus:
    4
    6
"""
for i in range(4, 8, 2):
    print(i)

"""
Um über eine Liste zu iterieren und sowohl den Index als auch den Wert jedes Elements
abzurufen, verwende enumerate()
gibt aus:
    0 hund
    1 katze
    2 maus
"""
tiere = ["hund", "katze", "maus"]
for i, wert in enumerate(tiere):
    print(i, wert)

"""
While-Schleifen werden ausgeführt, bis eine Bedingung nicht mehr erfüllt ist.
gibt aus:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # Abkürzung für x = x + 1

# Exception-Handling mit einem try/except-Block
try:
    # Verwende "raise", um einen Fehler auszulösen
    raise IndexError("Das ist ein Index-Fehler")
except IndexError as e:
    pass                 # Refrainkeyword; nichts tun, einfach weitermachen
except (TypeError, NameError):
    pass                 # Mehrere Exceptions können bei Bedarf gemeinsam behandelt werden.
else:                    # Optionale Klausel zum try/except-Block. Muss nach allen except-Blöcken folgen
    print("Alles gut!")  # Wird nur ausgeführt, wenn der Code in try keine Exception auslöst
finally:                 # Wird in allen Fällen ausgeführt
    print("Wir können hier Ressourcen bereinigen")

# Anstelle von try/finally zum Bereinigen von Ressourcen kannst du eine with-Anweisung verwenden
with open("myfile.txt") as f:
    for line in f:
        print(line)

# Das Schreiben in eine Datei
inhalt = {"aa": 12, "bb": 21}
with open("myfile1.txt", "w+") as datei:
    datei.write(str(inhalt))        # schreibt einen String in eine Datei

import json
with open("myfile2.txt", "w+") as datei:
    datei.write(json.dumps(inhalt)) # schreibt ein Objekt in eine Datei

# Aus einer Datei lesen
with open("myfile1.txt") as datei:
    inhalt = datei.read()           # liest einen String aus einer Datei
print(inhalt)
# print: {"aa": 12, "bb": 21}

with open("myfile2.txt") as datei:
    inhalt = json.load(datei)       # liest ein json-Objekt aus einer Datei
print(inhalt)
# print: {"aa": 12, "bb": 21}

# Python bietet eine grundlegende Abstraktion namens Iterable.
# Ein iterierbares Objekt ist ein Objekt, das als Sequenz behandelt werden kann.
# Das Objekt, das von der range-Funktion zurückgegeben wird, ist ein iterierbares Objekt.

filled_dict = {"eins": 1, "zwei": 2, "drei": 3}
our_iterable = filled_dict.keys()
print(our_iterable)  # => dict_keys(['eins', 'zwei', 'drei']). Dies ist ein Objekt, das das Iterable-Interface implementiert.

# Wir können darüber schleifen.
for i in our_iterable:
    print(i)  # Gibt eins, zwei, drei aus

# Wir können es jedoch nicht nach Index adressieren.
our_iterable[1]  # Löst einen TypeError aus

# Ein iterierbares Objekt ist ein Objekt, das weiß, wie es einen Iterator erstellt.
our_iterator = iter(our_iterable)

# Unser Iterator ist ein Objekt, das sich merken kann, welchen Zustand wir durchlaufen haben.
# Wir erhalten das nächste Objekt mit "next()".
next(our_iterator)  # => "eins"

# Es behält den Zustand bei, während wir iterieren.
next(our_iterator)  # => "zwei"
next(our_iterator)  # => "drei"

# Nachdem der Iterator alle seine Daten zurückgegeben hat, löst er eine StopIteration-Exception aus
next(our_iterator)  # Löst StopIteration aus

# Wir können alle Elemente aus einem Iterator abrufen, indem wir list() darauf aufrufen.
list(filled_dict.keys())  # => ["eins", "zwei", "drei"]


####################################################
## 4. Funktionen
####################################################

# Verwende "def", um neue Funktionen zu erstellen
def addiere(x, y):
    print("x ist {} und y ist {}".format(x, y))
    return x + y  # Werte mit einer return-Anweisung zurückgeben

# Aufrufen von Funktionen mit Parametern
addiere(5, 6)  # => gibt "x ist 5 und y ist 6" aus und gibt 11 zurück

# Ein weiterer Weg, Funktionen aufzurufen, ist mit Schlüsselwort-Argumenten
addiere(y=6, x=5)  # Schlüsselwort-Argumente können in beliebiger Reihenfolge erscheinen.

# Du kannst Funktionen definieren, die eine variable Anzahl von
# Positionsargumenten annehmen
def varargs(*args):
    return args

varargs(1, 2, 3)  # => (1, 2, 3)

# Du kannst auch Funktionen definieren, die eine variable Anzahl von
# Schlüsselwort-Argumenten annehmen
def keyword_args(**kwargs):
    return kwargs

# Lass uns sie aufrufen, um zu sehen, was passiert
keyword_args(big="foot", loch="ness")  # => {"big": "foot", "loch": "ness"}


# Du kannst beides gleichzeitig tun, wenn du möchtest
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) gibt aus:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Beim Aufrufen von Funktionen kannst du das Gegenteil von args/kwargs machen!
# Verwende * um Tupel zu entpacken und ** um kwargs zu entpacken.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)            # entspricht all_the_args(1, 2, 3, 4)
all_the_args(**kwargs)         # entspricht all_the_args(a=3, b=4)
all_the_args(*args, **kwargs)  # entspricht all_the_args(1, 2, 3, 4, a=3, b=4)

# Rückgabe mehrerer Werte (mit Tupel-Zuweisung)
def tausche(x, y):
    return y, x  # Gibt mehrere Werte als Tupel ohne Klammern zurück.
                 # (Hinweis: Klammern wurden weggelassen, könnten aber eingefügt werden)

x = 1
y = 2
x, y = tausche(x, y)     # => x = 2, y = 1
# (x, y) = tausche(x,y)  # Klammern wurden erneut weggelassen, könnten aber eingefügt werden.

# Funktionsgültigkeitsbereich
x = 5

def setze_x(num):
    # Lokale Variable x ist nicht die gleiche wie globale Variable x
    x = num    # => 43
    print(x)   # => 43

def setze_global_x(num):
    global x
    print(x)   # => 5
    x = num    # Globale Variable x ist jetzt auf 6 gesetzt
    print(x)   # => 6

setze_x(43)
setze_global_x(6)


# Python hat Funktionen erster Klasse
def erstelle_addierer(x):
    def addierer(y):
        return x + y
    return addierer

addiere_10 = erstelle_addierer(10)
addiere_10(3)   # => 13

# Es gibt auch anonyme Funktionen
(lambda x: x > 2)(3)                  # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1)  # => 5

# Es gibt eingebaute höherwertige Funktionen
list(map(addiere_10, [1, 2, 3]))          # => [11, 12, 13]
list(map(max, [1, 2, 3], [4, 2, 1]))      # => [4, 2, 3]

list(filter(lambda x: x > 5, [3, 4, 5, 6, 7]))  # => [6, 7]

# Wir können List Comprehensions für schöne Maps und Filter verwenden
# List Comprehension speichert die Ausgabe als Liste (die selbst ein iterierbares Objekt sein kann).
[addiere_10(i) for i in [1, 2, 3]]         # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]      # => [6, 7]

# Du kannst auch Set- und Dict-Comprehensions konstruieren.
{x for x in 'abcddeef' if x not in 'abc'}  # => {'d', 'e', 'f'}
{x: x**2 for x in range(5)}  # => {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}


####################################################
## 5. Module
####################################################

# Du kannst Module importieren
import math
print(math.sqrt(16))  # => 4.0

# Du kannst spezifische Funktionen aus einem Modul importieren
from math import ceil, floor
print(ceil(3.7))   # => 4.0
print(floor(3.7))  # => 3.0

# Du kannst alle Funktionen aus einem Modul importieren.
# Warnung: Dies wird nicht empfohlen
from math import *

# Du kannst Modulnamen verkürzen
import math as m
math.sqrt(16) == m.sqrt(16)  # => True

# Python-Module sind einfach gewöhnliche Python-Dateien. Du
# kannst deine eigenen schreiben und sie importieren. Der Name des
# Moduls ist der gleiche wie der Name der Datei.

# Du kannst herausfinden, welche Funktionen und Attribute in einem Modul definiert sind.
import math
dir(math)

# Wenn du ein Python-Skript namens math.py im selben Verzeichnis hast
# wie dein aktuelles Skript, wird die Datei math.py geladen
# anstelle des eingebauten Python-Moduls.
# Dies geschieht, weil der lokale Ordner Vorrang vor
# den eingebauten Python-Bibliotheken hat.


####################################################
## 6. Klassen
####################################################

# Wir verwenden die "class"-Anweisung, um eine Klasse zu erstellen
class Mensch:

    # Ein Klassenattribut. Es wird von allen Instanzen dieser Klasse geteilt
    spezies = "H. sapiens"

    # Basis-Initialisierer, dies wird aufgerufen, wenn diese Klasse instanziiert wird.
    # Beachte, dass die doppelten Unterstriche vor und nach einem Namen eine Konvention sind,
    # um Objekte oder Attribute zu kennzeichnen, die von Python verwendet werden, aber die
    # im benutzergesteuerten Namensraum leben. Methoden (oder Objekte oder Attribute) wie:
    # __init__, __str__, __repr__ usw. werden als magische Methoden (oder manchmal auch als
    # Dunder-Methoden) bezeichnet. Du solltest solche Namen nicht selbst erfinden.
    def __init__(self, name):
        # Parameter einer Instanzvariablen zuweisen
        self.name = name

        # Eine Eigenschaft initialisieren
        self._alter = 0  # das führende Unterstrich-Zeichen zeigt an, dass das Objekt oder
                         # Attribut "privat" ist, obwohl Python keine wirklich privaten
                         # Attribute außer bei Objekten hat

    # Eine Instanzmethode. Alle Methoden nehmen "self" als erstes Argument
    def sagen(self, nachricht):
        print("{name}: {nachricht}".format(name=self.name, nachricht=nachricht))

    # Eine weitere Instanzmethode
    def singen(self):
        return 'yo... yo... microphone check... one two... one two...'

    # Eine Klassenmethode wird von allen Instanzen geteilt.
    # Sie werden mit der aufrufenden Klasse als erstem Argument aufgerufen
    @classmethod
    def get_spezies(cls):
        return cls.spezies

    # Eine statische Methode wird ohne Klassen- oder Instanzreferenz aufgerufen
    @staticmethod
    def grunzen():
        return "*grunt*"

    # Eine Eigenschaft ist genau wie ein Getter.
    # Sie verwandelt die Methode alter() in ein schreibgeschütztes Attribut mit demselben Namen.
    # Es ist nicht nötig, triviale Getter und Setter in Python zu schreiben.
    @property
    def alter(self):
        return self._alter

    # Dies ermöglicht das Setzen der Eigenschaft
    @alter.setter
    def alter(self, alter):
        self._alter = alter

    # Dies ermöglicht das Löschen der Eigenschaft
    @alter.deleter
    def alter(self):
        del self._alter


# Wenn ein Python-Interpreter eine Quelldatei liest, führt er den gesamten Code aus.
# Diese __name__-Prüfung stellt sicher, dass dieser Codeblock nur ausgeführt wird, wenn dieses
# Modul das Hauptprogramm ist.
if __name__ == "__main__":
    # Eine Instanz einer Klasse instanziieren
    i = Mensch(name="Ian")
    i.sagen("hallo")                     # "Ian: hallo"
    j = Mensch("Joel")
    j.sagen("hallo")                     # "Joel: hallo"
    # i und j sind Instanzen vom Typ Mensch; d.h. sie sind Mensch-Objekte

    # Die Klassenmethode aufrufen
    i.sagen(i.get_spezies())          # "Ian: H. sapiens"
    # Klassenattribut ändern
    Mensch.spezies = "H. neanderthalensis"
    i.sagen(i.get_spezies())          # => "Ian: H. neanderthalensis"
    j.sagen(j.get_spezies())          # => "Joel: H. neanderthalensis"

    # Die statische Methode aufrufen
    print(Mensch.grunzen())            # => "*grunt*"

    # Eine Instanzmethode kann nicht mit der Klasse aufgerufen werden, weil sie
    # keine Referenz auf ein instanziiertes Objekt als erstes Argument nimmt
    print(Mensch.sagen())              # => TypeError

    # Die Eigenschaft aktualisieren
    i.alter = 42

    # Die Eigenschaft abrufen
    i.sagen(i.alter)                   # => "Ian: 42"
    j.sagen(j.alter)                   # => "Joel: 0"

    # Die Eigenschaft löschen
    del i.alter
    # i.alter                          # => dies würde einen AttributeError auslösen


####################################################
## 6.1 Vererbung
####################################################

# Vererbung ermöglicht es, neue Kindklassen zu definieren, die Methoden und Variablen
# von ihrer Elternklasse erben.

# Mit der oben definierten Mensch-Klasse als Basis- oder Elternklasse können wir
# eine Kindklasse Superheld definieren, die die Klassenvariablen wie "spezies", "name"
# und "alter" sowie Methoden wie "singen" und "grunzen" von der Mensch-Klasse erbt,
# aber auch seine eigenen einzigartigen Eigenschaften haben kann.

# Um von Modulvererbung zu profitieren, kannst du Klassen auf separate Dateien
# aufteilen, z.B. http://github.com/pythonpracticeprojects/learnpython/blob/master/python3/learnpython3.py

# Um aus einem anderen Verzeichnis zu importieren, kannst du hinzufügen:
# from "filename-without-extension" import "class-or-function-name"

from mensch import Mensch


# Spezifiziere die Elternklasse(n) als Parameter der Klassendefinition
class Superheld(Mensch):

    # Wenn die Kindklasse alle Definitionen der Elternklasse ohne
    # Modifikationen erben soll, kannst du einfach "pass" verwenden (und nichts anderes)
    # aber in diesem Fall wird das auskommentiert, um eine eindeutige Kindklasse zu ermöglichen:
    # pass

    # Kindklassen können die Attribute ihrer Eltern überschreiben
    spezies = "Übermenschlich"

    # Kindklassen erben automatisch den Konstruktor ihrer Elternklasse einschließlich
    # ihrer Argumente, können aber auch zusätzliche Argumente oder Definitionen definieren
    # und ihre Methoden wie den Klassenkonstruktor überschreiben.
    # Dieser Konstruktor erbt das Argument "name" von der Klasse "Mensch" und
    # fügt die Argumente "superpowers" und "movie" hinzu:
    def __init__(self, name, film=False,
                 superpowers=["super strength", "bulletproofing"]):

        # zusätzliche Klassenattribute hinzufügen:
        self.fictional = True
        self.movie = film
        # sei dir veränderlicher Standardwerte bewusst, da Standardwerte geteilt werden
        self.superpowers = superpowers

        # Die "super"-Funktion ermöglicht den Zugriff auf die Methoden der Elternklasse,
        # die vom Kind überschrieben wurden, in diesem Fall die __init__-Methode.
        # Dies ruft den Konstruktor der Elternklasse auf:
        super().__init__(name)

    # sing-Methode überschreiben
    def singen(self):
        return "Dun, dun, DUN!"

    # Eine zusätzliche Instanzmethode hinzufügen
    def prahlen(self):
        for power in self.superpowers:
            print("Ich verfüge über die Macht von {pow}!".format(pow=power))


if __name__ == "__main__":
    sup = Superheld(name="Tick")

    # Instanztypprüfungen
    if isinstance(sup, Mensch):
        print("Ich bin ein Mensch")
    if type(sup) is Superheld:
        print("Ich bin ein Superheld")

    # Die "Method Resolution Order" abrufen, die sowohl von getattr() als auch von super() verwendet wird
    # (die Reihenfolge, in der Klassen nach einem Attribut oder einer Methode durchsucht werden)
    # Dieses Attribut ist dynamisch und kann aktualisiert werden
    print(Superheld.__mro__)    # => (<class '__main__.Superheld'>,
                                # => <class 'mensch.Mensch'>, <class 'object'>)

    # Elternmethode aufrufen, aber eigenes Klassenattribut verwenden
    print(sup.get_spezies())    # => Übermenschlich

    # Überschriebene Methode aufrufen
    print(sup.singen())           # => Dun, dun, DUN!

    # Methode von Mensch aufrufen
    sup.sagen("Löffel")            # => Tick: Löffel

    # Methode aufrufen, die nur in Superheld existiert
    sup.prahlen()                 # => Ich verfüge über die Macht von super strength!
                                # => Ich verfüge über die Macht von bulletproofing!

    # Vererbtes Klassenattribut
    sup.alter = 31
    print(sup.alter)              # => 31

    # Attribut, das nur in Superheld existiert
    print("Bin ich Oscar-berechtigt? " + str(sup.movie))

####################################################
## 6.2 Mehrfachvererbung
####################################################


# Eine weitere Klassendefinition
# bat.py
class Fledermaus:

    spezies = "Fleddy"

    def __init__(self, kann_fliegen=True):
        self.flug = kann_fliegen

    # Diese Klasse hat auch eine sagen-Methode
    def sagen(self, msg):
        msg = "... ... ..."
        return msg

    # Und auch ihre eigene Methode
    def sonar(self):
        return "))) ... ((("


if __name__ == "__main__":
    b = Fledermaus()
    print(b.sagen("hallo"))
    print(b.flug)


# Und noch eine weitere Klassendefinition, die von Superheld und Fledermaus erbt
# superhero.py
from superhero import Superheld
from bat import Fledermaus

# Batman als Kind definieren, das von Superheld und Fledermaus erbt
class Batman(Superheld, Fledermaus):

    def __init__(self, *args, **kwargs):
        # Normalerweise musst du super aufrufen, um Attribute zu erben:
        # super(Batman, self).__init__(*args, **kwargs)
        # Wir haben es hier jedoch mit Mehrfachvererbung zu tun, und super()
        # funktioniert nur mit der nächsten Basisklasse in der MRO-Liste.
        # Stattdessen rufen wir __init__ explizit für alle Vorfahren auf.
        # Die Verwendung von *args und **kwargs ermöglicht einen sauberen Weg, um
        # Argumente weiterzugeben, wobei jeder Elternteil "eine Schicht der Zwiebel abschält".
        Superheld.__init__(self, "anonym", movie=True,
                           superpowers=["Vermögend"], *args, **kwargs)
        Fledermaus.__init__(self, *args, kann_fliegen=False, **kwargs)
        # den Wert für das Name-Attribut überschreiben
        self.name = "Trauriger Affleck"

    def singen(self):
        return "nan nan nan nan nan batman!"


if __name__ == "__main__":
    sup = Batman()

    # Die Method Resolution Order
    print(Batman.__mro__)     # => (<class '__main__.Batman'>,
                              # => <class 'superhero.Superheld'>,
                              # => <class 'mensch.Mensch'>,
                              # => <class 'bat.Fledermaus'>, <class 'object'>)

    # Elternmethode aufrufen, aber eigenes Klassenattribut verwenden
    print(sup.get_spezies())  # => Übermenschlich

    # Überschriebene Methode aufrufen
    print(sup.singen())         # => nan nan nan nan nan batman!

    # Methode von Mensch aufrufen, weil Vererbungsreihenfolge wichtig ist
    sup.sagen("Ich stimme zu")        # => Trauriger Affleck: Ich stimme zu

    # Methode aufrufen, die nur in 2. Vorfahre existiert
    print(sup.sonar())        # => ))) ... (((

    # Vererbtes Klassenattribut
    sup.alter = 100
    print(sup.alter)            # => 100

    # Vererbtes Attribut vom 2. Vorfahren, dessen Standardwert überschrieben wurde.
    print("Kann ich fliegen? " + str(sup.flug))  # => Kann ich fliegen? False


####################################################
## 7. Erweitert
####################################################

# Generatoren helfen dir, faulen Code zu schreiben.
def doppelte_zahlen(iterable):
    for i in iterable:
        yield i + i

# Generatoren sind speichereffizient, weil sie nur die Daten laden, die benötigt werden, um
# den nächsten Wert im iterierbaren Objekt zu verarbeiten. Dies ermöglicht es ihnen,
# Operationen an ansonsten unzulässig großen Wertebereichen durchzuführen.
# HINWEIS: `range` ersetzt `xrange` in Python 3.
for i in doppelte_zahlen(range(1, 900000000)):  # `range` ist ein Generator.
    print(i)
    if i >= 30:
        break

# Genau wie du eine List Comprehension erstellen kannst, kannst du auch Generator-
# Comprehensions erstellen.
values = (-x for x in [1,2,3,4,5])
for x in values:
    print(x)  # gibt -1 -2 -3 -4 -5 auf Konsole/Terminal aus

# Du kannst auch eine Generator-Comprehension direkt in eine Liste umwandeln.
values = (-x for x in [1,2,3,4,5])
gen_to_list = list(values)
print(gen_to_list)  # => [-1, -2, -3, -4, -5]


# Dekoratoren sind eine Form von syntaktischem Zucker.
# Sie machen Code leichter lesbar und erreichen dabei unhandliche Syntax.

# Wrapper sind eine Art von Dekorator.
# Sie sind wirklich nützlich, um Logging zu bestehenden Funktionen hinzuzufügen, ohne sie ändern zu müssen.

def log_funktion(func):
    def wrapper(*args, **kwargs):
        print("Betrete Funktion", func.__name__)
        result = func(*args, **kwargs)
        print("Verlasse Funktion", func.__name__)
        return result
    return wrapper

@log_funktion               # entspricht:
def meine_funktion(x,y):       # def meine_funktion(x,y):
    """Addiert zwei Zahlen."""
    return x+y              #   return x+y
                            # meine_funktion = log_funktion(meine_funktion)
# Der Dekorator @log_funktion teilt uns mit, wenn wir die Funktionsdefinition
# für meine_funktion zu lesen beginnen, dass diese Funktion mit log_funktion umhüllt wird.
# Wenn Funktionsdefinitionen lang sind, kann es schwierig sein, die nicht-dekorierte
# Zuweisung am Ende der Definition zu parsen.

meine_funktion(1,2)  # => "Betrete Funktion meine_funktion"
                  # => "3"
                  # => "Verlasse Funktion meine_funktion"

# Es gibt aber ein Problem.
# Was passiert, wenn wir versuchen, einige Informationen über meine_funktion zu erhalten?

print(meine_funktion.__name__)  # => 'wrapper'
print(meine_funktion.__doc__)  # => None (Wrapper-Funktion hat keinen Docstring)

# Weil unser Dekorator meine_funktion = log_funktion(meine_funktion) entspricht,
# haben wir Informationen über meine_funktion durch Informationen von wrapper ersetzt

# Dies mit functools beheben

from functools import wraps

def log_funktion(func):
    @wraps(func)  # dies stellt sicher, dass Docstring, Funktionsname, Argumentliste usw. alle kopiert werden
                  # zur umhüllten Funktion - anstatt mit den Informationen von wrapper ersetzt zu werden
    def wrapper(*args, **kwargs):
        print("Betrete Funktion", func.__name__)
        result = func(*args, **kwargs)
        print("Verlasse Funktion", func.__name__)
        return result
    return wrapper

@log_funktion
def meine_funktion(x,y):
    """Addiert zwei Zahlen."""
    return x+y

meine_funktion(1,2)  # => "Betrete Funktion meine_funktion"
                  # => "3"
                  # => "Verlasse Funktion meine_funktion"

print(meine_funktion.__name__)  # => 'meine_funktion'
print(meine_funktion.__doc__)  # => 'Addiert zwei Zahlen.'
```

### Kostenlos Online (Englisch)

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com)
* [The Official Docs](https://docs.python.org/3/)
* [Hitchhiker's Guide to Python](https://docs.python-guide.org/)
* [Python Course](https://www.python-course.eu)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)
* [A curated list of awesome Python frameworks, libraries and software](https://github.com/vinta/awesome-python)
* [Official Style Guide for Python](https://peps.python.org/pep-0008/)
* [Python 3 Computer Science Circles](https://cscircles.cemc.uwaterloo.ca/)
* [Dive Into Python 3](https://www.diveintopython3.net/)
* [Python Tutorial for Intermediates](https://pythonbasics.org/)
* [Build a Desktop App with Python](https://pythonpyqt.com/)
