---
language: Python
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
translators:
    - ["kultprok", "http:/www.kulturproktologie.de"]
    - ["matthiaskern", "https://github.com/matthiaskern"]
filename: learnpython-de.py
lang: de-de
---

Anmerkungen des ursprünglichen Autors:
Python wurde in den frühen Neunzigern von Guido van Rossum entworfen. Es ist heute eine der beliebtesten Sprachen. Ich habe mich in Python wegen seiner syntaktischen Übersichtlichkeit verliebt. Eigentlich ist es ausführbarer Pseudocode.

Feedback ist herzlich willkommen! Ihr erreicht mich unter [@louiedinh](http://twitter.com/louiedinh) oder louiedinh [at] [google's email service].

Hinweis: Dieser Beitrag bezieht sich implizit auf Python 3. Falls du lieber Python 2.7 lernen möchtest, schau [hier](http://learnxinyminutes.com/docs/pythonlegacy/) weiter. Beachte hierbei,
dass Python 2 als veraltet gilt und für neue Projekte nicht mehr verwendet werden sollte.

```python

# Einzeilige Kommentare beginnen mit einer Raute (Doppelkreuz)

""" Mehrzeilige Strings werden mit
    drei '-Zeichen geschrieben und werden
    oft als Kommentare genutzt.
"""

####################################################
## 1. Primitive Datentypen und Operatoren
####################################################

# Die Zahlen
3 #=> 3

# Mathematik funktioniert so, wie man das erwartet
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20

# Außer Division, welche automatisch Gleitkommazahlen zurückgibt
35 / 5  # => 7.0

# Eine Division kann mit "//" für positive sowie negative Werte abgerundet werden.
5 // 3     # => 1
5.0 // 3.0 # => 1.0 # funktioniert auch mit floats
-5 // 3  # => -2
-5.0 // 3.0 # => -2.0

# Benutzt man eine Gleitkommazahl, ist auch das Ergebnis eine solche
3 * 2.0 # => 6.0

# Der Rest einer Division
7 % 3 # => 1

# Potenz
2**4 # => 16

# Rangfolge wird mit Klammern erzwungen
(1 + 3) * 2 #=> 8

# Boolesche Ausdrücke sind primitive Datentypen
True
False

# Mit not wird negiert
not True #=> False
not False #=> True

# Boolesche Operatoren
# Hinweis: "and" und "or" müssen klein geschrieben werden
True and False #=> False
False or True #=> True

# Für die Benutzung von Booleschen Operatoren und ganzen Zahlen
0 and 2 #=> 0
-5 or 0 #=> -5
0 == False #=> True
2 == True #=> False
1 == True #=> True

# Gleichheit ist ==
1 == 1 #=> True
2 == 1 #=> False

# Ungleichheit ist !=
1 != 1 #=> False
2 != 1 #=> True

# Ein paar weitere Vergleiche
1 < 10 #=> True
1 > 10 #=> False
2 <= 2 #=> True
2 >= 2 #=> True

# Vergleiche können verknüpft werden!
1 < 2 < 3 #=> True
2 < 3 < 2 #=> False

# Strings werden mit " oder ' gebildet
"Das ist ein String."
'Das ist auch ein String.'

# Strings können auch addiert werden! Vermeide dies aber lieber.
"Hallo " + "Welt!" #=> "Hallo Welt!"
# Strings können ohne "+" addiert werden
"Hallo " "welt!"  # => "Hallo Welt!"

# Ein String kann wie eine Liste von Zeichen verwendet werden
"Das ist ein String"[0] #=> 'D'

# .format kann Strings formatieren
"{} können {} werden".format("Strings", "formatiert")

# Schneller geht das mit Wiederholungen
"{0} mag Spagetthi, {0} liebt es zu Schwimmen und ganz besonders mag {0} {1}".format("Hans", "Blattsalat")
#=> "Hans mag Spagetthi, Hans liebt es zu Schwimmen und ganz besonders mag Hans Blattsalat"

# Die Formatierung kann auch mit `f-strings` oder formattierten Strings gemacht
# werden (ab Python 3.6+)
name = "Sandra"
f"Sie hat gesagt, ihr name sei {name}." # => Sie hat gesagt, ihr Name sei Sandra."
# Es ist möglich, andere Anweisungen innerhalb der geschweiften Klammern zu 
# setzen, welche dann im Output des Strings angezeigt werden.
f"{name} ist {len(name)} Zeichen lang" # => Sandra ist 6 Zeichen lang.

# None ist ein Objekt
None #=> None

# Verwendet nicht das Symbol für Gleichheit `==`, um Objekte mit None zu vergleichen
# Benutzt stattdessen `is`. Dieser Operator testet Objektidentität
"etc" is None #=> False
None is None  #=> True

# None, 0, und leere Strings/Listen werden alle als False bewertet.
# Alle anderen Werte sind True
bool(0)  # => False
bool("")  # => False
bool([]) #=> False
bool({}) #=> False

####################################################
## 2. Variablen und Collections
####################################################

# Textausgabe ist sehr einfach
print("Ich bin Python. Schön, dich kennenzulernen!")

# Es gibt keinen Grund, Variablen vor der Zuweisung zu deklarieren.
some_var = 5    # kleinschreibung_mit_unterstrichen entspricht der Norm
some_var #=> 5

# Das Ansprechen einer noch nicht deklarierten Variable löst eine Exception aus.
# Unter "Kontrollstruktur" kann noch mehr über
# Ausnahmebehandlung erfahren werden.
some_unknown_var  # Löst einen NameError aus

# Listen speichern Sequenzen
li = []
# Wir können mit einer bereits gefüllten Liste anfangen
other_li = [4, 5, 6]

# append fügt Daten am Ende der Liste ein
li.append(1)    #li ist jetzt [1]
li.append(2)    #li ist jetzt [1, 2]
li.append(4)    #li ist jetzt [1, 2, 4]
li.append(3)    #li ist jetzt [1, 2, 4, 3]
# Vom Ende der Liste mit pop entfernen
li.pop()        #=> 3 und li ist jetzt [1, 2, 4]
# und dann wieder hinzufügen
li.append(3)    # li ist jetzt wieder [1, 2, 4, 3].

# Greife auf Listen wie auf Arrays zu
li[0] #=> 1
# Das letzte Element ansehen
li[-1] #=> 3

# Bei Zugriffen außerhalb der Liste kommt es jedoch zu einem IndexError
li[4] # Verursacht einen IndexError

# Wir können uns Ranges mit Slice-Syntax ansehen
li[1:3] #=> [2, 4]
# Den Anfang auslassen
li[2:] #=> [4, 3]
# Das Ende auslassen
li[:3] #=> [1, 2, 4]
# Jeden Zweiten Eintrag auswählen
li[::2]   # =>[1, 4]
# Eine umgekehrte Kopie zurückgeben
li[::-1]   # => [3, 4, 2, 1]
# Jegliche Kombination dieser Syntax machen fortgeschrittene Slices möglich
# li[Start:Ende:Schritt]

# Ein bestimmtes Element mit del aus der Liste entfernen
del li[2] # li ist jetzt [1, 2, 3]

# Listen können addiert werden
li + other_li #=> [1, 2, 3, 4, 5, 6] - Hinweis: li und other_li werden in Ruhe gelassen

# Listen mit extend verknüpfen
li.extend(other_li) # Jetzt ist li [1, 2, 3, 4, 5, 6]

# Mit in auf Existenz eines Elements prüfen
1 in li #=> True

# Die Länge der Liste mit len ermitteln
len(li) #=> 6

# Tupel sind wie Listen, nur unveränderlich.
tup = (1, 2, 3)
tup[0] #=> 1
tup[0] = 3  # Löst einen TypeError aus

# Wir können all diese Listen-Dinge auch mit Tupeln anstellen
len(tup) #=> 3
tup + (4, 5, 6) #=> (1, 2, 3, 4, 5, 6)
tup[:2] #=> (1, 2)
2 in tup #=> True

# Wir können Tupel (oder Listen) in Variablen entpacken
a, b, c = (1, 2, 3)     # a ist jetzt 1, b ist jetzt 2 und c ist jetzt 3
# Tupel werden standardmäßig erstellt, wenn wir uns die Klammern sparen
d, e, f = 4, 5, 6
# Es ist kinderleicht, zwei Werte zu tauschen
e, d = d, e     # d ist nun 5 und e ist nun 4

# Dictionarys (Wörterbucher) speichern Schlüssel-Werte-Paare
empty_dict = {}
# Hier ein gefülltes Wörterbuch
filled_dict = {"one": 1, "two": 2, "three": 3}

# Wir können Einträge mit [] nachschlagen
filled_dict["one"] #=> 1

# So holen wir alle Keys (Schlüssel) als Liste
list(filled_dict.keys()) #=> ["three", "two", "one"]
# Hinweis - Die Reihenfolge von Schlüsseln in der Liste ist nicht garantiert.
# Einzelne Resultate können anders angeordnet sein.

# Alle Values (Werte) als Liste
list(filled_dict.values()) #=> [3, 2, 1]
# Hinweis - Hier gelten dieselben Einschränkungen für die Reihenfolge wie bei Schlüsseln.

# Das Vorhandensein eines Schlüssels im Wörterbuch mit "in" prüfen
"one" in filled_dict #=> True
1 in filled_dict #=> False

# Einen nicht vorhandenenen Schlüssel zu suchen, löst einen KeyError aus
filled_dict["four"] # KeyError

# Mit der get-Methode verhindern wir das
filled_dict.get("one") #=> 1
filled_dict.get("four") #=> None
# Die get-Methode unterstützt auch ein Standardargument, falls der Wert fehlt
filled_dict.get("one", 4) #=> 1
filled_dict.get("four", 4) #=> 4

# Die setdefault-Methode ist ein sicherer Weg, ein neues Schlüssel-Wert-Paar anzulegen
filled_dict.setdefault("five", 5) #filled_dict["five"] wird auf 5 gesetzt
filled_dict.setdefault("five", 6) #filled_dict["five"] ist noch immer 5

# Einträge zu einem Wörterbuch hinzufügen
filled_dict.update({"four":4}) #=> {"one": 1, "two": 2, "three": 3, "four": 4}
#filled_dict["four"] = 4  # noch ein Weg, Werte hinzuzufügen

# Schlüssel von einem Wörterbuch entfernen
del filled_dict["one"]  # Entfert den Schlüssel "one"

# Sets speichern Mengen
empty_set = set()
# Initialisieren wir ein Set mit ein paar Werten
some_set = {1, 1, 2, 2, 3, 4} # some_set ist jetzt {1, 2, 3, 4}

# Neue Variablen können einer Menge gleichgesetzt werden
filled_set = some_set

# Mehr Elemente hinzufügen
filled_set.add(5) # filled_set ist jetzt {1, 2, 3, 4, 5}

# Schnittmengen werden mit & gebildet
other_set = {3, 4, 5, 6}
filled_set & other_set #=> {3, 4, 5}

# Mengen werden mit | vereinigt
filled_set | other_set #=> {1, 2, 3, 4, 5, 6}

# Die Differenz einer Menge mit - bilden
{1,2,3,4} - {2,3,5} #=> {1, 4}

# Auf Vorhandensein von Elementen mit in prüfen
2 in filled_set #=> True
10 in filled_set #=> False


####################################################
## 3. Kontrollstruktur und Iteratoren
####################################################

# Erstellen wir mal eine Variable
some_var = 5

# Hier eine if-Anweisung. Die Einrückung ist in Python wichtig!
# gibt "some_var ist kleiner als 10" aus
if some_var > 10:
    print("some_var ist viel größer als 10.")
elif some_var < 10:    # Dieser elif-Absatz ist optional.
    print("some_var ist kleiner als 10.")
else:           # Das hier ist auch optional.
    print("some_var ist tatsächlich 10.")


"""
For-Schleifen iterieren über Listen
Ausgabe:
    hund ist ein Säugetier
    katze ist ein Säugetier
    maus ist ein Säugetier
"""
for animal in ["hund", "katze", "maus"]:
    # Wir können Strings mit format() formatieren
    print("{}  ist ein Säugetier".format(animal))

"""
`range(Zahl)` gibt eine null-basierte Liste bis zur angegebenen Zahl wieder
Ausgabe:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
"range(unten, oben)" gibt eine Liste von der unteren Zahl bis zur oberen Zahl aus
Ausgabe:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
While-Schleifen laufen, bis eine Bedingung erfüllt ist.
Ausgabe:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # Kurzform für x = x + 1

# Ausnahmebehandlung mit einem try/except-Block
try:
    # Mit raise wird ein Fehler ausgegeben
    raise IndexError("Das hier ist ein Index-Fehler")
except IndexError as e:
    pass    # Pass ist nur eine no-op. Normalerweise würden wir hier den Fehler klären.
except (TypeError, NameError):
    pass    # Mehrere Fehler können zusammen geklärt werden, falls erforderlich.
else:   # Optional, hinter allen except-Blöcken
    print("Keine Probleme!")   # Wird nur ausgeführt, wenn keine Ausnahmen aufgetreten sind
finally: #  Wird immer ausgeführt
    print("Hier können wir Ressourcen aufräumen")

# alternativ zu einem try/finally Block um Aufzuräumen:
with open("meineDatei.txt") as f:
    for line in f:
        print(line)

# Python bietet ein fundamentales Konzept der Iteration.
# Das Objekt, auf das die Iteration, also die Wiederholung einer Methode
# angewandt wird, heißt auf Englisch "iterable".
# Die range Methode gibt ein solches Objekt aus.

filled_dict = {"one": 1, "two": 2, "three": 3}
our_iterable = filled_dict.keys()
print(our_iterable) #=> range(1,10). Dies ist ein "iterable" Objekt.

# Über dieses können  wir auch iterieren
for i in our_iterable:
    print(i)    # Gibt one, two, three aus

# Allerdings können wir die einzelnen Elemente nicht mit ihrem Index ausgeben
our_iterable[1]  # TypeError

# Ein iterable ist ein Objekt, das weiß wie es einen Iteratoren erschafft.
our_iterator = iter(our_iterable)

# Unser Iterator ist ein Objekt, das sich merkt, welchen Status es gerade hat
# während wir durch es gehen. Das jeweils nächste Objekt bekommen wir mit "next()"
next(our_iterator)  #=> "one"

# Es hält den vorherigen Status
next(our_iterator)  #=> "two"
next(our_iterator)  #=> "three"

# Nachdem alle Daten ausgegeben worden sind, kommt eine StopIterator Ausnahme zurück
next(our_iterator) # Gibt StopIteration aus

# Alle Elemente können mit "list()" ausgegeben werden
list(filled_dict.keys())  #=> ["one", "two", "three"]

####################################################
## 4. Funktionen
####################################################

# Mit def neue Funktionen erstellen
def add(x, y):
    print("x ist %s und y ist %s" % (x, y))
    return x + y    # Werte werden mit return zurückgegeben

# Funktionen mit Parametern aufrufen
add(5, 6) #=> Ausgabe ist "x ist 5 und y ist 6" und gibt 11 zurück

# Ein anderer Weg des Funktionsaufrufs sind Schlüsselwort-Argumente
add(y=6, x=5)   # Schlüsselwörter können in beliebiger Reihenfolge übergeben werden.

# Wir können Funktionen mit beliebiger Anzahl von # Positionsargumenten definieren
def varargs(*args):
    return args

varargs(1, 2, 3) #=> (1,2,3)


# Wir können auch Funktionen mit beliebiger Anzahl
# Schlüsselwort-Argumenten definieren
def keyword_args(**kwargs):
    return kwargs

# Rufen wir es mal auf, um zu sehen, was passiert
keyword_args(big="foot", loch="ness") #=> {"big": "foot", "loch": "ness"}

# Wir können beides gleichzeitig machen, wenn wir wollen
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) Ausgabe:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Beim Aufruf von Funktionen können wir das Gegenteil von varargs/kwargs machen!
# Wir benutzen dann *, um Tupel auszuweiten, und ** für kwargs.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args) # äquivalent zu foo(1, 2, 3, 4)
all_the_args(**kwargs) # äquivalent zu foo(a=3, b=4)
all_the_args(*args, **kwargs) # äquivalent zu  foo(1, 2, 3, 4, a=3, b=4)


# Anwendungsbereich von Funktionen
x = 5

def setX(num):
    # lokale Variable x ist nicht die globale Variable x
    x = num # => 43
    print (x) # => 43

def setGlobalX(num):
    global x
    print (x) # => 5
    x = num # globale Variable x ist jetzt 6
    print (x) # => 6

setX(43)
setGlobalX(6)


# Python hat First-Class-Funktionen
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3) #=> 13

# Es gibt auch anonyme Funktionen
(lambda x: x > 2)(3) #=> True

# Es gibt auch Funktionen höherer Ordnung als Built-Ins
map(add_10, [1,2,3]) #=> [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7]) #=> [6, 7]

# Wir können bei map- und filter-Funktionen auch List Comprehensions einsetzen
[add_10(i) for i in [1, 2, 3]]  #=> [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5] #=> [6, 7]

####################################################
## 5. Klassen
####################################################

# Wir bilden die Unterklasse eines Objekts, um Klassen zu erhalten.
class Human(object):

    # Ein Klassenattribut. Es wird von allen Instanzen einer Klasse geteilt
    species = "H. sapiens"

    # Ein simpler Konstruktor
    def __init__(self, name):
        # Wir weisen das Argument name dem name-Attribut der Instanz zu
        self.name = name

    # Eine Instanzmethode. Alle Methoden erhalten self als erstes Argument.
    def say(self, msg):
        return "{name}: {message}".format(name=self.name, message=msg)

    # Eine Klassenmethode wird von allen Instanzen geteilt.
    # Sie werden mit der aufrufenden Klasse als erstem Argument aufgerufen
    @classmethod
    def get_species(cls):
        return cls.species

    # Eine statische Methode wird ohne Klasse oder Instanz aufgerufen
    @staticmethod
    def grunt():
        return "*grunt*"


# Eine Instanz einer Klasse erstellen
i = Human(name="Ian")
print(i.say("hi"))     # gibt "Ian: hi" aus

j = Human("Joel")
print(j.say("hello"))  #gibt "Joel: hello" aus

# Rufen wir mal unsere Klassenmethode auf
i.get_species() #=> "H. sapiens"

# Ändern wir mal das gemeinsame Attribut
Human.species = "H. neanderthalensis"
i.get_species() #=> "H. neanderthalensis"
j.get_species() #=> "H. neanderthalensis"

# Aufruf der statischen Methode
Human.grunt() #=> "*grunt*"


####################################################
## 6. Module
####################################################

# Wir können Module importieren
import math
print(math.sqrt(16)) #=> 4.0

# Wir können auch nur spezielle Funktionen eines Moduls importieren
from math import ceil, floor
print(ceil(3.7))  #=> 4.0
print(floor(3.7)) #=> 3.0

# Wir können auch alle Funktionen eines Moduls importieren
# Warnung: Dies wird nicht empfohlen
from math import *

# Wir können Modulnamen abkürzen
import math as m
math.sqrt(16) == m.sqrt(16) #=> True

# Module sind in Python nur gewöhnliche Dateien. Wir
# können unsere eigenen schreiben und importieren. Der Name des
# Moduls ist der Dateiname.

# Wir können auch die Funktionen und Attribute eines
# Moduls herausfinden.
import math
dir(math)

####################################################
## 7. Fortgeschritten
####################################################

# Generatoren helfen, um Code schnell und einfach zu schreiben
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# Ein Generator erschafft Werte spontan
# Statt alle Werte auf einmal, wird bei jeder Iteration einer erschaffen.
# iteration.  Das heißt, Werte größer als 15 werden nicht behandelt.
# Die range-Methode ist auch ein Generator. Im Fall einer Liste von 1-900000000
# würde das sehr viel Zeit in Anspruch nehmen.
# Wenn wir eine Variable mit einem Namen erschaffen wollen, das
# normalerweise mit einem Python - Schlüsselwort kollidieren würde,
# benutzen wir einen Unterstrich nach dem Wort.
range_ = range(1, 900000000)
# Alle Nummern bis zu einem Ergebnis von >=30 werden verdoppelt
for i in double_numbers(range_):
    print(i)
    if i >= 30:
        break

# Dekoratoren
# In diesem Beispiel die Methode beg umwickelt say
# Beim Aufruf von beg, wird say aufgerufen
# Falls say_please true ist, ändert sich die ausgegebene Nachricht
from functools import wraps


def beg(target_function):
    @wraps(target_function)
    def wrapper(*args, **kwargs):
        msg, say_please = target_function(*args, **kwargs)
        if say_please:
            return "{} {}".format(msg, "Please! I am poor :(")
        return msg

    return wrapper


@beg
def say(say_please=False):
    msg = "Can you buy me a beer?"
    return msg, say_please


print(say())  # Can you buy me a beer?
print(say(say_please=True))  # Can you buy me a beer? Please! I am poor :(

```

## Lust auf mehr?

### Kostenlos online (Englisch)

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com)
* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [Ideas for Python Projects](http://pythonpracticeprojects.com)
* [The Official Docs](http://docs.python.org/3/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [A Crash Course in Python for Scientists](http://nbviewer.ipython.org/5920182)
* [Python Course](http://www.python-course.eu/index.php)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)

### Totholz (Englisch)

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)
