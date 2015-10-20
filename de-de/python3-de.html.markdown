---
language: python3
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
translators:
    - ["Valentin Ochs", "http://github.com/Cat-Ion"]
filename: learnpython3.py
---

Anmerkungen des ursprünglichen Autors:
Python wurde in den frühen Neunzigern von Guido Van Rossum entworfen. Es ist heute eine der beliebtesten Sprachen. Ich habe mich in Python wegen seiner syntaktischen Übersichtlichkeit verliebt. Im Prinzip ist es ausführbarer Pseudocode.

Feedback ist herzlich willkommen! Ihr erreicht mich unter [@louiedinh](http://twitter.com/louiedinh) oder louiedinh [at] [google's email service]

Hinweis: Dieser Artikel bezieht sich eigens auf Python 3. Lest  [das hier](http://learnxinyminutes.com/docs/python/) falls ihr das alte Python 2.7 lernen wollt.

```python
# Einzeilige Kommentare beginnen mit einer Raute

""" Mehrzeilige Zeichenketten können mit drei Anführungs-
    zeichen geschrieben werden, und werden häufig als
    Kommentare verwendet.
"""

####################################################
## 1. Primitive Datentypen und Operatoren
####################################################

# Es gibt Zahlen
3  # => 3

# Mathe funktioniert so, wie man es erwartet
1 + 1  # => 2
8 - 1  # => 7
10 * 2  # => 20

# Bis auf Division, die standardmäßig Gleitkommazahlen liefert
35 / 5  # => 7.0

# Ergebnisse von ganzzahliger Division werden nach minus unendlich
# gerundet
5 // 3     # => 1
5.0 // 3.0 # => 1.0 # funktioniert auch mit Gleitkommazahlen
-5 // 3  # => -2
-5.0 // 3.0 # => -2.0

# Wenn ihr mit Gleitkommazahlen rechnet, werden die Ergebnisse auch
# Gleitkommazahlen
3 * 2.0 # => 6.0

# Modulo Operation
7 % 3 # => 1

# Potenzierung (x hoch y)
2**4 # => 16

# Präzedenz wird mit Klammern erzwungen
(1 + 3) * 2  # => 8

# Boolesche Werte sind primitive Datentype
True
False

# Mit not wird negiert
not True  # => False
not False  # => True

# Boolesche Operatoren
# Hinweis: "and" und "or" werden klein geschrieben
True and False #=> False
False or True #=> True

# Boolesche Operatoren können mit Zahlen verwendet werden
0 and 2 #=> 0
-5 or 0 #=> -5
0 == False #=> True
2 == True #=> False
1 == True #=> True

# Gleichheit ist ==
1 == 1  # => True
2 == 1  # => False

# Ungleichheit ist !=
1 != 1  # => False
2 != 1  # => True

# Mehr Vergleiche
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# Vergleiche können verknüpft werden!
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# Strings werden mit " oder ' erzeugt
"Dies ist ein String."
'Das hier auch."

# Strings können auch addiert werden. Aber tut es bitte nicht.
"Hello " + "world!"  # => "Hello world!"
# Strings können auch ohne '+' addiert werden
"Hello " "world!"  # => "Hello world!"

# Ein String kann wie eine Liste von Zeichen verwendet werden
"Dies ist ein String"[0]  # => 'D'

# Mit .format kann man Strings formatieren:
"{} koennen {} werden".format("Strings", "interpoliert")

# Die Argumente von .format können wiederholt werden, um Tipparbeit
# zu sparen.
"{0} be nimble, {0} be quick, {0} jump over the {1}".format("Jack", "candle stick")
#=> "Jack be nimble, Jack be quick, Jack jump over the candle stick"

# Ihr könnt auch Schlüsselwörter verwenden, wenn ihr nicht zählen wollt.
"{name} möchte {nahrung} essen".format(name="Bob", nahrung="Lasagne") #=> "Bob möchte Lasagne essen"

# Falls euer Python 3 Code auch auf Python 2.5 oder älter laufen muss,
# könnt ihr auch den alten Formatierungsstil verwenden:
"%s können auch auf die %s Art %s werden" % ("Strings", "alte", "interpoliert")


# None ist ein Objekt
None  # => None

# Verwendet nicht das Gleichheitssymbol '==' um Objekte mit None
# zu vergleichen. Nutzt stattdessen 'is'. Dies überprüft Gleichheit
# der Objektidentität.
"etc" is None  # => False
None is None  # => True

# None, 0, und leere Strings/Listen/Dictionarys sind alle False.
# Alle anderen Werte sind True.
bool(0)  # => False
bool("") # => False
bool([]) #=> False
bool({}) #=> False


####################################################
## 2. Variablen und Sammlungen
####################################################

# Python hat eine print Funktion
print("Ich bin Python. Schön, dich kennenzulernen!")

# Variablen müssen vor der Zuweisung nicht deklariert werden.
# kleinschreibung_mit_unterstrichen ist die Norm.
eine_variable = 5
eine_variable     # => 5

# Zugriff auf eine bis jetzt nicht deklarierte Variable löst eine
# Exception aus. Unter "Kontrollstruktur" könnt ihr mehr über die
# Behandlung von Ausnahmen lernen.
eine_unbekannte_variable # Löst einen NameError aus

# Listen speichern Sequenzen
li = []
# Ihr könnt mit einer bereits gefüllten Liste anfangen
andere_li = [4, 5, 6]

# append fügt am Ende der Liste neue Daten ein
li.append(1)    # li ist jetzt [1]
li.append(2)    # li ist jetzt [1, 2]
li.append(4)    # li ist jetzt [1, 2, 4]
li.append(3)    # li ist jetzt [1, 2, 4, 3]
# pop entfernt Daten vom Ende
li.pop()        # => 3, und li ist jetzt [1, 2, 4]
# und dann fügen wir es wieder ein
li.append(3)    # li ist wieder [1, 2, 4, 3].

# Auf Listen kann man wie auf Arrays zugreifen
li[0]  # => 1
# Das letzte Element:
li[-1]  # => 3

# Zugriffe außerhalb der Liste führen zu einem IndexError
li[4]  # Raises an IndexError

# Auf Intervalle kann man mit der Slice-Syntax zugreifen
# (Für Mathematiker: Das Intervall ist geschlossen/offen)
li[1:3]  # => [2, 4]
# Den Anfang auslassen
li[2:]  # => [4, 3]
# Das Ende auslassen
li[:3]  # => [1, 2, 4]
# Nur jeden zweiten Eintrag auswählen
li[::2]   # =>[1, 4]
# Eine umgekehrte Kopie der Liste
li[::-1]   # => [3, 4, 2, 1]
# Ihr könnt diese Syntax beliebig kombinieren um fortgeschrittene Slices
# zu bilden:
# li[anfang:ende:schritt]

# Beliebige Elemente können mit 'del' aus Listen gelöscht werden
del li[2]   # li ist jetzt [1, 2, 3]

# Listen können addiert werden
# Hinweis: li und andere_li werden nicht modifiziert
li + andere_li   # => [1, 2, 3, 4, 5, 6]

# 'extend()' hängt Listen aneinander
li.extend(andere_li)   # Jetzt ist li [1, 2, 3, 4, 5, 6]

# Mit 'in' wird auf Existenz in einer Liste geprüft
1 in li   # => True

# 'len()' ermittelt die Länge
len(li)   # => 6


# Tupel sind wie Listen, aber unveränderlich
tup = (1, 2, 3)
tup[0]   # => 1
tup[0] = 3  # Löst einen TypeError aus

# Die meisten Listenoperationen funktionieren auch mit Listen
len(tup)   # => 3
tup + (4, 5, 6)   # => (1, 2, 3, 4, 5, 6)
tup[:2]   # => (1, 2)
2 in tup   # => True

# Tupel (oder Listen) können in Variablen entpackt werden
a, b, c = (1, 2, 3)     # a ist jetzt 1, b ist 2, und c ist 3
# Tupel werden standardmäßig erzeugt, wenn ihr die Klammern auslasst
d, e, f = 4, 5, 6
# Zwei Werte zu tauschen ist kinderleicht
e, d = d, e     # d ist jetzt 5 und e ist 4


# Dictionarys (Wörterbücher) speichern Abbildungen
leeres_dict = {}
# Hier ist ein gefülltes Wörterbuch
gefuelltes_dict = {"eins": 1, "zwei": 2, "drei": 3}

# Auf Werte wird mit [] zugegriffen
gefuelltes_dict["eins"]   # => 1

# Mit 'keys()' erhaltet ihr alle Schlüssel als ein iterierbares Objekt.
# Um daraus eine Liste zu machen, packen wir den Aufruf in 'list()'
# ein. Über iterierbare Objekte kümmern wir uns später.
# Hinweis: Sortierung von Schlüsseln in Dictionarys ist nicht
# garantiert. Eure Ergebnisse können leicht abweichen.
list(gefuelltes_dict.keys())   # => ["drei", "zwei", "eins"]


# 'values()' gibt euch ein iterierbares Objekt für alle Werte. Auch hier
# müssen wir 'list()' verwenden, um eine Liste zu erhalten. Auch hier
# ist die Reihenfolge undefiniert.
list(gefuelltes_dict.values())   # => [3, 2, 1]


# Auf Existenz in Dictionarys wird mit 'in' getestet
"eins" in gefuelltes_dict   # => True
1 in gefuelltes_dict   # => False

# Zugriff auf nicht existierende Schlüssel ist ein KeyError
gefuelltes_dict["vier"]   # KeyError

# Mit 'get()' vermeidet man den KeyError
gefuelltes_dict.get("eins")   # => 1
gefuelltes_dict.get("vier")   # => None
# Die get Methode unterstützt ein Standardargument, das zurückgegeben
# wird, wenn der Schlüssel fehlt
gefuelltes_dict.get("one", 4)   # => 1
gefuelltes_dict.get("vier", 4)  # => 4

# 'setdefault()' ändert ein Dictionary nur, wenn der gegebene Schlüssel
# noch nicht existiert
gefuelltes_dict.setdefault("fuenf", 5)  # gefuelltes_dict["fuenf"] ist jetzt 5
gefuelltes_dict.setdefault("fuenf", 6)  # Immer noch 5

# In ein Dictionary einfügen
gefuelltes_dict.update({"vier":4}) #=> {"eins": 1, "zwei": 2, "drei": 3, "vier": 4}
# gefuelltes_dict["vier"] = 4 # So geht es auch

# Mit 'del' löscht man Schlüssel aus einem Wörterbuch
del gefuelltes_dict["eins"]


# Sets speichern Mengen
leere_menge = set()
# So wird ein Set mit ein paar Werten initialisiert.
# Ja, es sieht ein bisschen aus wie ein Dictionary.
ne_menge = {1, 1, 2, 2, 3, 4}   # ne_menge ist jetzt {1, 2, 3, 4}

# Sets können auch neuen Variablen zugewiesen werden
gefuellte_menge = ne_menge

# Mehr Elemente hinzufügen
gefuellte_menge.add(5)   # gefuellte_menge ist jetzt {1, 2, 3, 4, 5}

# Mit '&' bildet ihr die Schnittmenge
andere_menge = {3, 4, 5, 6}
gefuellte_menge & andere_menge   # => {3, 4, 5}

# '|' ist die Vereinigung
gefuellte_menge | andere_menge   # => {1, 2, 3, 4, 5, 6}

# '-' bildet die Differenz
{1, 2, 3, 4} - {2, 3, 5}   # => {1, 4}

# Mit 'in' wird auf Existenz geprüft
2 in gefuellte_menge  # => True
10 in gefuellte_menge # => False


####################################################
## 3. Kontrollstruktur und iterierbare Objekte
####################################################

# Erstellen wir mal eine Variable
eine_var = 5

# Hier ist eine if-Anweisung. Einrückung ist in Python wichtig!
# Gibt "eine_var ist kleiner als 10" aus.
if eine_var > 10:
    print("eine_var ist viel groesser als 10.")
elif eine_var < 10:    # Der elif-Abschnitt ist optional
    print("eine_var ist kleiner als 10.")
else:                  # Das hier ist auch optional
    print("eine_var muss wohl 10 sein.")


"""
For Schleifen iterieren über Listen
Ausgabe:
    hund ist ein Säugetier
    katze ist ein Säugetier
    maus ist ein Säugetier
"""
for tier in ["hund", "katze", "maus"]:
    # Mit format() könnt ihr formatierte Strings interpolieren
    print("{} ist ein Säugetier".format(tier))

"""
"range(Zahl)" gibt ein iterierbares Objekt zurück, das von 0 bis zur
gegebenen Zahl geht
Ausgabe:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
"range(untere_zahl, obere_zahl)" gibt ein iterierbares Objekt zurück,
das von untere_zahl bis obere_zahl geht.
Ausgabe:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
While Schleifen laufen, bis eine Bedingung nicht mehr zutrifft.
Ausgabe:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # Kurz für x = x + 1

# Mit try/except Blöcken werden Ausnahmen behandelt
try:
    # Mit "raise" werden Fehler ausgegeben
    raise IndexError("Dies ist ein IndexError")
except IndexError as e:
    pass    # Pass tut nichts. Normalerweise würden wir hier den Fehler behandeln.
except (TypeError, NameError):
    pass    # Mehrere Ausnahmen können zusammen behandelt werden.
else:   # Optionaler Block, der zu try/except gehört.
    print("Alles gut!")   # Wird nur ausgeführt, wenn es in try keine Ausnahmen gab
finally: #  Wird auf jeden Fall ausgeführt
    print("Hier können Ressourcen befreit werden")

# Anstatt Ressourcen mit try/finally zu befreien können wir eine with-Anweisung verwenden
with open("datei.txt") as f:
    for line in f:
        print(line)

# Python hat eine grundlegende Abstraktion, die 'Iterable'
# (iterierbares Objekt) heißt. Iterables können wie Sequenzen behandelt
# werden. Das Objekt, das von der range Funktion zurückgegeben wird, ist
# iterierbar.

gefuelltes_dict = {"eins": 1, "zwei": 2, "drei": 3}
unser_iterable = gefuelltes_dict.keys()
print(unser_iterable) #=> range(1,10). Dies ist ein Objekt, das das Iterable Interface implementiert.

# Wir können es in einer Schleife durchlaufen.
for i in unser_iterable:
    print(i)    # Gibt eins, zwei, drei aus

# Wir können allerdings nicht mit einem Index darauf zugreifen
unser_iterable[1]  # Löst einen TypeError aus

# Aus iterierbaren Objekten kann man Iteratoren bilden
unser_iterator = iter(unser_iterable)

# Unser Iterator ist ein Objekt, das sich den Zustand merkt während wir
# es durchlaufen.
# Das nächste Objekt wird mit 'next()' geholt.
next(unser_iterator)  #=> "eins"

# Es merkt sich beim Iterieren die aktuelle Position
# It maintains state as we iterate.
next(unser_iterator)  #=> "zwei"
next(unser_iterator)  #=> "drei"

# Wenn der Iterator alle Werte ausgegeben hat, löst er eine StopIterator
# Exception aus.
next(unser_iterator) # Löst StopIteration aus

# Mit 'list()' könnt ihr alle Elemente eines Iterators auf ein Mal holen.
list(gefuelltes_dict.keys())  #=> Gibt ["eins", "zwei", "drei"] aus


####################################################
## 4. Funktionen
####################################################

# Mit 'def' erzeugen wir neue Funktionen
def addiere(x, y):
    print("x ist {} und y ist {}".format(x, y))
    return x + y    # Werte werden mit return zurückgegeben

# Funktionen mit Parametern aufrufen
addiere(5, 6)   # => Gibt "x ist 5 und y ist 6" aus, und 11 zurück

# Ein anderer Weg des Funktionsaufrufs sind Schlüsselwort Argumente
addiere(y=6, x=5)   # Schlüsselwörter können in beliebiger Reihenfolge übergeben werden

# Ihr könnt Funktionen definieren, die eine variable Anzahl von
# Positionsargumenten annehmen.
def varargs(*args):
    return args

varargs(1, 2, 3)   # => (1, 2, 3)

# Ihr könnt auch Funktionen definieren, die eine variable Anzahl von
# Schlüsselwortargumenten annehmen
def schluesselwort_args(**kwargs):
    return kwargs

# Rufen wir sie mal auf, um zu sehen, was passiert
schluesselwort_args(big="foot", loch="ness")   # => {"big": "foot", "loch": "ness"}


# Ihr könnt auch beides gleichzeitig tun, wenn ihr wollt
def alle_argumente(*args, **kwargs):
    print(args)
    print(kwargs)
"""
alle_argumente(1, 2, a=3, b=4) gibt aus:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Beim Funktionsaufruf könnt ihr das Gegenteil von args/kwargs machen!
# Mit * werden Tupel ausgeweitet, und mit ** kwargs.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
alle_argumente(*args)    # Äquivalent zu alle_argumente(1, 2, 3, 4)
alle_argumente(**kwargs) # Äquivalent zu alle_argumente(a=3, b=4)
alle_argumente(*args, **kwargs) # Äquivalent zu alle_argumente(1, 2, 3, 4, a=3, b=4)


# Sichtbarkeitsbereich von Funktionen
x = 5

def setze_x(num):
    # Lokale Variable x ist nicht das gleiche wie die globale Variable x
    x = num # => 43
    print (x) # => 43

def setze_globales_x(num):
    global x
    print (x) # => 5
    x = num # Jetzt ist das globale x 6
    print (x) # => 6

setze_x(43)
setze_globales_x(6)


# Python hat First-Class-Funktionen
def erzeuge_addierer(x):
    def addierer(y):
        return x + y
    return addierer

addiere_10 = erzeuge_addierer(10)
addiere_10(3)   # => 13

# Es gibt auch anonyme Funktionen
(lambda x: x > 2)(3)   # => True

# TODO: Für iterierbare Objekte anpassen
# Es gibt auch Funktionen höherer Ordnung als Built-Ins
map(addiere_10, [1, 2, 3])   # => [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7])   # => [6, 7]

# Wir können für map und filter auch List Comprehensions verwenden
# List Comprehensions speichern die Ausgabe als eine Liste, die auch
# eine verschachtelte Liste sein kann.
[addiere_10(i) for i in [1, 2, 3]]  # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]   # => [6, 7]

####################################################
## 5. Klassen
####################################################


# Wir bilden die Unterklasse von 'object' um eine Klasse zu erhalten
class Mensch(object):

    # Ein Klassenattribut. Es wird von allen Instanzen der Klasse geteilt.
    spezies = "H. sapiens"

    # Ein einfacher Konstruktor der gerufen wird, wenn diese Klasse
    # instanziiert wird.
    # Hinweis: Die doppelten Unterstriche am Anfang und Ende zeigen,
    # dass dies Objekte oder Attribute sind, die von Python verwendet
    # werden, aber vom Benutzer kontrolliert werden. Methoden (oder
    # Objekte, oder Attribute) wie __init__, __str__, __repr__ und
    # so weiter sind so genannte 'magische Methoden' (manchmal auch
    # 'dunder' Methoden).
    # Solche Namen solltet ihr nicht selber erfinden.
    def __init__(self, name):
        # Weise das Argument dem name-Attribut der Instanz zu
        self.name = name

    # Eine Instanzmethode. Alle Methoden nehmen 'self' als erstes
    # Argument.
    def sag(self, nachricht):
        return "{name}: {nachricht}".format(name=self.name, nachricht=nachricht)

    # Eine Klassenmethode wird von allen Instanzen geteilt.
    # Ihr erstes Argument ist die aufrufende Klasse.
    @classmethod
    def get_spezies(cls):
        return cls.spezies

    # Eine statische Methode wird ohne Klassen- oder Instanzenreferenz
    # aufgerufen.
    @staticmethod
    def grunze():
        return "*grunz*"


# Instanziiere eine Klasse
i = Mensch(name="Ian")
print(i.sag("Hi"))     # Gibt "Ian: Hi" aus

j = Mensch("Joel")
print(j.sag("Hallo"))  # Gibt "Joel: Hallo" aus

# Rufen wir mal unsere Klassenmethode auf
i.get_spezies()   # => "H. sapiens"

# Jetzt ändern wir das gemeinsame Attribut
Mensch.spezies = "H. neanderthalensis"
i.get_spezies()   # => "H. neanderthalensis"
j.get_spezies()   # => "H. neanderthalensis"

# Aufruf der statischen Methode
Mensch.grunze()   # => "*grunz*"


####################################################
## 6. Module
####################################################

# Wir können Module importieren
import math
print(math.sqrt(16))  # => 4

# Wir können auch nur bestimmte Funktionen importieren
from math import ceil, floor
print(ceil(3.7))  # => 4.0
print(floor(3.7))   # => 3.0

# Wir können auch alle Funktionen aus einem Modul importieren.
# Warnung: Dies wird nicht empfohlen
from math import *

# Wir können Modulnamen abkürzen
import math as m
math.sqrt(16) == m.sqrt(16)   # => True

# Python Module sind ganz normale Python Dateien. Wir können unsere
# eigenen schreiben und sie importieren. Der Name des Moduls ist der
# Dateiname.

# Wir können auch herausfinden, welche Funktionen und Attribute ein
# Modul definiert.
import math
dir(math)


####################################################
## 7. Fortgeschritten
####################################################

# Generatoren helfen uns, faulen Code zu schreiben
def verdopple_zahlen(iterable):
    for i in iterable:
        yield i + i

# Generatoren erzeugen Werte im Fluge.
# Anstatt alle Werte auf einmal zu erzeugen und zurückzugeben erzeugen
# sie einen Wert pro Iteration, so dass im folgenden Beispiel von
# verdopple_zahlen() keine Zahlen verarbeitet werden, die größer als 15
# sind.
# Auch 'range' ist ein Generator. Eine Liste die von 1 bis 900000000
# läuft zu erzeugen würde sehr lange dauern.
# Wir verwenden einen abschließenden Unterstrich in Variablennamen,
# wenn der Name sonst mit einem Schlüsselwort von Python kollidieren
# würde.
range_ = range(1, 900000000)
# Verdopple alle Zahlen, bis ein Ergebnis >= 30 gefunden wird
for i in double_numbers(range_):
    print(i)
    if i >= 30:
        break


# Dekoratoren
# In diesem Beispiel ist 'sage' in 'flehe' eingepackt.
# 'flehe' wird 'sage' aufrufen. Falls 'sage_bitte' True ist,
# wird die zurückgegebene Nachricht geändert.
from functools import wraps


def flehe(ziel_funktion):
    @wraps(ziel_funktion)
    def wrapper(*args, **kwargs):
        nachricht, sage_bitte = ziel_funktion(*args, **kwargs)
        if sage_bitte:
            return "{} {}".format(nachricht, "Bitte! Ich bin arm :(")
        return msg

    return wrapper


@flehe
def sage(sage_bitte=False):
    nachricht = "Kannst Du mir ein Bier kaufen?"
    return nachricht, sage_bitte


print(sage())  # Kannst Du mir ein Bier kaufen?
print(sage(sage_bitte=True))  # Kannst Du mir ein Bier kaufen? Bitte! Ich bin arm :(
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
