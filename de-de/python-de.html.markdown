---
language: python
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
	- ["kultprok", "http:/www.kulturproktologie.de"]
filename: learnpython.py
---

Anmerkungen des urspr�nglichen Autors:
Python wurde in den fr�hen Neunzigern von Guido van Rossum entworfen. Es ist heute eine der beliebtesten Sprachen. Ich habe mich in Python wegen seiner syntaktischen �bersichtlichkeit verliebt. Eigentlich ist es ausf�hrbarer Pseudocode.

Feedback ist herzlich willkommen! Ihr erreicht mich unter [@louiedinh](http://twitter.com/louiedinh) oder louiedinh [at] [google's email service]

Hinweis: Dieser Beitrag bezieht sich besonders auf Python 2.7, er sollte aber auf Python 2.x anwendbar sein. Haltet Ausschau nach einem Rundgang durch Python 3, der bald erscheinen soll.

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

# Mathematik ist das, was man erwartet
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# Division ist ein wenig kniffliger. Es ist ganzzahlige Division
# und rundet automatisch ab.
5 / 2 #=> 2

# Um das zu �ndern, m�ssen wir Gleitkommazahlen kennenlernen.
2.0     # Das ist eine Gleitkommazahl
11.0 / 4.0 #=> 2.75 Ahhh...schon besser

# Rangfolge wird mit Klammern erzwungen
(1 + 3) * 2 #=> 8

# Boolesche Ausdr�cke sind primitive Datentypen
True
False

# Mit not wird negiert
not True #=> False
not False #=> True

# Gleichheit ist ==
1 == 1 #=> True
2 == 1 #=> False

# Ungleichheit is !=
1 != 1 #=> False
2 != 1 #=> True

# Ein paar weitere Vergleiche
1 < 10 #=> True
1 > 10 #=> False
2 <= 2 #=> True
2 >= 2 #=> True

# Vergleiche k�nnen verkn�pft werden!
1 < 2 < 3 #=> True
2 < 3 < 2 #=> False

# Strings werden mit " oder ' gebildet
"Das ist ein String."
'Das ist auch ein String.'

# Strings k�nnen addiert werden!
"Hello " + "world!" #=> "Hello world!"

# Ein String kann wie eine Liste von Zeichen verwendet werden
"Das ist ein String"[0] #=> 'D'

# Mit % k�nnen Strings formatiert werden, etwa so:
"%s k�nnen %s werden" % ("Strings", "interpoliert")

# Ein neuerer Weg, um Strings zu formatieren, ist die format-Methode.
# Diese Methode wird bevorzugt
"{0} k�nnen {1} werden".format("Strings", "formatiert")
# Wir k�nnen Schl�sselw�rter verwenden, wenn wir nicht abz�hlen wollen.
"{name} will {food} essen".format(name="Bob", food="Lasagne")

# None ist ein Objekt
None #=> None

# Verwendet nicht das Symbol f�r Gleichheit `==`, um Objekte mit None zu vergleichen
# Benutzt stattdessen `is`
"etc" is None #=> False
None is None  #=> True

# Der 'is'-Operator testet Objektidentit�t. Das ist nicht
# sehr n�tzlich, wenn wir mit primitiven Datentypen arbeiten, aber
# sehr n�tzlich bei Objekten.

# None, 0, und leere Strings/Listen werden alle als False bewertet.
# Alle anderen Werte sind True
0 == False  #=> True
"" == False #=> True


####################################################
## 2. Variablen und Collections
####################################################

# Ausgabe ist sehr einfach
print "Ich bin Python. Sch�n, dich kennenzulernen!"


# Es gibt keinen Grund, Variablen vor der Zuweisung zu deklarieren.
some_var = 5    # kleinschreibung_mit_unterstrichen entspricht der Norm
some_var #=> 5

# Eine noch nicht deklarierte Variable anzusprechen, l�st eine Exception aus.
# Siehe Kontrollstruktur, um mehr �ber Ausnahmebehandlung zu lernen.
some_other_var  # L�st einen NameError aus

# if kann als Ausdruck verwendet werden
"yahoo!" if 3 > 2 else 2 #=> "yahoo!"

# Listen speichern Sequenzen
li = []
# Wir k�nnen mit einer bereits gef�llten Liste anfangen
other_li = [4, 5, 6]

# append f�gt Daten am Ende der Liste ein
li.append(1)    #li ist jetzt [1]
li.append(2)    #li ist jetzt [1, 2]
li.append(4)    #li ist jetzt [1, 2, 4]
li.append(3)    #li ist jetzt [1, 2, 4, 3]
# Vom Ende der Liste mit pop entfernen
li.pop()        #=> 3 und li ist jetzt [1, 2, 4]
# F�gen wir es wieder hinzu
li.append(3)    # li ist jetzt wieder [1, 2, 4, 3].

# Greife auf Listen wie auf Arrays zu
li[0] #=> 1
# Das letzte Element ansehen
li[-1] #=> 3

# Au�erhalb der Liste ist es ein IndexError
li[4] # Raises an IndexError

# Wir k�nnen uns Ranges mit Slice-Syntax ansehen
li[1:3] #=> [2, 4]
# Den Anfang auslassen
li[2:] #=> [4, 3]
# Das Ende auslassen
li[:3] #=> [1, 2, 4]

# Ein bestimmtes Element mit del aus der Liste entfernen
del li[2] # li ist jetzt [1, 2, 3]

# Listen k�nnen addiert werden
li + other_li #=> [1, 2, 3, 4, 5, 6] - Hinweis: li und other_li werden in Ruhe gelassen

# Listen mit extend verkn�pfen
li.extend(other_li) # Jetzt ist li [1, 2, 3, 4, 5, 6]

# Mit in auf Existenz eines Elements pr�fen
1 in li #=> True

# Die L�nge der Liste mit len ermitteln
len(li) #=> 6


# Tupel sind wie Listen, nur unver�nderlich.
tup = (1, 2, 3)
tup[0] #=> 1
tup[0] = 3  # L�st einen TypeError aus

# Wir k�nnen all diese Listen-Dinge auch mit Tupeln anstellen
len(tup) #=> 3
tup + (4, 5, 6) #=> (1, 2, 3, 4, 5, 6)
tup[:2] #=> (1, 2)
2 in tup #=> True

# Wir k�nnen Tupel (oder Listen) in Variablen entpacken
a, b, c = (1, 2, 3)     # a ist jetzt 1, b ist jetzt 2 und c ist jetzt 3
# Tuple werden standardm��ig erstellt, wenn wir uns die Klammern sparen
d, e, f = 4, 5, 6
# Es ist kinderleicht zwei Werte zu tauschen
e, d = d, e     # d is now 5 and e is now 4


# Dictionarys (W�rterbucher) speichern Key-Value-Paare
empty_dict = {}
# Hier ein gef�lltes W�rterbuch
filled_dict = {"one": 1, "two": 2, "three": 3}

# Wir k�nnen Eintr�ge mit [] nachschlagen
filled_dict["one"] #=> 1

# So holen wir alle Keys (Schl�ssel) als Liste
filled_dict.keys() #=> ["three", "two", "one"]
# Hinweis - Die Reihenfolge von Schl�sseln in der Liste ist nicht garantiert.
# Einzelne Resultate k�nnen anders angeordnet sein.

# Alle Values (Werte) als Liste
filled_dict.values() #=> [3, 2, 1]
# Hinweis - Hier gelten dieselben Einschr�nkungen f�r die Reihenfolge wie bei Schl�sseln.

# Das Vorhandensein eines Schl�ssels im W�rterbuch mit in pr�fen
"one" in filled_dict #=> True
1 in filled_dict #=> False

# Einen nicht vorhandenenen Schl�ssel zu suchen, l�st einen KeyError aus
filled_dict["four"] # KeyError

# Mit der get-Methode verhindern wir das
filled_dict.get("one") #=> 1
filled_dict.get("four") #=> None
# Die get-Methode unterst�tzt auch ein Standardargument, falls der Wert fehlt
filled_dict.get("one", 4) #=> 1
filled_dict.get("four", 4) #=> 4

# Die setdefault-Methode ist ein sicherer Weg, ein neues Schl�ssel-Wert-Paar anzulegen
filled_dict.setdefault("five", 5) #filled_dict["five"] wird auf 5 gesetzt
filled_dict.setdefault("five", 6) #filled_dict["five"] ist noch immer 5


# Sets speichern Mengen
empty_set = set()
# Initialisieren wir ein Set mit ein paar Werten
some_set = set([1,2,2,3,4]) # some_set ist jetzt set([1, 2, 3, 4])

# Seit Python 2.7 kann {} benutzt werden, um ein Set zu erstellen
filled_set = {1, 2, 2, 3, 4} # => {1 2 3 4}

# Mehr Elemente hinzuf�gen
filled_set.add(5) # filled_set is now {1, 2, 3, 4, 5}

# Schnittmengen werden mit & gebildet
other_set = {3, 4, 5, 6}
filled_set & other_set #=> {3, 4, 5}

# Mengen werden mit | vereinigt
filled_set | other_set #=> {1, 2, 3, 4, 5, 6}

# Die Differenz einer Menge mit - bilden
{1,2,3,4} - {2,3,5} #=> {1, 4}

# Auf Vorhandensein mit in pr�fen
2 in filled_set #=> True
10 in filled_set #=> False


####################################################
## 3. Kontrollstruktur
####################################################

# Erstellen wir mal eine Variable
some_var = 5

# Hier eine if-Anweisung. Die Einr�ckung ist in Python wichtig!
# gibt "some_var ist kleiner als 10" aus
if some_var > 10:
    print "some_var ist viel gr��er als 10."
elif some_var < 10:    # Dieser elif-Absatz ist optional.
    print "some_var ist kleiner als 10."
else:           # Das hier ist auch optional.
    print "some_var ist tats�chlich 10."


"""
For-Schleifen iterieren �ber Listen
Ausgabe:
    hund ist ein S�ugetier
    katze ist ein S�ugetier
    maus ist ein S�ugetier
"""
for animal in ["hund", "katze", "maus"]:
    # Wir k�nnen Strings mit % formatieren
    print "%s  ist ein S�ugetier" % animal
    
"""
`range(Zahl)` gibt eine null-basierte Liste bis zur angegebenen Zahl wieder
Ausgabe:
    0
    1
    2
    3
"""
for i in range(4):
    print i

"""
While-Schleifen laufen, bis eine Bedingung erf�llt ist.
Ausgabe:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # Kurzform f�r x = x + 1

# Ausnahmebehandlung mit einem try/except-Block

# Funktioniert in Python 2.6 und h�her:
try:
    # Mit raise wird ein Fehler ausgegeben
    raise IndexError("Das hier ist ein Index-Fehler")
except IndexError as e:
    pass    # Pass ist nur eine no-op. Normalerweise w�rden wir hier den Fehler kl�ren.


####################################################
## 4. Funktionen
####################################################

# Mit def neue Funktionen erstellen
def add(x, y):
    print "x ist %s und y ist %s" % (x, y)
    return x + y    # Werte werden mit return zur�ckgegeben

# Funktionen mit Parametern aufrufen
add(5, 6) #=> Ausgabe ist "x ist 5 und y ist 6" und gibt 11 zur�ck

# Ein anderer Weg des Funktionsaufrufs sind Schl�sselwort-Argumente
add(y=6, x=5)   # Schl�sselw�rter k�nnen in beliebiger Reihenfolge �bergeben werden.

# Wir k�nnen Funktionen mit beliebiger Anzahl von # Positionsargumenten definieren
def varargs(*args):
    return args

varargs(1, 2, 3) #=> (1,2,3)


# Wir k�nnen auch Funktionen mit beliebiger Anzahl
# Schl�sselwort-Argumenten definieren
def keyword_args(**kwargs):
    return kwargs

# Rufen wir es mal auf, um zu sehen, was passiert
keyword_args(big="foot", loch="ness") #=> {"big": "foot", "loch": "ness"}

# Wir k�nnen beides gleichzeitig machem, wenn wir wollen
def all_the_args(*args, **kwargs):
    print args
    print kwargs
"""
all_the_args(1, 2, a=3, b=4) Ausgabe:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Beim Aufruf von Funktionen k�nnen wir das Gegenteil von varargs/kwargs machen!
# Wir benutzen dann *, um Tupel auszuweiten, und ** f�r kwargs.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args) # �quivalent zu foo(1, 2, 3, 4)
all_the_args(**kwargs) # �quivalent zu foo(a=3, b=4)
all_the_args(*args, **kwargs) # �quivalent zu  foo(1, 2, 3, 4, a=3, b=4)

# Python hat First-Class-Funktionen
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3) #=> 13

# Es gibt auch anonyme Funktionen
(lambda x: x > 2)(3) #=> True

# Es gibt auch Funktionen h�herer Ordnung als Built-Ins
map(add_10, [1,2,3]) #=> [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7]) #=> [6, 7]

# Wir k�nnen bei map- und filter-Funktionen auch List Comprehensions einsetzen
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
       return "%s: %s" % (self.name, msg)

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
print i.say("hi")     # gitbt "Ian: hi" aus

j = Human("Joel")
print j.say("hello")  #gibt "Joel: hello" aus

# Rufen wir mal unsere Klassenmethode auf
i.get_species() #=> "H. sapiens"

# �ndern wir mal das gemeinsame Attribut
Human.species = "H. neanderthalensis"
i.get_species() #=> "H. neanderthalensis"
j.get_species() #=> "H. neanderthalensis"

# Aufruf der statischen Methode
Human.grunt() #=> "*grunt*"


####################################################
## 6. Module
####################################################

# Wir k�nnen Module importieren
import math
print math.sqrt(16) #=> 4

# Wir k�nnen auch nur spezielle Funktionen eines Moduls importieren
from math import ceil, floor
print ceil(3.7)  #=> 4.0
print floor(3.7) #=> 3.0

# Wir k�nnen auch alle Funktionen eines Moduls importieren
# Warnung: Dies wird nicht empfohlen
from math import *

# Wir k�nnen Modulnamen abk�rzen
import math as m
math.sqrt(16) == m.sqrt(16) #=> True

# Module sind in Python nur gew�hnliche Dateien. Wir
# k�nnen unsere eigenen schreiben und importieren. Der Name des 
# Moduls ist der Dateiname.

# Wir k�nnen auch die Funktionen und Attribute eines
# Moduls herausfinden.
import math
dir(math)


```

## Lust auf mehr?

### Kostenlos online (Englisch)

* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [The Official Docs](http://docs.python.org/2.6/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Module of the Week](http://pymotw.com/2/)

### Totholz (Englisch)

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)

