---
language: python
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
translators:
    - ["kultprok", "http:/www.kulturproktologie.de"]
filename: learnpython-de.py
lang: de-de
---

Anmerkungen des ursprünglichen Autors:
Python wurde in den frühen Neunzigern von Guido van Rossum entworfen. Es ist heute eine der beliebtesten Sprachen. Ich habe mich in Python wegen seiner syntaktischen Übersichtlichkeit verliebt. Eigentlich ist es ausführbarer Pseudocode.

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

# Mathematik funktioniert so, wie man das erwartet
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# Division ist ein wenig kniffliger. Ganze Zahlen werden ohne Rest dividiert
# und das Ergebnis wird automatisch abgerundet.
5 / 2 #=> 2

# Um das zu ändern, müssen wir Gleitkommazahlen einführen und benutzen
2.0     # Das ist eine Gleitkommazahl
11.0 / 4.0 #=> 2.75 Ahhh...schon besser

# Rangfolge wird mit Klammern erzwungen
(1 + 3) * 2 #=> 8

# Boolesche Ausdrücke sind primitive Datentypen
True
False

# Mit not wird negiert
not True #=> False
not False #=> True

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

# Strings können addiert werden!
"Hello " + "world!" #=> "Hello world!"

# Ein String kann wie eine Liste von Zeichen verwendet werden
"Das ist ein String"[0] #=> 'D'

# Mit % können Strings formatiert werden, etwa so:
"%s können %s werden" % ("Strings", "interpoliert")

# Ein modernerer Weg, um Strings zu formatieren, ist die format-Methode.
# Diese Methode wird bevorzugt
"{0} können {1} werden".format("Strings", "formatiert")
# Wir können Schlüsselwörter verwenden, wenn wir nicht abzählen wollen.
"{name} will {food} essen".format(name="Bob", food="Lasagne")

# None ist ein Objekt
None #=> None

# Verwendet nicht das Symbol für Gleichheit `==`, um Objekte mit None zu vergleichen
# Benutzt stattdessen `is`
"etc" is None #=> False
None is None  #=> True

# Der 'is'-Operator testet Objektidentität. Das ist nicht
# sehr nützlich, wenn wir mit primitiven Datentypen arbeiten, aber
# sehr nützlich bei Objekten.

# None, 0, und leere Strings/Listen werden alle als False bewertet.
# Alle anderen Werte sind True
0 == False  #=> True
"" == False #=> True


####################################################
## 2. Variablen und Collections
####################################################

# Textausgabe ist sehr einfach
print "Ich bin Python. Schön, dich kennenzulernen!"


# Es gibt keinen Grund, Variablen vor der Zuweisung zu deklarieren.
some_var = 5    # kleinschreibung_mit_unterstrichen entspricht der Norm
some_var #=> 5

# Das Ansprechen einer noch nicht deklarierte Variable löst eine Exception aus.
# Unter "Kontrollstruktur" kann noch mehr über
# Ausnahmebehandlung erfahren werden.
some_other_var  # Löst einen NameError aus

# if kann als Ausdruck verwendet werden
"yahoo!" if 3 > 2 else 2 #=> "yahoo!"

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
li[4] # Raises an IndexError

# Wir können uns Ranges mit Slice-Syntax ansehen
li[1:3] #=> [2, 4]
# Den Anfang auslassen
li[2:] #=> [4, 3]
# Das Ende auslassen
li[:3] #=> [1, 2, 4]

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
# Es ist kinderleicht zwei Werte zu tauschen
e, d = d, e     # d is now 5 and e is now 4


# Dictionarys (Wörterbucher) speichern Key-Value-Paare
empty_dict = {}
# Hier ein gefülltes Wörterbuch
filled_dict = {"one": 1, "two": 2, "three": 3}

# Wir können Einträge mit [] nachschlagen
filled_dict["one"] #=> 1

# So holen wir alle Keys (Schlüssel) als Liste
filled_dict.keys() #=> ["three", "two", "one"]
# Hinweis - Die Reihenfolge von Schlüsseln in der Liste ist nicht garantiert.
# Einzelne Resultate können anders angeordnet sein.

# Alle Values (Werte) als Liste
filled_dict.values() #=> [3, 2, 1]
# Hinweis - Hier gelten dieselben Einschränkungen für die Reihenfolge wie bei Schlüsseln.

# Das Vorhandensein eines Schlüssels im Wörterbuch mit in prüfen
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


# Sets speichern Mengen
empty_set = set()
# Initialisieren wir ein Set mit ein paar Werten
some_set = set([1,2,2,3,4]) # some_set ist jetzt set([1, 2, 3, 4])

# Seit Python 2.7 kann {} benutzt werden, um ein Set zu erstellen
filled_set = {1, 2, 2, 3, 4} # => {1 2 3 4}

# Mehr Elemente hinzufügen
filled_set.add(5) # filled_set is now {1, 2, 3, 4, 5}

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
## 3. Kontrollstruktur
####################################################

# Erstellen wir mal eine Variable
some_var = 5

# Hier eine if-Anweisung. Die Einrückung ist in Python wichtig!
# gibt "some_var ist kleiner als 10" aus
if some_var > 10:
    print "some_var ist viel größer als 10."
elif some_var < 10:    # Dieser elif-Absatz ist optional.
    print "some_var ist kleiner als 10."
else:           # Das hier ist auch optional.
    print "some_var ist tatsächlich 10."


"""
For-Schleifen iterieren über Listen
Ausgabe:
    hund ist ein Säugetier
    katze ist ein Säugetier
    maus ist ein Säugetier
"""
for animal in ["hund", "katze", "maus"]:
    # Wir können Strings mit % formatieren
    print "%s  ist ein Säugetier" % animal
    
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
While-Schleifen laufen, bis eine Bedingung erfüllt ist.
Ausgabe:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # Kurzform für x = x + 1

# Ausnahmebehandlung mit einem try/except-Block

# Funktioniert in Python 2.6 und höher:
try:
    # Mit raise wird ein Fehler ausgegeben
    raise IndexError("Das hier ist ein Index-Fehler")
except IndexError as e:
    pass    # Pass ist nur eine no-op. Normalerweise würden wir hier den Fehler klären.


####################################################
## 4. Funktionen
####################################################

# Mit def neue Funktionen erstellen
def add(x, y):
    print "x ist %s und y ist %s" % (x, y)
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

# Wir können beides gleichzeitig machem, wenn wir wollen
def all_the_args(*args, **kwargs):
    print args
    print kwargs
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
## 5. Module
####################################################

# Wir können Module importieren
import math
print math.sqrt(16) #=> 4.0

# Wir können auch nur spezielle Funktionen eines Moduls importieren
from math import ceil, floor
print ceil(3.7)  #=> 4.0
print floor(3.7) #=> 3.0

# Wir können auch alle Funktionen eines Moduls importieren
# Warnung: Dies wird nicht empfohlen
from math import *

# Wir können Modulnamen abkürzen
import math as m
math.sqrt(16) == m.sqrt(16) #=> True

# Module sind in Python nur gewöhnliche Dateien. Wir
# können unsere eigenen schreiben und importieren. Der Name des 
# Moduls ist der Dateiname.

# Wir können herausfinden, welche Funktionen und Attribute in einem 
# Modul definiert sind.
import math
dir(math)

# Wenn Sie ein Python-Skript namens math.py im selben Ordner 
# wie Ihr aktuelles Skript haben, wird die Datei math.py 
# anstelle des integrierten Python-Moduls geladen.
# Dies geschieht, weil der lokale Ordner Vorrang
# vor den in Python integrierten Bibliotheken hat.


####################################################
## 6. Klassen
####################################################

# Wir verwenden das Schlüsselwort "class" um eine Klasse zu erzeugen.
class Human(object):

    # Ein Klassenattribut. Es wird von allen Instanzen einer Klasse geteilt
    species = "H. sapiens"

    # Ein simpler Konstruktor, wird aufgerufen, wenn diese Klasse instanziiert wird.
    # Beachten Sie, dass die doppelten vorangestellten und nachgestellten 
    # Unterstriche Objekte oder Attribute bezeichnen, die von Python verwendet werden, 
    # aber in benutzergesteuerten Namespaces leben. 
    # Methoden (oder Objekte oder Attribute) wie: __init__, __str__, __repr__ usw. 
    # werden als Sondermethoden (oder manchmal als Dundermethoden bezeichnet) bezeichnet.
    # Sie sollten solche Namen nicht selbst erfinden.
    def __init__(self, name):
        # Wir weisen das Argument name dem name-Attribut der Instanz zu
        self.name = name

    # Eine Instanzmethode. Alle Methoden erhalten "self" als erstes Argument.
    def say(self, msg):
       return "%s: %s" % (self.name, msg)

    # Eine weitere Instanzmethode
    def sing(self):
        return 'yo... yo... microphone check... one two... one two...'

    # Eine Klassenmethode wird von allen Instanzen geteilt.
    # Sie werden mit der aufrufenden Klasse als erstem Argument aufgerufen
    @classmethod
    def get_species(cls):
        return cls.species

    # Eine statische Methode wird ohne Klasse oder Instanz aufgerufen
    @staticmethod
    def grunt():
        return "*grunt*"

    # Eine Eigenschaft (Property) ist wie ein Getter.
    # Es verwandelt die Methode age() in ein schreibgeschütztes Attribut mit demselben Namen.
    # Es ist jedoch nicht nötig, triviale Getter und Setter in Python zu schreiben.
    @property
    def age(self):
        return self._age

    # Damit kann die Eigenschaft festgelegt werden
    @age.setter
    def age(self, age):
        self._age = age

    # Damit kann die Eigenschaft gelöscht werden
    @age.deleter
    def age(self):
        del self._age

# Wenn ein Python-Interpreter eine Quelldatei liest, führt er den gesamten Code aus.
# Diese __name__-Prüfung stellt sicher, dass dieser Codeblock nur ausgeführt wird, 
# wenn dieses Modul das Hauptprogramm ist.
if __name__ == '__main__':
    # Eine Instanz einer Klasse erstellen
    i = Human(name="Ian")
    i.say("hi")                     # "Ian: hi"
    j = Human("Joel")
    j.say("hello")                  # "Joel: hello"
    # i und j sind Instanzen des Typs Mensch, oder anders ausgedrückt: Sie sind Objekte des Menschen

    # Rufen wir unsere Klassenmethode auf
    i.say(i.get_species())          # "Ian: H. sapiens"

    # Ändern wir das gemeinsame Attribut
    Human.species = "H. neanderthalensis"
    i.say(i.get_species())          # => "Ian: H. neanderthalensis"
    j.say(j.get_species())          # => "Joel: H. neanderthalensis"
    
    # Aufruf der statischen Methode
    print(Human.grunt())            # => "*grunt*"

    # Kann keine statische Methode mit Instanz des Objekts aufrufen, 
    # da i.grunt () automatisch "self" (das Objekt i) als Argument verwendet
    print(i.grunt())                # => TypeError: grunt() takes 0 positional arguments but 1 was given

    # Die Eigenschaft für diese Instanz aktualisieren
    i.age = 42
    # die Eigenschaft auslesen
    i.say(i.age)                    # => "Ian: 42"
    j.say(j.age)                    # => "Joel: 0"
    # die Eigenschaft löschen
    del i.age
    # i.age                         # => würde einen AttributeError werfen
    
####################################################
## 6.1 Inheritance
####################################################
    
# Vererbung ermöglicht die Definition neuer untergeordneter Klassen, 
# die Methoden und Variablen von ihrer übergeordneten Klasse erben.

# Wenn Sie die oben definierte Human-Klasse als Basis- oder Elternklasse verwenden, 
# können Sie eine untergeordnete Klasse, Superhero, definieren, die die Klassenvariablen 
# wie "species", "name" und "age" sowie Methoden wie "sing" und "grunzen" aus der Klasse Human erbt. 
# Die Untergeordnete Klasse kann aber auch eigene Eigenschaften haben.

# Um von der Modularisierung per Datei zu profitieren, können Sie die Klassen 
# in ihren eigenen Dateien platzieren, z. B. human.py

# Um Funktionen aus anderen Dateien zu importieren, verwenden Sie das folgende Format 
# from "Dateiname-ohne-Erweiterung" impotr "Funktion-oder-Klasse"

from human import Human

# Geben Sie die übergeordnete(n) Klasse(n) als Parameter für die Klassendefinition an
class Superhero(Human):

    # Wenn die untergeordnete Klasse alle Definitionen des übergeordneten Elements 
    # ohne Änderungen erben soll, können Sie einfach das Schlüsselwort "pass" 
    # (und nichts anderes) verwenden. In diesem Fall wird jedoch auskommentiert, 
    # um eine eindeutige untergeordnete Klasse zuzulassen:
    # pass

    # Kindklassen können die Attribute ihrer Eltern überschreiben
    species = 'Superhuman'

    # Kinder erben automatisch den Konstruktor ihrer übergeordneten Klasse 
    # einschließlich ihrer Argumente, können aber auch zusätzliche Argumente oder 
    # Definitionen definieren und ihre Methoden zB den Klassenkonstruktor überschreiben.
    # Dieser Konstruktor erbt das Argument "name" von der Klasse "Human" und 
    # fügt die Argumente "superpowers" und "movie" hinzu:
    def __init__(self, name, movie=False,
                     superpowers=["super strength", "bulletproofing"]):

        # zusätzliche Klassenattribute hinzufügen:
        self.fictional = True
        self.movie = movie
        # Beachten Sie die veränderlichen Standardwerte, da die Standardwerte gemeinsam genutzt werden
        self.superpowers = superpowers
        
        # Mit der Funktion "super" können Sie auf die Methoden der übergeordneten Klasse 
        # zugreifen, die vom untergeordneten Objekt überschrieben werden, 
        # in diesem Fall die Methode __init__.
        # Dies ruft den Konstruktor der übergeordneten Klasse auf:
        super().__init__(name)
        
    # überschreiben der "sing" Methode
    def sing(self):
        return 'Dun, dun, DUN!'
        
    # eine zusätzliche Instanzmethode hinzufügen
    def boast(self):
        for power in self.superpowers:
            print("I wield the power of {pow}!".format(pow=power))

if __name__ == '__main__':
    sup = Superhero(name="Tick")
    
    # Instanztypprüfungen
    if isinstance(sup, Human):
        print('I am human')
    if type(sup) is Superhero:
        print('I am a superhero')
        
    # Die Reihenfolge der Methodenauflösung (MRO = Method Resolution Order) anzeigen, die sowohl von getattr() als auch von super() verwendet wird.
    # Dieses Attribut ist dynamisch und kann aktualisiert werden.
    print(Superhero.__mro__)    # => (<class '__main__.Superhero'>,
                                # => <class 'human.Human'>, <class 'object'>)
                                
    # Ruft die übergeordnete Methode auf, verwendet jedoch das eigene Klassenattribut
    print(sup.get_species())    # => Superhuman
    
    # Ruft die überschriebene Methode auf
    print(sup.sing())           # => Dun, dun, DUN!
    
    # Ruft die Methode von Human auf
    sup.say('Spoon')            # => Tick: Spoon
    
    # Aufruf einer Methode, die nur in Superhero existiert
    sup.boast()                 # => I wield the power of super strength!
                                # => I wield the power of bulletproofing!
                                    
    # Vererbtes Klassenattribut
    sup.age = 31
    print(sup.age)              # => 31
    
    # Attribut, das nur in Superhero existiert
    print('Am I Oscar eligible? ' + str(sup.movie))

####################################################
## 6.2 Multiple Inheritance
####################################################

# Eine weitere Klassendefinition
# bat.py

class Bat:

    species = 'Baty'

    def __init__(self, can_fly=True):
        self.fly = can_fly

    # This class also has a say method
    def say(self, msg):
        msg = '... ... ...'
        return msg

    # And its own method as well
    def sonar(self):
        return '))) ... ((('

if __name__ == '__main__':
    b = Bat()
    print(b.say('hello'))
    print(b.fly)
    
# Und noch eine andere Klassendefinition, die von Superhero und Bat erbt
# superhero.py    
from superhero import Superhero
from bat import Bat

# Definieren Sie Batman als eine Kindklasse, das von Superheld und Bat erbt
class Batman(Superhero, Bat):

    def __init__(self, *args, **kwargs):
        # In der Regel müssen Sie super aufrufen, um Attribute zu erben:
        # super (Batman, selbst) .__ init__ (* args, ** kwargs)
        # Allerdings handelt es sich hier um Mehrfachvererbung, und super() 
        # funktioniert nur mit der nächsten Basisklasse in der MRO-Liste.
        # Stattdessen rufen wir explizit __init__ für alle Vorfahren auf.
        # Die Verwendung von *args und **kwargs ermöglicht die saubere Übergabe von 
        # Argumenten, wobei jedes übergeordnete Element eine Schicht der Zwiebel "abschält".
        Superhero.__init__(self, 'anonymous', movie=True, 
                           superpowers=['Wealthy'], *args, **kwargs)
        Bat.__init__(self, *args, can_fly=False, **kwargs)
        # überschreibt den Wert für das Namensattribut
        self.name = 'Sad Affleck'

    def sing(self):
        return 'nan nan nan nan nan batman!'
        
if __name__ == '__main__':
    sup = Batman()
    
    # Die Reihenfolge der Methodenauflösung (MRO = Method Resolution Order) anzeigen, 
    # die sowohl von getattr() als auch von super() verwendet wird.
    # Dieses Attribut ist dynamisch und kann aktualisiert werden.
    print(Batman.__mro__)       # => (<class '__main__.Batman'>, 
                                # => <class 'superhero.Superhero'>, 
                                # => <class 'human.Human'>, 
                                # => <class 'bat.Bat'>, <class 'object'>)
                                
    # Ruft die übergeordnete Methode auf, verwendet jedoch das eigene Klassenattribut                                
    print(sup.get_species())    # => Superhuman                                
    
    # Ruft die überschriebene Methode auf
    print(sup.sing())           # => nan nan nan nan nan batman!
    
    # Ruft die Methode von Human auf, weil die Reihenfolge der Vererbung wichtig ist
    sup.say('I agree')          # => Sad Affleck: I agree
    
    # Aufrufmethode, die nur im 2. Vorfahren existiert
    print(sup.sonar())          # => ))) ... (((
    
    # Vererbtes Klassenattribut
    sup.age = 100
    print(sup.age)              # => 100
    
    # Vererbtes Attribut vom 2. Vorfahren, dessen Standardwert überschrieben wurde.
    print('Can I fly? ' + str(sup.fly)) # => Can I fly? False
    
    
####################################################
## 7. Fortgeschrittenes
####################################################    
    
# Generatoren helfen Ihnen, lazy Code zu erstellen.
def double_numbers(iterable):
    for i in iterable:
        yield i + i
        
# Generatoren sind speichereffizient, da sie nur die Daten laden, 
# die zur Verarbeitung des nächsten Werts in der iterierbaren Komponente 
# erforderlich sind. Dadurch können sie ansonsten unzulässig große Wertebereiche ausführen.
# HINWEIS: `range` ersetzt` xrange` in Python 3.        
for i in double_numbers(range(1, 900000000)):  # `range` ist ein Generator.
    print(i)
    if i >= 30:
        break

# Genauso wie Sie ein 'list comprehension' (Listen Abstraktion) erstellen können, können Sie auch 'generator comprehension' (Generator Abstraktion) erstellen.
values = (-x for x in [1,2,3,4,5])
for x in values:
    print(x)  # prints -1 -2 -3 -4 -5 to console/terminal
    
# Sie können eine Generator Abstraktion auch direkt in eine Liste umwandeln (casten).
values = (-x for x in [1,2,3,4,5])
gen_to_list = list(values)
print(gen_to_list)  # => [-1, -2, -3, -4, -5]

# Decorators
# In diesem Beispiel umschliesst "beg" "say". Wenn say_please True ist, wird die zurückgegebene Nachricht geändert.
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


print(say())                 # Can you buy me a beer?
print(say(say_please=True))  # Can you buy me a beer? Please! I am poor :(

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

