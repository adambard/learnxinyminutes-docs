---
language: Python 2 (legacy)
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
    - ["Amin Bandali", "https://aminb.org"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["evuez", "http://github.com/evuez"]
    - ["asyne", "https://github.com/justblah"]
    - ["habi", "http://github.com/habi"]
translators:
    - ["Tamás Diószegi", "https://github.com/ditam"]
filename: learnpythonlegacy-hu.py
lang: hu-hu
---

A Python nyelvet Guido Van Rossum alkotta meg a 90-es évek elején. Manapság az
egyik legnépszerűbb programozási nyelv. Én a tiszta szintaxisa miatt szerettem
bele. Tulajdonképpen futtatható pszeudokód.

Szívesen fogadok visszajelzéseket! Elérsz itt: [@louiedinh](http://twitter.com/louiedinh)
vagy pedig a louiedinh [kukac] [google email szolgáltatása] címen.

Figyelem: ez a leírás a Python 2.7 verziójára vonatkozok, illetve
általánosságban a 2.x verziókra. A Python 2.7 azonban már csak 2020-ig lesz
támogatva, ezért kezdőknek ajánlott, hogy a Python 3-mal kezdjék az
ismerkedést. A Python 3.x verzióihoz a [Python 3 bemutató](http://learnxinyminutes.com/docs/python/)
ajánlott.

Lehetséges olyan Python kódot írni, ami egyszerre kompatibilis a 2.7 és a 3.x
verziókkal is, a Python [`__future__` imports](https://docs.python.org/2/library/__future__.html) használatával.
A `__future__` import használata esetén Python 3-ban írhatod a kódot, ami
Python 2 alatt is futni fog, így ismét a fenti Python 3 bemutató ajánlott.

```python

# Az egysoros kommentek kettőskereszttel kezdődnek

""" Többsoros stringeket három darab " közé
    fogva lehet írni, ezeket gyakran használják
    több soros kommentként.
"""

####################################################
# 1. Egyszerű adattípusok és operátorok
####################################################

# Használhatsz számokat
3  # => 3

# Az alapműveletek meglepetésektől mentesek
1 + 1  # => 2
8 - 1  # => 7
10 * 2  # => 20
35 / 5  # => 7

# Az osztás kicsit trükkös. Egész osztást végez, és a hányados alsó egész része
# lesz az eredmény
5 / 2  # => 2

# Az osztás kijavításához a (lebegőpontos) float típust kell használnunk
2.0  # Ez egy float
11.0 / 4.0  # => 2.75 áh... máris jobb

# Az egész osztás a negatív számok esetén is az alsó egész részt eredményezi
5 // 3  # => 1
5.0 // 3.0  # => 1.0 # floatok esetén is
-5 // 3  # => -2
-5.0 // 3.0  # => -2.0

# Ha importáljuk a division modult (ld. 6. Modulok rész),
# akkor a '/' jellel pontos osztást tudunk végezni.
from __future__ import division

11 / 4  # => 2.75  ...sima osztás
11 // 4  # => 2 ...egész osztás

# Modulo művelet
7 % 3  # => 1

# Hatványozás (x az y. hatványra)
2 ** 4  # => 16

# A precedencia zárójelekkel befolyásolható
(1 + 3) * 2  # => 8

# Logikai operátorok
# Megjegyzés: az "and" és "or" csak kisbetűkkel helyes
True and False  # => False
False or True  # => True

# A logikai operátorok egészeken is használhatóak
0 and 2  # => 0
-5 or 0  # => -5
0 == False  # => True
2 == True  # => False
1 == True  # => True

# Negálni a not kulcsszóval lehet
not True  # => False
not False  # => True

# Egyenlőségvizsgálat ==
1 == 1  # => True
2 == 1  # => False

# Egyenlőtlenség !=
1 != 1  # => False
2 != 1  # => True

# További összehasonlítások
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# Az összehasonlítások láncolhatóak!
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# Stringeket " vagy ' jelek közt lehet megadni
"Ez egy string."
'Ez egy másik string.'

# A stringek összeadhatóak!
"Hello " + "world!"  # => "Hello world!"
# '+' jel nélkül is összeadhatóak
"Hello " "world!"  # => "Hello world!"

# ... illetve szorozhatóak
"Hello" * 3  # => "HelloHelloHello"

# Kezelhető karakterek indexelhető listájaként
"This is a string"[0]  # => 'T'

# A string hosszát a len függvény adja meg
len("This is a string")  # => 16

# String formázáshoz a % jel használható
# A Python 3.1-gyel a % már deprecated jelölésű, és később eltávolításra fog
# kerülni, de azért jó tudni, hogyan működik.
x = 'alma'
y = 'citrom'
z = "A kosárban levő elemek: %s és %s" % (x, y)

# A string formázás újabb módja a format metódus használatával történik.
# Jelenleg ez a javasolt megoldás.
"{} egy {} szöveg".format("Ez", "helytartó")
"A {0} pedig {1}".format("string", "formázható")
# Ha nem akarsz számolgatni, nevesíthetőek a pozíciók.
"{name} kedvence a {food}".format(name="Bob", food="lasagna")

# None egy objektum
None  # => None

# A None-nal való összehasonlításhoz ne használd a "==" jelet,
# használd az "is" kulcsszót helyette
"etc" is None  # => False
None is None  # => True

# Az 'is' operátor objektum egyezést vizsgál.
# Primitív típusok esetén ez nem túl hasznos,
# objektumok esetén azonban annál inkább.

# Bármilyen objektum használható logikai kontextusban.
# A következő értékek hamis-ra értékelődnek ki (ún. "falsey" értékek):
#    - None
#    - bármelyik szám típus 0 értéke (pl. 0, 0L, 0.0, 0j)
#    - üres sorozatok (pl. '', (), [])
#    - üres konténerek (pl., {}, set())
#    - egyes felhasználó által definiált osztályok példányai bizonyos szabályok szerint,
#      ld: https://docs.python.org/2/reference/datamodel.html#object.__nonzero__
#
# Minden egyéb érték "truthy" (a bool() függvénynek átadva igazra értékelődnek ki)
bool(0)  # => False
bool("")  # => False


####################################################
# 2. Változók és kollekciók
####################################################

# Létezik egy print utasítás
print "I'm Python. Nice to meet you!"  # => I'm Python. Nice to meet you!

# Így lehet egyszerűen bemenetet kérni a konzolról:
input_string_var = raw_input(
    "Enter some data: ")  # Visszatér a megadott stringgel
input_var = input("Enter some data: ")  # Kiértékeli a bemenetet python kódként
# Vigyázat: a fentiek miatt az input() metódust körültekintően kell használni
# Megjegyzés: Python 3-ban az input() már deprecated, és a raw_input() lett input()-ra átnevezve

# A változókat nem szükséges a használat előtt deklarálni
some_var = 5  # Konvenció szerint a névben kisbetu_es_alulvonas
some_var  # => 5

# Érték nélküli változóra hivatkozás hibát dob.
# Lásd a Control Flow szekciót a kivételkezelésről.
some_other_var  # name error hibát dob

# az if használható kifejezésként
# a C nyelv '?:' ternáris operátorával egyenértékűen
"yahoo!" if 3 > 2 else 2  # => "yahoo!"

# A listákban sorozatok tárolhatóak
li = []
# Már inicializáláskor megadhatóak elemek
other_li = [4, 5, 6]

# A lista végére az append metódus rak új elemet
li.append(1)  # li jelenleg [1]
li.append(2)  # li jelenleg [1, 2]
li.append(4)  # li jelenleg [1, 2, 4]
li.append(3)  # li jelenleg [1, 2, 4, 3]
# A végéről a pop metódus távolít el elemet
li.pop()  # => 3 és li jelenleg [1, 2, 4]
# Rakjuk vissza
li.append(3)  # li jelenleg [1, 2, 4, 3], újra.

# A lista elemeket tömb indexeléssel lehet hivatkozni
li[0]  # => 1
# A már inicializált értékekhez a = jellel lehet új értéket rendelni
li[0] = 42
li[0]  # => 42
li[0] = 1  # csak visszaállítjuk az eredeti értékére
# Így is lehet az utolsó elemre hivatkozni
li[-1]  # => 3

# A túlindexelés eredménye IndexError
li[4]  # IndexError hibát dob

# A lista részeit a slice szintaxissal lehet kimetszeni
# (Matekosoknak ez egy zárt/nyitott intervallum.)
li[1:3]  # => [2, 4]
# A lista eleje kihagyható így
li[2:]  # => [4, 3]
# Kihagyható a vége
li[:3]  # => [1, 2, 4]
# Minden második elem kiválasztása
li[::2]  # =>[1, 4]
# A lista egy másolata, fordított sorrendben
li[::-1]  # => [3, 4, 2, 1]
# A fentiek kombinációival bonyolultabb slice parancsok is képezhetőek
# li[start:end:step]

# Listaelemek a "del" paranccsal törölhetőek
del li[2]  # li jelenleg [1, 2, 3]

# A listák összeadhatóak
li + other_li  # => [1, 2, 3, 4, 5, 6]
# Megjegyzés: az eredeti li és other_li értékei változatlanok

# Összefőzhetőek (konkatenálhatóak) az "extend()" paranccsal
li.extend(other_li)  # li jelenleg [1, 2, 3, 4, 5, 6]

# Egy elem első előfordulásának eltávolítása
li.remove(2)  # li jelenleg [1, 3, 4, 5, 6]
li.remove(2)  # ValueError hibát dob, mivel a 2 nem szerepel már a listában

# Elemek beszúrhatóak tetszőleges helyre
li.insert(1, 2)  # li jelenleg [1, 2, 3, 4, 5, 6], ismét

# Egy elem első előfordulási helye
li.index(2)  # => 1
li.index(7)  # ValueError hibát dob, mivel a 7 nem szerepel a listában

# Egy listában egy elem előfordulása az "in" szóval ellenőrizhető
1 in li  # => True

# A lista hossza a "len()" függvénnyel
len(li)  # => 6

# Az N-esek ("tuple") hasonlítanak a listákhoz, de nem módosíthatóak
tup = (1, 2, 3)
tup[0]  # => 1
tup[0] = 3  # TypeError hibát dob

# Az összes lista-műveletet ezeken is használható
len(tup)  # => 3
tup + (4, 5, 6)  # => (1, 2, 3, 4, 5, 6)
tup[:2]  # => (1, 2)
2 in tup  # => True

# Az N-esek (és listák) kicsomagolhatóak külön változókba
a, b, c = (1, 2, 3)  # az a így 1, a b 2 és a c pedig 3
d, e, f = 4, 5, 6  # a zárójel elhagyható
# Ha elhagyod a zárójeleket, alapértelmezés szerint tuple képződik
g = 4, 5, 6  # => (4, 5, 6)
# Nézd, milyen egyszerű két értéket megcserélni
e, d = d, e  # d most már 5 és az e 4

# A Dictionary típusokban hozzárendelések (kulcs-érték párok) tárolhatók
empty_dict = {}
# Ez pedig rögtön értékekkel van inicializálva
filled_dict = {"one": 1, "two": 2, "three": 3}

# Egy dictionary értékei [] jelek közt indexelhetőek
filled_dict["one"]  # => 1

# A "keys()" metódus visszatér a kulcsok listájával
filled_dict.keys()  # => ["three", "two", "one"]
# Megjegyzés: egy dictionary párjainak sorrendje nem garantált
# Lehet, hogy már a fenti példán is más sorrendben kaptad meg az elemeket.

# Az értékek listája a "values()" metódussal kérhető le
filled_dict.values()  # => [3, 2, 1]
# ld. a fenti megjegyzést az elemek sorrendjéről.

# Az összes kulcs-érték pár megkapható N-esek listájaként az "items()" metódussal
filled_dict.items()  # => [("one", 1), ("two", 2), ("three", 3)]

# Az "in" kulcssszóval ellenőrizhető, hogy egy kulcs szerepel-e a dictionary-ben
"one" in filled_dict  # => True
1 in filled_dict  # => False

# Nem létező kulcs hivatkozása KeyError hibát dob
filled_dict["four"]  # KeyError

# A "get()" metódus használatával elkerülhető a KeyError
filled_dict.get("one")  # => 1
filled_dict.get("four")  # => None
# A metódusnak megadható egy alapértelmezett visszatérési érték is, hiányzó értékek esetén
filled_dict.get("one", 4)  # => 1
filled_dict.get("four", 4)  # => 4
# Megjegyzés: ettől még filled_dict.get("four") => None
# (vagyis a get nem állítja be az alapértelmezett értéket a dictionary-ben)

# A kulcsokhoz értékek a listákhoz hasonló szintaxissal rendelhetőek:
filled_dict["four"] = 4  # ez után filled_dict["four"] => 4

# A "setdefault()" metódus csak akkor állít be egy értéket, ha az adott kulcshoz még nem volt más megadva
filled_dict.setdefault("five", 5)  # filled_dict["five"] beállítva 5-re
filled_dict.setdefault("five", 6)  # filled_dict["five"] még mindig 5

# Egy halmaz ("set") olyan, mint egy lista, de egy elemet csak egyszer tárolhat
empty_set = set()
# Inicializáljuk ezt a halmazt néhány elemmel
some_set = set([1, 2, 2, 3, 4])  # some_set jelenleg set([1, 2, 3, 4])

# A sorrend itt sem garantált, még ha néha rendezettnek is tűnhet
another_set = set([4, 3, 2, 2, 1])  # another_set jelenleg set([1, 2, 3, 4])

# Python 2.7 óta már {} jelek közt is lehet halmazt definiálni
filled_set = {1, 2, 2, 3, 4}  # => {1, 2, 3, 4}

# Új halmaz-elemek hozzáadása
filled_set.add(5)  # filled_set is now {1, 2, 3, 4, 5}

# Halmaz metszés a & operátorral
other_set = {3, 4, 5, 6}
filled_set & other_set  # => {3, 4, 5}

# Halmaz unió | operátorral
filled_set | other_set  # => {1, 2, 3, 4, 5, 6}

# Halmaz különbség -
{1, 2, 3, 4} - {2, 3, 5}  # => {1, 4}

# Szimmetrikus differencia ^
{1, 2, 3, 4} ^ {2, 3, 5}  # => {1, 4, 5}

# Vizsgáljuk, hogy a bal oldali halmaz magában foglalja-e a jobb oldalit
{1, 2} >= {1, 2, 3}  # => False

# Vizsgáljuk, hogy a bal oldali halmaz részhalmaza-e a jobb oldalinak
{1, 2} <= {1, 2, 3}  # => True

# Halmazbeli elemek jelenléte az in kulcssszóval vizsgálható
2 in filled_set  # => True
10 in filled_set  # => False


####################################################
#  3. Control Flow
####################################################

# Legyen egy változónk
some_var = 5

# Ez egy if elágazás. A behúzás mértéke (az indentáció) jelentéssel bír a nyelvben!
# Ez a kód ezt fogja kiírni: "some_var kisebb 10-nél"
if some_var > 10:
    print "some_var nagyobb, mint 10."
elif some_var < 10:  # Az elif kifejezés nem kötelező az if szerkezetben.
    print "some_var kisebb 10-nél"
else:  # Ez sem kötelező.
    print "some_var kereken 10."

"""
For ciklusokkal végigiterálhatunk listákon
a kimenet:
    A(z) kutya emlős
    A(z) macska emlős
    A(z) egér emlős
"""
for animal in ["kutya", "macska", "egér"]:
    # A {0} kifejezéssel formázzuk a stringet, ld. korábban.
    print "A(z) {0} emlős".format(animal)

"""
"range(number)" visszatér számok listájával 0-től number-ig
a kimenet:
    0
    1
    2
    3
"""
for i in range(4):
    print i

"""
"range(lower, upper)" visszatér a lower és upper közti számok listájával
a kimenet:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print i

"""
A while ciklus a feltétel hamissá válásáig fut.
a kimenet:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # Rövidítés az x = x + 1 kifejezésre

# A kivételek try/except blokkokkal kezelhetőek

# Python 2.6-tól felfele:
try:
    # A "raise" szóval lehet hibát dobni
    raise IndexError("Ez egy index error")
except IndexError as e:
    pass  # A pass egy üres helytartó művelet. Itt hívnánk a hibakezelő kódunkat.
except (TypeError, NameError):
    pass  # Ha szükséges, egyszerre több hiba típus is kezelhető
else:  # Az except blokk után opcionálisan megadható
    print "Minden rendben!"  # Csak akkor fut le, ha fentebb nem voltak hibák
finally:  # Mindenképpen lefut
    print "Itt felszabadíthatjuk az erőforrásokat például"

# Az erőforrások felszabadításához try/finally helyett a with használható
with open("myfile.txt") as f:
    for line in f:
        print line


####################################################
# 4. Függvények
####################################################

# A "def" szóval hozhatunk létre új függvényt
def add(x, y):
    print "x is {0} and y is {1}".format(x, y)
    return x + y  # A return szóval tudunk értékeket visszaadni


# Így hívunk függvényt paraméterekkel
add(5, 6)  # => a konzol kimenet "x is 5 and y is 6", a visszatérési érték 11

# Nevesített paraméterekkel (ún. "keyword arguments") is hívhatunk egy függvényt
add(y=6, x=5)  # Ez esetben a sorrendjük nem számít


# Változó számú paramétert fogadó függvény így definiálható.
# A * használatával a paramétereket egy N-esként kapjuk meg.
def varargs(*args):
    return args


varargs(1, 2, 3)  # => (1, 2, 3)


# Változó számú nevesített paramétert fogadó függvény is megadható,
# a ** használatával a paramétereket egy dictionary-ként kapjuk meg
def keyword_args(**kwargs):
    return kwargs


# Nézzük meg, mi történik
keyword_args(big="foot", loch="ness")  # => {"big": "foot", "loch": "ness"}


# A két módszer egyszerre is használható
def all_the_args(*args, **kwargs):
    print args
    print kwargs


"""
all_the_args(1, 2, a=3, b=4) kimenete:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Függvények hívásakor a fenti args és kwargs módszerek inverze használható
# A * karakter kifejt egy listát külön paraméterekbe, a ** egy dictionary-t nevesített paraméterekbe.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)  # egyenértékű: foo(1, 2, 3, 4)
all_the_args(**kwargs)  # egyenértékű: foo(a=3, b=4)
all_the_args(*args, **kwargs)  # egyenértékű: foo(1, 2, 3, 4, a=3, b=4)


# A fenti arg és kwarg paraméterek továbbadhatóak egyéb függvényeknek,
# a * illetve ** operátorokkal kifejtve
def pass_all_the_args(*args, **kwargs):
    all_the_args(*args, **kwargs)
    print varargs(*args)
    print keyword_args(**kwargs)


# Függvény scope
x = 5


def set_x(num):
    # A lokális x változó nem ugyanaz, mint a globális x
    x = num  # => 43
    print x  # => 43


def set_global_x(num):
    global x
    print x  # => 5
    x = num  # a globális x-et 6-ra állítjuk
    print x  # => 6


set_x(43)
set_global_x(6)


# A pythonban a függvény elsőrendű (ún. "first class") típus
def create_adder(x):
    def adder(y):
        return x + y

    return adder


add_10 = create_adder(10)
add_10(3)  # => 13

# Névtelen függvények is definiálhatóak
(lambda x: x > 2)(3)  # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1)  # => 5

# Léteznek beépített magasabb rendű függvények
map(add_10, [1, 2, 3])  # => [11, 12, 13]
map(max, [1, 2, 3], [4, 2, 1])  # => [4, 2, 3]

filter(lambda x: x > 5, [3, 4, 5, 6, 7])  # => [6, 7]

# A listaképző kifejezések ("list comprehensions") jól használhatóak a map és filter függvényekkel
[add_10(i) for i in [1, 2, 3]]  # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]  # => [6, 7]

# halmaz és dictionary képzők is léteznek
{x for x in 'abcddeef' if x in 'abc'}  # => {'a', 'b', 'c'}
{x: x ** 2 for x in range(5)}  # => {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}


####################################################
# 5. Osztályok
####################################################

# Az object osztály egy alosztályát képezzük
class Human(object):
    # Osztály szintű mező: az osztály összes példányában azonos
    species = "H. sapiens"

    # Ez a függvény meghívódik az osztály példányosításakor.
    # Megjegyzés: a dupla aláhúzás a név előtt és után egy konvenció a python
    # előre definiált, a nyelv által belsőleg használt, de a felhasználó által
    # is látható objektumok és mezők neveire.
    # Ne vezessünk be új, ilyen elnevezési sémát használó neveket!
    def __init__(self, name):
        # A paramétert értékül adjuk a példány name attribútumának
        self.name = name

        # Inicializálunk egy mezőt
        self.age = 0

    # Példány metódus. Minden metódus első paramétere a "self", a példány maga
    def say(self, msg):
        return "{0}: {1}".format(self.name, msg)

    # Egy osztálymetódus az osztály összes példány közt meg van osztva.
    # Hívásukkor az első paraméter mindig a hívó osztály.
    @classmethod
    def get_species(cls):
        return cls.species

    # Egy statikus metódus osztály és példányreferencia nélkül hívódik
    @staticmethod
    def grunt():
        return "*grunt*"

    # Egy property jelölésű függvény olyan, mint egy getter.
    # Használatával az age mező egy csak-olvasható attribútummá válik.
    @property
    def age(self):
        return self._age

    # Így lehet settert megadni egy mezőhöz
    @age.setter
    def age(self, age):
        self._age = age

    # Így lehet egy mező törlését engedélyezni
    @age.deleter
    def age(self):
        del self._age


# Példányosítsuk az osztályt
i = Human(name="Ian")
print i.say("hi")  # kimenet: "Ian: hi"

j = Human("Joel")
print j.say("hello")  # kimenet: "Joel: hello"

# Hívjuk az osztály metódusunkat
i.get_species()  # => "H. sapiens"

# Változtassuk meg az osztály szintű attribútumot
Human.species = "H. neanderthalensis"
i.get_species()  # => "H. neanderthalensis"
j.get_species()  # => "H. neanderthalensis"

# Hívjuk meg a statikus metódust
Human.grunt()  # => "*grunt*"

# Adjunk új értéket a mezőnek
i.age = 42

# Kérjük le a mező értékét
i.age  # => 42

# Töröljük a mezőt
del i.age
i.age  # => AttributeError hibát dob

####################################################
# 6. Modulok
####################################################

# Modulokat így lehet importálni
import math

print math.sqrt(16)  # => 4.0

# Lehetséges csak bizonyos függvényeket importálni egy modulból
from math import ceil, floor

print ceil(3.7)  # => 4.0
print floor(3.7)  # => 3.0

# Egy modul összes függvénye is importálható
# Vigyázat: ez nem ajánlott.
from math import *

# A modulok nevei lerövidíthetőek
import math as m

math.sqrt(16) == m.sqrt(16)  # => True
# Meggyőződhetünk róla, hogy a függvények valóban azonosak
from math import sqrt

math.sqrt == m.sqrt == sqrt  # => True

# A Python modulok egyszerű fájlok.
# Írhatsz sajátot és importálhatod is.
# A modul neve azonos a tartalmazó fájl nevével.

# Így lehet megtekinteni, milyen mezőket és függvényeket definiál egy modul.
import math

dir(math)


# Ha van egy math.py nevű Python scripted a jelenleg futó scripttel azonos
# mappában, a math.py fájl lesz betöltve a beépített Python modul helyett.
# A lokális mappa prioritást élvez a beépített könyvtárak felett.


####################################################
# 7. Haladóknak
####################################################

# Generátorok
# Egy generátor értékeket "generál" amikor kérik, a helyett, hogy előre eltárolná őket.

# A következő metódus (ez még NEM egy generátor) megduplázza a kapott iterable elemeit,
# és eltárolja őket. Nagy méretű iterable esetén ez nagyon sok helyet foglalhat!
def double_numbers(iterable):
    double_arr = []
    for i in iterable:
        double_arr.append(i + i)
    return double_arr


# A következő kód futtatásakor az összes szám kétszeresét kiszámítanánk, és visszaadnánk
# ezt a nagy listát a ciklus vezérléséhez.
for value in double_numbers(range(1000000)):  # `test_non_generator`
    print value
    if value > 5:
        break


# Használjunk inkább egy generátort, ami "legenerálja" a soron következő elemet,
# amikor azt kérik tőle
def double_numbers_generator(iterable):
    for i in iterable:
        yield i + i


# A lenti kód mindig csak a soron következő számot generálja a logikai vizsgálat előtt.
# Így amikor az érték eléri a > 5 határt, megszakítjuk a ciklust, és a lista számainak
# nagy részénél megspóroltuk a duplázás műveletet (ez sokkal gyorsabb így!).
for value in double_numbers_generator(xrange(1000000)):  # `test_generator`
    print value
    if value > 5:
        break

# Feltűnt, hogy a `test_non_generator` esetén `range`, a `test_generator` esetén
# pedig `xrange` volt a segédfüggvény neve? Ahogy `double_numbers_generator` a
# generátor változata a `double_numbers` függvénynek, úgy az `xrange` a `range`
# generátor megfelelője, csak akkor generálja le a következő számot, amikor kérjük
# - esetünkben a ciklus következő iterációjakor

# A lista képzéshez hasonlóan generátor képzőket is használhatunk
# ("generator comprehensions").
values = (-x for x in [1, 2, 3, 4, 5])
for x in values:
    print(x)  # kimenet: -1 -2 -3 -4 -5

# Egy generátor összes generált elemét listaként is elkérhetjük:
values = (-x for x in [1, 2, 3, 4, 5])
gen_to_list = list(values)
print(gen_to_list)  # => [-1, -2, -3, -4, -5]

# Dekorátorok
# A dekorátor egy magasabb rendű függvény, aminek bemenete és kimenete is egy függvény.
# A lenti egyszerű példában az add_apples dekorátor a dekorált get_fruits függvény
# kimenetébe beszúrja az 'Apple' elemet.
def add_apples(func):
    def get_fruits():
        fruits = func()
        fruits.append('Apple')
        return fruits
    return get_fruits

@add_apples
def get_fruits():
    return ['Banana', 'Mango', 'Orange']

# A kimenet tartalmazza az 'Apple' elemet:
# Banana, Mango, Orange, Apple
print ', '.join(get_fruits())

# Ebben a példában a beg dekorátorral látjuk el a say függvényt.
# Beg meghívja say-t. Ha a say_please paraméter igaz, akkor 
# megváltoztatja az eredmény mondatot.
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


print say()  # Can you buy me a beer?
print say(say_please=True)  # Can you buy me a beer? Please! I am poor :(
```

## Még több érdekel?

### Ingyenes online tartalmak

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com)
* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [The Official Docs](http://docs.python.org/2/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Module of the Week](http://pymotw.com/2/)
* [A Crash Course in Python for Scientists](http://nbviewer.ipython.org/5920182)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)
* [LearnPython](http://www.learnpython.org/)
* [Fullstack Python](https://www.fullstackpython.com/)

### Könyvek

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)
