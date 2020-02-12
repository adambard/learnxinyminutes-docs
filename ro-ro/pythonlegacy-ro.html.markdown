---
language: Python 2 (legacy)
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
translators:
    - ["Ovidiu Ciule", "https://github.com/ociule"]
filename: learnpythonlegacy-ro.py
lang: ro-ro
---

Python a fost creat de Guido Van Rossum la începutul anilor '90. Python a
devenit astăzi unul din cele mai populare limbaje de programare.
M-am indrăgostit de Python pentru claritatea sa sintactică. Python este aproape
pseudocod executabil.

Opinia dumneavoastră este binevenită! Puteţi sa imi scrieţi la [@ociule](http://twitter.com/ociule)
sau ociule [at] [google's email service]

Notă: Acest articol descrie Python 2.7, dar este util şi pentru Python 2.x.
O versiune Python 3 va apărea în curând, în limba engleză mai întâi.

```python
# Comentariile pe o singură linie încep cu un caracter diez.
""" Şirurile de caractere pe mai multe linii pot fi încadrate folosind trei caractere ", şi sunt des
    folosite ca şi comentarii pe mai multe linii.
"""

####################################################
## 1. Operatori şi tipuri de date primare 
####################################################

# Avem numere
3 #=> 3

# Matematica se comportă cum ne-am aştepta
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# Împărţirea este un pic surprinzătoare. Este de fapt împărţire pe numere
# întregi şi rotunjeşte
# automat spre valoarea mai mică
5 / 2 #=> 2

# Pentru a folosi împărţirea fără rest avem nevoie de numere reale 
2.0     # Acesta e un număr real
11.0 / 4.0 #=> 2.75 ahhh ... cum ne aşteptam 

# Ordinea operaţiilor se poate forţa cu paranteze
(1 + 3) * 2 #=> 8

# Valoriile boolene sunt şi ele valori primare
True
False

# Pot fi negate cu operatorul not
not True #=> False
not False #=> True

# Egalitatea este ==
1 == 1 #=> True
2 == 1 #=> False

# Inegalitate este !=
1 != 1 #=> False
2 != 1 #=> True

# Comparaţii
1 < 10 #=> True
1 > 10 #=> False
2 <= 2 #=> True
2 >= 2 #=> True

# Comparaţiile pot fi inlănţuite! 
1 < 2 < 3 #=> True
2 < 3 < 2 #=> False

# Şirurile de caractere pot fi încadrate cu " sau '
"Acesta e un şir de caractere."
'Şi acesta este un şir de caractere.'

# Şirurile de caractere pot fi adăugate!
"Hello " + "world!" #=> "Hello world!"

# Un şir de caractere poate fi folosit ca o listă 
"Acesta e un şir de caractere"[0] #=> 'A'

# Caracterul % (procent) poate fi folosit pentru a formata şiruri de caractere :
"%s pot fi %s" % ("şirurile", "interpolate")

# O metodă mai nouă de a formata şiruri este metoda "format"
# Este metoda recomandată
"{0} pot fi {1}".format("şirurile", "formatate")
# Puteţi folosi cuvinte cheie dacă nu doriţi sa număraţi 
"{nume} vrea să mănânce {fel}".format(nume="Bob", fel="lasagna")

# "None", care reprezintă valoarea nedefinită, e un obiect
None #=> None

# Nu folosiţi operatorul == pentru a compara un obiect cu None
# Folosiţi operatorul "is"
"etc" is None #=> False
None is None  #=> True

# Operatorul "is" testeaza dacă obiectele sunt identice.
# Acastă operaţie nu e foarte folositoare cu tipuri primare,
# dar e foarte folositoare cu obiecte.

# None, 0, şi şiruri de caractere goale sunt evaluate ca si fals, False. 
# Toate celelalte valori sunt adevărate, True.
0 == False  #=> True
"" == False #=> True


####################################################
## 2. Variabile şi colecţii
####################################################

# Printarea este uşoară 
print "Eu sunt Python. Încântat de cunoştinţă!" 


# Nu este nevoie sa declari variabilele înainte de a le folosi 
o_variabila = 5    # Convenţia este de a folosi caractere_minuscule_cu_underscore 
o_variabila #=> 5

# Dacă accesăm o variabilă nefolosită declanşăm o excepţie.
# Vezi secţiunea Control de Execuţie pentru mai multe detalii despre excepţii.
alta_variabila # Declanşează o eroare de nume 

# "If" poate fi folosit într-o expresie.
"yahoo!" if 3 > 2 else 2 #=> "yahoo!"

# Listele sunt folosite pentru colecţii
li = []
# O listă poate avea valori de la început 
alta_li = [4, 5, 6]

# Se adaugă valori la sfârşitul lister cu append 
li.append(1)    #li e acum [1]
li.append(2)    #li e acum [1, 2]
li.append(4)    #li e acum [1, 2, 4]
li.append(3)    #li este acum [1, 2, 4, 3]
# Se şterg de la sfarşit cu pop 
li.pop()        #=> 3 şi li e acum [1, 2, 4]
# Să o adaugăm înapoi valoarea
li.append(3)    # li e din nou [1, 2, 4, 3]

# Putem accesa valorile individuale dintr-o listă cu operatorul index 
li[0] #=> 1
# Valoarea speciala -1 pentru index accesează ultima valoare
li[-1] #=> 3

# Dacă depaşim limitele listei declanşăm o eroare IndexError
li[4] # Declanşează IndexError

# Putem să ne uităm la intervale folosind sintaxa de "felii" 
# În Python, intervalele sunt închise la început si deschise la sfârşit. 
li[1:3] #=> [2, 4]
# Fără început
li[2:] #=> [4, 3]
# Fără sfarşit
li[:3] #=> [1, 2, 4]

# Putem şterge elemente arbitrare din lista cu operatorul "del" care primeşte indexul lor
del li[2] # li e acum [1, 2, 3]

# Listele pot fi adăugate
li + alta_li #=> [1, 2, 3, 4, 5, 6] - Notă: li si alta_li nu sunt modificate! 

# Concatenăm liste cu "extend()"
li.extend(alta_li) # Acum li este [1, 2, 3, 4, 5, 6]

# Se verifică existenţa valorilor in lista cu "in" 
1 in li #=> True

# Şi lungimea cu "len()"
len(li) #=> 6


# Tuplele sunt ca şi listele dar imutabile 
tup = (1, 2, 3)
tup[0] #=> 1
tup[0] = 3  # Declanşează TypeError

# Pot fi folosite ca şi liste 
len(tup) #=> 3
tup + (4, 5, 6) #=> (1, 2, 3, 4, 5, 6)
tup[:2] #=> (1, 2)
2 in tup #=> True

# Tuplele pot fi despachetate 
a, b, c = (1, 2, 3)     # a este acum 1, b este acum 2 şi c este acum 3
# Tuplele pot fi folosite şi fără paranteze 
d, e, f = 4, 5, 6
# Putem inversa valori foarte uşor! 
e, d = d, e     # d este acum 5 şi e este acum 4


# Dicţionarele stochează chei şi o valoare pentru fiecare cheie
dict_gol = {}
# Şi un dicţionar cu valori 
dict_cu_valori = {"unu": 1, "doi": 2, "trei": 3}

# Căutaţi valori cu []
dict_cu_valori["unu"] #=> 1

# Obţinem lista cheilor cu "keys()"
dict_cu_valori.keys() #=> ["trei", "doi", "unu"]
# Notă - ordinea cheilor obţinute cu keys() nu este garantată. 
# Puteţi obţine rezultate diferite de exemplul de aici. 

# Obţinem valorile cu values() 
dict_cu_valori.values() #=> [3, 2, 1]
# Notă - aceeaşi ca mai sus, aplicată asupra valorilor.

# Verificăm existenţa unei valori cu "in" 
"unu" in dict_cu_valori #=> True
1 in dict_cu_valori #=> False

# Accesarea unei chei care nu exista declanşează o KeyError
dict_cu_valori["four"] # KeyError

# Putem folosi metoda "get()" pentru a evita KeyError
dict_cu_valori.get("one") #=> 1
dict_cu_valori.get("four") #=> None
# Metoda get poate primi ca al doilea argument o valoare care va fi returnată
# când cheia nu este prezentă. 
dict_cu_valori.get("one", 4) #=> 1
dict_cu_valori.get("four", 4) #=> 4

# "setdefault()" este o metodă pentru a adăuga chei-valori fără a le modifica, dacă cheia există deja
dict_cu_valori.setdefault("five", 5) #dict_cu_valori["five"] este acum 5
dict_cu_valori.setdefault("five", 6) #dict_cu_valori["five"] exista deja, nu este modificată, tot 5


# Set este colecţia mulţime
set_gol = set()
# Putem crea un set cu valori
un_set = set([1,2,2,3,4]) # un_set este acum set([1, 2, 3, 4]), amintiţi-vă ca mulţimile garantează unicatul!

# În Python 2.7, {} poate fi folosit pentru un set 
set_cu_valori = {1, 2, 2, 3, 4} # => {1 2 3 4}

# Putem adăuga valori cu add 
set_cu_valori.add(5) # set_cu_valori este acum {1, 2, 3, 4, 5}

# Putem intersecta seturi 
alt_set = {3, 4, 5, 6}
set_cu_valori & alt_set #=> {3, 4, 5}

# Putem calcula uniunea cu |
set_cu_valori | alt_set #=> {1, 2, 3, 4, 5, 6}

# Diferenţa între seturi se face cu -
{1,2,3,4} - {2,3,5} #=> {1, 4}

# Verificăm existenţa cu "in" 
2 in set_cu_valori #=> True
10 in set_cu_valori #=> False


####################################################
## 3. Controlul Execuţiei
####################################################

# O variabilă 
o_variabila = 5

# Acesta este un "if". Indentarea este importanta în python!
# Printează "o_variabila este mai mică ca 10"
if o_variabila > 10:
    print "o_variabila e mai mare ca 10."
elif o_variabila < 10:    # Clauza elif e opţională. 
    print "o_variabila este mai mică ca 10."
else:           # Şi else e opţional.
    print "o_variabila este exact 10."


"""
Buclele "for" pot fi folosite pentru a parcurge liste
Vom afişa:
    câinele este un mamifer 
    pisica este un mamifer
    şoarecele este un mamifer
"""
for animal in ["câinele", "pisica", "şoarecele"]:
    # Folosim % pentru a compune mesajul 
    print "%s este un mamifer" % animal
    
"""
"range(număr)" crează o lista de numere 
de la zero la numărul dat
afişează:
    0
    1
    2
    3
"""
for i in range(4):
    print i

"""
While repetă pana când condiţia dată nu mai este adevărată. 
afişează:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # Prescurtare pentru x = x + 1

# Recepţionăm excepţii cu blocuri try/except 

# Acest cod e valid in Python > 2.6:
try:
    # Folosim "raise" pentru a declanşa o eroare 
    raise IndexError("Asta este o IndexError")
except IndexError as e:
    pass    # Pass nu face nimic. În mod normal aici ne-am ocupa de eroare. 


####################################################
## 4. Funcţii
####################################################

# Folosim "def" pentru a defini funcţii 
def add(x, y):
    print "x este %s şi y este %s" % (x, y)
    return x + y    # Funcţia poate returna valori cu "return" 

# Apelăm funcţia "add" cu parametrii 
add(5, 6) #=> Va afişa "x este 5 şi y este 6" şi va returna 11 

# Altă cale de a apela funcţii: cu parametrii numiţi 
add(y=6, x=5)   # Ordinea parametrilor numiţi nu contează 

# Putem defini funcţii care primesc un număr variabil de parametrii nenumiţi
# Aceşti parametrii nenumiţi se cheamă si poziţinali
def varargs(*args):
    return args

varargs(1, 2, 3) #=> (1,2,3)


# Şi putem defini funcţii care primesc un număr variabil de parametrii numiţi
def keyword_args(**kwargs):
    return kwargs

# Hai să vedem cum merge 
keyword_args(big="foot", loch="ness") #=> {"big": "foot", "loch": "ness"}

# Se pot combina 
def all_the_args(*args, **kwargs):
    print args
    print kwargs
"""
all_the_args(1, 2, a=3, b=4) va afişa:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Când apelăm funcţii, putem face inversul args/kwargs!
# Folosim * pentru a expanda tuple şi ** pentru a expanda kwargs.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args) # echivalent cu foo(1, 2, 3, 4)
all_the_args(**kwargs) # echivalent cu foo(a=3, b=4)
all_the_args(*args, **kwargs) # echivalent cu foo(1, 2, 3, 4, a=3, b=4)

# În Python, funcţiile sunt obiecte primare
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3) #=> 13

# Funcţiile pot fi anonime 
(lambda x: x > 2)(3) #=> True

# Există funcţii de ordin superior (care operează pe alte funcţii) predefinite 
map(add_10, [1,2,3]) #=> [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7]) #=> [6, 7]

# Putem folosi scurtături de liste pentru a simplifica munca cu map si filter 
[add_10(i) for i in [1, 2, 3]]  #=> [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5] #=> [6, 7]

####################################################
## 5. Clase
####################################################

# Moştenim object pentru a crea o nouă clasă 
class Om(object):

    # Acesta este un atribut al clasei. Va fi moştenit de toate instanţele. 
    species = "H. sapiens"

    # Constructor (mai degrabă, configurator de bază) 
    def __init__(self, nume):
        # Valoarea parametrului este stocată in atributul instanţei 
        self.nume = nume

    # Aceasta este o metoda a instanţei.
    # Toate metodele primesc "self" ca si primul argument.
    def spune(self, mesaj):
       return "%s: %s" % (self.nume, mesaj)

    # O metodă a clasei. Este partajată de toate instanţele.
    # Va primi ca si primul argument clasa căreia îi aparţine. 
    @classmethod
    def get_species(cls):
        return cls.species

    # O metoda statica nu primeste un argument automat. 
    @staticmethod
    def exclama():
        return "*Aaaaaah*"


# Instanţiem o clasă 
i = Om(nume="Ion")
print i.spune("salut")  # afişează: "Ion: salut"

j = Om("George")
print j.spune("ciau")   # afişează George: ciau"

# Apelăm metoda clasei 
i.get_species() #=> "H. sapiens"

# Modificăm atributul partajat 
Om.species = "H. neanderthalensis"
i.get_species() #=> "H. neanderthalensis"
j.get_species() #=> "H. neanderthalensis"

# Apelăm metoda statică 
Om.exclama() #=> "*Aaaaaah*"


####################################################
## 6. Module
####################################################

# Pentru a folosi un modul, trebuie importat 
import math
print math.sqrt(16) #=> 4.0

# Putem importa doar anumite funcţii dintr-un modul 
from math import ceil, floor
print ceil(3.7)  #=> 4.0
print floor(3.7) #=> 3.0

# Putem importa toate funcţiile dintr-un modul, dar nu este o idee bună
# Nu faceţi asta!
from math import *

# Numele modulelor pot fi modificate la import, de exemplu pentru a le scurta 
import math as m
math.sqrt(16) == m.sqrt(16) #=> True

# Modulele python sunt pur şi simplu fişiere cu cod python. 
# Puteţi sa creaţi modulele voastre, şi sa le importaţi.
# Numele modulului este acelasi cu numele fişierului.

# Cu "dir" inspectăm ce funcţii conţine un modul
import math
dir(math)


```

## Doriţi mai mult?

### Gratis online, în limba engleză

* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [The Official Docs](http://docs.python.org/2.6/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Module of the Week](http://pymotw.com/2/)

### Cărţi, în limba engleză

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)

