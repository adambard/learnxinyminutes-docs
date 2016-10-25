---
language: python
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
    - ["Amin Bandali", "http://aminbandali.com"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["evuez", "http://github.com/evuez"]
filename: learnpython.py
translators:
    - ["Ale46", "http://github.com/Ale46/"]
    - ["Tommaso Pifferi", "http://github.com/neslinesli93/"]
lang: it-it
---
Python è stato creato da Guido Van Rossum agli inizi degli anni 90. Oggi è uno dei più popolari
linguaggi esistenti. Mi sono innamorato di Python per la sua chiarezza sintattica. E' sostanzialmente
pseudocodice eseguibile.

Feedback sono altamente apprezzati! Potete contattarmi su [@louiedinh](http://twitter.com/louiedinh) oppure [at] [google's email service]

Nota: questo articolo è riferito a Python 2.7 in modo specifico, ma dovrebbe andar
bene anche per Python 2.x. Python 2.7 sta raggiungendo il "fine vita", ovvero non sarà
più supportato nel 2020. Quindi è consigliato imparare Python utilizzando Python 3.
Per maggiori informazioni su Python 3.x, dai un'occhiata al [tutorial di Python 3](http://learnxinyminutes.com/docs/python3/).

E' possibile anche scrivere codice compatibile sia con Python 2.7 che con Python 3.x,
utilizzando [il modulo `__future__`](https://docs.python.org/2/library/__future__.html) di Python.
Il modulo `__future__` permette di scrivere codice in Python 3, che può essere eseguito
utilizzando Python 2: cosa aspetti a vedere il tutorial di Python 3?

```python

# I commenti su una sola linea iniziano con un cancelletto

""" Più stringhe possono essere scritte
    usando tre ", e sono spesso usate
    come commenti
"""

####################################################
## 1. Tipi di dati primitivi ed Operatori
####################################################

# Hai i numeri
3  # => 3

# La matematica è quello che vi aspettereste
1 + 1  # => 2
8 - 1  # => 7
10 * 2  # => 20
35 / 5  # => 7

# La divisione è un po' complicata. E' una divisione fra interi in cui viene 
# restituito in automatico il risultato intero.
5 / 2  # => 2

# Per le divisioni con la virgola abbiamo bisogno di parlare delle variabili floats.
2.0     # Questo è un float
11.0 / 4.0  # => 2.75 ahhh...molto meglio

# Il risultato di una divisione fra interi troncati positivi e negativi
5 // 3     # => 1
5.0 // 3.0 # => 1.0 # funziona anche per i floats
-5 // 3  # => -2
-5.0 // 3.0 # => -2.0

# E' possibile importare il modulo "division" (vedi la sezione 6 di questa guida, Moduli)
# per effettuare la divisione normale usando solo '/'.
from __future__ import division
11/4    # => 2.75  ...divisione normale
11//4   # => 2 ...divisione troncata

# Operazione Modulo
7 % 3 # => 1

# Elevamento a potenza (x alla y-esima potenza)
2**4 # => 16

# Forzare le precedenze con le parentesi
(1 + 3) * 2  # => 8

# Operatori Booleani
# Nota "and" e "or" sono case-sensitive
True and False #=> False
False or True #=> True

# Note sull'uso di operatori Bool con interi
0 and 2 #=> 0
-5 or 0 #=> -5
0 == False #=> True
2 == True #=> False
1 == True #=> True

# nega con not
not True  # => False
not False  # => True

# Uguaglianza è ==
1 == 1  # => True
2 == 1  # => False

# Disuguaglianza è !=
1 != 1  # => False
2 != 1  # => True

# Altri confronti
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# I confronti possono essere concatenati!
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# Le stringhe sono create con " o '
"Questa è una stringa."
'Anche questa è una stringa.'

# Anche le stringhe possono essere sommate!
"Ciao " + "mondo!"  # => Ciao mondo!"
# Le stringhe possono essere sommate anche senza '+'
"Ciao " "mondo!"  # => Ciao mondo!"

# ... oppure moltiplicate
"Hello" * 3  # => "HelloHelloHello"

# Una stringa può essere considerata come una lista di caratteri
"Questa è una stringa"[0]  # => 'Q'

# Per sapere la lunghezza di una stringa
len("Questa è una stringa")  # => 20

# Formattazione delle stringhe con %
# Anche se l'operatore % per le stringe sarà deprecato con Python 3.1, e verrà rimosso
# successivamente, può comunque essere utile sapere come funziona
x = 'mela'
y = 'limone'
z = "La cesta contiene una %s e un %s" % (x,y)

# Un nuovo modo per fomattare le stringhe è il metodo format.
# Questo metodo è quello consigliato
"{} è un {}".format("Questo", "test")
"{0} possono essere {1}".format("le stringhe", "formattate")
# Puoi usare delle parole chiave se non vuoi contare
"{nome} vuole mangiare {cibo}".format(nome="Bob", cibo="lasagna")

# None è un oggetto
None  # => None

# Non usare il simbolo di uguaglianza "==" per comparare oggetti a None
# Usa "is" invece
"etc" is None  # => False
None is None  # => True

# L'operatore 'is' testa l'identità di un oggetto. Questo non è
# molto utile quando non hai a che fare con valori primitivi, ma lo è
# quando hai a che fare con oggetti.
 
# Qualunque oggetto può essere usato nei test booleani
# I seguenti valori sono considerati falsi:
#     - None
#     - Lo zero, come qualunque tipo numerico (quindi 0, 0L, 0.0, 0.j)
#     - Sequenze vuote (come '', (), [])
#     - Contenitori vuoti (tipo {}, set())
#     - Istanze di classi definite dall'utente, che soddisfano certi criteri
#       vedi: https://docs.python.org/2/reference/datamodel.html#object.__nonzero__
#       
# Tutti gli altri valori sono considerati veri: la funzione bool() usata su di loro, ritorna True.
bool(0)  # => False
bool("")  # => False


####################################################
## 2. Variabili e Collections
####################################################

# Python ha una funzione di stampa
print "Sono Python. Piacere di conoscerti!" # => Sono Python. Piacere di conoscerti!

# Un modo semplice per ricevere dati in input dalla riga di comando
variabile_stringa_input = raw_input("Inserisci del testo: ") # Ritorna i dati letti come stringa
variabile_input = input("Inserisci del testo: ") # Interpreta i dati letti come codice python
# Attenzione: bisogna stare attenti quando si usa input()
# Nota: In python 3, input() è deprecato, e raw_input() si chiama input()

# Non c'è bisogno di dichiarare una variabile per assegnarle un valore
una_variabile = 5    # Convenzionalmente si usa caratteri_minuscoli_con_underscores
una_variabile  # => 5

# Accedendo ad una variabile non precedentemente assegnata genera un'eccezione.
# Dai un'occhiata al Control Flow per imparare di più su come gestire le eccezioni.
un_altra_variabile  # Genera un errore di nome

# if può essere usato come un'espressione
# E' l'equivalente dell'operatore ternario in C
"yahoo!" if 3 > 2 else 2  # => "yahoo!"

# Liste immagazzinano sequenze
li = []
# Puoi partire con una lista pre-riempita
altra_li = [4, 5, 6]

# Aggiungi cose alla fine di una lista con append
li.append(1)    # li ora è [1]
li.append(2)    # li ora è [1, 2]
li.append(4)    # li ora è [1, 2, 4]
li.append(3)    # li ora è [1, 2, 4, 3]
# Rimuovi dalla fine della lista con pop
li.pop()        # => 3 e li ora è [1, 2, 4]
# Rimettiamolo a posto
li.append(3)    # li ora è [1, 2, 4, 3] di nuovo.

# Accedi ad una lista come faresti con un array
li[0]  # => 1
# Assegna nuovo valore agli indici che sono già stati inizializzati con =
li[0] = 42
li[0]  # => 42
li[0] = 1  # Nota: è resettato al valore iniziale
# Guarda l'ultimo elemento
li[-1]  # => 3

# Guardare al di fuori dei limiti è un IndexError
li[4]  # Genera IndexError

# Puoi guardare gli intervalli con la sintassi slice (a fetta).
# (E' un intervallo chiuso/aperto per voi tipi matematici.)
li[1:3]  # => [2, 4]
# Ometti l'inizio
li[2:]  # => [4, 3]
# Ometti la fine
li[:3]  # => [1, 2, 4]
# Seleziona ogni seconda voce
li[::2]   # =>[1, 4]
# Copia al contrario della lista
li[::-1]   # => [3, 4, 2, 1]
# Usa combinazioni per fare slices avanzate
# li[inizio:fine:passo]

# Rimuovi arbitrariamente elementi da una lista con "del"
del li[2]   # li è ora [1, 2, 3]
# Puoi sommare le liste
li + altra_li   # => [1, 2, 3, 4, 5, 6]
# Nota: i valori per li ed altra_li non sono modificati.

# Concatena liste con "extend()"
li.extend(altra_li)   # Ora li è [1, 2, 3, 4, 5, 6]

# Rimuove la prima occorrenza di un elemento
li.remove(2)  # Ora li è [1, 3, 4, 5, 6]
li.remove(2)  # Emette un ValueError, poichè 2 non è contenuto nella lista

# Inserisce un elemento all'indice specificato
li.insert(1, 2)  # li è di nuovo [1, 2, 3, 4, 5, 6]

# Ritorna l'indice della prima occorrenza dell'elemento fornito
li.index(2)  # => 1
li.index(7)  # Emette un ValueError, poichè 7 non è contenuto nella lista

# Controlla l'esistenza di un valore in una lista con "in"
1 in li   # => True

# Esamina la lunghezza con "len()"
len(li)   # => 6


# Tuple sono come le liste ma immutabili.
tup = (1, 2, 3)
tup[0]   # => 1
tup[0] = 3  # Genera un TypeError

# Puoi fare tutte queste cose da lista anche sulle tuple
len(tup)   # => 3
tup + (4, 5, 6)   # => (1, 2, 3, 4, 5, 6)
tup[:2]   # => (1, 2)
2 in tup   # => True

# Puoi scompattare le tuple (o liste) in variabili
a, b, c = (1, 2, 3)     # a è ora 1, b è ora 2 and c è ora 3
d, e, f = 4, 5, 6       # puoi anche omettere le parentesi
# Le tuple sono create di default se non usi le parentesi
g = 4, 5, 6             # => (4, 5, 6)
# Guarda come è facile scambiare due valori
e, d = d, e     # d è ora 5 ed e è ora 4


# Dizionari immagazzinano mappature
empty_dict = {}
# Questo è un dizionario pre-riempito
filled_dict = {"uno": 1, "due": 2, "tre": 3}

# Accedi ai valori con []
filled_dict["uno"]   # => 1

# Ottieni tutte le chiavi come una lista con "keys()"
filled_dict.keys()   # => ["tre", "due", "uno"]
# Nota - Nei dizionari l'ordine delle chiavi non è garantito.
# Il tuo risultato potrebbe non essere uguale a questo.

# Ottieni tutt i valori come una lista con "values()"
filled_dict.values()   # => [3, 2, 1]
# Nota - Come sopra riguardo l'ordinamento delle chiavi.

# Ottieni tutte le coppie chiave-valore, sotto forma di lista di tuple, utilizzando "items()"
filled_dicts.items()    # => [("uno", 1), ("due", 2), ("tre", 3)]

# Controlla l'esistenza delle chiavi in un dizionario con "in"
"uno" in filled_dict   # => True
1 in filled_dict   # => False

# Cercando una chiave non esistente è un KeyError
filled_dict["quattro"]   # KeyError

# Usa il metodo "get()" per evitare KeyError
filled_dict.get("uno")   # => 1
filled_dict.get("quattro")   # => None
# Il metodo get supporta un argomento di default quando il valore è mancante
filled_dict.get("uno", 4)   # => 1
filled_dict.get("quattro", 4)   # => 4
# nota che filled_dict.get("quattro") è ancora => None
# (get non imposta il valore nel dizionario)

# imposta il valore di una chiave con una sintassi simile alle liste
filled_dict["quattro"] = 4  # ora, filled_dict["quattro"] => 4

# "setdefault()" aggiunge al dizionario solo se la chiave data non è presente
filled_dict.setdefault("five", 5)  # filled_dict["five"] è impostato a 5
filled_dict.setdefault("five", 6)  # filled_dict["five"] è ancora 5


# Sets immagazzina ... sets (che sono come le liste, ma non possono contenere doppioni)
empty_set = set()
# Inizializza un "set()" con un po' di valori
some_set = set([1, 2, 2, 3, 4])   # some_set è ora set([1, 2, 3, 4])

# l'ordine non è garantito, anche se a volta può sembrare ordinato
another_set = set([4, 3, 2, 2, 1])  # another_set è ora set([1, 2, 3, 4])

# Da Python 2.7, {} può essere usato per dichiarare un set
filled_set = {1, 2, 2, 3, 4}   # => {1, 2, 3, 4}

# Aggiungere elementi ad un set
filled_set.add(5)   # filled_set è ora {1, 2, 3, 4, 5}

# Fai intersezioni su un set con &
other_set = {3, 4, 5, 6}
filled_set & other_set   # => {3, 4, 5}

# Fai unioni su set con |
filled_set | other_set   # => {1, 2, 3, 4, 5, 6}

# Fai differenze su set con -
{1, 2, 3, 4} - {2, 3, 5}   # => {1, 4}

# Effettua la differenza simmetrica con ^
{1, 2, 3, 4} ^ {2, 3, 5}  # => {1, 4, 5}

# Controlla se il set a sinistra contiene quello a destra
{1, 2} >= {1, 2, 3} # => False

# Controlla se il set a sinistra è un sottoinsieme di quello a destra
{1, 2} <= {1, 2, 3} # => True

# Controlla l'esistenza in un set con in
2 in filled_set   # => True
10 in filled_set   # => False


####################################################
## 3. Control Flow
####################################################

# Dichiariamo una variabile
some_var = 5

# Questo è un controllo if. L'indentazione è molto importante in python!
# stampa "some_var è più piccola di 10"
if some_var > 10:
    print "some_var è decisamente più grande di 10."
elif some_var < 10:    # Questa clausola elif è opzionale.
    print "some_var è più piccola di 10."
else:           # Anche questo è opzionale.
    print "some_var è precisamente 10."


"""
I cicli for iterano sulle liste
stampa:
    cane è un mammifero
    gatto è un mammifero
    topo è un mammifero
"""
for animale in ["cane", "gatto", "topo"]:
    # Puoi usare {0} per interpolare le stringhe formattate. (Vedi di seguito.)
    print "{0} è un mammifero".format(animale)

"""
"range(numero)" restituisce una lista di numeri
da zero al numero dato
stampa:
    0
    1
    2
    3
"""
for i in range(4):
    print i

"""
"range(lower, upper)" restituisce una lista di numeri
dal più piccolo (lower) al più grande (upper)
stampa:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print i

"""
I cicli while vengono eseguiti finchè una condizione viene a mancare
stampa:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # Forma compatta per x = x + 1

# Gestisci le eccezioni con un blocco try/except

# Funziona da Python 2.6 in su:
try:
    # Usa "raise" per generare un errore
    raise IndexError("Questo è un errore di indice")
except IndexError as e:
    pass    # Pass è solo una non-operazione. Solitamente vorrai fare un recupero.
except (TypeError, NameError):
    pass    # Eccezioni multiple possono essere gestite tutte insieme, se necessario.
else:   # Clausola opzionale al blocco try/except. Deve seguire tutti i blocchi except
    print "Tutto ok!"   # Viene eseguita solo se il codice dentro try non genera eccezioni
finally: #  Eseguito sempre
    print "Possiamo liberare risorse qui"

# Invece di try/finally per liberare risorse puoi usare il metodo with
with open("myfile.txt") as f:
    for line in f:
        print line

####################################################
## 4. Funzioni
####################################################

# Usa "def" per creare nuove funzioni
def aggiungi(x, y):
    print "x è {0} e y è {1}".format(x, y)
    return x + y    # Restituisce valori con il metodo return

# Chiamare funzioni con parametri
aggiungi(5, 6)   # => stampa "x è 5 e y è 6" e restituisce 11

# Un altro modo per chiamare funzioni  è con parole chiave come argomenti
aggiungi(y=6, x=5)   # Le parole chiave come argomenti possono arrivare in ogni ordine.


# Puoi definire funzioni che accettano un numero variabile di argomenti posizionali
# che verranno interpretati come tuple usando il *
def varargs(*args):
    return args

varargs(1, 2, 3)   # => (1, 2, 3)


# Puoi definire funzioni che accettano un numero variabile di parole chiave
# come argomento, che saranno interpretati come un dizionario usando **
def keyword_args(**kwargs):
    return kwargs

# Chiamiamola per vedere cosa succede
keyword_args(big="foot", loch="ness")   # => {"big": "foot", "loch": "ness"}


# Puoi farle entrambi in una volta, se ti va
def all_the_args(*args, **kwargs):
    print args
    print kwargs
"""
all_the_args(1, 2, a=3, b=4) stampa:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Quando chiami funzioni, puoi fare l'opposto di args/kwargs!
# Usa * per sviluppare gli argomenti posizionale ed usa ** per espandere gli argomenti parola chiave
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)   # equivalente a foo(1, 2, 3, 4)
all_the_args(**kwargs)   # equivalente a foo(a=3, b=4)
all_the_args(*args, **kwargs)   # equivalente a foo(1, 2, 3, 4, a=3, b=4)

# puoi passare args e kwargs insieme alle altre funzioni che accettano args/kwargs
# sviluppandoli, rispettivamente, con * e **
def pass_all_the_args(*args, **kwargs):
    all_the_args(*args, **kwargs)
    print varargs(*args)
    print keyword_args(**kwargs)

# Funzioni Scope
x = 5

def set_x(num):
    # La variabile locale x non è uguale alla variabile globale x
    x = num # => 43
    print x # => 43

def set_global_x(num):
    global x
    print x # => 5
    x = num # la variabile globable x è ora 6
    print x # => 6

set_x(43)
set_global_x(6)

# Python ha funzioni di prima classe
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# Ci sono anche funzioni anonime
(lambda x: x > 2)(3)   # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1) # => 5

# Esse sono incluse in funzioni di alto livello
map(add_10, [1, 2, 3])   # => [11, 12, 13]
map(max, [1, 2, 3], [4, 2, 1])   # => [4, 2, 3]

filter(lambda x: x > 5, [3, 4, 5, 6, 7])   # => [6, 7]

# Possiamo usare la comprensione delle liste per mappe e filtri
[add_10(i) for i in [1, 2, 3]]  # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]   # => [6, 7]

# Puoi fare anche la comprensione di set e dizionari
{x for x in 'abcddeef' if x in 'abc'}  # => {'d', 'e', 'f'}
{x: x**2 for x in range(5)}  # => {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}


####################################################
## 5. Classi
####################################################

# Usiamo una sottoclasse da un oggetto per avere una classe.
class Human(object):

    # Un attributo della classe. E' condiviso da tutte le istanze delle classe
    species = "H. sapiens"

    # Costruttore base, richiamato quando la classe viene inizializzata.
    # Si noti che il doppio leading e gli underscore finali denotano oggetti
    # o attributi che sono usati da python ma che vivono nello spazio dei nome controllato
    # dall'utente. Non dovresti usare nomi di questo genere.
    def __init__(self, name):
        # Assegna l'argomento all'attributo name dell'istanza
        self.name = name

        # Inizializza una proprietà
        self.age = 0

    # Un metodo dell'istanza. Tutti i metodi prendo "self" come primo argomento
    def say(self, msg):
        return "{0}: {1}".format(self.name, msg)

    # Un metodo della classe è condiviso fra tutte le istanze
    # Sono chiamate con la classe chiamante come primo argomento
    @classmethod
    def get_species(cls):
        return cls.species

    # Un metodo statico è chiamato senza una classe od una istanza di riferimento
    @staticmethod
    def grunt():
        return "*grunt*"

    # Una proprietà è come un metodo getter.
    # Trasforma il metodo age() in un attributo in sola lettura, che ha lo stesso nome
    @property
    def age(self):
        return self._age

    # Questo metodo permette di modificare la proprietà
    @age.setter
    def age(self, age):
        self._age = age

    # Questo metodo permette di cancellare la proprietà
    @age.deleter
    def age(self):
        del self._age

# Instanziare una classe
i = Human(name="Ian")
print i.say("hi")     # stampa "Ian: hi"

j = Human("Joel")
print j.say("hello")  # stampa "Joel: hello"

# Chiamare metodi della classe
i.get_species()   # => "H. sapiens"

# Cambiare l'attributo condiviso
Human.species = "H. neanderthalensis"
i.get_species()   # => "H. neanderthalensis"
j.get_species()   # => "H. neanderthalensis"

# Chiamare il metodo condiviso
Human.grunt()   # => "*grunt*"

# Aggiorna la proprietà
i.age = 42

# Ritorna il valore della proprietà
i.age # => 42

# Cancella la proprietà
del i.age
i.age  # => Emette un AttributeError


####################################################
## 6. Moduli
####################################################

# Puoi importare moduli
import math
print math.sqrt(16)  # => 4

# Puoi ottenere specifiche funzione da un modulo
from math import ceil, floor
print ceil(3.7)  # => 4.0
print floor(3.7)   # => 3.0

# Puoi importare tutte le funzioni da un modulo
# Attenzione: questo non è raccomandato
from math import *

# Puoi abbreviare i nomi dei moduli
import math as m
math.sqrt(16) == m.sqrt(16)   # => True
# puoi anche verificare che le funzioni sono equivalenti
from math import sqrt
math.sqrt == m.sqrt == sqrt  # => True

# I moduli di Python sono normali file python. Ne puoi
# scrivere di tuoi ed importarli. Il nome del modulo
# è lo stesso del nome del file.

# Potete scoprire quali funzioni e attributi
# definiscono un modulo
import math
dir(math)

# Se nella cartella corrente hai uno script chiamato math.py,
# Python caricherà quello invece del modulo math.
# Questo succede perchè la cartella corrente ha priorità
# sulle librerie standard di Python


####################################################
## 7. Avanzate
####################################################

# Generatori
# Un generatore appunto "genera" valori solo quando vengono richiesti,
# invece di memorizzarli tutti subito fin dall'inizio

# Il metodo seguente (che NON è un generatore) raddoppia tutti i valori e li memorizza
# dentro `double_arr`. Se gli oggetti iterabili sono grandi, il vettore risultato
# potrebbe diventare enorme!
def double_numbers(iterable):
    double_arr = []
    for i in iterable:
        double_arr.append(i + i)

# Eseguendo il seguente codice, noi andiamo a raddoppiare prima tutti i valori, e poi
# li ritorniamo tutti e andiamo a controllare la condizione
for value in double_numbers(range(1000000)):  # `test_senza_generatore`
    print value
    if value > 5:
        break

# Invece, potremmo usare un generatore per "generare" il valore raddoppiato non
# appena viene richiesto
def double_numbers_generator(iterable):
    for i in iterable:
        yield i + i

# Utilizzando lo stesso test di prima, stavolta però con un generatore, ci permette
# di iterare sui valori e raddoppiarli uno alla volta, non appena vengono richiesti dalla
# logica del programma. Per questo, non appena troviamo un valore > 5, usciamo dal ciclo senza
# bisogno di raddoppiare la maggior parte dei valori del range (MOLTO PIU VELOCE!)
for value in double_numbers_generator(xrange(1000000)):  # `test_generatore`
    print value
    if value > 5:
        break

# Nota: hai notato l'uso di `range` in `test_senza_generatore` e `xrange` in `test_generatore`?
# Proprio come `double_numbers_generator` è la versione col generatore di `double_numbers`
# Abbiamo `xrange` come versione col generatore di `range`
# `range` ritorna un array di 1000000 elementi
# `xrange` invece genera 1000000 valori quando lo richiediamo/iteriamo su di essi

# Allo stesso modo della comprensione delle liste, puoi creare la comprensione
# dei generatori.
values = (-x for x in [1,2,3,4,5])
for x in values:
    print(x)  # stampa -1 -2 -3 -4 -5

# Puoi anche fare il cast diretto di una comprensione di generatori ad una lista.
values = (-x for x in [1,2,3,4,5])
gen_to_list = list(values)
print(gen_to_list)  # => [-1, -2, -3, -4, -5]


# Decoratori
# in questo esempio beg include say
# Beg chiamerà say. Se say_please è True allora cambierà il messaggio
# ritornato
from functools import wraps

def beg(target_function):
    @wraps(target_function)
    def wrapper(*args, **kwargs):
        msg, say_please = target_function(*args, **kwargs)
        if say_please:
            return "{} {}".format(msg, "Per favore! Sono povero :(")
        return msg

    return wrapper


@beg
def say(say_please=False):
    msg = "Puoi comprarmi una birra?"
    return msg, say_please


print say()  # Puoi comprarmi una birra?
print say(say_please=True)  # Puoi comprarmi una birra? Per favore! Sono povero :(
```

## Pronto per qualcosa di più?

### Gratis Online

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

### Libri cartacei

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)
