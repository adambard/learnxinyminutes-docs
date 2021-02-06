---
language: Python
filename: learnpython-it.py
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
    - ["Zachary Ferguson", "http://github.com/zfergus2"]
    - ["evuez", "http://github.com/evuez"]
    - ["Rommel Martinez", "https://ebzzry.io"]
translators:
    - ["Draio", "http://github.com/Draio/"]
    - ["Ale46", "http://github.com/Ale46/"]
    - ["Tommaso Pifferi", "http://github.com/neslinesli93/"]
lang: it-it    
---

Python è stato creato da Guido Van Rossum agli inizi degli anni 90. Oggi è uno dei più popolari linguaggi esistenti. Mi sono innamorato di Python per la sua chiarezza sintattica. E' sostanzialmente pseudocodice eseguibile.

Feedback sono altamente apprezzati! Potete contattarmi su [@louiedinh](http://twitter.com/louiedinh) oppure [at] [google's email service]

Nota: Questo articolo è riferito a Python 3 in modo specifico. Se volete avete la necessità di utilizzare Python 2.7 potete consultarla [qui](https://learnxinyminutes.com/docs/it-it/python-it/)

```python

# I commenti su una sola linea iniziano con un cancelletto


""" Più stringhe possono essere scritte
    usando tre ", e sono spesso usate
    come documentazione
"""

####################################################
## 1. Tipi di dati primitivi ed Operatori
####################################################

# Ci sono i numeri
3  # => 3

# La matematica è quello che vi aspettereste
1 + 1   # => 2
8 - 1   # => 7
10 * 2  # => 20
35 / 5  # => 7.0

# Risultato della divisione intera troncata sia in positivo che in negativo
5 // 3       # => 1
5.0 // 3.0   # => 1.0 # works on floats too
-5 // 3      # => -2
-5.0 // 3.0  # => -2.0

# Il risultato di una divisione è sempre un numero decimale (float)
10.0 / 3  # => 3.3333333333333335

# Operazione Modulo
7 % 3  # => 1

# Elevamento a potenza (x alla y-esima potenza)
2**3  # => 8

# Forzare le precedenze con le parentesi
(1 + 3) * 2  # => 8

# I valori booleani sono primitive del linguaggio (nota la maiuscola)
True
False

# nega con not
not True   # => False
not False  # => True

# Operatori Booleani
# Nota "and" e "or" sono case-sensitive
True and False  # => False
False or True   # => True

# Note sull'uso di operatori Bool con interi
# False è 0 e True è 1
# Non confonderti tra bool(ints) e le operazioni bitwise and/or (&,|)
0 and 2     # => 0
-5 or 0     # => -5
0 == False  # => True
2 == True   # => False
1 == True   # => True
-5 != False != True #=> True

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

# ('is' vs. '==') 
# 'is' controlla se due variabili si riferiscono allo stesso oggetto
# '==' controlla se gli oggetti puntati hanno lo stesso valore.
a = [1, 2, 3, 4]  # a punta ad una nuova lista [1, 2, 3, 4]
b = a             # b punta a ciò a cui punta a
b is a            # => True, a e b puntano allo stesso oggeto
b == a            # => True, gli oggetti di a e b sono uguali
b = [1, 2, 3, 4]  # b punta ad una nuova lista  [1, 2, 3, 4]
b is a            # => False, a e b non puntano allo stesso oggetto
b == a            # => True, gli oggetti di a e b sono uguali

# Le stringhe sono create con " o '
"Questa è una stringa."
'Anche questa è una stringa.'

# Anche le stringhe possono essere sommate! Ma cerca di non farlo.
"Hello " + "world!"  # => "Hello world!"
# Le stringhe (ma non le variabili contenenti stringhe) possono essere 
# sommate anche senza '+'
"Hello " "world!"    # => "Hello world!"

# Una stringa può essere considerata come una lista di caratteri
"Questa è una stringa"[0]  # => 'Q'

# Puoi conoscere la lunghezza di una stringa
len("Questa è una stringa")  # => 20

# .format può essere usato per formattare le stringhe, in questo modo:
"{} possono essere {}".format("Le stringhe", "interpolate")  # => "Le stringhe possono essere interpolate"

# Puoi ripetere gli argomenti di formattazione per risparmiare un po' di codice
"{0} be nimble, {0} be quick, {0} jump over the {1}".format("Jack", "candle stick")
# => "Jack be nimble, Jack be quick, Jack jump over the candle stick"

# Puoi usare dei nomi se non vuoi contare gli argomenti
"{nome} vuole mangiare {cibo}".format(nome="Bob", cibo="le lasagne")  # => "Bob vuole mangiare le lasagne"

# Se il tuo codice Python 3 necessita di eseguire codice Python 2.x puoi ancora
# utilizzare il vecchio stile di formattazione:
"%s possono essere %s nel %s modo" % ("Le stringhe", "interpolate", "vecchio")  # => "Le stringhe possono essere interpolate nel vecchio modo"

# None è un oggetto
None  # => None

# Non usare il simbolo di uguaglianza "==" per comparare oggetti a None
# Usa "is" invece
"etc" is None  # => False
None is None   # => True

# None, 0, e stringhe/liste/dizionari/tuple vuoti vengono considerati
# falsi (False). Tutti gli altri valori sono considerati veri (True).
bool(0)   # => False
bool("")  # => False
bool([])  # => False
bool({})  # => False
bool(())  # => False

####################################################
## 2. Variabili e Collections
####################################################

# Python ha una funzione per scrivere (sul tuo schermo)
print("Sono Python. Piacere di conoscerti!")  # => Sono Python. Piacere di conoscerti!

# Di default la funzione print() scrive e va a capo aggiungendo un carattere 
# newline alla fine della stringa. È possibile utilizzare l'argomento opzionale
# end per cambiare quest'ultimo carattere aggiunto.
print("Hello, World", end="!")  # => Hello, World!

# Un modo semplice per ricevere dati in input dalla riga di comando
variabile_stringa_input  = input("Inserisci del testo: ") # Restituisce i dati letti come stringa
# Nota: Nelle precedenti vesioni di Python, il metodo input() 
# era chiamato raw_input()

# Non c'è bisogno di dichiarare una variabile per assegnarle un valore
# Come convenzione, per i nomi delle variabili, si utilizzano i caratteri 
# minuscoli separati, se necessario, da underscore
some_var = 5
some_var          # => 5

# Accedendo ad una variabile non precedentemente assegnata genera un'eccezione.
# Dai un'occhiata al Control Flow per imparare di più su come gestire 
# le eccezioni.
some_unknown_var  # Genera un errore di nome

# if può essere usato come un'espressione
# È l'equivalente dell'operatore ternario in C
"yahoo!" if 3 > 2 else 2  # => "yahoo!"

# Le liste immagazzinano sequenze
li = []
# Puoi partire con una lista pre-riempita
other_li = [4, 5, 6]

# Aggiungere alla fine di una lista con append
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
# Guarda l'ultimo elemento
li[-1]  # => 3

# Guardare al di fuori dei limiti genera un IndexError
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

# Crea una copia (one layer deep copy) usando la sintassi slices
li2 = li[:]  # => li2 = [1, 2, 4, 3] ma (li2 is li) risulterà falso.

# Rimuovi arbitrariamente elementi da una lista con "del"
del li[2]   # li è ora [1, 2, 3]

# Rimuove la prima occorrenza di un elemento
li.remove(2)  # Ora li è [1, 3, 4, 5, 6]
li.remove(2)  # Emette un ValueError, poichè 2 non è contenuto nella lista

# Inserisce un elemento all'indice specificato
li.insert(1, 2)  # li è di nuovo [1, 2, 3, 4, 5, 6]

 Ritorna l'indice della prima occorrenza dell'elemento fornito
li.index(2)  # => 1
li.index(7)  # Emette un ValueError, poichè 7 non è contenuto nella lista

# Puoi sommare le liste
# Nota: i valori per li e per other_li non vengono modificati.
li + other_li  # => [1, 2, 3, 4, 5, 6]

# Concatena le liste con "extend()"
li.extend(other_li)  # Adesso li è [1, 2, 3, 4, 5, 6]

# Controlla l'esistenza di un valore in una lista con "in"
1 in li   # => True

# Esamina la lunghezza con "len()"
len(li)   # => 6


# Le tuple sono come le liste ma immutabili.
tup = (1, 2, 3)
tup[0]      # => 1
tup[0] = 3  # Genera un TypeError

# Note that a tuple of length one has to have a comma after the last element but
# tuples of other lengths, even zero, do not.
type((1))   # => <class 'int'>
type((1,))  # => <class 'tuple'>
type(())    # => <class 'tuple'>

# Puoi fare tutte queste cose da lista anche sulle tuple
len(tup)         # => 3
tup + (4, 5, 6)  # => (1, 2, 3, 4, 5, 6)
tup[:2]          # => (1, 2)
2 in tup         # => True

# Puoi scompattare le tuple (o liste) in variabili
a, b, c = (1, 2, 3)     # a è ora 1, b è ora 2 e c è ora 3
d, e, f = 4, 5, 6       # puoi anche omettere le parentesi
# Le tuple sono create di default se non usi le parentesi
g = 4, 5, 6             # => (4, 5, 6)
# Guarda come è facile scambiare due valori
e, d = d, e             # d è ora 5 ed e è ora 4

# I dizionari memorizzano insiemi di dati indicizzati da nomi arbitrari (chiavi)
empty_dict= {}
# Questo è un dizionario pre-caricato
filled_dict = {"uno": 1, "due": 2, "tre": 3}

# Nota: le chiavi  dei dizionari devono essere di tipo immutabile. Questo per
# assicurare che le chiavi possano essere convertite in calori hash costanti 
# per un risposta più veloce.
invalid_dict = {[1,2,3]: "123"}  # => Emette un TypeError: unhashable type: 'list'
valid_dict = {(1,2,3):[1,2,3]}   # I valori, invece, possono essere di qualunque tipo

# Accedi ai valori indicando la chiave tra []
filled_dict["uno"]   # => 1

# Puoi ottenere tutte le chiavi di un dizionario con "keys()" 
# (come oggetto iterabile). Per averle in formato lista è necessario 
# utilizzare list().
# Nota - Nei dizionari l'ordine delle chiavi non è garantito.
# Il tuo risultato potrebbe non essere uguale a questo.
list(filled_dict.keys())  # => ["tre", "due", "uno"]


# Puoi ottenere tutti i valori di un dizionario con "values()" 
# (come oggetto iterabile). 
# Anche in questo caso, er averle in formato lista, è necessario utilizzare list()
# Anche in questo caso, come per le chiavi, l'ordine non è garantito
list(filled_dict.values())  # => [3, 2, 1]

# Controlla l'esistenza delle chiavi in un dizionario con "in"
"uno" in filled_dict   # => True
1 in filled_dict       # => False

# Cercando una chiave non esistente genera un KeyError
filled_dict["quattro"]   # KeyError

# Usa il metodo "get()" per evitare KeyError
filled_dict.get("uno")      # => 1
filled_dict.get("quattro")  # => None
# Il metodo get supporta un argomento di default quando il valore è mancante
filled_dict.get("uno", 4)   # => 1
filled_dict.get("quattro", 4)   # => 4


# "setdefault()" inserisce un valore per una chiave in un dizionario 
# solo se la chiave data non è già presente
filled_dict.setdefault("cinque", 5)  # filled_dict["cinque"] viene impostato a 5
filled_dict.setdefault("cinque", 6)  # filled_dict["cinque"] rimane 5

# Aggiungere una coppia chiave->valore a un dizionario
filled_dict.update({"quattro":4})  # => {"uno": 1, "due": 2, "tre": 3, "quattro": 4}
filled_dict["quattro"] = 4         # un altro modo pe aggiungere a un dizionario

# Rimuovi una chiave da un dizionario con del
del filled_dict["uno"]  # Rimuove la chiave "uno" dal dizionario

# Da Python 3.5 puoi anche usare ulteriori opzioni di spacchettamento
{'a': 1, **{'b': 2}}  # => {'a': 1, 'b': 2}
{'a': 1, **{'a': 2}}  # => {'a': 2}

# I set sono come le liste ma non possono contenere doppioni
empty_set = set()
# Inizializza un "set()" con un dei valori. Sì, sembra un dizionario.
some_set = {1, 1, 2, 2, 3, 4}  # set_nuovo è {1, 2, 3, 4}

# Come le chiavi di un dizionario, gli elementi di un set devono essere 
# di tipo immutabile
invalid_set = {[1], 1}  # => Genera un "TypeError: unhashable type: 'list'""
valid_set = {(1,), 1}

# Aggiungere uno o più elementi ad un set
some_set.add(5)  # some_set ora è  {1, 2, 3, 4, 5}

# Fai intersezioni su un set con &
other_set = {3, 4, 5, 6}
some_set & other_set  # => {3, 4, 5}

# Fai unioni su set con |
some_set | other_set  # => {1, 2, 3, 4, 5, 6}

# Fai differenze su set con -
{1, 2, 3, 4} - {2, 3, 5}  # => {1, 4}

# Effettua la differenza simmetrica con ^
{1, 2, 3, 4} ^ {2, 3, 5}  # => {1, 4, 5}

# Controlla se il set a sinistra contiene quello a destra
{1, 2} >= {1, 2, 3} # => False

# Controlla se il set a sinistra è un sottoinsieme di quello a destra
{1, 2} <= {1, 2, 3} # => True

# Controlla l'esistenza in un set con in
2 in some_set   # => True
10 in some_set  # => False



####################################################
## 3. Control Flow e oggetti Iterabili
####################################################

# Dichiariamo  una variabile
some_var = 5

# Questo è un controllo if. L'indentazione è molto importante in python!
# Come convenzione si utilizzano quattro spazi, non la tabulazione.
# Il seguente codice stampa "some_var è minore di 10"
if some_var > 10:
    print("some_var è maggiore di 10")
elif some_var < 10:    # La clausolo elif è opzionale
    print("some_var è minore di 10")
else:                  # Anche else è opzionale
    print("some_var è  10.")

"""
I cicli for iterano sulle liste, cioè ripetono un codice per ogni elemento 
di una lista.
Il seguente codice scriverà:
    cane è un mammifero
    gatto è un mammifero
    topo è un mammifero
"""
for animale in ["cane", "gatto", "topo"]:
    # Puoi usare format() per interpolare le stringhe formattate.
    print("{} è un mammifero".format(animale))

"""
"range(numero)" restituisce una lista di numeri da zero al numero dato
Il seguente codice scriverà:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
"range(lower, upper)" restituisce una lista di numeri dal più piccolo (lower)
al più grande (upper).
Il seguente codice scriverà:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
"range(lower, upper, step)" rrestituisce una lista di numeri dal più piccolo 
(lower) al più grande (upper), incrementando del valore step.
Se step non è indicato, avrà come valore di default 1.
Il seguente codice scriverà:
    4
    6
"""
for i in range(4, 8, 2):
    print(i)
"""

I cicli while vengono eseguiti finchè una condizione viene a mancare
Il seguente codice scriverà:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # Forma compatta per x = x + 1

# Gestione delle eccezioni con un blocco try/except
try:
    # Usa "raise" per generare un errore
    raise IndexError("Questo è un IndexError")
except IndexError as e:
    pass    # Pass è solo una non-operazione. Solitamente vorrai rimediare all'errore.
except (TypeError, NameError):
    pass    # Eccezioni multiple possono essere gestite tutte insieme, se necessario.
else:   # Clausola opzionale al blocco try/except. Deve essere dopo tutti i blocchi except
    print("Tutto ok!")   # Viene eseguita solo se il codice dentro try non genera eccezioni
finally: #  Eseguito sempre
    print("Possiamo liberare risorse qui")

# Se ti serve solo un try/finally, per liberare risorse, puoi usare il metodo with
with open("myfile.txt") as f:
    for line in f:
        print(line)

# In Python qualunque oggetto in grado di essere trattato come una 
# sequenza è definito un oggetto Iterable (itarabile).
# L'oggetto restituito da una funzione range è un iterabile.

filled_dict = {"uno": 1, "due": 2, "tre": 3}
our_iterable = filled_dict.keys()
print(our_iterable)  # => dict_keys(['uno', 'due', 'tre']). 
# Questo è un oggetto che implementa la nostra interfaccia Iterable.

# È possibile utilizzarlo con i loop:
for i in our_iterable:
    print(i)  # Scrive uno, due, tre

# Tuttavia non possiamo recuperarne i valori tramite indice.
our_iterable[1]  # Genera un TypeError

# Un oggetto iterabile è in grado di generare un iteratore
our_iterator = iter(our_iterable)

# L'iteratore è un oggetto che ricorda il suo stato mentro lo si "attraversa"
# Possiamo accedere al successivo elemento con "next()".
next(our_iterator)  # => "uno"

# Mantiene il suo stato mentro eseguiamo l'iterazione
next(our_iterator)  # => "due"
next(our_iterator)  # => "tre"

# Dopo che un iteratore ha restituito tutti i suoi dati, genera 
# un'eccezione StopIteration 
next(our_iterator)  # Raises StopIteration

# Puoi prendere tutti gli elementi di un iteratore utilizzando list().
list(filled_dict.keys())  # => Returns ["one", "two", "three"]



####################################################
## 4. Funzioni
####################################################

# Usa "def" per creare nuove funzioni
def aggiungi(x, y):
    print("x è {} e y è {}".format(x, y)) // Scrive i valori formattati in una stringa
    return x + y  # Restituisce la somma dei valori con il metodo return

# Chiamare funzioni con parametri
aggiungi(5, 6)  # => scrive "x è 5 e y è 6" e restituisce 11

# Un altro modo per chiamare funzioni  è con parole chiave come argomenti
aggiungi(y=6, x=5)  # In questo modo non è necessario rispettare l'ordine degli argomenti

# Puoi definire funzioni che accettano un numero non definito di argomenti
def varargs(*args):
    return args

varargs(1, 2, 3)  # => (1, 2, 3)

# Puoi definire funzioni che accettano un numero variabile di parole chiave
# come argomento, che saranno interpretati come un dizionario usando **
def keyword_args(**kwargs):
    return kwargs

# Chiamiamola per vedere cosa succede
keyword_args(big="foot", loch="ness")   # => {"big": "foot", "loch": "ness"}


# Puoi farle entrambi in una volta, se ti va
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) stampa:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Quando chiami funzioni, puoi fare l'opposto di args/kwargs!
# Usa * per sviluppare gli argomenti posizionale ed usa ** per 
# espandere gli argomenti parola chiave
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)            # equivalente a foo(1, 2, 3, 4)
all_the_args(**kwargs)         # equivalente a foo(a=3, b=4)
all_the_args(*args, **kwargs)  # equivalente a foo(1, 2, 3, 4, a=3, b=4)


# Restituire valori multipli (with tuple assignments)
def swap(x, y):
    return y, x  # Restituisce valori multipli come tupla senza parentesi
                 # (Nota: le parentesi sono state escluse ma possono essere messe)

x = 1
y = 2
x, y = swap(x, y)     # => x = 2, y = 1
# (x, y) = swap(x,y)  # Le parentesi sono state escluse ma possono essere incluse.

# Funzioni - Visibilità delle variabili (variable scope)
x = 5

def set_x(num):
    # La variabile locale x non è la variabile globale x
    x = num    # => 43
    print(x)   # => 43

def set_global_x(num):
    global x
    print(x)   # => 5
    x = num    # la variabile globable x è ora 6
    print(x)   # => 6

set_x(43)
set_global_x(6)


# Python ha "first class functions"
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# Ci sono anche funzioni anonime
(lambda x: x > 2)(3)                  # => True
(lambda x, y: x ** 2 + y ** 2)(2, 1)  # => 5

# È possibile creare "mappe" e "filtri"
list(map(add_10, [1, 2, 3]))          # => [11, 12, 13]
list(map(max, [1, 2, 3], [4, 2, 1]))  # => [4, 2, 3]

list(filter(lambda x: x > 5, [3, 4, 5, 6, 7]))  # => [6, 7]

# Possiamo usare le "list comprehensions" per mappe e filtri
# Le "list comprehensions" memorizzano l'output come una lista che può essere 
# di per sé una lista annidata
[add_10(i) for i in [1, 2, 3]]         # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]  # => [6, 7]

# Puoi fare anche la comprensione di set e dizionari
{x for x in 'abcddeef' if x not in 'abc'}  # => {'d', 'e', 'f'}
{x: x**2 for x in range(5)}  # => {0: 0, 1: 1, 2: 4, 3: 9, 4: 16}


####################################################
## 5. Modules
####################################################

# Puoi importare moduli
import math
print(math.sqrt(16))  # => 4.0

# Puoi ottenere specifiche funzione da un modulo
from math import ceil, floor
print(ceil(3.7))   # => 4.0
print(floor(3.7))  # => 3.0

# Puoi importare tutte le funzioni da un modulo
# Attenzione: questo non è raccomandato
from math import *

# Puoi abbreviare i nomi dei moduli
import math as m
math.sqrt(16) == m.sqrt(16)  # => True


# I moduli di Python sono normali file python. Ne puoi
# scrivere di tuoi ed importarli. Il nome del modulo
# è lo stesso del nome del file.

# Potete scoprire quali funzioni e attributi
# sono definiti in un modulo
import math
dir(math)

# Se nella cartella corrente hai uno script chiamato math.py,
# Python caricherà quello invece del modulo math.
# Questo succede perchè la cartella corrente ha priorità
# sulle librerie standard di Python

# Se hai uno script Python chiamato math.py nella stessa 
# cartella del tua script, Python caricherà quello al posto del
# comune modulo math.
# Questo accade perché la cartella locale ha la priorità
# sulle librerie built-in di Python.


####################################################
## 6. Classes
####################################################

# Usiamo l'istruzione "class" per creare una classe
class Human:

    # Un attributo della classe. E' condiviso tra tutte le istanze delle classe
    species = "H. sapiens"

    # Si noti che i doppi underscore iniziali e finali denotano gli oggetti o
    # attributi utilizzati da Python ma che vivono nel namespace controllato 
    # dall'utente
    # Metodi, oggetti o attributi come: __init__, __str__, __repr__, etc. sono
    # chiamati metodi speciali (o talvolta chiamati "dunder methods").
    # Non dovresti inventare tali nomi da solo.

    def __init__(self, name):
        # Assegna l'argomento all'attributo name dell'istanza
        self.name = name

        # Inizializza una proprietà
        self._age = 0

    # Un metodo dell'istanza. Tutti i metodi prendo "self" come primo argomento
    def say(self, msg):
        print("{name}: {message}".format(name=self.name, message=msg))

    # Un altro metodo dell'istanza
    def sing(self):
        return 'yo... yo... microphone check... one two... one two...'

    # Un metodo della classe è condiviso fra tutte le istanze
    # Sono chiamati con la classe chiamante come primo argomento
    @classmethod
    def get_species(cls):
        return cls.species

    # Un metodo statico è chiamato senza classe o istanza di riferimento
    @staticmethod
    def grunt():
        return "*grunt*"

    # Una property è come un metodo getter.
    # Trasforma il metodo age() in un attributo in sola lettura, che ha 
    # lo stesso nome 
    # In Python non c'è bisogno di scrivere futili getter e setter.
    @property
    def age(self):
        return self._age

    # Questo metodo permette di modificare una property
    @age.setter
    def age(self, age):
        self._age = age

    # Questo metodo permette di cancellare una property
    @age.deleter
    def age(self):
        del self._age

# Quando l'interprete Python legge un sorgente esegue tutto il suo codice.
# Questo controllo su __name__ assicura che questo blocco di codice venga 
# eseguito solo quando questo modulo è il programma principale.

if __name__ == '__main__':
    # Crea un'istanza della classe
    i = Human(name="Ian")
    i.say("hi")                     # "Ian: hi"
    j = Human("Joel")
    j.say("hello")                  # "Joel: hello"
    # i e j sono istanze del tipo Human, o in altre parole sono oggetti Human

    # Chiama un metodo della classe
    i.say(i.get_species())          # "Ian: H. sapiens"
    # Cambia l'attributo condiviso
    Human.species = "H. neanderthalensis"
    i.say(i.get_species())          # => "Ian: H. neanderthalensis"
    j.say(j.get_species())          # => "Joel: H. neanderthalensis"

    # Chiama un metodo statico 
    print(Human.grunt())            # => "*grunt*"
    
    # Non è possibile chiamare il metodo statico con l'istanza dell'oggetto
    # poiché i.grunt() metterà automaticamente "self" (l'oggetto i) 
    # come argomento 
    print(i.grunt())                # => TypeError: grunt() takes 0 positional arguments but 1 was given
                                    
    # Aggiorna la property (age) di questa istanza
    i.age = 42
    # Leggi la property
    i.say(i.age)                    # => "Ian: 42"
    j.say(j.age)                    # => "Joel: 0"
    # Cancella la property
    del i.age
    i.age                           # => questo genererà un AttributeError


####################################################
## 6.1 Ereditarietà  (Inheritance)
####################################################

# L'ereditarietà consente di definire nuove classi figlio che ereditano metodi e
# variabili dalla loro classe genitore.

# Usando la classe Human definita sopra come classe base o genitore, possiamo
# definire una classe figlia, Superhero, che erediterà le variabili di classe
# come "species", "name" e "age", così come i metodi, come "sing" e "grunt",
# dalla classe Human, ma potrà anche avere le sue proprietà uniche.

# Per importare le funzioni da altri file usa il seguente formato 
# from "nomefile-senza-estensione" import "funzione-o-classe"

from human import Human

# Specificare le classi genitore come parametri della definizione della classe
class Superhero(Human):

    # Se la classe figlio deve ereditare tutte le definizioni del genitore  
    # senza alcuna modifica, puoi semplicemente usare la parola chiave "pass"
    # (e nient'altro)

    #Le classi figlio possono sovrascrivere gli attributi dei loro genitori
    species = 'Superhuman'

    # Le classi figlie ereditano automaticamente il costruttore della classe 
    # genitore, inclusi i suoi argomenti, ma possono anche definire ulteriori 
    # argomenti o definizioni e sovrascrivere i suoi metodi (compreso  il 
    # costruttore della classe).
    # Questo costruttore eredita l'argomento "nome" dalla classe "Human" e 
    # aggiunge gli argomenti "superpowers" e "movie":

    def __init__(self, name, movie=False,
                 superpowers=["super strength", "bulletproofing"]):

        # aggiungi ulteriori attributi della classe
        self.fictional = True
        self.movie = movie
        self.superpowers = superpowers

        # La funzione "super" ti consente di accedere ai metodi della classe 
        # genitore che sono stati sovrascritti dalla classe figlia,  
        # in questo caso il metodo __init__.
        # Il seguente codice esegue il costruttore della classe genitore:
        super().__init__(name)

    # Sovrascrivere il metodo "sing"
    def sing(self):
        return 'Dun, dun, DUN!'

    # Aggiungi un ulteriore metodo dell'istanza
    def boast(self):
        for power in self.superpowers:
            print("I wield the power of {pow}!".format(pow=power))


if __name__ == '__main__':
    sup = Superhero(name="Tick")

    # Controllo del tipo di istanza
    if isinstance(sup, Human):
        print('I am human')
    if type(sup) is Superhero:
        print('I am a superhero')

    # Ottieni il "Method Resolution search Order" usato sia da getattr () 
    # che da super (). Questo attributo è dinamico e può essere aggiornato
    print(Superhero.__mro__)    # => (<class '__main__.Superhero'>,
                                # => <class 'human.Human'>, <class 'object'>)

    # Esegui il metodo principale ma utilizza il proprio attributo di classe
    print(sup.get_species())    # => Superhuman

    # Esegui un metodo che è stato sovrascritto
    print(sup.sing())           # => Dun, dun, DUN!

    # Esegui un metodo di Human
    sup.say('Spoon')            # => Tick: Spoon

    # Esegui un metodo che esiste solo in Superhero
    sup.boast()                 # => I wield the power of super strength!
                                # => I wield the power of bulletproofing!

    # Attributo di classe ereditato
    sup.age = 31
    print(sup.age)              # => 31

    # Attributo che esiste solo in Superhero
    print('Am I Oscar eligible? ' + str(sup.movie))

####################################################
## 6.2 Ereditarietà multipla
####################################################

# Un'altra definizione di classe
# bat.py
class Bat:

    species = 'Baty'

    def __init__(self, can_fly=True):
        self.fly = can_fly

    # Questa classe ha anche un metodo "say"
    def say(self, msg):
        msg = '... ... ...'
        return msg

    # E anche un suo metodo personale
    def sonar(self):
        return '))) ... ((('

if __name__ == '__main__':
    b = Bat()
    print(b.say('hello'))
    print(b.fly)

# Definizione di classe che eredita da Superhero e Bat
# superhero.py
from superhero import Superhero
from bat import Bat

# Definisci Batman come classe figlia che eredita sia da Superhero che da Bat
class Batman(Superhero, Bat):

    def __init__(self, *args, **kwargs):
        # In genere per ereditare gli attributi devi chiamare super:
        # super(Batman, self).__init__(*args, **kwargs)
        # Ma qui abbiamo a che fare con l'ereditarietà multipla, e super() 
        # funziona solo con la successiva classe nell'elenco MRO.
        # Quindi, invece, chiamiamo esplicitamente __init__ per tutti gli
        # antenati. L'uso di *args e **kwargs consente di passare in modo 
        # pulito gli argomenti, con ciascun genitore che "sbuccia un 
        # livello della cipolla".    
        Superhero.__init__(self, 'anonymous', movie=True, 
                           superpowers=['Wealthy'], *args, **kwargs)
        Bat.__init__(self, *args, can_fly=False, **kwargs)
        # sovrascrivere il valore per l'attributo name
        self.name = 'Sad Affleck'

    def sing(self):
        return 'nan nan nan nan nan batman!'


if __name__ == '__main__':
    sup = Batman()

    # Ottieni il "Method Resolution search Order" utilizzato da getattr() e super().
    # Questo attributo è dinamico e può essere aggiornato
    print(Batman.__mro__)       # => (<class '__main__.Batman'>, 
                                # => <class 'superhero.Superhero'>, 
                                # => <class 'human.Human'>, 
                                # => <class 'bat.Bat'>, <class 'object'>)

    # Esegui il metodo del genitore ma utilizza il proprio attributo di classe
    print(sup.get_species())    # => Superhuman

    # Esegui un metodo che è stato sovrascritto
    print(sup.sing())           # => nan nan nan nan nan batman!

    # Esegui un metodo da Human, perché l'ordine di ereditarietà è importante 
    sup.say('I agree')          # => Sad Affleck: I agree

    # Esegui un metodo che esiste solo nel 2o antenato
    print(sup.sonar())          # => ))) ... (((

    # Attributo di classe ereditato
    sup.age = 100
    print(sup.age)              # => 100

    # Attributo ereditato dal secondo antenato il cui valore predefinito
    # è stato ignorato.
    print('Can I fly? ' + str(sup.fly)) # => Can I fly? False



####################################################
## 7. Advanced
####################################################

# I generatori ti aiutano a creare codice pigro (lazy code).
# Codice che darà un risultato solo quando sarà "valutato"
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# I generatori sono efficienti in termini di memoria perché caricano
# solo i dati necessari per elaborare il valore successivo nell'iterabile. 
# Ciò consente loro di eseguire operazioni su intervalli di valori
# altrimenti proibitivi.
# NOTA: `range` sostituisce` xrange` in Python 3.
for i in double_numbers(range(1, 900000000)):  # `range` is a generator.
    print(i)
    if i >= 30:
        break

# Proprio come è possibile creare una "list comprehension", è possibile 
# creare anche delle "generator comprehensions".
values = (-x for x in [1,2,3,4,5])
for x in values:
    print(x)  # prints -1 -2 -3 -4 -5 to console/terminal

# Puoi anche trasmettere una "generator comprehensions" direttamente 
# ad un elenco.
values = (-x for x in [1,2,3,4,5])
gen_to_list = list(values)
print(gen_to_list)  # => [-1, -2, -3, -4, -5]


# Decoratori
# In questo esempio "beg" avvolge/wrappa  "say". 
# Se say_please è True, cambierà il messaggio restituito.
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


print(say())                 # Puoi comprarmi una birra?
print(say(say_please=True))  # Puoi comprarmi una birra? Per favore! Sono povero :(
```

## Pronto per qualcosa di più?

### Gratis Online

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com)
* [Ideas for Python Projects](http://pythonpracticeprojects.com)
* [The Official Docs](http://docs.python.org/3/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Course](http://www.python-course.eu/index.php)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)
* [A curated list of awesome Python frameworks, libraries and software](https://github.com/vinta/awesome-python)
* [30 Python Language Features and Tricks You May Not Know About](http://sahandsaba.com/thirty-python-language-features-and-tricks-you-may-not-know.html)
* [Official Style Guide for Python](https://www.python.org/dev/peps/pep-0008/)
* [Python 3 Computer Science Circles](http://cscircles.cemc.uwaterloo.ca/)
* [Dive Into Python 3](http://www.diveintopython3.net/index.html)
* [A Crash Course in Python for Scientists](http://nbviewer.jupyter.org/gist/anonymous/5924718)
