---
language: Python 2 (legacy)
filename: learnpythonlegacy-pl.py
contributors:
    - ["Louie Dinh", "http://ldinh.ca"]
    - ["Amin Bandali", "http://aminbandali.com"]
    - ["Andre Polykanine", "https://github.com/Oire"]
translators:
    - ["Dominik Krzemiński", "https://github.com/dokato"]
lang: pl-pl
---

Python został opracowany przez Guido Van Rossuma na początku lat 90-tych.
Obecnie jest jednym z najbardziej popularnych języków programowania.
Zakochałem się w Pythonie dzięki porządkowi, jaki utrzymywany jest w kodzie.
To po prostu wykonywalny pseudokod.

Zapraszam do kontaktu. Złapiecie nas na:
- kontakt polski: raymon92 [at] [google's email service]
- kontakt angielski: [@louiedinh](http://twitter.com/louiedinh) lub louiedinh [at] [google's email service]

Uwaga: Ten artykuł odnosi się do wersji Pythona 2.7, ale powinien
działać w wersjach 2.x. Dla wersji 3.x znajdziesz odpowiedni artykuł na stronie głównej.

```python
# -*- coding: utf-8 -*-

# Pojedyncze komentarze oznaczamy takim symbolem.

""" Wielolinijkowe napisy zapisywane są przy użyciu
    potrójnych cudzysłowów i często
    wykorzystywane są jako komentarze.
"""

####################################################
## 1. Podstawowe typy danych i operatory
####################################################

# Liczby to liczby
3  # => 3

# Matematyka jest intuicyjna
1 + 1  # => 2
8 - 1  # => 7
10 * 2  # => 20
35 / 5  # => 7

# Dzielenie może być kłopotliwe. Poniższe działanie to dzielenie
# całkowitoliczbowe(int) i wynik jest automatycznie zaokrąglany.
5 / 2  # => 2

# Aby to naprawić, musimy powiedzieć nieco o liczbach zmiennoprzecinkowych.
2.0     # To liczba zmiennoprzecinkowa, tzw. float
11.0 / 4.0  # => 2.75 ahhh...znacznie lepiej

# Wynik dzielenia całkowitoliczbowego jest obcinany dla liczb
# dodatnich i ujemnych.
5 // 3     # => 1
5.0 // 3.0 # => 1.0 # działa też na floatach
-5 // 3  # => -2
-5.0 // 3.0 # => -2.0

# Operator modulo - wyznaczanie reszty z dzielenia
7 % 3 # => 1

# Potęgowanie (x do potęgi y-tej)
2**4 # => 16

# Wymuszanie pierwszeństwa w nawiasach
(1 + 3) * 2  # => 8

# Operacje logiczne
# Zauważ, że przy "and" i "or" trzeba zwracać uwagę na rozmiar liter
True and False #=> False  # Fałsz
False or True #=> True   # Prawda

# Zauważ, że operatorów logicznych można używać z intami
0 and 2 #=> 0
-5 or 0 #=> -5
0 == False #=> True
2 == True #=> False
k1 == True #=> True

# aby zanegować, użyj "not"
not True  # => False
not False  # => True

# Równość ==
1 == 1  # => True
2 == 1  # => False

# Nierówność !=
1 != 1  # => False
2 != 1  # => True

# Więcej porównań
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# Porównania można układać w łańcuch!
1 < 2 < 3  # => True
2 < 3 < 2  # => False

# Napisy (typ string) tworzone są przy użyciu cudzysłowów " lub '
"Jestem napisem."
'Ja też jestem napisem.'

# Napisy można dodawać!
"Witaj " + "świecie!"  # => "Witaj świecie!"

# ... a nawet mnożyć
"Hej" * 3  # => "HejHejHej"

# Napis może być traktowany jako lista znaków
"To napis"[0]  # => 'T'

# % może być używane do formatowania napisów:
"%s są %s" % ("napisy", "fajne")

# Jednak nowszym sposobem formatowania jest metoda "format".
# Ta metoda jest obecnie polecana:
"{0} są {1}".format("napisy", "fajne")
# Jeśli nie chce ci się liczyć, użyj słów kluczowych.
"{imie} chce zjeść {jadlo}".format(imie="Bob", jadlo="makaron")

# None jest obiektem
None  # => None

# Nie używaj "==" w celu porównania obiektów z None
# Zamiast tego użyj "is"
"etc" is None  # => False
None is None  # => True

# Operator 'is' testuje identyczność obiektów. Nie jest to zbyt 
# pożyteczne, gdy działamy tylko na prostych wartościach,
# ale przydaje się, gdy mamy do czynienia z obiektami.

# None, 0 i pusty napis "" są odpowiednikami logicznego False.
# Wszystkie inne wartości są uznawane za prawdę (True)
bool(0)  # => False
bool("")  # => False


####################################################
## 2. Zmienne i zbiory danych
####################################################

# Python ma instrukcję wypisującą "print" we wszystkich wersjach 2.x, ale
# została ona usunięta z wersji 3.
print "Jestem Python. Miło Cię poznać!"
# Python ma też funkcję "print" dostępną w wersjach 2.7 i 3...
# ale w 2.7 musisz dodać import (odkomentuj):
# from __future__ import print_function
print("Ja też jestem Python! ")

# Nie trzeba deklarować zmiennych przed przypisaniem.
jakas_zmienna = 5    # Konwencja mówi: używaj małych liter i znaków podkreślenia _
jakas_zmienna  # => 5

# Próba dostępu do niezadeklarowanej zmiennej da błąd.
# Przejdź do sekcji Obsługa wyjątków, aby dowiedzieć się więcej...
inna_zmienna  # Wyrzuca nazwę błędu

# "if" może być użyte jako wyrażenie
"huraaa!" if 3 > 2 else 2  # => "huraaa!"

# Listy:
li = []
# Możesz zacząć od wypełnionej listy
inna_li = [4, 5, 6]

# Dodaj na koniec, używając "append"
li.append(1)    # li to teraz [1]
li.append(2)    # li to teraz [1, 2]
li.append(4)    # li to teraz [1, 2, 4]
li.append(3)    # li to teraz [1, 2, 4, 3]
# Usuwanie z konca da "pop"
li.pop()        # => 3 a li stanie się [1, 2, 4]
# Dodajmy ponownie
li.append(3)    # li to znowu [1, 2, 4, 3].

# Dostęp do list jak do każdej tablicy
li[0]  # => 1
# Aby nadpisać wcześniej wypełnione miejsca w liście, użyj znaku =
li[0] = 42
li[0]  # => 42
li[0] = 1  # Uwaga: ustawiamy starą wartość
# Tak podglądamy ostatni element
li[-1]  # => 3

# Jeżeli wyjdziesz poza zakres...
li[4]  # ... zobaczysz IndexError

# Możesz też tworzyć wycinki.
li[1:3]  # => [2, 4]
# Bez początku
li[2:]  # => [4, 3]
# Omijamy koniec
li[:3]  # => [1, 2, 4]
# Wybierz co drugi
li[::2]   # =>[1, 4]
# Odwróć listę
li[::-1]   # => [3, 4, 2, 1]
# Użyj kombinacji powyższych aby tworzyć bardziej skomplikowane wycinki
# li[poczatek:koniec:krok]

# Usuń element używając "del"
del li[2]   # li to teraz [1, 2, 3]

# Listy można dodawać
li + inna_li   # => [1, 2, 3, 4, 5, 6]
# Uwaga: wartości oryginalnych list li i inna_li się nie zmieniają.

# Do łączenia list użyj "extend()"
li.extend(other_li)   # li to teraz [1, 2, 3, 4, 5, 6]

# Sprawdź, czy element jest w liście używając "in"
1 in li   # => True

# "len()" pokazuje długość listy
len(li)   # => 6


# Krotki (tuple) są jak listy, ale nie można ich modyfikować.
tup = (1, 2, 3)
tup[0]   # => 1
tup[0] = 3  # wyrzuci TypeError

# Ale wielu akcji dla list możesz używać przy krotkach
len(tup)   # => 3
tup + (4, 5, 6)   # => (1, 2, 3, 4, 5, 6)
tup[:2]   # => (1, 2)
2 in tup   # => True

# Można rozpakować krotki i listy do poszczególych zmiennych
a, b, c = (1, 2, 3)     # a to teraz 1, b jest 2, a c to 3
# Jeżeli zapomnisz nawiasów, automatycznie tworzone są krotki
d, e, f = 4, 5, 6
# Popatrz jak prosto zamienić wartości
e, d = d, e     # d to teraz 5 a e to 4


# Słowniki są również pożyteczne
pusty_slownik = {}
# Tu tworzymy wypełniony:
pelen_slownik = {"raz": 1, "dwa": 2, "trzy": 3}

# Podglądany wartość
pelen_slownik["one"]   # => 1

# Wypisz wszystkie klucze, używając "keys()"
pelen_slownik.keys()   # => ["trzy", "dwa", "raz"]
# Uwaga: słowniki nie zapamiętują kolejności kluczy.

# A teraz wszystkie wartości "values()"
pelen_slownik.values()   # => [3, 2, 1]
# Uwaga: to samo dotyczy wartości.

# Sprawdzanie czy klucz występuje w słowniku za pomocą "in"
"raz" in pelen_slownik   # => True
1 in pelen_slownik   # => False

# Próba dobrania się do nieistniejącego klucza da KeyError
pelen_slownik["cztery"]   # KeyError

# Użyj metody "get()", aby uniknąć błędu KeyError
pelen_slownik.get("raz")   # => 1
pelen_slownik.get("cztery")   # => None
# Metoda get zwraca domyślną wartość gdy brakuje klucza
pelen_slownik.get("one", 4)   # => 1
pelen_slownik.get("cztery", 4)   # => 4
# zauważ, że pelen_slownik.get("cztery") wciąż zwraca => None
# (get nie ustawia wartości słownika)

# przypisz wartość do klucza podobnie jak w listach
pelen_slownik["cztery"] = 4  # teraz: pelen_slownik["cztery"] => 4

# "setdefault()" wstawia do słownika tylko jeśli nie było klucza
pelen_slownik.setdefault("piec", 5)  # pelen_slownik["piec"] daje 5
pelen_slownik.setdefault("piec", 6)  # pelen_slownik["piec"] to wciąż 5


# Teraz zbiory (set) - działają jak zwykłe listy, ale bez potórzeń
pusty_zbior = set()
# Inicjalizujemy "set()" pewnymi wartościami
jakis_zbior = set([1, 2, 2, 3, 4])   # jakis_zbior to teraz set([1, 2, 3, 4])

# kolejność nie jest zachowana, nawet gdy wydaje się posortowane
inny_zbior = set([4, 3, 2, 2, 1])  # inny_zbior to set([1, 2, 3, 4])

# Od Pythona 2.7 nawiasy klamrowe {} mogą być użyte do deklarowania zbioru
pelen_zbior = {1, 2, 2, 3, 4}   # => {1, 2, 3, 4}

# Dodaj więcej elementów przez "add()"
pelen_zbior.add(5)   # pelen_zbior is now {1, 2, 3, 4, 5}

# Znajdź przecięcie (część wspólną) zbiorów, używając &
inny_zbior = {3, 4, 5, 6}
pelen_zbior & other_set   # => {3, 4, 5}

# Suma zbiorów |  
pelen_zbior | other_set   # => {1, 2, 3, 4, 5, 6}

# Różnicę zbiorów da znak -
{1, 2, 3, 4} - {2, 3, 5}   # => {1, 4}

# Sprawdzanie obecności w zbiorze: "in".
2 in pelen_zbior   # => True
10 in pelen_zbior   # => False


####################################################
## 3. Kontrola przepływu
####################################################

# Tworzymy zmienną jakas_zm
jakas_zm = 5

# Tutaj widzisz wyrażenie warunkowe "if". Wcięcia w Pythonie są ważne!
# Poniższy kod wypisze "jakas_zm jest mniejsza niż 10"
if jakas_zm > 10:
    print("jakas_zm jest wieksza niż 10")
elif some_var < 10:    # Opcjonalna klauzula elif
    print("jakas_zm jest mniejsza niż 10")
else:           # Również opcjonalna klauzula else
    print("jakas_zm jest równa 10")


"""
Pętla for iteruje po elementach listy, wypisując:
    pies to ssak
    kot to ssak
    mysz to ssak
"""
for zwierze in ["pies", "kot", "mysz"]:
    # Użyj metody format, aby umieścić wartość zmiennej w ciągu
    print("{0} to ssak".format(zwierze))

"""
"range(liczba)" zwraca listę liczb
z przedziału od zera do wskazanej liczby (bez niej):
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
While to pętla, która jest wykonywana, dopóki spełniony jest warunek:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # Skrót od x = x + 1

# Wyjątki wyłapujemy, używając try i except

# Działa w Pythonie 2.6 i wyższych:
try:
    # Użyj "raise" aby wyrzucić wyjątek
    raise IndexError("To błąd indeksu")
except IndexError as e:
    pass    # Pass to brak reakcji na błąd. Zwykle opisujesz tutaj, jak program ma się zachować w przypadku błędu.
except (TypeError, NameError):
    pass    # kilka wyjątków można przechwycić jednocześnie.
else:   # Opcjonalna część bloku try/except. Musi wystąpić na końcu
    print "Wszystko ok!"   # Zadziała tylko, gdy program nie napotka wyjatku.


####################################################
## 4. Funkcje
####################################################

# Użyj "def", aby stworzyć nową funkcję
def dodaj(x, y):
    print("x to %s, a y to %s" % (x, y))
    return x + y    # słowo kluczowe return zwraca wynik działania

# Tak wywołuje się funkcję z parametrami:
dodaj(5, 6)   # => wypisze "x to 5, a y to 6" i zwróci 11

# Innym sposobem jest wywołanie z parametrami nazwanymi.
dodaj(y=6, x=5)   # tutaj kolejność podania nie ma znaczenia.


# Można też stworzyć funkcję, które przyjmują zmienną liczbę parametrów pozycyjnych,
# które zostaną przekazana jako krotka, pisząc w definicji funkcji "*args"
def varargs(*args):
    return args

varargs(1, 2, 3)   # => (1, 2, 3)


# Można też stworzyć funkcję, które przyjmują zmienną liczbę parametrów
# nazwanych kwargs, które zostaną przekazane jako słownik, pisząc w definicji funkcji "**kwargs"
def keyword_args(**kwargs):
    return kwargs

# Wywołajmy to i sprawdźmy co się dzieje
keyword_args(wielka="stopa", loch="ness")   # => {"wielka": "stopa", "loch": "ness"}


# Możesz też przyjmować jednocześnie zmienną liczbę parametrów pozycyjnych i nazwanych
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) wypisze:
    (1, 2)
    {"a": 3, "b": 4}
"""

# Użyj * aby rozwinąć parametry z krotki args
# i użyj ** aby rozwinąć parametry nazwane ze słownika kwargs.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)   # odpowiednik foo(1, 2, 3, 4)
all_the_args(**kwargs)   # odpowiednik foo(a=3, b=4)
all_the_args(*args, **kwargs)   # odpowiednik foo(1, 2, 3, 4, a=3, b=4)

# Możesz podać parametry args i kwargs do funkcji równocześnie
# przez rozwinięcie odpowiednio * i **
def pass_all_the_args(*args, **kwargs):
    all_the_args(*args, **kwargs)
    print varargs(*args)
    print keyword_args(**kwargs)

# Zasięg zmiennych
x = 5

def setX(num):
    # Lokalna zmienna x nie jest tym samym co zmienna x
    x = num # => 43
    print x # => 43
    
def setGlobalX(num):
    global x
    print x # => 5
    x = num # globalna zmienna to teraz 6
    print x # => 6

setX(43)
setGlobalX(6)

# Można tworzyć funkcje wewnętrzne i zwrócić je jako wynik
def rob_dodawacz(x):
    def dodawacz(y):
        return x + y
    return dodawacz

dodaj_10 = rob_dodawacz(10)
dodaj_10(3)   # => 13

# Są również funkcje anonimowe "lambda"
(lambda x: x > 2)(3)   # => True

# Python ma też wbudowane funkcje wyższego rzędu (przyjmujące inną funkcje jako parametr)
map(add_10, [1, 2, 3])   # => [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7])   # => [6, 7]

# Można używać wyrażeń listowych (list comprehensions) do mapowania i filtrowania
[add_10(i) for i in [1, 2, 3]]  # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]   # => [6, 7]


####################################################
## 5. Klasy
####################################################

# Wszystkie klasy są podklasą object
class Czlowiek(object):

    # Atrybut klasy. Występuje we wszystkich instancjach klasy.
    gatunek = "H. sapiens"
    
    # Podstawowa inicjalizacja - wywoływana podczas tworzenia instacji.
    # Zauważ, że podwójne podkreślenia przed i za nazwą oznaczają
    # specjalne obiekty lub atrybuty wykorzystywane wewnętrznie przez Pythona.
    # Nie używaj ich we własnych metodach.
    def __init__(self, nazwa):
        # przypisz parametr "nazwa" do atrybutu instancji
        self.nazwa = nazwa

    # Metoda instancji. Wszystkie metody przyjmują "self" jako pierwszy argument
    def mow(self, wiadomosc):
        return "%s: %s" % (self.nazwa, wiadomosc)

    # Metoda klasowa współdzielona przez instancje.
    # Przyjmuje wywołującą klasę jako pierwszy argument.
    @classmethod
    def daj_gatunek(cls):
        return cls.gatunek

    # Metoda statyczna jest wywoływana bez argumentów klasy czy instancji.
    @staticmethod
    def grunt():
        return "*grunt*"


# Instancja klasy
i = Czlowiek(name="Ian")
print(i.mow("cześć"))     # wypisze "Ian: cześć"

j = Czlowiek("Joel")
print(j.mow("cześć"))  # wypisze "Joel: cześć"

# Wywołujemy naszą metodę klasową
i.daj_gatunek()   # => "H. sapiens"

# Zmieniamy wspólny parametr
Czlowiek.gatunek = "H. neanderthalensis"
i.daj_gatunek()   # => "H. neanderthalensis"
j.daj_gatunek()   # => "H. neanderthalensis"

# Wywołanie metody statycznej
Czlowiek.grunt()   # => "*grunt*"


####################################################
## 6. Moduły
####################################################

# Tak importuje się moduły:
import math
print(math.sqrt(16))  # => 4.0

# Można podać konkretne funkcje, np. ceil, floor z modułu math
from math import ceil, floor
print(ceil(3.7))  # => 4.0
print(floor(3.7))   # => 3.0

# Można zaimportować wszystkie funkcje z danego modułu.
# Uwaga: nie jest to polecane, bo później w kodzie trudno połapać się,
# która funkcja pochodzi z którego modułu.
from math import *

# Można skracać nazwy modułów.
import math as m
math.sqrt(16) == m.sqrt(16)   # => True
# sprawdźmy czy funkcje są równoważne
from math import sqrt
math.sqrt == m.sqrt == sqrt  # => True

# Moduły Pythona to zwykłe skrypty napisane w tym języku. Możesz
# pisać własne i importować je. Nazwa modułu to nazwa pliku.

# W ten sposób sprawdzisz jakie funkcje wchodzą w skład modułu.
import math
dir(math)


####################################################
## 7. Zaawansowane
####################################################

# Generatory pomagają tworzyć tzw. "leniwy kod"
def podwojne_liczby(iterowalne):
    for i in iterowalne:
        yield i + i

# Generatory tworzą wartości w locie.
# Zamiast generować wartości raz i zapisywać je (np. w liście), 
# generator tworzy je na bieżąco, w wyniku iteracji. To oznacza,
# że w poniższym przykładzie wartości większe niż 15 nie będą przetworzone 
# w funkcji "podwojne_liczby".
# Zauważ, że xrange to generator, który wykonuje tę samą operację co range.
# Stworzenie listy od 1 do 900000000 zajęłoby sporo czasu i pamięci,
# a xrange tworzy obiekt generatora zamiast budować całą listę jak range.

# Aby odróżnić nazwę zmiennej od nazwy zarezerwowanej w Pythonie, używamy
# zwykle na końcu znaku podkreślenia
xrange_ = xrange(1, 900000000)

# poniższa pętla będzie podwajać liczby aż do 30 
for i in podwojne_liczby(xrange_):
    print(i)
    if i >= 30:
        break


# Dekoratory
# w tym przykładzie "beg" jest nakładką na "say"
# Beg wywołuje say. Jeśli say_please jest prawdziwe, wtedy zwracana wartość
# zostanie zmieniona

from functools import wraps


def beg(target_function):
    @wraps(target_function)
    def wrapper(*args, **kwargs):
        msg, say_please = target_function(*args, **kwargs)
        if say_please:
            return "{} {}".format(msg, "Proszę! Jestem spłukany :(")
        return msg
    return wrapper


@beg
def say(say_please=False):
    msg = "Kupisz mi piwo?"
    return msg, say_please


print(say())  # Kupisz mi piwo?
print(say(say_please=True))  # Kupisz mi piwo? Proszę! Jestem spłukany :(
```

## Gotowy na więcej?
### Polskie

* [Zanurkuj w Pythonie](http://pl.wikibooks.org/wiki/Zanurkuj_w_Pythonie)
* [LearnPythonPl](http://www.learnpython.org/pl/)

### Angielskie:
#### Darmowe źródła online

* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [The Official Docs](http://docs.python.org/2.6/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Module of the Week](http://pymotw.com/2/)
* [A Crash Course in Python for Scientists](http://nbviewer.ipython.org/5920182)

#### Inne

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)

