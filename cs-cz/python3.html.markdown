---
language: python3
contributors:
    - ["Louie Dinh", "http://pythonpracticeprojects.com"]
    - ["Steven Basart", "http://github.com/xksteven"]
    - ["Andre Polykanine", "https://github.com/Oire"]
translators:
    - ["Tomáš Bedřich", "http://tbedrich.cz"]
filename: learnpython3.py
---

Python byl vytvořen Guidem Van Rossum v raných 90. letech. Nyní je jedním z nejpopulárnějších jazyků.
Zamiloval jsem si Python pro jeho syntaktickou čistotu - je to vlastně spustitelný pseudokód.

Vaše zpětná vazba je vítána! Můžete mě zastihnout na [@louiedinh](http://twitter.com/louiedinh) nebo louiedinh [at] [email od googlu] (anglicky).

Poznámka: Tento článek je zaměřen na Python 3. Zde se můžete [naučit starší Python 2.7](http://learnxinyminutes.com/docs/python/).

```python

# Jednořádkový komentář začíná křížkem

""" Víceřádkové komentáře používají 3x"
    a jsou často využívány jako dokumentační komentáře k metodám
"""

####################################################
## 1. Primitivní datové typy a operátory
####################################################

# Čísla
3  # => 3

# Aritmetické operace se chovají běžným způsobem
1 + 1   # =>  2
8 - 1   # =>  7
10 * 2  # => 20

# Až na dělení, které vrací desetinné číslo
35 / 5  # => 7.0

# Při celočíselném dělení je desetinná část oříznuta (pro kladná i záporná čísla)
5 // 3       # => 1
5.0 // 3.0   # => 1.0  #  celočíselně dělit lze i desetinným číslem
-5 // 3      # => -2
-5.0 // 3.0  # => -2.0

# Pokud použiteje desetinné číslo, výsledek je jím také
3 * 2.0  # => 6.0

# Modulo
7 % 3  # => 1

# Mocnění (x na y-tou)
2**4  # => 16

# Pro vynucení priority použijte závorky
(1 + 3) * 2  # => 8

# Logické hodnoty
True
False

# Negace se provádí pomocí not
not True   # => False
not False  # => True

# Logické operátory
# U operátorů záleží na velikosti písmen
True and False  # => False
False or True   # => True

# Používání logických operátorů s čísly
0 and 2     # => 0
-5 or 0     # => -5
0 == False  # => True
2 == True   # => False
1 == True   # => True

# Rovnost je ==
1 == 1  # => True
2 == 1  # => False

# Nerovnost je !=
1 != 1  # => False
2 != 1  # => True

# Další porovnání
1 < 10  # => True
1 > 10  # => False
2 <= 2  # => True
2 >= 2  # => True

# Porovnání se dají řetězit!
1 < 2 < 3  # => True
2 < 3 < 2  # => False


# Řetězce používají " nebo ' a mohou obsahovat UTF8 znaky
"Toto je řetězec."
'Toto je také řetězec.'

# Řetězce se také dají sčítat, ale nepoužívejte to
"Hello " + "world!"  # => "Hello world!"
# Dají se spojovat i bez '+'
"Hello " "world!"  # => "Hello world!"

# Řetězec lze považovat za seznam znaků
"Toto je řetězec"[0]  # => 'T'

# .format lze použít ke skládání řetězců
"{} mohou být {}".format("řetězce", "skládány")

# Formátovací argumenty můžete opakovat
"{0} {1} stříkaček stříkalo přes {0} {1} střech".format("tři sta třicet tři", "stříbrných")
# => "tři sta třicet tři stříbrných stříkaček stříkalo přes tři sta třicet tři stříbrných střech"

# Pokud nechcete počítat, můžete použít pojmenované argumenty
"{jmeno} si dal {jidlo}".format(jmeno="Franta", jidlo="guláš")  # => "Franta si dal guláš"

# Pokud zároveň potřebujete podporovat Python 2.5 a nižší, můžete použít starší způsob formátování
"%s se dají %s jako v %s" % ("řetězce", "skládat", "jazyce C")


# None je objekt (jinde NULL, nil, ...)
None  # => None

# Pokud porovnáváte něco s None, nepoužívejte operátor rovnosti "==",
# použijte raději operátor "is", který testuje identitu.
"něco" is None  # => False
None is None    # => True

# None, 0, a prázdný řetězec/seznam/slovník se vyhodnotí jako False
# Vše ostatní se vyhodnotí jako True
bool(0)   # => False
bool("")  # => False
bool([])  # => False
bool({})  # => False


####################################################
## 2. Proměnné a kolekce
####################################################

# Python má funkci print
print("Jsem 3. Python 3.")

# Proměnné není třeba deklarovat před přiřazením
# Konvence je používat male_pismo_s_podtrzitky
nazev_promenne = 5
nazev_promenne  # => 5

# Přístup k předtím nepoužité proměnné vyvolá výjimku
# Odchytávání vyjímek - viz další kapitola
neznama_promenna  # Vyhodí NameError

# Seznam se používá pro ukládání sekvencí
sez = []
# Lze ho rovnou naplnit
jiny_seznam = [4, 5, 6]

# Na konec seznamu se přidává pomocí append
sez.append(1)    # sez je nyní [1]
sez.append(2)    # sez je nyní [1, 2]
sez.append(4)    # sez je nyní [1, 2, 4]
sez.append(3)    # sez je nyní [1, 2, 4, 3]
# Z konce se odebírá se pomocí pop
sez.pop()        # => 3 a sez je nyní [1, 2, 4]
# Vložme trojku zpátky
sez.append(3)    # sez je nyní znovu [1, 2, 4, 3]

# Přístup k prvkům funguje jako v poli
sez[0]  # => 1
# Mínus počítá odzadu (-1 je poslední prvek)
sez[-1]  # => 3

# Přístup mimo seznam vyhodí IndexError
sez[4]  # Vyhodí IndexError

# Pomocí řezů lze ze seznamu vybírat různé intervaly
# (pro matematiky: jedná se o uzavřený/otevřený interval)
sez[1:3]  # => [2, 4]
# Odříznutí začátku
sez[2:]  # => [4, 3]
# Odříznutí konce
sez[:3]  # => [1, 2, 4]
# Vybrání každého druhého prvku
sez[::2]   # =>[1, 4]
# Vrácení seznamu v opačném pořadí
sez[::-1]   # => [3, 4, 2, 1]
# Lze použít jakoukoliv kombinaci parametrů pro vytvoření složitějšího řezu
# sez[zacatek:konec:krok]

# Odebírat prvky ze seznamu lze pomocí del
del sez[2]   # sez je nyní [1, 2, 3]

# Seznamy můžete sčítat
# Hodnoty sez a jiny_seznam přitom nejsou změněny
sez + jiny_seznam   # => [1, 2, 3, 4, 5, 6]

# Spojit seznamy lze pomocí extend
sez.extend(jiny_seznam)   # sez je nyní [1, 2, 3, 4, 5, 6]

# Kontrola, jestli prvek v seznamu existuje, se provádí pomocí in
1 in sez   # => True

# Délku seznamu lze zjistit pomocí len
len(sez)   # => 6


# N-tice je jako seznam, ale je neměnná
ntice = (1, 2, 3)
ntice[0]      # => 1
ntice[0] = 3  # Vyhodí TypeError

# S n-ticemi lze dělat většinu operací, jako se seznamy
len(ntice)         # => 3
ntice + (4, 5, 6)  # => (1, 2, 3, 4, 5, 6)
ntice[:2]          # => (1, 2)
2 in ntice         # => True

# N-tice (nebo seznamy) lze rozbalit do proměnných jedním přiřazením
a, b, c = (1, 2, 3)     # a je nyní 1, b je nyní 2 a c je nyní 3
# N-tice jsou vytvářeny automaticky, když vynecháte závorky
d, e, f = 4, 5, 6
# Prohození proměnných je tak velmi snadné
e, d = d, e     # d je nyní 5, e je nyní 4


# Slovníky ukládají klíče a hodnoty
prazdny_slovnik = {}
# Lze je také rovnou naplnit
slovnik = {"jedna": 1, "dva": 2, "tři": 3}

# Přistupovat k hodnotám lze pomocí []
slovnik["jedna"] # => 1

# Všechny klíče dostaneme pomocí keys() jako iterátor. Nyní ještě potřebujeme
# obalit volání v list(), abychom dostali seznam. To rozebereme později.
# Pozor, že jakékoliv pořadí klíčů není garantováno - může být různé.
list(slovnik.keys())  # => ["dva", "jedna", "tři"]

# Všechny hodnoty opět jako iterátor získáme pomocí values(). Opět tedy
# potřebujeme použít list(), abychom dostali seznam. Stejně jako
# v předchozím případě, pořadí není garantováno a může být různé
list(slovnik.values())  # => [3, 2, 1]

# Operátorem in se lze dotázat na přítomnost klíče
"jedna" in slovnik  # => True
1 in slovnik        # => False

# Přístup k neexistujícímu klíči vyhodí KeyError
slovnik["four"]  # Vyhodí KeyError

# Metoda get() funguje podobně jako [], ale vrátí None místo vyhození KeyError
slovnik.get("jedna")   # => 1
slovnik.get("čtyři")   # => None
# Metodě get() lze předat i výchozí hodnotu místo None
slovnik.get("jedna", 4)   # => 1
slovnik.get("čtyři", 4)   # => 4

# metoda setdefault() vloží prvek do slovníku pouze pokud tam takový klíč není
slovnik.setdefault("pět", 5)  # slovnik["pět"] je nastaven na 5
slovnik.setdefault("pět", 6)  # slovnik["pět"] je pořád 5

# Přidání nové hodnoty do slovníku
slovnik["čtyři"] = 4
# Hromadně aktualizovat nebo přidat data lze pomocí update(), parametrem je opět slovník
slovnik.update({"čtyři": 4})  # slovnik je nyní {"jedna": 1, "dva": 2, "tři": 3, "čtyři": 4, "pět": 5}

# Odebírat ze slovníku dle klíče lze pomocí del
del slovnik["jedna"]  # odebere klíč "jedna" ze slovnik


# Množiny ukládají ... překvapivě množiny
prazdna_mnozina = set()
# Také je lze rovnou naplnit. A ano, budou se vám plést se slovníky. Bohužel.
mnozina = {1, 1, 2, 2, 3, 4}   # mnozina je nyní {1, 2, 3, 4}

# Přidání položky do množiny
mnozina.add(5)   # mnozina je nyní {1, 2, 3, 4, 5}

# Průnik lze udělat pomocí operátoru &
jina_mnozina = {3, 4, 5, 6}
mnozina & jina_mnozina   # => {3, 4, 5}

# Sjednocení pomocí operátoru |
mnozina | jina_mnozina   # => {1, 2, 3, 4, 5, 6}

# Rozdíl pomocí operátoru -
{1, 2, 3, 4} - {2, 3, 5}   # => {1, 4}

# Operátorem in se lze dotázat na přítomnost prvku v množině
2 in mnozina   # => True
10 in mnozina   # => False


####################################################
## 3. Řízení toku a iterace
####################################################

# Let's just make a variable
some_var = 5

# Here is an if statement. Indentation is significant in python!
# prints "some_var is smaller than 10"
if some_var > 10:
    print("some_var is totally bigger than 10.")
elif some_var < 10:    # This elif clause is optional.
    print("some_var is smaller than 10.")
else:                  # This is optional too.
    print("some_var is indeed 10.")


"""
For loops iterate over lists
prints:
    dog is a mammal
    cat is a mammal
    mouse is a mammal
"""
for animal in ["dog", "cat", "mouse"]:
    # You can use format() to interpolate formatted strings
    print("{} is a mammal".format(animal))

"""
"range(number)" returns an iterable of numbers
from zero to the given number
prints:
    0
    1
    2
    3
"""
for i in range(4):
    print(i)

"""
"range(lower, upper)" returns an iterable of numbers
from the lower number to the upper number
prints:
    4
    5
    6
    7
"""
for i in range(4, 8):
    print(i)

"""
While loops go until a condition is no longer met.
prints:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print(x)
    x += 1  # Shorthand for x = x + 1

# Handle exceptions with a try/except block
try:
    # Use "raise" to raise an error
    raise IndexError("This is an index error")
except IndexError as e:
    pass    # Pass is just a no-op. Usually you would do recovery here.
except (TypeError, NameError):
    pass    # Multiple exceptions can be handled together, if required.
else:   # Optional clause to the try/except block. Must follow all except blocks
    print("All good!")   # Runs only if the code in try raises no exceptions
finally:  #   Execute under all circumstances
    print("We can clean up resources here")
 		 
# Instead of try/finally to cleanup resources you can use a with statement
with open("myfile.txt") as f:
    for line in f:
        print(line)

# Python offers a fundamental abstraction called the Iterable.
# An iterable is an object that can be treated as a sequence.
# The object returned the range function, is an iterable.

slovnik = {"one": 1, "two": 2, "three": 3}
our_iterable = slovnik.keys()
print(our_iterable)  # => range(1,10). This is an object that implements our Iterable interface

# We can loop over it.
for i in our_iterable:
    print(i)    # Prints one, two, three

# However we cannot address elements by index.
our_iterable[1]  # Raises a TypeError

# An iterable is an object that knows how to create an iterator.
our_iterator = iter(our_iterable)

# Our iterator is an object that can remember the state as we traverse through it.
# We get the next object with "next()".
next(our_iterator)  # => "one"

# It maintains state as we iterate.
next(our_iterator)  # => "two"
next(our_iterator)  # => "three"

# After the iterator has returned all of its data, it gives you a StopIterator Exception
next(our_iterator)  #  Raises StopIteration

# You can grab all the elements of an iterator by calling list() on it.
list(slovnik.keys())  # => Returns ["one", "two", "three"]


####################################################
## 4. Functions
####################################################

# Use "def" to create new functions
def add(x, y):
    print("x is {} and y is {}".format(x, y))
    return x + y    # Return values with a return statement

# Calling functions with parameters
add(5, 6)   # => prints out "x is 5 and y is 6" and returns 11

# Another way to call functions is with keyword arguments
add(y=6, x=5)   # Keyword arguments can arrive in any order.

# You can define functions that take a variable number of
# positional arguments
def varargs(*args):
    return args

varargs(1, 2, 3)   # => (1, 2, 3)

# You can define functions that take a variable number of
# keyword arguments, as well
def keyword_args(**kwargs):
    return kwargs

# Let's call it to see what happens
keyword_args(big="foot", loch="ness")   # => {"big": "foot", "loch": "ness"}


# You can do both at once, if you like
def all_the_args(*args, **kwargs):
    print(args)
    print(kwargs)
"""
all_the_args(1, 2, a=3, b=4) prints:
    (1, 2)
    {"a": 3, "b": 4}
"""

# When calling functions, you can do the opposite of args/kwargs!
# Use * to expand tuples and use ** to expand kwargs.
args = (1, 2, 3, 4)
kwargs = {"a": 3, "b": 4}
all_the_args(*args)   # equivalent to foo(1, 2, 3, 4)
all_the_args(**kwargs)   # equivalent to foo(a=3, b=4)
all_the_args(*args, **kwargs)   # equivalent to foo(1, 2, 3, 4, a=3, b=4)


# Function Scope
x = 5

def setX(num):
    # Local var x not the same as global variable x
    x = num  # => 43
    print (x)  # => 43

def setGlobalX(num):
    global x
    print (x)  # => 5
    x = num  #  global var x is now set to 6
    print (x)  # => 6

setX(43)
setGlobalX(6)


# Python has first class functions
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3)   # => 13

# There are also anonymous functions
(lambda x: x > 2)(3)   # => True

# TODO - Fix for iterables
# There are built-in higher order functions
map(add_10, [1, 2, 3])   # => [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7])   # => [6, 7]

# We can use list comprehensions for nice maps and filters
# List comprehension stores the output as a list which can itself be a nested list
[add_10(i) for i in [1, 2, 3]]  # => [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5]   # => [6, 7]

####################################################
## 5. Classes
####################################################


# We subclass from object to get a class.
class Human(object):

    # A class attribute. It is shared by all instances of this class
    species = "H. sapiens"

    # Basic initializer, this is called when this class is instantiated.
    # Note that the double leading and trailing underscores denote objects
    # or attributes that are used by python but that live in user-controlled
    # namespaces. Methods(or objects or attributes) like: __init__, __str__,
    # __repr__ etc. are called magic methods (or sometimes called dunder methods)
    # You should not invent such names on your own.
    def __init__(self, name):
        # Assign the argument to the instance's name attribute
        self.name = name

    # An instance method. All methods take "self" as the first argument
    def say(self, msg):
        return "{name}: {message}".format(name=self.name, message=msg)

    # A class method is shared among all instances
    # They are called with the calling class as the first argument
    @classmethod
    def get_species(cls):
        return cls.species

    # A static method is called without a class or instance reference
    @staticmethod
    def grunt():
        return "*grunt*"


# Instantiate a class
i = Human(name="Ian")
print(i.say("hi"))     # prints out "Ian: hi"

j = Human("Joel")
print(j.say("hello"))  # prints out "Joel: hello"

# Call our class method
i.get_species()   # => "H. sapiens"

# Change the shared attribute
Human.species = "H. neanderthalensis"
i.get_species()   # => "H. neanderthalensis"
j.get_species()   # => "H. neanderthalensis"

# Call the static method
Human.grunt()   # => "*grunt*"


####################################################
## 6. Modules
####################################################

# You can import modules
import math
print(math.sqrt(16))  # => 4

# You can get specific functions from a module
from math import ceil, floor
print(ceil(3.7))  # => 4.0
print(floor(3.7))   # => 3.0

# You can import all functions from a module.
# Warning: this is not recommended
from math import *

# You can shorten module names
import math as m
math.sqrt(16) == m.sqrt(16)   # => True

# Python modules are just ordinary python files. You
# can write your own, and import them. The name of the
# module is the same as the name of the file.

# You can find out which functions and attributes
# defines a module.
import math
dir(math)


####################################################
## 7. Advanced
####################################################

# Generators help you make lazy code
def double_numbers(iterable):
    for i in iterable:
        yield i + i

# A generator creates values on the fly.
# Instead of generating and returning all values at once it creates one in each
# iteration.  This means values bigger than 15 wont be processed in
# double_numbers.
# Note range is a generator too. Creating a list 1-900000000 would take lot of
# time to be made
# We use a trailing underscore in variable names when we want to use a name that
# would normally collide with a python keyword
range_ = range(1, 900000000)
# will double all numbers until a result >=30 found
for i in double_numbers(range_):
    print(i)
    if i >= 30:
        break


# Decorators
# in this example beg wraps say
# Beg will call say. If say_please is True then it will change the returned
# message
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

## Ready For More?

### Free Online

* [Automate the Boring Stuff with Python](https://automatetheboringstuff.com)
* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [Ideas for Python Projects](http://pythonpracticeprojects.com)
* [The Official Docs](http://docs.python.org/3/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [A Crash Course in Python for Scientists](http://nbviewer.ipython.org/5920182)
* [Python Course](http://www.python-course.eu/index.php)
* [First Steps With Python](https://realpython.com/learn/python-first-steps/)

### Dead Tree

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)
