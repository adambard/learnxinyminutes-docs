---
language: Python 2 (legacy)
filename: learnpythonlegacy-fr.py
contributors:
  - ["Louie Dinh", "http://ldinh.ca"]
translators:
  - ["Sylvain Zyssman", "https://github.com/sylzys"]
  - ["Nami-Doc", "https://github.com/Nami-Doc"]
lang: fr-fr
---

Python a été créé par Guido Van Rossum au début des années 90. C'est maintenant un des langages de programmation les plus populaires.
Je suis tombé amoureux de Python de par la clarté de sa syntaxe. C'est pratiquement du pseudo-code exécutable.

Vos retours sont grandement appréciés. Vous pouvez me contacter sur Twitter [@louiedinh](http://twitter.com/louiedinh) ou par e-mail: louiedinh [at] [google's email service]

N.B. : Cet article s'applique spécifiquement à Python 2.7, mais devrait s'appliquer pour toute version Python 2.x. Python 2.7 est en fin de vie et ne sera plus maintenu à partir de 2020, il est donc recommandé d'apprendre Python avec Python 3. Pour Python 3.x, il existe un autre [tutoriel pour Python 3](http://learnxinyminutes.com/docs/fr-fr/python3-fr/).

```python
# Une ligne simple de commentaire commence par un dièse
""" Les lignes de commentaires multipes peuvent être écrites
    en utilisant 3 guillemets ("), et sont souvent utilisées
    pour les commentaires
"""

####################################################
## 1. Types Primaires et Opérateurs
####################################################

# Les nombres
3 #=> 3

# Les calculs produisent les résultats mathématiques escomptés
1 + 1 #=> 2
8 - 1 #=> 7
10 * 2 #=> 20
35 / 5 #=> 7

# La division est un peu spéciale. C'est une division d'entiers, et Python arrondi le résultat par défaut automatiquement.
5 / 2 #=> 2

# Pour corriger ce problème, on utilise les float.
2.0     # Voici un float
11.0 / 4.0 #=> 2.75 ahhh... beaucoup mieux

# Forcer la priorité avec les parenthèses
(1 + 3) * 2 #=> 8

# Les valeurs booléenes sont de type primitif
True
False

# Pour la négation, on utilise "not"
not True #=> False
not False #=> True

# Pour l'égalité, ==
1 == 1 #=> True
2 == 1 #=> False

# L'inégalité est symbolisée par !=
1 != 1 #=> False
2 != 1 #=> True

# D'autres comparateurs
1 < 10 #=> True
1 > 10 #=> False
2 <= 2 #=> True
2 >= 2 #=> True

# On peut enchaîner les comparateurs !
1 < 2 < 3 #=> True
2 < 3 < 2 #=> False

# Les chaînes de caractères sont créées avec " ou '
"C'est une chaîne."
'C\'est aussi une chaîne.'

# On peut aussi les "additioner" !
"Hello " + "world!" #=> "Hello world!"

# Une chaîne peut être traitée comme une liste de caractères
"C'est une chaîne"[0] #=> 'C'

# % peut être utilisé pour formatter des chaîne, comme ceci:
"%s can be %s" % ("strings", "interpolated")

# Une autre manière de formatter les chaînes de caractères est d'utiliser la méthode 'format'
# C'est la méthode à privilégier
"{0} peut être {1}".format("La chaîne", "formattée")
# On peut utiliser des mot-clés au lieu des chiffres.
"{name} veut manger des {food}".format(name="Bob", food="lasagnes")

# None est un objet
None #=> None

# Ne pas utiliser le symbole d'inégalité "==" pour comparer des objet à None
# Il faut utiliser "is"
"etc" is None #=> False
None is None  #=> True

# L'opérateur 'is' teste l'identité de l'objet.
# Ce n'est pas très utilisé avec les types primitifs, mais cela peut être très utile
# lorsque l'on utilise des objets.

# None, 0, et les chaînes de caractères vides valent False.
# Toutes les autres valeurs valent True
0 == False  #=> True
"" == False #=> True


####################################################
## 2. Variables et Collections
####################################################

# Afficher du texte, c'est facile
print "Je suis Python. Enchanté!"


# Il n'y a pas besoin de déclarer les variables avant de les assigner.
some_var = 5    # La convention veut que l'on utilise des minuscules_avec_underscores
some_var #=> 5

# Accéder à une variable non assignée lève une exception
# Voyez les structures de contrôle pour en apprendre plus sur la gestion des exceptions.
some_other_var  # Lève une exception

# 'if' peut être utilisé comme expression
"yahoo!" if 3 > 2 else 2 #=> "yahoo!"

# Listes
li = []
# On peut remplir liste dès l'instanciation
other_li = [4, 5, 6]

# On ajoute des éléments avec 'append'
li.append(1)    #li contient [1]
li.append(2)    #li contient [1, 2]
li.append(4)    #li contient [1, 2, 4]
li.append(3)    #li contient [1, 2, 4, 3]

# Et on les supprime avec 'pop'
li.pop()        #=> 3 et li contient [1, 2, 4]
# Remettons-le dans la liste
li.append(3)    # li contient [1, 2, 4, 3] de nouveau.

# On accède aux éléments d'une liste comme à ceux un tableau.
li[0] #=> 1
# Le dernier élément
li[-1] #=> 3

# Accèder aux indices hors limite lève une exception
li[4] # Lève un 'IndexError'

# On peut accèder à des rangs de valeurs avec la syntaxe "slice"
# (C'est un rang de type 'fermé/ouvert' pour les plus matheux)
li[1:3] #=> [2, 4]
# Sans spécifier de fin de rang, on "saute" le début de la liste
li[2:] #=> [4, 3]
# Sans spécifier de début de rang, on "saute" la fin de la liste
li[:3] #=> [1, 2, 4]

# Retirer un élément spécifique dee la liste avec "del"
del li[2] # li contient [1, 2, 3]

# On peut additionner des listes entre elles
li + other_li #=> [1, 2, 3, 4, 5, 6] - Note: li et other_li existent toujours à part entière

# Concaténer des listes avec "extend()"
li.extend(other_li) # li vaut maintenant [1, 2, 3, 4, 5, 6]

# Vérifier l'existence d'un élément dans une liste avec "in"
1 in li #=> True

# Récupérer la longueur avec "len()"
len(li) #=> 6


# Les "tuples" sont comme des listes, mais sont immuables.
tup = (1, 2, 3)
tup[0] #=> 1
tup[0] = 3  # Lève un 'TypeError'

# Mais vous pouvez faire tout ceci sur les tuples:
len(tup) #=> 3
tup + (4, 5, 6) #=> (1, 2, 3, 4, 5, 6)
tup[:2] #=> (1, 2)
2 in tup #=> True

# Vous pouvez "dé-packager" les tuples (ou les listes) dans des variables
a, b, c = (1, 2, 3)     # a vaut maintenant 1, b vaut maintenant 2 and c vaut maintenant 3
# Sans parenthèses, un tuple est créé par défaut
d, e, f = 4, 5, 6
# Voyez maintenant comme il est facile d'inverser 2 valeurs
e, d = d, e     # d is now 5 and e is now 4


# Dictionnaires
empty_dict = {}
# Un dictionnaire pré-rempli
filled_dict = {"one": 1, "two": 2, "three": 3}

# Trouver des valeurs avec []
filled_dict["one"] #=> 1

# Récupérer toutes les clés sous forme de liste avec "keys()"
filled_dict.keys() #=> ["three", "two", "one"]
# Note - l'ordre des clés du dictionnaire n'est pas garanti.
# Vos résultats peuvent différer de ceux ci-dessus.

# Récupérer toutes les valeurs sous forme de liste avec "values()"
filled_dict.values() #=> [3, 2, 1]
# Note - Même remarque qu'au-dessus concernant l'ordre des valeurs.

# Vérifier l'existence d'une clé dans le dictionnaire avec "in"
"one" in filled_dict #=> True
1 in filled_dict #=> False

# Chercher une clé non existante lève une 'KeyError'
filled_dict["four"] # KeyError

# Utiliser la méthode "get()" pour éviter 'KeyError'
filled_dict.get("one") #=> 1
filled_dict.get("four") #=> None
# La méthode get() prend un argument par défaut quand la valeur est inexistante
filled_dict.get("one", 4) #=> 1
filled_dict.get("four", 4) #=> 4

# La méthode "setdefault()" permet d'ajouter de manière sécuris une paire clé-valeur dans le dictionnnaire
filled_dict.setdefault("five", 5) #filled_dict["five"] vaut 5
filled_dict.setdefault("five", 6) #filled_dict["five"] is toujours 5


# Les sets stockent ... des sets
empty_set = set()
# On initialise un "set()" avec tout un tas de valeurs
some_set = set([1,2,2,3,4]) # some_set vaut maintenant set([1, 2, 3, 4])

# Depuis Python 2.7, {} peut être utilisé pour déclarer un 'set'
filled_set = {1, 2, 2, 3, 4} # => {1 2 3 4}

# Ajouter plus d'éléments au set
filled_set.add(5) # filled_set contient maintenant {1, 2, 3, 4, 5}

# Intersection de sets avec &
other_set = {3, 4, 5, 6}
filled_set & other_set #=> {3, 4, 5}

# Union de sets avec |
filled_set | other_set #=> {1, 2, 3, 4, 5, 6}

# Différence de sets avec -
{1,2,3,4} - {2,3,5} #=> {1, 4}

# Vérifier l'existence d'une valeur dans un set avec "in"
2 in filled_set #=> True
10 in filled_set #=> False


####################################################
## 3. Structure de contrôle
####################################################

# Initialisons une variable
some_var = 5

# Voici une condition 'if'. L'indentation est significative en Python !
# Affiche "some_var est inférieur à 10"
if some_var > 10:
    print "some_var est supérieur à 10."
elif some_var < 10:    # La clause elif est optionnelle
    print "some_var iinférieur à 10."
else:           # La clause else également
    print "some_var vaut 10."


"""
Les boucles "for" permettent d'itérer sur les listes
Affiche:
    chien : mammifère
    chat : mammifère
    souris : mammifère
"""
for animal in ["chien", "chat", "souris"]:
    # On peut utiliser % pour l'interpolation des chaînes formattées
    print "%s : mammifère" % animal

"""
"range(number)" retourne une liste de nombres
de 0 au nombre donné
Affiche:
    0
    1
    2
    3
"""
for i in range(4):
    print i

"""
Les boucles "while" boucle jusqu'à ce que leur condition ne soit plus vraie
Affiche:
    0
    1
    2
    3
"""
x = 0
while x < 4:
    print x
    x += 1  # Raccourci pour x = x + 1

# Gérer les exceptions avec un bloc try/except

# Fonctionne pour Python 2.6 et ultérieur:
try:
    # Utiliser "raise" pour lever une exception
    raise IndexError("This is an index error")
except IndexError as e:
    pass    # Pass ne prend pas d'arguments. Généralement, on gère l'erreur ici.


####################################################
## 4. Fonctions
####################################################

# Utiliser "def" pour créer une nouvelle fonction
def add(x, y):
    print "x vaut %s et y vaur %s" % (x, y)
    return x + y    # Renvoi de valeur avec 'return'

# Appeller une fonction avec des paramètres
add(5, 6) #=> Affichet "x is 5 et y vaut 6" et renvoie 11

# Une autre manière d'appeller une fonction, avec les arguments
add(y=6, x=5)   # Les arguments peuvent venir dans n'importe quel ordre.

# On peut définir une foncion qui prend un nombre variable de paramètres
def varargs(*args):
    return args

varargs(1, 2, 3) #=> (1,2,3)


# On peut également définir une fonction qui prend un nombre
# variable d'arguments
def keyword_args(**kwargs):
    return kwargs

# Appelons-là et voyons ce qu'il se passe
keyword_args(big="foot", loch="ness") #=> {"big": "foot", "loch": "ness"}

# On peut faire les deux à la fois si on le souhaite
def all_the_args(*args, **kwargs):
    print args
    print kwargs
"""
all_the_args(1, 2, a=3, b=4) affiche:
    (1, 2)
    {"a": 3, "b": 4}
"""

# En appellant les fonctions, on peut faire l'inverse des paramètres / arguments !
# Utiliser * pour développer les paramètres, et ** pour développer les arguments
params = (1, 2, 3, 4)
args = {"a": 3, "b": 4}
all_the_args(*args) # equivaut à foo(1, 2, 3, 4)
all_the_args(**kwargs) # equivaut à foo(a=3, b=4)
all_the_args(*args, **kwargs) # equivaut à foo(1, 2, 3, 4, a=3, b=4)

# Python a des fonctions de première classe
def create_adder(x):
    def adder(y):
        return x + y
    return adder

add_10 = create_adder(10)
add_10(3) #=> 13

# Mais également des fonctions anonymes
(lambda x: x > 2)(3) #=> True

# On trouve aussi des fonctions intégrées plus évoluées
map(add_10, [1,2,3]) #=> [11, 12, 13]
filter(lambda x: x > 5, [3, 4, 5, 6, 7]) #=> [6, 7]

# On peut utiliser la syntaxe des liste pour construire les "maps" et les "filters"
[add_10(i) for i in [1, 2, 3]]  #=> [11, 12, 13]
[x for x in [3, 4, 5, 6, 7] if x > 5] #=> [6, 7]

####################################################
## 5. Classes
####################################################

# Une classe est un objet
class Human(object):

    # Un attribut de classe. Il est partagé par toutes les instances de cette classe.
    species = "H. sapiens"

    # Initialiseur basique
    def __init__(self, name):
        # Assigne le paramètre à l'attribut de l'instance de classe.
        self.name = name

    # Une méthode de l'instance. Toutes les méthodes prennent "self" comme 1er paramètre.
    def say(self, msg):
       return "%s: %s" % (self.name, msg)

    # Une méthode de classe est partagée par toutes les instances.
    # On les appelle avec le nom de la classe en premier paramètre
    @classmethod
    def get_species(cls):
        return cls.species

    # Une méthode statique est appellée sans référence à une classe ou à une instance 
    @staticmethod
    def grunt():
        return "*grunt*"


# Instancier une classe
i = Human(name="Ian")
print i.say("hi")     # Affiche "Ian: hi"

j = Human("Joel")
print j.say("hello")  #Affiche "Joel: hello"

# Appeller notre méthode de classe
i.get_species() #=> "H. sapiens"

# Changer les attributs partagés
Human.species = "H. neanderthalensis"
i.get_species() #=> "H. neanderthalensis"
j.get_species() #=> "H. neanderthalensis"

# Appeller la méthode statique
Human.grunt() #=> "*grunt*"


####################################################
## 6. Modules
####################################################

# On peut importer des modules
import math
print math.sqrt(16) #=> 4.0

# Et récupérer des fonctions spécifiques d'un module
from math import ceil, floor
print ceil(3.7)  #=> 4.0
print floor(3.7) #=> 3.0

# Récuperer toutes les fonctions d'un module
# Attention, ce n'est pas recommandé.
from math import *

# On peut raccourcir le nom d'un module
import math as m
math.sqrt(16) == m.sqrt(16) #=> True

# Les modules Python sont juste des fichiers Python ordinaires.
# On peut écrire ses propres modules et les importer.
# Le nom du module doit être le même que le nom du fichier.

# On peut trouver quelle fonction et attributs déterminent un module
import math
dir(math)


```

## Prêt à aller plus loin?

### En ligne gratuitement

* [Learn Python The Hard Way](http://learnpythonthehardway.org/book/)
* [Dive Into Python](http://www.diveintopython.net/)
* [The Official Docs](http://docs.python.org/2.6/)
* [Hitchhiker's Guide to Python](http://docs.python-guide.org/en/latest/)
* [Python Module of the Week](http://pymotw.com/2/)

### Format papier

* [Programming Python](http://www.amazon.com/gp/product/0596158106/ref=as_li_qf_sp_asin_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0596158106&linkCode=as2&tag=homebits04-20)
* [Dive Into Python](http://www.amazon.com/gp/product/1441413022/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=1441413022&linkCode=as2&tag=homebits04-20)
* [Python Essential Reference](http://www.amazon.com/gp/product/0672329786/ref=as_li_tf_tl?ie=UTF8&camp=1789&creative=9325&creativeASIN=0672329786&linkCode=as2&tag=homebits04-20)

