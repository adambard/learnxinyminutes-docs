---
language: LiveScript
filename: learnLivescript-fr.ls
contributors:
    - ["Christina Whyte", "http://github.com/kurisuwhyte/"]
translators:
    - ["Morgan Bohn", "https://github.com/dotmobo"]
lang: fr-fr
---

LiveScript est un langage qui compile en JavaScript. Il a un rapport direct 
avec JavaScript, et vous permet d'écrire du JavaScript plus simplement, plus 
efficacement et sans répétitivité. LiveScript ajoute non seulement des 
fonctionnalités pour écrire du code fonctionnel, mais possède aussi nombre 
d'améliorations pour la programmation orientée objet et la programmation 
impérative.

LiveScript est un descendant direct de [Coco][], indirect de [CoffeeScript][],
avec lequel il a beaucoup plus de compatibilité.

[Coco]: http://satyr.github.io/coco/
[CoffeeScript]: http://coffeescript.org/

Vous pouvez contacter l'auteur du guide original en anglais ici :
[@kurisuwhyte](https://twitter.com/kurisuwhyte)


```coffeescript
# Comme son cousin CoffeeScript, LiveScript utilise le symbole dièse pour les 
# commentaires sur une ligne.

/*
 Les commentaires sur plusieurs lignes utilisent la syntaxe du C. Utilisez-les
 si vous voulez préserver les commentaires dans la sortie JavaScript.
 */
```
```coffeescript
# LiveScript utilise l'indentation pour délimiter les blocs de code plutôt que 
# les accolades, et les espaces pour appliquer les fonctions (bien que les 
# parenthèses soient utilisables).


########################################################################
## 1. Valeurs basiques
########################################################################

# Les valeurs non définies sont représentées par le mot clé `void` à la place de
# `undefined`
void            # comme `undefined` mais plus sûr (ne peut pas être redéfini)

# Une valeur non valide est représentée par Null.
null


# Les booléens s'utilisent de la façon suivante:
true
false

# Et il existe divers alias les représentant également:
on; off
yes; no


# Puis viennent les nombres entiers et décimaux.
10
0.4     # Notez que le `0` est requis

# Dans un souci de lisibilité, vous pouvez utiliser les tirets bas et les 
# suffixes sur les nombres. Il seront ignorés à la compilation.
12_344km


# Les chaînes sont des séquences immutables de caractères, comme en JS:
"Christina"             # Les apostrophes fonctionnent également!
"""Multi-line
   strings
   are
   okay
   too."""

# De temps à autre, vous voulez encoder un mot clé; la notation en backslash 
# rend cela facile:
\keyword                # => 'keyword'


# Les tableaux sont des collections ordonnées de valeurs.
fruits =
  * \apple
  * \orange
  * \pear

# Il peuvent être écrits de manière plus consises à l'aide des crochets:
fruits = [ \apple, \orange, \pear ]

# Vous pouvez également utiliser la syntaxe suivante, à l'aide d'espaces, pour 
# créer votre liste de valeurs:
fruits = <[ apple orange pear ]>

# Vous pouvez récupérer une entrée à l'aide de son index:
fruits[0]       # => "apple"

# Les objets sont une collection non ordonnées de paires clé/valeur, et 
# d'autres choses (que nous verrons plus tard).
person =
  name: "Christina"
  likes:
    * "kittens"
    * "and other cute stuff"

# A nouveau, vous pouvez utiliser une expression plus consise à l'aide des 
# accolades:
person = {name: "Christina", likes: ["kittens", "and other cute stuff"]}

# Vous pouvez récupérer une entrée via sa clé:
person.name     # => "Christina"
person["name"]  # => "Christina"


# Les expressions régulières utilisent la même syntaxe que JavaScript:
trailing-space = /\s$/          # les mots-composés deviennent motscomposés

# A l'exception que vous pouvez pouvez utiliser des expressions sur plusieurs
# lignes!
# (les commentaires et les espaces seront ignorés)
funRE = //
        function\s+(.+)         # nom
        \s* \((.*)\) \s*        # arguments
        { (.*) }                # corps
        //


########################################################################
## 2. Les opérations basiques
########################################################################

# Les opérateurs arithmétiques sont les mêmes que pour JavaScript:
1 + 2   # => 3
2 - 1   # => 1
2 * 3   # => 6
4 / 2   # => 2
3 % 2   # => 1


# Les comparaisons sont presque identiques, à l'exception que `==` équivaut au
# `===` de JS, là où le `==` de JS est `~=` en LiveScript, et `===` active la 
# comparaison d'objets et de tableaux, ainsi que les comparaisons strictes 
# (sans conversion de type)
2 == 2          # => true
2 == "2"        # => false
2 ~= "2"        # => true
2 === "2"       # => false

[1,2,3] == [1,2,3]        # => false
[1,2,3] === [1,2,3]       # => true

+0 == -0     # => true
+0 === -0    # => false

# Les opérateurs suivants sont également disponibles: <, <=, > et >=

# Les valeurs logiques peuvent être combinéees grâce aux opérateurs logiques 
# `or`, `and` et `not`
true and false  # => false
false or true   # => true
not false       # => true


# Les collections ont également des opérateurs additionnels
[1, 2] ++ [3, 4]                # => [1, 2, 3, 4]
'a' in <[ a b c ]>              # => true
'name' of { name: 'Chris' }     # => true


########################################################################
## 3. Fonctions
########################################################################        

# Puisque LiveScript est fonctionnel, vous vous attendez à une bonne prise en 
# charge des fonctions. En LiveScript, il est encore plus évident que les 
# fonctions sont de premier ordre:
add = (left, right) -> left + right
add 1, 2        # => 3

# Les fonctions qui ne prennent pas d'arguments peuvent être appelées avec un 
# point d'exclamation!
two = -> 2
two!

# LiveScript utilise l'environnement de la fonction, comme JavaScript.
# A l'inverse de JavaScript, le `=` fonctionne comme un opérateur de 
# déclaration, et il déclarera toujours la variable située à gauche (sauf si
# la variable a été déclarée dans l'environnement parent). 

# L'opérateur `:=` est disponible pour réutiliser un nom provenant de 
# l'environnement parent.


# Vous pouvez extraire les arguments d'une fonction pour récupérer 
# rapidement les valeurs qui vous intéressent dans une structure de données 
# complexe:
tail = ([head, ...rest]) -> rest
tail [1, 2, 3]  # => [2, 3]

# Vous pouvez également transformer les arguments en utilisant les opérateurs
# binaires et unaires. Définir des arguments par défaut est aussi possible.
foo = (a = 1, b = 2) -> a + b
foo!    # => 3

# You pouvez utiliser cela pour cloner un argument en particulier pour éviter 
# les effets secondaires. Par exemple:
copy = (^^target, source) ->
  for k,v of source => target[k] = v
  target
a = { a: 1 }
copy a, { b: 2 }        # => { a: 1, b: 2 }
a                       # => { a: 1 }


# Une fonction peut être curryfiée en utilisant une longue flèche à la place
# d'une courte:
add = (left, right) --> left + right
add1 = add 1
add1 2          # => 3

# Les fonctions ont un argument `it` implicite si vous n'en déclarez pas:
identity = -> it
identity 1      # => 1

# Les opérateurs ne sont pas des fonctions en LiveScript, mais vous pouvez 
# facilement les transformer en fonction:
divide-by-two = (/ 2)
[2, 4, 8, 16].map(divide-by-two).reduce (+)

# Comme dans tout bon langage fonctionnel, vous pouvez créer des fonctions 
# composées d'autres fonctions:
double-minus-one = (- 1) . (* 2)

# En plus de la formule mathématique `f . g`, vous avez les opérateurs `>>`
# et `<<`, qui décrivent l'ordre d'application des fonctions composées. 
double-minus-one = (* 2) >> (- 1)
double-minus-one = (- 1) << (* 2)


# Pour appliquer une valeur à une fonction, vous pouvez utiliser les opérateurs
# `|>` et `<|`:
map = (f, xs) --> xs.map f
[1 2 3] |> map (* 2)            # => [2 4 6]

# La version sans pipe correspond à:
((map (* 2)) [1, 2, 3])

# You pouvez aussi choisir où vous voulez que la valeur soit placée, en 
# marquant la position avec un tiret bas (_):
reduce = (f, xs, initial) --> xs.reduce f, initial
[1 2 3] |> reduce (+), _, 0     # => 6


# Le tiret bas est également utilisé pour l'application partielle,
# que vous pouvez utiliser pour toute fonction:
div = (left, right) -> left / right
div-by-two = div _, 2
div-by-two 4      # => 2


# Pour conclure, LiveScript vous permet d'utiliser les fonctions de rappel.
# (mais vous devriez essayer des approches plus fonctionnelles, comme 
# Promises).
# Un fonction de rappel est une fonction qui est passée en argument à une autre
# fonction:
readFile = (name, f) -> f name
a <- readFile 'foo'
b <- readFile 'bar'
console.log a + b

# Equivalent à:
readFile 'foo', (a) -> readFile 'bar', (b) -> console.log a + b


########################################################################
## 4. Conditionnalités
########################################################################

# Vous pouvez faire de la conditionnalité à l'aide de l'expression `if...else`:
x = if n > 0 then \positive else \negative

# A la place de `then`, vous pouvez utiliser `=>`
x = if n > 0 => \positive
    else        \negative

# Pour les conditions complexes, il vaut mieux utiliser l'expresssion `switch`:
y = {}
x = switch
  | (typeof y) is \number => \number
  | (typeof y) is \string => \string
  | 'length' of y         => \array
  | otherwise             => \object      # `otherwise` et `_` correspondent.

# Le corps des fonctions, les déclarations et les assignements disposent d'un
# `switch` implicite, donc vous n'avez pas besoin de le réécrire: 
take = (n, [x, ...xs]) -->
    | n == 0 => []
    | _      => [x] ++ take (n - 1), xs


########################################################################
## 5. Compréhensions
########################################################################

# Comme en python, vous allez pouvoir utiliser les listes en compréhension,
# ce qui permet de générer rapidement et de manière élégante une liste de 
# valeurs:
oneToTwenty = [1 to 20]
evens       = [x for x in oneToTwenty when x % 2 == 0]

# `when` et `unless` peuvent être utilisés comme des filtres.

# Cette technique fonctionne sur les objets de la même manière. Vous allez
# pouvoir générer l'ensemble de paires clé/valeur via la syntaxe suivante:
copy = { [k, v] for k, v of source }


########################################################################
## 4. Programmation orientée objet
########################################################################

# Bien que LiveScript soit un langage fonctionnel, il dispose d'intéressants
# outils pour la programmation objet. La syntaxe de déclaration d'une classe
# est héritée de CoffeeScript:
class Animal
  (@name, kind) ->
    @kind = kind
  action: (what) -> "*#{@name} (a #{@kind}) #{what}*"

class Cat extends Animal
  (@name) -> super @name, 'cat'
  purr: -> @action 'purrs'

kitten = new Cat 'Mei'
kitten.purr!      # => "*Mei (a cat) purrs*"

# En plus de l'héritage classique, vous pouvez utiliser autant de mixins
# que vous voulez pour votre classe. Les mixins sont juste des objets:
Huggable =
  hug: -> @action 'is hugged'

class SnugglyCat extends Cat implements Huggable

kitten = new SnugglyCat 'Purr'
kitten.hug!     # => "*Mei (a cat) is hugged*"
```

## Lectures complémentaires

Il y a beaucoup plus de choses à dire sur LiveScript, mais ce guide devrait 
suffire pour démarrer l'écriture de petites fonctionnalités.
Le [site officiel](http://livescript.net/) dispose de beaucoup d'information,
ainsi que d'un compilateur en ligne vous permettant de tester le langage!

Jetez également un coup d'oeil à [prelude.ls](http://gkz.github.io/prelude-ls/),
et consultez le channel `#livescript` sur le réseau Freenode.
