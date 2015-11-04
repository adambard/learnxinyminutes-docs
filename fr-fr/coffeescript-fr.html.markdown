---
language: coffeescript
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
translators:
  - ["Geoffrey Roguelon", "https://github.com/GRoguelon"]
lang: fr-fr
filename: coffeescript-fr.coffee
---

``` coffeescript
# CoffeeScript est un langage préprocesseur, il permet de générer du Javascript.
# Il suit les tendances de certains langages récents.
# Par exemple, les commentaires se définissent comme en Ruby ou en Python.

###
Ceci est un bloc de commentaires
il est converti directement avec '/ *' et '* /'
pour correspondre aux commentaires Javascript

Vous devez comprendre la syntaxe du langage JavaScript pour continuer.
###

# Affectation :
number   = 42 #=> var number = 42;
opposite = true #=> var opposite = true;

# Structures de contrôle :
number = -42 if opposite #=> if(opposite) { number = -42; }

# Fonctions :
square = (x) -> x * x #=> var square = function(x) { return x * x; }

# Intervals :
list = [1..5] #=> var list = [1, 2, 3, 4, 5];

# Objets :
math =
  root:   Math.sqrt
  square: square
  cube:   (x) -> x * square x
#=> var math = {
#  "root": Math.sqrt,
#  "square": square,
#  "cube": function(x) { return x * square(x); }
#}

# Liste d'arguments variables :
race = (winner, runners...) ->
  print winner, runners

# Existance :
alert "I knew it!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null) { alert("I knew it!"); }

# Lecture d'un tableau :
cubes = (math.cube num for num in list) #=> ...
```
