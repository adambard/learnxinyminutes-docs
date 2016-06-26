---
language: coffeescript
contributors:
  - ["Tenor Biel", "http://github.com/L8D"]
translators:
  - ["Geoffrey Roguelon", "https://github.com/GRoguelon"]
  - ["Thibault", "https://github.com/napnac"]
lang: fr-fr
filename: coffeescript-fr.coffee
---

CoffeeScript est un langage préprocesseur, il permet de générer du JavaScript. En tant que l'un des successeurs de JavaScript, CoffeeScript se doit de générer un fichier de sortie agréable à lire, et rapide à exécuter.

Le [site officiel](http://coffeescript.org/) de CoffeScript contient un tutoriel complet sur le langage.

``` coffeescript
# CoffeeScript suit les tendances de certains langages récents.
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

# Intervalles :
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
#=>race = function() {
#    var runners, winner;
#    winner = arguments[0], runners = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
#    return print(winner, runners);
#  };

# Existance :
alert "I knew it!" if elvis?
#=> if(typeof elvis !== "undefined" && elvis !== null) { alert("I knew it!"); }

# Lecture d'un tableau :
cubes = (math.cube num for num in list)
#=>cubes = (function() {
#         var _i, _len, _results;
#         _results = [];
#       for (_i = 0, _len = list.length; _i < _len; _i++) {
#                 num = list[_i];
#                 _results.push(math.cube(num));
#         }
#         return _results;
# })();

foods = ['broccoli', 'spinach', 'chocolate']
eat food for food in foods when food isnt 'chocolate'
#=>foods = ['broccoli', 'spinach', 'chocolate'];
#
#for (_k = 0, _len2 = foods.length; _k < _len2; _k++) {
#  food = foods[_k];
#  if (food !== 'chocolate') {
#    eat(food);
#  }
#}
```

## Ressources supplémentaires

- [Smooth CoffeeScript](http://autotelicum.github.io/Smooth-CoffeeScript/)
- [CoffeeScript Ristretto](https://leanpub.com/coffeescript-ristretto/read)
